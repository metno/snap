from enum import Enum
from collections import namedtuple
import csv
import numpy
import io
import typing

_ExplosionType = namedtuple('ExplosionType', ['value', 'radius_sizes', "size_distribution"])

class ExplosionType(Enum):    
    @property
    def radius_sizes(self):
        return self.value.radius_sizes

    @property
    def size_distribution(self):
        return self.value.size_distribution

    # default snap
    MIXED = _ExplosionType(0,
                           [2.2, 4.4, 8.6, 14.6, 22.8, 36.1, 56.5, 92.3, 173.2, 250.0],
                           [ .1,  .1,  .1,   .1,   .1,   .1,   .1,   .1,    .1,    .1]
                           )
    # TBD
    SURFACE = _ExplosionType(0,
                           [2.2, 4.4, 8.6, 14.6, 22.8, 36.1, 56.5, 92.3, 173.2, 250.0],
                           [ .1,  .1,  .1,   .1,   .1,   .1,   .1,   .1,    .1,    .1] 
                           )
    # TBD
    HIGH_ALTITUDE = _ExplosionType(0,
                           [2.2, 4.4, 8.6, 14.6, 22.8, 36.1, 56.5, 92.3, 173.2, 250.0],
                           [ .1,  .1,  .1,   .1,   .1,   .1,   .1,   .1,    .1,    .1] 
                           )

    @classmethod
    def by_argosname(cls, name: str):
        """get a ExplosionType by name used in Argos. Unknown names will be translated to MIXED"""
        name = name.lower().strip()
        if name == 'surface':
            return ExplosionType.SURFACE
        elif name == '1000 meters':
            return ExplosionType.HIGH_ALTITUDE
        else:
            return ExplosionType.MIXED


def _lin_interpol(a0,a,b, x,y):
    '''linear interpolation of x=f(a), y=f(b) to f(a0)'''
    if (a == b):
        return x
    else:
        return x + (a0-a)*(y-x)/(b-a)

class YieldParameters():
    """
    class translating yield parameters depending on ExplosionType
    """
    _cloud_defs1 = '''yield	bottom	top	thickness
0	0	0	0
0.5	1500	2250	750
1	3000	4000	1000
2	3389	4611	1222
3	3778	5222	1444
4	4167	5833	1667
5	4556	6444	1889
6	4944	7056	2111
7	5333	7667	2333
8	5722	8278	2556
9	6111	8889	2778
10	6500	9500	3000
15	6500	9500	3000
20	6500	9500	3000
25	6500	9500	3000
30	6500	9500	3000
'''

    def __init__(self, nuclear_yield: float = 15, explosion_type: ExplosionType = ExplosionType.MIXED) -> None:
        self._nuclear_yield = nuclear_yield
        self._explosion_type = explosion_type
        self._cloud_defs = self._parse_clouds(io.StringIO(self._cloud_defs1))
        return
    
    @property
    def nuclear_yield(self):
        return self._nuclear_yield

    @property
    def explosion_type(self):
        return self._explosion_type


    def _parse_clouds(self, cloud_def: typing.TextIO) -> dict:
        '''ensure that input-file is ordered by yield'''
        retval = {
            'yield': [],
            'bottom': [],
            'top': []
        }
        reader = csv.DictReader(cloud_def, delimiter="\t")
        for row in reader:
            for tag in retval.keys():
                retval[tag].append(float(row[tag]))
        for tag in retval.keys():
            retval[tag] = numpy.asarray(retval[tag])
        return retval
    
    def _get_linear_cloud_def(self, tag) -> dict:
        '''get a dict of cloud bottom, top, and radius and stemradius depending on yield'''
        cloudyield = self._cloud_defs['yield']
        pos = numpy.argmin(numpy.abs(cloudyield-self._nuclear_yield))
        # linear interpolation
        if cloudyield[pos] > self._nuclear_yield:
            pos1 = pos - 1
            if pos1 < 0:
                pos1 = 0
        else:
            pos1 = pos + 1
            if pos1 >= cloudyield.shape[0]:
                pos1 = pos
        return _lin_interpol(self._nuclear_yield, cloudyield[pos], cloudyield[pos1],
                            self._cloud_defs[tag][pos], self._cloud_defs[tag][pos1])


    def activity_after_1h(self) -> float:
        """
        Get the total activity for relatively long-lived isotopes
        """
        # formula from 2^19Bq/kT(TNT) as of
        # Fission Products from Nuclear Weapons Explosions (Tovedal)
        # https://inis.iaea.org/collection/NCLCollectionStore/_Public/32/055/32055989.pdf 
        return self._nuclear_yield * 2e19

    def cloud_bottom(self):
        '''cloud bottom in m'''
        return self._get_linear_cloud_def('bottom')
    def cloud_top(self):
        '''cloud top in m'''
        return self._get_linear_cloud_def('top')
    def cloud_radius(self):
        '''cloud radius in m'''
        # Typical radius of mushroom cloud after ~ 10 - 15 min is 1-3 km seen from different studies (Kanarska et al., 2009, Arthur et al., 2021) 
        return 2500.
    def stem_radius(self):
        '''cloud radius in m'''
        # Typical radius of mushroom cloud after ~ 10 - 15 min is 1-3 km seen from different studies (Kanarska et al., 2009, Arthur et al., 2021) 
        return 0.
    




class SnapInputBomb():
    '''
    Description of the bomb-part of a snap.input file
    Excluding meteorology and start-position
    '''
    def __init__(self, nuclear_yield: float = 15, explosion_type: ExplosionType = ExplosionType.MIXED) -> None:
        """
        Parameters
        ----------
        nuclear_yield : float, optional
            nuclear yield of explosion in kilotonnes TNT, default 15
        explosion_type : ExplosionType, optional
            type of explosion (defines size distributions)
        """
        self.set_bomb(nuclear_yield, explosion_type)
        # _yield_parameters
        # _radius_sizes
        # _size_distribution
        self._component_basename = 'Aerosol'
        self._component_formatter = "{component}_{size:.1f}mym"
        self._gravity = [] # might be empty array or fixed gravity per size
        self._default_density = 2.95 # general rock/sand density
        self._densities = [] # might be empty array of densities per size
        self._minutes = 0. # explosion starts minutes after full hour

        return
    
    @property
    def nuclear_yield(self) -> float:
        """nuclear yield in Mg(=kTonnes) TNT"""
        return self._yield_parameters.nuclear_yield
    
    @property
    def explosion_type(self) -> ExplosionType:
        return self._yield_parameters.explosion_type

    def set_bomb(self, nuclear_yield: float, explosion_type: ExplosionType = None) -> None:
        """
        set yield, sizes, size_distribution and activity base on nuclear_yield and explosion_type
        """
        self._yield_parameters = YieldParameters(nuclear_yield, explosion_type)

        self.radius_sizes = self.explosion_type.radius_sizes
        self.size_distribution = self.explosion_type.size_distribution
        return None

    @property
    def activity_after_1h(self) -> float:
        """
        Get the total activity for relatively long-lived isotopes
        """
        return self._yield_parameters.activity_after_1h()

    @property
    def cloud_bottom(self) -> float:
        """cloud bottom height in m"""
        return self._yield_parameters.cloud_bottom()
    
    @property
    def cloud_top(self) -> float:
        """cloud top height in m"""
        return self._yield_parameters.cloud_top()

    @property
    def cloud_radius(self) -> float:
        """cloud top height in m"""
        return self._yield_parameters.cloud_radius()

    @property
    def stem_radius(self) -> float:
        """cloud top height in m"""
        return self._yield_parameters.stem_radius()
    
    @property
    def total_activity(self) -> float:
        """total activity of cloud and stem in Bq"""
        return self._total_activity

    @property
    def component_basename(self) -> str:
        """name of the component used in snap"""
        return self._component_basename
    
    def component_name(self, pos: int) -> str:
        """name of component of size[pos] in SNAP"""
        return self._component_formatter.format(component=self.component_basename,
                                                size=self.radius_sizes[pos])

    @property
    def minutes(self) -> float:
        """offset in minutes the explosion start after start of SNAP run (starting at full hour)"""
        return self._minutes
    
    @minutes.setter
    def minutes(self, minutes: float) -> None:
        self._minutes = minutes
        return

    @property
    def radius_sizes(self) -> list[float]:
        """retrieve a list of radius sizes in µm for different size-classes"""
        return self._radius_sizes
        
    @radius_sizes.setter
    def radius_sizes(self, radius_sizes: list[float]) -> None:
        """
        Set a list of radius sizes in µm for different size-classes.
        This method also resets the size-distribution to an equal distribution.
        """
        self._radius_sizes = radius_sizes
        self._size_distribution = []
        return
        
    @property
    def size_distribution(self) -> list[float]:
        """
        retrieve a list of fractions per radius_sizes.
        ensures that the sum of size_distribution equals to 1
        """
        sum_dist = 0
        size_dist = []
        l = min(len(self.radius_sizes), len(self._size_distribution))
        for i in range(l):
            nextsum = sum_dist + self._size_distribution[i]
            if nextsum <= 1:
                size_dist.append(self._size_distribution[i])
                sum_dist = nextsum
            else:
                size_dist.append(1-sum_dist)
                sum_dist = 1
        # append equal size_distribution for the remaining parts
        extras = len(self._radius_sizes) - len(self._size_distribution)
        if extras > 0:
            frac = (1-sum_dist)/extras
            for i in range(extras):
                size_dist.append(frac)            
        # assertion
        sum_size_dist = sum(size_dist)
        if abs(1-sum_size_dist) > 0.01:
            raise Exception(f"sum of size_dist == {sum_size_dist} != 1: {size_dist}")
        self._size_distribution = size_dist
        return self._size_distribution

    @size_distribution.setter
    def size_distribution(self, size_distribution: list[float]) -> None:
        if (sum(size_distribution) > 1):
            raise Exception(f"size_distribution > 1: {size_distribution}")
        self._size_distribution = size_distribution
        return

    @property
    def default_density(self) -> float:
        '''density in g/cm³ used for all particle classes unless specified otherwise'''
        return self._default_density
    
    @default_density.setter
    def default_density(self, default_density: float) -> None:
        '''set the default density in g/cm^3'''
        self._default_density = default_density
        return

    @property
    def densities(self) -> list[float]:
        ''' list of densities in g/cm^3 for each size-class, uses default_density if none given'''
        '''
        set the list of densities in g/cm^3 for each size-classes

        a 0 or undefined density will give the default_density
        a negative density will disable gravity
        '''
        retlist = []
        for i in range(len(self.radius_sizes)):
            if i >= len(self._densities):
                retlist.append(self.default_density)
            else:
                dens = self._densities[i]
                if dens == 0:
                    retlist.append(self.default_density)
                else:
                    retlist.append(dens)
        return retlist

    @densities.setter
    def densities(self, densities: list[float]):
        self._densities = densities
        return


    def snap_input(self) -> str:
        """get the bomb-input as partial snap.input string"""
        lines = []
        lines.append('MAX.PARTICLES.PER.RELEASE= 1000000\n')

        lines.append(f"** Explosive yield {self.nuclear_yield}ktonnes")
        lines.append("TIME.RELEASE.PROFILE.BOMB")
        relradius = []
        rellower = []
        relupper = []
        relstem = []
        activity = []
        relmins = [0]
        if self.minutes > 0:
            relmins.append(self.minutes)
            relradius.append(0)
            rellower.append(0)
            relupper.append(0)
            relstem.append(0)
            activity.append("0.")
        relradius.append(self.cloud_radius)
        rellower.append(self.cloud_bottom)
        relupper.append(self.cloud_top)
        relstem.append(self.stem_radius)

        lines.append(f"""
RELEASE.MINUTE= {",".join(map(str, relmins))}
RELEASE.RADIUS.M= {",".join(map(str, relradius))}
RELEASE.LOWER.M= {",".join(map(str, rellower))}
RELEASE.UPPER.M= {",".join(map(str, relupper))}
RELEASE.MUSHROOM.STEM.RADIUS.M= {",".join(map(str, relstem))}
                     """)

        lines.append('* PARTICLE CLASSES')
        pclass_tmpl = """COMPONENT= {classname}
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.BOMB
RADIUS.MICROMETER= {radius}
DENSITY.G/CM3={density}
{gravity}
FIELD.IDENTIFICATION={identification:03d}
"""
        densities = self.densities
        for i, radius in enumerate(self.radius_sizes):
            dens = densities[i]
            gravity = ''
            if dens < 0:
                gravity = 'GRAVITY.OFF'
            else:
                gravity = '*GRAVITY.OFF'
            lines.append(pclass_tmpl.format(radius=radius,
                                            density=dens,
                                            classname=self.component_name(i),
                                            gravity=gravity,
                                            identification=i+1))
        
        for i, frac in enumerate(self.size_distribution):
            size_activity = activity + [f"{self.activity_after_1h*frac:.3E}"]
            lines.append(f"RELEASE.BQ/STEP.COMP= {','.join(size_activity)} '{self.component_name(i)}'")


        return "\n".join(lines)

if __name__ == "__main__":
    # unit test
    nyield = 15
    yp = YieldParameters(.25, ExplosionType.MIXED)
    assert(abs(yp.cloud_bottom()-750) < .1)
    assert(abs(yp.cloud_top()-1125) < .1)
    assert(abs(yp.cloud_radius()-2500) < .1)
    assert(abs(yp.stem_radius()-0) < .1)

    sib = SnapInputBomb(nyield)
    assert(nyield == sib.nuclear_yield)
    assert(sib.radius_sizes[0] == 2.2)
    print(sib.component_name(0))
    assert(sib.component_name(0) == 'Aerosol_2.2mym')
    assert(abs(1-sum(sib.size_distribution)) <= .01)
    sib.radius_sizes = [1,2,3,4]
    assert(abs(1-sum(sib.size_distribution)) <= .01)
    try:
        sib.size_distribution = [.3,.4,.5,.6]
        assert(False)
    except:
        pass
    sib.size_distribution = [.3,.4,.2]
    assert(abs(sib.size_distribution[3]-0.1) <= 0.01)
    sib.minutes = 30
    print(sib.snap_input())
    #print(SnapInputBomb(.25).snap_input())
