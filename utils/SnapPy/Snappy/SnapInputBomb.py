from enum import Enum
from collections import namedtuple

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
        # _nuclear_yield
        # _explosion_type
        # _radius_sizes
        # _size_distribution
        self._component_basename = 'Aerosol'
        self._component_formatter = "{component}_{size:.1f}mym"
        self._gravity = [] # might be empty array or fixed gravity per size
        self._minutes = 0. # explosion starts minutes after full hour

        # set from parametrization
        self._cloud_bottom = 0
        self._cloud_top = 0
        self._total_activity = 0

        self._stem_fraction = 0. # ratio of Bq in stem vs cloud
        pass

    @property
    def nuclear_yield(self) -> float:
        """nuclear yield in Mg(=kTonnes) TNT"""
        return self._nuclear_yield
    
    @property
    def explosion_type(self) -> ExplosionType:
        return self._explosion_type

    def set_bomb(self, nuclear_yield: float, explosion_type: ExplosionType = None) -> None:
        """
        set yield, sizes, size_distribution and activity base on nuclear_yield and explosion_type
        """
        self._nuclear_yield = nuclear_yield
        if explosion_type is not None:
            self._explosion_type = explosion_type
        self.radius_sizes = self.explosion_type.radius_sizes
        self.size_distribution = self.explosion_type.size_distribution
        return None

    @property
    def activity_after_1h(self) -> float:
        """
        Get the total activity for relatively long-lived isotopes
        """
        # formula from 2^19Bq/kT(TNT) as of
        # Fission Products from Nuclear Weapons Explosions (Tovedal)
        # https://inis.iaea.org/collection/NCLCollectionStore/_Public/32/055/32055989.pdf 
        return self.nuclear_yield * 2e19

    @property
    def cloud_bottom(self) -> float:
        """cloud bottom height in m"""
        return self._cloud_bottom
    
    @property
    def cloud_top(self) -> float:
        """cloud top height in m"""
        return self._cloud_top

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
    def stem_fraction(self) -> float:
        """fraction of activity in stem (surface to lower cloud part of mushroom-cloud)"""
        return self._stem_fraction
        
if __name__ == "__main__":
    # unit test
    nyield = 15
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
