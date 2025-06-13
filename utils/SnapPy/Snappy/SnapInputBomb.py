from enum import Enum
from collections import namedtuple
import csv
import math
import numpy
import io
import typing

_ExplosionType = namedtuple(
    "ExplosionType", ["name", "radius_sizes", "size_distribution"]
)


class ExplosionType(Enum):
    @property
    def radius_sizes(self):
        return self.value.radius_sizes

    @property
    def size_distribution(self):
        return self.value.size_distribution

    # fmt: off
    # default snap
    MIXED = _ExplosionType("Mixed",
                           # upper size, radius in µm
                           [3.0, 6.5, 11.5, 18.5, 29., 45.0, 71.0, 120., 250.0, 1000.],
                           [ .1,  .1,  .1,   .1,   .1,   .1,   .1,   .1,    .1,    .1]
                           )
    # Glasstone Dolan, lognormal ~(3.78, 0.68), + ~50-60% local = in final class
    SURFACE = _ExplosionType("Surface",
                           [          11.5, 18.5, 29., 45.0, 71.0, 120., 250.0, 1000.],
                           #[          .02,  .08,  .17,  .25,  .24,  .17,   .06,   .01], # ~(3.78, 0.68)
                           [          .01,  .04,  .08,  .125,  .12,  .08,   .03,   .53], # ~(3.78, 0.68) + 50% in last
                           )
    # Glassstone Dolan, uniform below 20µm
    HIGH_ALTITUDE = _ExplosionType("High Altitude",
                           [3.0, 6.5, 11.5, 18.5],
                           [.25, .25, .25,  .25]
                           )
    # fmt: on

    @classmethod
    def by_argosname(cls, name: str):
        """get a ExplosionType by name used in Argos. Unknown names will be translated to MIXED"""
        name = name.lower().strip()
        if name == "surface":
            return ExplosionType.SURFACE
        elif name == "1000 meters" or name == "high altitude":
            # '1000 meters' was previous name
            return ExplosionType.HIGH_ALTITUDE
        else:
            return ExplosionType.MIXED


def _lin_interpol(a0, a, b, x, y):
    """linear interpolation of x=f(a), y=f(b) to f(a0)"""
    if a == b:
        return x
    else:
        return x + (a0 - a) * (y - x) / (b - a)


class YieldParameters:
    """Yield Parameters of a Nuclear Bomb

    :param nuclear_yield: yield in kT, defaults to 15
    :param explosion_type: type of explosion, defaults to ExplosionType.MIXED
    """

    _cloud_defs1 = """yield	bottom	top	thickness
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
"""

    def __init__(
        self,
        nuclear_yield: float = 15,
        explosion_type: ExplosionType = ExplosionType.MIXED,
    ) -> None:
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
        """ensure that input-file is ordered by yield"""
        retval = {"yield": [], "bottom": [], "top": []}
        reader = csv.DictReader(cloud_def, delimiter="\t")
        for row in reader:
            for tag in retval.keys():
                retval[tag].append(float(row[tag]))
        for tag in retval.keys():
            retval[tag] = numpy.asarray(retval[tag])
        return retval

    def _get_linear_cloud_def(self, tag) -> dict:
        """get a dict of cloud bottom, top, and radius and stemradius depending on yield"""
        cloudyield = self._cloud_defs["yield"]
        pos = numpy.argmin(numpy.abs(cloudyield - self._nuclear_yield))
        # linear interpolation
        if cloudyield[pos] > self._nuclear_yield:
            pos1 = pos - 1
            if pos1 < 0:
                pos1 = 0
        else:
            pos1 = pos + 1
            if pos1 >= cloudyield.shape[0]:
                pos1 = pos
        return _lin_interpol(
            self._nuclear_yield,
            cloudyield[pos],
            cloudyield[pos1],
            self._cloud_defs[tag][pos],
            self._cloud_defs[tag][pos1],
        )

    def activity_after_1hour(self) -> float:
        """
        Get the total activity for relatively long-lived isotopes
        """
        # formula from 2^19Bq/kT(TNT) from
        #   Glasstone and Dolan (9.159): 530 gamma-megacuries per kiloton fission yield at 1 hour
        #      -> 5.30E6 x 3.7E10 Bq = 1.96E+19 Bq/kt  # curie -> Bq conversion
        #   ARGOS has a depo-gamma factor in operational mode coming from GD (9.150)
        #      2,900 rads/hr per kt/mi2 = 7.5E7  Sv/h per kt/m2 = 2.08E4 Sv/s per kt/m2
        #      2.08E4 Sv/s per kt/m2  / 2E19 Bq/kt = 1E-15 Sv/s per Bq/m2
        #      depo-gamma factor = 1E-15 Sv/s/Bq/m2
        #
        # Fission Products from Nuclear Weapons Explosions (Tovedal)
        # https://inis.iaea.org/collection/NCLCollectionStore/_Public/32/055/32055989.pdf
        # and SSM report (2023)(selecting worst case bomb, U-235)
        # https://www.stralsakerhetsmyndigheten.se/contentassets/6a9a09c95ba14e3fbd78d911906ba2fa/202305e-radiological-consequences-of-fallout-from-nuclear-explosions.pdf
        # use 1.6e19, but this requires dose calculations, so we stick to GD / H+1 and ARGOS factor
        return self._nuclear_yield * 1.96e19

    def cloud_bottom(self):
        """cloud bottom in m"""
        return self._get_linear_cloud_def("bottom")

    def cloud_top(self):
        """cloud top in m"""
        return self._get_linear_cloud_def("top")

    def cloud_radius(self):
        """cloud radius in m"""
        # Typical radius of mushroom cloud after ~ 10 - 15 min is 1-3 km seen from different studies (Kanarska et al., 2009, Arthur et al., 2021)
        return 2500.0

    def stem_radius(self):
        """cloud radius in m"""
        if self.explosion_type == ExplosionType.HIGH_ALTITUDE:
            return 0.0
        else:
            # Glasstone-Dolan 9.61: the mushroom head from a contact land-surface burst initially contains about 90 percent with the remainder residing in the stem.
            fraction = 0.1
            vol_head = (
                math.pi
                * (self.cloud_radius() * self.cloud_radius())
                * (self.cloud_top() - self.cloud_bottom())
            )
            stem_radius = math.sqrt(
                fraction * vol_head / (math.pi * self.cloud_bottom())
            )
            return round(stem_radius)


class SnapInputBomb:
    """
    Description of the bomb-part of a snap.input file
    Excluding meteorology and start-position

    Parameters
    ----------
    nuclear_yield : float, optional
        nuclear yield of explosion in kilotonnes TNT, default 15
    explosion_type : ExplosionType, optional
        type of explosion (defines size distributions)
    argos_operational : operational mode, i.e. non-decay H+1 run
    """
    SIZE_INTERPOLATION = 5

    def __init__(
        self,
        nuclear_yield: float = 15,
        explosion_type: ExplosionType = ExplosionType.MIXED,
        argos_operational: bool = False,
    ) -> None:
        self.set_bomb(nuclear_yield, explosion_type)
        # _yield_parameters
        # _radius_sizes
        # _size_distribution
        self._argos_operational = argos_operational
        self._component_basename = "Aerosol"
        self._component_formatter = "{component}_{size:.1f}mym"
        self._gravity = []  # might be empty array or fixed gravity per size
        self._default_density = 2.95  # general rock/sand density
        self._densities = []  # might be empty array of densities per size
        self._minutes = 0.0  # explosion starts minutes after full hour

        return

    @property
    def nuclear_yield(self) -> float:
        """nuclear yield in Mg(=kTonnes) TNT"""
        return self._yield_parameters.nuclear_yield

    @property
    def explosion_type(self) -> ExplosionType:
        return self._yield_parameters.explosion_type

    @property
    def argos_operational(self) -> bool:
        """Operational mode in ARGOS, i.e. fast run without decay and H+1 particles only"""
        return self._argos_operational

    def set_bomb(
        self, nuclear_yield: float, explosion_type: ExplosionType = None
    ) -> None:
        """
        set yield, sizes, size_distribution and activity base on nuclear_yield and explosion_type
        """
        self._yield_parameters = YieldParameters(nuclear_yield, explosion_type)

        self.radius_sizes = self.explosion_type.radius_sizes
        self.size_distribution = self.explosion_type.size_distribution
        self._interpolate_size_distribution(self.SIZE_INTERPOLATION)
        return None

    @property
    def activity_after_1hour(self) -> float:
        """
        Get the total activity for relatively long-lived isotopes
        """
        return self._yield_parameters.activity_after_1hour()

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
        return self._component_formatter.format(
            component=self.component_basename, size=self.radius_sizes[pos]
        )

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
                size_dist.append(1 - sum_dist)
                sum_dist = 1
        # append equal size_distribution for the remaining parts
        extras = len(self._radius_sizes) - len(self._size_distribution)
        if extras > 0:
            frac = (1 - sum_dist) / extras
            for i in range(extras):
                size_dist.append(frac)
        # assertion
        sum_size_dist = sum(size_dist)
        if abs(1 - sum_size_dist) > 0.01:
            raise Exception(f"sum of size_dist == {sum_size_dist} != 1: {size_dist}")
        self._size_distribution = size_dist
        return self._size_distribution

    @size_distribution.setter
    def size_distribution(self, size_distribution: list[float]) -> None:
        if abs(1 - sum(size_distribution)) > 0.01:
            raise Exception(f"size_distribution {sum(size_distribution)} > 1: {size_distribution}")
        self._size_distribution = size_distribution
        return

    def _interpolate_size_distribution(self, num: int) -> None:
        """Interpolate radii of size distribution into
        several sub-steps to simulate a more continuous distribution

        :param num: number of substeps
        """
        if num <= 0:
            raise Exception("interpolate_size_distribution needs positive number of steps")
        sizes = []
        distribution = []
        for i in range(len(self.radius_sizes)):
            if i == 0:
                prev_r = 0
            else:
                prev_r = self.radius_sizes[i-1]
            new_dist = self.size_distribution[i] / num
            r_inc = (self.radius_sizes[i] - prev_r) / num
            for j in range(num):
                sizes.append(prev_r + (j+1)*r_inc)
                distribution.append(new_dist) # step function

        self.radius_sizes = sizes
        self.size_distribution = distribution
        return


    @property
    def default_density(self) -> float:
        """density in g/cm³ used for all particle classes unless specified otherwise"""
        return self._default_density

    @default_density.setter
    def default_density(self, default_density: float) -> None:
        """set the default density in g/cm^3"""
        self._default_density = default_density
        return

    @property
    def densities(self) -> list[float]:
        """list of densities in g/cm^3 for each size-class, uses default_density if none given"""
        """
        set the list of densities in g/cm^3 for each size-classes

        a 0 or undefined density will give the default_density
        a negative density will disable gravity
        """
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

    def snap_input(self) -> str:
        """get the bomb-input as partial snap.input string"""
        lines = []
        lines.append("MAX.PARTICLES.PER.RELEASE= 1000000\n")

        lines.append(f"** Explosive yield {self.nuclear_yield}ktonnes")
        lines.append("TIME.RELEASE.PROFILE.BOMB")
        lines.append(
            f"""
RELEASE.MINUTE= {self.minutes}
RELEASE.RADIUS.M= {self.cloud_radius}
RELEASE.LOWER.M= {self.cloud_bottom}
RELEASE.UPPER.M= {self.cloud_top}
RELEASE.MUSHROOM.STEM.RADIUS.M= {self.stem_radius}
                     """
        )

        lines.append("* PARTICLE CLASSES")
        pclass_tmpl = """COMPONENT= {classname}
MERGE.NAME= {basename}
DRY.DEP.ON
WET.DEP.ON
RADIOACTIVE.DECAY.{decay_mode}
RADIUS.MICROMETER= {radius}
DENSITY.G/CM3={density}
{gravity}
"""
        densities = self.densities
        for i, radius in enumerate(self.radius_sizes):
            dens = densities[i]
            gravity = ""
            if dens < 0:
                gravity = "GRAVITY.OFF"
            else:
                gravity = "*GRAVITY.OFF"
            if self.argos_operational:
                decay_mode = "OFF"
            else:
                decay_mode = "BOMB"
            lines.append(
                pclass_tmpl.format(
                    radius=radius,
                    density=dens,
                    classname=self.component_name(i),
                    basename=self.component_basename,
                    gravity=gravity,
                    decay_mode=decay_mode,
                    identification=i + 1,
                )
            )

        for i, frac in enumerate(self.size_distribution):
            lines.append(
                f"RELEASE.BQ/STEP.COMP= {self.activity_after_1hour*frac:.3E} '{self.component_name(i)}'"
            )

        return "\n".join(lines) + "\n"


if __name__ == "__main__":
    # unit test
    nyield = 15
    yp = YieldParameters(0.25, ExplosionType.MIXED)
    assert abs(yp.cloud_bottom() - 750) < 0.1
    assert abs(yp.cloud_top() - 1125) < 0.1
    assert abs(yp.cloud_radius() - 2500) < 0.1
    assert abs(yp.stem_radius() - 559) < 0.1

    sib = SnapInputBomb(nyield)
    assert nyield == sib.nuclear_yield
    assert abs( 2.2 - (SnapInputBomb.SIZE_INTERPOLATION * sib.radius_sizes[0]) ) < 0.01
    print(sib.component_name(0))
    assert sib.component_name(0) == "Aerosol_0.4mym"
    assert abs(1 - sum(sib.size_distribution)) <= 0.01
    sib.radius_sizes = [1, 2, 3, 4]
    sib.size_distribution = [0.1, 0.2, 0.3, 0.4]
    assert abs(1 - sum(sib.size_distribution)) <= 0.01
    try:
        sib.size_distribution = [0.3, 0.4, 0.5, 0.6]
        assert False
    except:
        pass
    sib.minutes = 30
    print(sib.snap_input())
    # print(SnapInputBomb(.25).snap_input())

    sib_op = SnapInputBomb(nyield, argos_operational=True)
    assert "RADIOACTIVE.DECAY.OFF" in sib_op.snap_input()
