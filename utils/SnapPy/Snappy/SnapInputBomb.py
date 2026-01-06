import csv
import io
import math
import typing
from collections import namedtuple
from enum import Enum

import numpy as np
from Snappy.ActivityHeightDistribution import (
    ActivityHeightDistribution,
    ActivityHeightKdfoc3,
    ActivityHeightRolph,
    ActivityHeightVolumentric,
    Particles,
)


class ActivityHeightType(Enum):
    VOLUMETRIC = 0  # even distribution in cloud volume
    TRIANGULAR = 1  # kdfoc3 like
    ROLPH = 2  # activity height distribution from Hysplit/Rolph 2014, e.g. 3 cap, 3 stem layers
    VOLUMETRIC_NO_STEM = 3  # even distribution in cloud volume without stem


_ParticleDistribution = namedtuple(
    "ParticleDistribution",
    ["name", "radius_sizes", "size_distribution", "g0_fraction", "height_distribution"],
)


class ParticleDistribution(Enum):
    def radius_sizes(self, num: int = 1) -> tuple[float]:
        if num == 1:
            return self.value.radius_sizes
        else:
            return self._interpolate_size_distribution(num)[0]

    def size_distribution(self, num: int = 1) -> tuple[float]:
        if num == 1:
            return self.value.radius_sizes
        else:
            return self._interpolate_size_distribution(num)[1]

    def _interpolate_size_distribution(
        self, num: int
    ) -> tuple[tuple[float], tuple[float]]:
        """Interpolate radii of size distribution into
        several sub-steps to simulate a more continuous distribution

        :param num: number of substeps
        :return: (tuple of radius-sizes, tuple of distribution)
        """
        if num <= 0:
            raise Exception(
                "interpolate_size_distribution needs positive number of steps"
            )
        sizes = []
        distribution = []
        for i in range(len(self.value.radius_sizes)):
            if i == 0:
                prev_r = 0
            else:
                prev_r = self.value.radius_sizes[i - 1]
            new_dist = self.value.size_distribution[i] / num
            r_inc = (self.value.radius_sizes[i] - prev_r) / num
            for j in range(num):
                sizes.append(prev_r + (j + 1) * r_inc)
                distribution.append(new_dist)  # step function

        return (sizes, distribution)

    @property
    def height_distribution(self) -> ActivityHeightType:
        return self.value.height_distribution

    @property
    def g0_fraction(self) -> float:
        """fraction of total activity which is deposited close to ground-zero, i.e. which is not
        transported far. Usually everything above 200µm radius is not transported more than 30min,
        so it is considered g0.

        :return: fraction to be removed from transport
        """
        return self.value.g0_fraction

    MIXED = _ParticleDistribution(
        "Mixed",
        # upper size, radius in µm
        [3.0, 6.5, 11.5, 18.5, 29.0, 45.0, 71.0, 120.0, 250.0, 500.0],
        [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
        0.0,  # 30% deposition in ground-0
        ActivityHeightType.VOLUMETRIC,
    )
    # Glasstone Dolan, lognormal ~(3.78, 0.68), + ~50-60% local = in final class
    SURFACE = _ParticleDistribution(
        "Surface",
        [3.0, 6.5, 11.5, 18.5, 29.0, 45.0, 71.0, 120.0, 250.0, 500.0],
        [0.01, 0.01, 0.018, 0.08, 0.17, 0.25, 0.24, 0.17, 0.06, 0.01],  # ~(3.78, 0.68)
        0.00,  # 5% deposition in GZ
        #                           0.25, # global fallout (<3µm) # nancy best with 90%, harry/smoky: 75%, sunbeam/trinity: 25% (for wdep)
        ActivityHeightType.TRIANGULAR,
    )

    # Glassstone Dolan: below 20µm, using distribution µ=1, σ=0.8
    BOMB = _ParticleDistribution(
        "Bomb",
        [3.0, 6.5, 11.5, 18.5, 29.0, 45.0, 71.0, 120.0, 250.0, 500.0],
        [0.55, 0.31, 0.10, 0.04, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        0.0,  # 0% in ground-0
        ActivityHeightType.VOLUMETRIC_NO_STEM,
    )
    # fmt: on


_ExplosionType = namedtuple(
    "ExplosionType",
    ["name", "distributions"],
)


class ExplosionType(Enum):
    @property
    def particle_distributions(self) -> list[tuple[float, ParticleDistribution]]:
        """get the dict of particle distributions with their fractions"""
        return self.value.distributions

    # fmt: off
    # default snap
    # surface == spriggs r6
    SURFACE  = _ExplosionType("Surface",      [(0.75, ParticleDistribution.SURFACE),
                                               (0.25, ParticleDistribution.BOMB)])
    SPRIGGS_R6 = _ExplosionType("Spriggs R6", [(0.75, ParticleDistribution.SURFACE),
                                               (0.25, ParticleDistribution.BOMB)])
    SPRIGGS_R5_5 = _ExplosionType("Spriggs R5.5", [(0.5, ParticleDistribution.SURFACE),
                                                   (0.5, ParticleDistribution.BOMB)])
    SPRIGGS_R5 = _ExplosionType("Spriggs R5", [(0.25, ParticleDistribution.SURFACE),
                                               (0.75, ParticleDistribution.BOMB)])
    SPRIGGS_R4_5 = _ExplosionType("Spriggs R4.5", [(0.13, ParticleDistribution.SURFACE),
                                                   (0.87, ParticleDistribution.BOMB)])
    SPRIGGS_R4 = _ExplosionType("Spriggs R4", [(0.05, ParticleDistribution.SURFACE),
                                               (0.95, ParticleDistribution.BOMB)])
    MIXED = _ExplosionType("Mixed",           [(1.0, ParticleDistribution.MIXED)])
    HIGH = _ExplosionType("High Altitude",    [(1.0, ParticleDistribution.BOMB)])


    @classmethod
    def by_argosname(cls, name: str):
        """get a ExplosionType by name used in Argos. Unknown names will be translated to MIXED"""
        name = name.lower().strip()
        if name == "surface":
            return ExplosionType.SPRIGGS_R6
        elif name == "1000 meters" or name == "high altitude":
            # '1000 meters' was previous name
            return ExplosionType.HIGH
        elif name == "spriggs r4":
            return ExplosionType.SPRIGGS_R4
        elif name == "spriggs r4.5":
            return ExplosionType.SPRIGGS_R4_5
        elif name == "spriggs r5":
            return ExplosionType.SPRIGGS_R5
        elif name == "spriggs r5.5":
            return ExplosionType.SPRIGGS_R5_5
        elif name == "spriggs r6":
            return ExplosionType.SPRIGGS_R6
        elif name == "mixed":
            return ExplosionType.MIXED
        else:
            raise Exception(f"unknown explosion type name: {name}")


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

    # rolph == ATP-45
    _clouds_defs_rolph = """yield	bottom	top	thickness
0	0	0	0
0.5	1500	2250	750
2.5	2000	3700	1222
7.5	3700	6300	1444
12.5	5100	8200	1667
17.5	6200	9700	1889
22.5	7000	10800	2111
27.5	7300	11200	2333
32.5	7500	11600	2556
37.5	7700	11900	2778
42.5	7900	12200	3000
45.0	8000	12500	3000
"""

    def __init__(
        self,
        nuclear_yield: float = 15,
        explosion_type: ExplosionType = ExplosionType.MIXED,
    ) -> None:
        self._nuclear_yield = nuclear_yield
        self._explosion_type = explosion_type
        # self._cloud_defs = self._parse_clouds(io.StringIO(self._cloud_defs1))
        self._cloud_defs = self._parse_clouds(io.StringIO(self._clouds_defs_rolph))
        return

    @property
    def nuclear_yield(self):
        return self._nuclear_yield

    @property
    def explosion_type(self):
        return self._explosion_type

    def _parse_clouds(self, cloud_def: typing.TextIO) -> dict[str, np.ndarray]:
        """ensure that input-file is ordered by yield"""
        retval_l = {"yield": [], "bottom": [], "top": []}
        reader = csv.DictReader(cloud_def, delimiter="\t")
        for row in reader:
            for tag in retval_l.keys():
                retval_l[tag].append(float(row[tag]))
        retval = {}
        for tag in retval_l.keys():
            retval[tag] = np.asarray(retval_l[tag])
        return retval

    def _get_linear_cloud_def(self, tag) -> dict:
        """get a dict of cloud bottom, top, and radius and stemradius depending on yield"""
        cloudyield = self._cloud_defs["yield"]
        pos = np.argmin(np.abs(cloudyield - self._nuclear_yield))
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

        Formula for 2^19Bq/kT(TNT) from
          Glasstone and Dolan (9.159): 530 gamma-megacuries per kiloton fission yield at 1 hour
             -> 5.30E6 x 3.7E10 Bq = 1.96E+19 Bq/kt  # curie -> Bq conversion
          ARGOS has a depo-gamma factor in operational mode coming from GD (9.150)
             2,900 rads/hr per kt/mi2 = 7.5E7  Sv/h per kt/m2 = 2.08E4 Sv/s per kt/m2
             2.08E4 Sv/s per kt/m2  / 2E19 Bq/kt = 1E-15 Sv/s per Bq/m2
             depo-gamma factor = 1E-15 Sv/s/Bq/m2

        Fission Products from Nuclear Weapons Explosions (Tovedal)
        https://inis.iaea.org/collection/NCLCollectionStore/_Public/32/055/32055989.pdf
        and SSM report (2023)(selecting worst case bomb, U-235)
        https://www.stralsakerhetsmyndigheten.se/contentassets/6a9a09c95ba14e3fbd78d911906ba2fa/202305e-radiological-consequences-of-fallout-from-nuclear-explosions.pdf
        use 1.6e19, but this requires dose calculations, so we stick to GD / H+1 and ARGOS factor
        """
        return self._nuclear_yield * 1.96e19

    def cloud_bottom(self):
        """cloud bottom in m"""
        return self._get_linear_cloud_def("bottom")

    def cloud_top(self):
        """cloud top in m"""
        return self._get_linear_cloud_def("top")

    def cloud_radius(self):
        """cloud radius in m

        Typical radius of mushroom cloud after ~ 10 - 15 min is 1-3 km seen from different studies (Kanarska et al., 2009, Arthur et al., 2021)
        """
        return 2500.0

    def stem_radius(self):
        """stem radius in m"""
        if self.explosion_type == ExplosionType.HIGH:
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

        radius_sizes = None
        dist = None
        for (frac, pd) in self.explosion_type.particle_distributions:
            if radius_sizes is None:
                radius_sizes = pd.radius_sizes(self.SIZE_INTERPOLATION)
                dist = [x * frac for x in pd.size_distribution(self.SIZE_INTERPOLATION)]
            else:
                try:
                    assert np.allclose(
                        radius_sizes,
                        pd.radius_sizes(self.SIZE_INTERPOLATION),
                        atol=1e-5,
                    )
                except Exception as ex:
                    raise AssertionError(
                        f"size distributions do not match in explosion_type: {radius_sizes}, {pd.radius_sizes(self.SIZE_INTERPOLATION)}, {ex}"
                    )
                for i, d in enumerate(pd.size_distribution(self.SIZE_INTERPOLATION)):
                    dist[i] += d * frac

        assert (
            sum(dist) > 0.98 and sum(dist) < 1.02
        ), f"size_distribution does not sum to 1 != {sum(dist)}: {dist}"

        self.radius_sizes = radius_sizes
        self.size_distribution = dist
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
        if abs(1 - sum(size_distribution)) > 0.02:
            raise Exception(
                f"size_distribution {sum(size_distribution)} > 1: {size_distribution}"
            )
        self._size_distribution = size_distribution
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

    def vertical_slices(self) -> list[tuple[int, int]]:
        """devide the column from ground to cloud_top into slices,
        9 slices for the stem, 9 slices for the cloud-head

        :return: list of (lower, upper) pairs
        """
        n = 9
        lower = 0
        dist = self.cloud_bottom / n
        ret = []
        for i in range(n):
            upper = lower + dist
            ret.append((round(lower), round(upper)))
            lower = upper
        dist = (self.cloud_top - lower) / n
        for i in range(n):
            upper = lower + dist
            ret.append((round(lower), round(upper)))
            lower = upper
        return ret

    def _height_type_name_to_ahd(
        self, ahd: ActivityHeightType
    ) -> ActivityHeightDistribution:
        """convert height distribution enum to ActivityHeightDistribution instance"""
        if ahd == ActivityHeightType.VOLUMETRIC:
            return ActivityHeightVolumentric(
                cap_top_height=self.cloud_top,
                cap_bottom_height=self.cloud_bottom,
                cap_radius=self.cloud_radius,
                stem_radiusn=self.stem_radius,
            )
        elif ahd == ActivityHeightType.VOLUMETRIC_NO_STEM:
            return ActivityHeightVolumentric(
                cap_top_height=self.cloud_top,
                cap_bottom_height=self.cloud_bottom,
                cap_radius=self.cloud_radius,
                stem_radius=0.0,
            )
        elif ahd == ActivityHeightType.TRIANGULAR:
            return ActivityHeightKdfoc3(zmax=self.cloud_top, part=Particles.SMALL)
        elif ahd == ActivityHeightType.ROLPH:
            return ActivityHeightRolph(
                cap_top_height=self.cloud_top, cap_bottom_height=self.cloud_bottom
            )
        else:
            raise Exception(f"undefined height_distribution: {ahd}")

    def snap_release(self) -> str | None:
        """get the string needed for the the snap release.txt file, e.g. emissions per layer and timestep

        :return: str of release, or None
        """
        if len(self.explosion_type.particle_distributions) == 1 and (
            self.explosion_type.particle_distributions[0][1].height_distribution
            == ActivityHeightType.VOLUMETRIC
            or self.explosion_type.particle_distributions[0][1].height_distribution
            == ActivityHeightType.VOLUMETRIC_NO_STEM
        ):
            # in this case, release is defined in snap.input file
            return None

        release = ["*runtime[h] lower[m] comp release[Bq/s]"]
        release_particle_distribution = [
            0.0 for _ in self.explosion_type.particle_distributions
        ]
        total_release = 0.0
        for lower, upper in self.vertical_slices():
            for i, radius in enumerate(self.radius_sizes):
                ahd_frac_sum = 0.0
                rel = 0.0
                for pdi, (ahd_frac, pd) in enumerate(
                    self.explosion_type.particle_distributions
                ):
                    ahd = self._height_type_name_to_ahd(pd.height_distribution)
                    ahd_frac_sum += ahd_frac
                    relpd = ahd_frac * (
                        self.activity_after_1hour
                        * pd.size_distribution(self.SIZE_INTERPOLATION)[i]
                        * ahd.layer_fraction(lower, upper)
                        * (1.0 - (pd.g0_fraction))
                    )
                    if lower == 0 and i == (len(self.radius_sizes) - 1):
                        # add all "local" g0 deposition to lowest layer and heaviest particles
                        relpd += ahd_frac * pd.g0_fraction * self.activity_after_1hour
                    release_particle_distribution[pdi] += relpd
                    rel += relpd
                total_release += rel
                assert (
                    abs(ahd_frac_sum - 1.0) < 0.01
                ), f"sum of activity height fractions != 1: {ahd_frac_sum}"
                release.append(
                    f"{self.minutes/60:.2f} {lower} {self.component_name(i)} {rel:.4E}"
                )
        assert np.isclose(
            total_release, self.activity_after_1hour, rtol=0.05
        ), f"total release {total_release} != activity_after_1hour {self.activity_after_1hour}"
        for pdi, (ahd_frac, pd) in enumerate(
            self.explosion_type.particle_distributions
        ):
            frac = release_particle_distribution[pdi] / self.activity_after_1hour
            assert np.isclose(
                frac, ahd_frac, atol=0.05
            ), f"total release fraction for particle distribution {pd.name} {frac} != expected {ahd_frac}"
        return "\n".join(release)

    def snap_input(self, releasefile="release.txt") -> str:
        """get the bomb-input as partial snap.input string"""
        lines = []

        lines.append(f"** Explosive yield {self.nuclear_yield}ktonnes")
        lines.append("TIME.RELEASE.PROFILE.BOMB")
        if len(self.explosion_type.particle_distributions) == 1 and (
            self.explosion_type.particle_distributions[0][1].height_distribution
            == ActivityHeightType.VOLUMETRIC
            or self.explosion_type.particle_distributions[0][1].height_distribution
            == ActivityHeightType.VOLUMETRIC_NO_STEM
        ):
            lines.append(
                f"""
    MAX.PARTICLES.PER.RELEASE= 1000000
    RELEASE.MINUTE= {self.minutes}
    RELEASE.RADIUS.M= {self.cloud_radius}
    RELEASE.LOWER.M= {self.cloud_bottom}
    RELEASE.UPPER.M= {self.cloud_top}
    RELEASE.MUSHROOM.STEM.RADIUS.M= {self.stem_radius}
                        """
            )
            particle_dist = self.explosion_type.particle_distributions[0][1]
            for i, frac in enumerate(self.size_distribution):
                rel = self.activity_after_1hour * frac * (1 - particle_dist.g0_fraction)
                if i == len(self.size_distribution) - 1:
                    # g0 deposition in largest particle class
                    rel += self.activity_after_1hour * particle_dist.g0_fraction
                lines.append(
                    f"RELEASE.BQ/STEP.COMP= {rel:.3E} '{self.component_name(i)}'"
                )
        else:
            lower = []
            upper = []
            radii = []
            for l, u in self.vertical_slices():
                lower.append(f"{l:.0f}")
                upper.append(f"{u:.0f}")
                if l > self.cloud_bottom:
                    radii.append(f"{self.cloud_radius:.0f}")
                else:
                    radii.append(f"{self.stem_radius:.0f}")
            components = [
                f"'{self.component_name(i)}'" for i, _ in enumerate(self.radius_sizes)
            ]
            max_particles = 1000000 / len(lower)
            lines.append(
                f"""
    MAX.PARTICLES.PER.RELEASE= {max_particles:.0f}
    RELEASE.FILE= {releasefile}
    RELEASE.HEIGHTLOWER.M = {','.join(lower)}
    RELEASE.HEIGHTUPPER.M = {','.join(upper)}
    RELEASE.HEIGHTRADIUS.M = {','.join(radii)}
    RELEASE.COMPONENTS= {', '.join(components)}
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

        return "\n".join(lines) + "\n"


if __name__ == "__main__":
    # unit test
    yp = YieldParameters(0.25, ExplosionType.MIXED)
    assert abs(yp.cloud_bottom() - 750) < 0.1
    assert abs(yp.cloud_top() - 1125) < 0.1
    assert abs(yp.cloud_radius() - 2500) < 0.1
    assert abs(yp.stem_radius() - 559) < 0.1

    nyield = 15
    sib = SnapInputBomb(nyield)
    assert nyield == sib.nuclear_yield
    print(SnapInputBomb.SIZE_INTERPOLATION, sib.radius_sizes)
    assert abs(3.0 - (SnapInputBomb.SIZE_INTERPOLATION * sib.radius_sizes[0])) < 0.01
    print(sib.component_name(0))
    assert sib.component_name(0) == "Aerosol_0.6mym"
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
    # print(sib.snap_input())
    # print(SnapInputBomb(.25).snap_input())

    sib_op = SnapInputBomb(nyield, argos_operational=True)
    rel = sib_op.snap_release()
    assert rel is None
    assert "RADIOACTIVE.DECAY.OFF" in sib_op.snap_input()

    sib_op_surf = SnapInputBomb(
        nyield, explosion_type=ExplosionType.SURFACE, argos_operational=True
    )
    rel = sib_op_surf.snap_release()
    assert rel is not None
    assert "RELEASE.FILE" in sib_op_surf.snap_input()
