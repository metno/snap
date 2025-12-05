import abc
from enum import Enum

import numpy as np


class Particles(Enum):
    SMALL = 0
    LARGE = 1


class ActivityHeightDistribution(abc.ABC):
    @abc.abstractmethod
    def layer_fraction(self, zb: float, zt: float) -> float:
        """Fraction of activity released between zb and zt

        The maximum layer_fraction between 0m and max cloud height will be 1.

        :param zb: bottom height of layer in m
        :param zt: top height of layer in m
        """
        ...

class ActivityHeightRolph(ActivityHeightDistribution):
    """Simple 6 layer distribution, 3 equal metric layers in stem, 3 equal metric layers in top,
    from top to bottom
    (18, 30, 30) in cap, (15, 5, 2) in stem

    for more, see Rolph et al., 2014: https://doi.org/10.1016/j.jenvrad.2014.05.006

    """
    def __init__(self, cap_top_height, cap_bottom_height):
        if cap_top_height < cap_bottom_height:
            raise RuntimeError(f"top {cap_top_height} must be > bottom f{cap_bottom_height}")
        self.cap_top_height = cap_top_height
        self.cap_bottom_height = cap_bottom_height
        self.layer_fractions = (0.02, 0.05, 0.15, 0.30, 0.30, 0.18)
        stem_layer = self.cap_bottom_height / 3
        cap_layer = (self.cap_top_height - self.cap_bottom_height) / 3
        self.layer_heights = (stem_layer, 2 * stem_layer, self.cap_bottom_height,
                              self.cap_bottom_height + cap_layer,
                              self.cap_bottom_height + 2 * cap_layer,
                              self.cap_top_height)

    def layer_fraction(self, zb, zt) -> float:
        if zb > self.cap_top_height:
            return 0
        if zt > self.cap_top_height:
            zt = self.cap_top_height
        if zt <= zb:
            return 0
        min_layer = 0
        max_layer = -1
        last_height = 0
        for i, height in enumerate(self.layer_heights):
            if last_height < zb <= height:
                min_layer = i
            if last_height < zt <= height:
                max_layer = i
            last_height = height
        # print(zb, zt, min_layer, max_layer)
        assert(max_layer != -1)
        assert(min_layer != len(self.layer_heights))

        frac = 0
        for i in range(min_layer+1, max_layer):
            frac += self.layer_fractions[i]
        if min_layer == 0:
            min_height = 0
        else:
            min_height = self.layer_heights[min_layer-1]
        if max_layer == min_layer:
            # zb and zt are in same layer, area_fraction of zt-zb of this layer-fraction
            frac += self.layer_fractions[min_layer] * (zt-zb) / (self.layer_heights[min_layer]-min_height)
        else:
            # area fraction of zb in min_layer
            frac += self.layer_fractions[min_layer] * (self.layer_heights[min_layer]-zb) / (self.layer_heights[min_layer]-min_height)
            # area fraction of zt in max_layer-1
            frac += self.layer_fractions[max_layer] * (zt - self.layer_heights[max_layer-1]) / (self.layer_heights[max_layer]-self.layer_heights[max_layer-1])

        return frac

class ActivityHeightVolumentric(ActivityHeightDistribution):
    def __init__(self, cap_top_height: float, cap_bottom_height: float, cap_radius: float = 1.0, stem_radius: float = 0.0):
        if cap_top_height < cap_bottom_height:
            raise RuntimeError(f"top {cap_top_height} must be > bottom f{cap_bottom_height}")
        self.cap_top_height = cap_top_height
        self.cap_bottom_height = cap_bottom_height
        self.cap_radius = cap_radius
        self.stem_radius = stem_radius
        self.cap_height = self.cap_top_height - self.cap_bottom_height
        self.cap_volume = np.pi * (self.cap_radius**2) * self.cap_height
        self.stem_volume = np.pi * (self.stem_radius**2) * self.cap_bottom_height
        self.total_volume = self.cap_volume + self.stem_volume

    def layer_fraction(self, zb, zt) -> float:
        if zb > self.cap_top_height:
            return 0
        if zt > self.cap_top_height:
            zt = self.cap_top_height
        if zt <= zb:
            return 0

        def volume_up_to_height(z):
            if z <= self.cap_bottom_height:
                return np.pi * (self.stem_radius**2) * z
            else:
                h_cap = z - self.cap_bottom_height
                vol_stem = np.pi * (self.stem_radius**2) * self.cap_bottom_height
                vol_cap = np.pi * (self.cap_radius**2) * h_cap
                return vol_stem + vol_cap

        vol_zb = volume_up_to_height(zb)
        vol_zt = volume_up_to_height(zt)

        return (vol_zt - vol_zb) / self.total_volume


class ActivityHeightKdfoc3(ActivityHeightDistribution):
    r"""
    Assumptions:

    zmax: max height of plume

    zhat: height where triangular activity height distribution is at its max.
        This is  2/3 zmax for light paricles and 1/10 zmax for heavy particles

    z0: ground

    zmin: -3/10 zmax

    $f(zmax) = 0$

    $f(zmin) = 0$

    A = $\int_{z0}^{zmax} f(z) dz$ = 1
    """

    def __init__(self, zmax: float, part: Particles):
        self.zmax = zmax
        self.part = part
        # maximum of activity at zmax
        if part == Particles.SMALL:
            self.zhat = 2 / 3 * self.zmax
            self.fraction = 0.77
        else:
            self.zhat = 1 / 10 * self.zmax
            self.fraction = 0.23
        self.zmin = -3 / 10 * self.zmax
        self.fzhat = (
            2
            * (self.zhat - self.zmin)
            / ((self.zhat * self.zmax) - self.zmin * (self.zmax + self.zhat))
        )

    def _activity_height_func(self, z: float):
        if z < self.zmin:
            return float(0)
        if z < self.zhat:
            return self.fzhat * (z - self.zmin) / (self.zhat - self.zmin)
        if z < self.zmax:
            return self.fzhat * (z - self.zmax) / (self.zhat - self.zmax)
        else:
            return float(0)

    def activity_height_func(self, z: np.array):
        return np.vectorize(self._activity_height_func)(z)

    def area_fraction(self, zb: float, zt: float):
        if zt <= self.zmin or zb >= self.zmax:
            return 0

        # AF1: area below zhat
        if zb >= self.zhat:
            AF1 = 0
        else:
            z1 = min(self.zhat, zt)
            AF1 = (z1 - zb) * (0.5 * (z1 + zb) - self.zmin) / (self.zhat - self.zmin)

        # AF2: area above zhat
        if zt <= self.zhat:
            AF2 = 0
        else:
            z2 = min(self.zmax, zt)
            z3 = max(self.zhat, zb)
            AF2 = (z2 - z3) * (0.5 * (z2 + z3) - self.zmax) / (self.zhat - self.zmax)

        return self.fzhat * (AF1 + AF2)

    def layer_fraction(self, zb, zt) -> float:
        return self.area_fraction(zb, zt)


if __name__ == "__main__":

    ahd = ActivityHeightRolph(12000, 9000)

    assert ahd.layer_fraction(0, 12000) == 1
    #print(ahd.layer_fraction(0,9000))
    assert ahd.layer_fraction(0, 9000) == 0.22
    #print(ahd.layer_fraction(1000, 2000))
    assert abs(ahd.layer_fraction(1000, 2000) - 0.02/3) < 1e-3
    #print(ahd.layer_fraction(1000, 11500))
    assert ahd.layer_fraction(1000, 11500) > 0.9
