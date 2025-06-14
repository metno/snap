from enum import Enum

import numpy as np


class Particles(Enum):
    SMALL = 0
    LARGE = 1

class ActivityHeightKdfoc3():
    def __init__(self, zmax: int, part: Particles):
        self.zmax = zmax
        self.part = part
        # maximum of activity at zmax
        if part == Particles.SMALL:
            self.zhat = 2/3 * self.zmax
            self.fraction = 0.77
        else:
            self.zhat = 1/10 * self.zmax
            self.fraction = 0.23
        self.zmin = -3/10 * self.zmax
        self.fzhat = 2 * (self.zhat - self.zmin)/ ((self.zhat * self.zmax)- self.zmin * (self.zmax + self.zhat))


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
            AF2 = (z2 - z3) * (0.5*(z2 + z3) - self.zmax)/(self.zhat - self.zmax)

        return self.fzhat* (AF1 + AF2)
