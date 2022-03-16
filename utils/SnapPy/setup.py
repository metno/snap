#!/usr/bin/env python3
#
# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2017   Norwegian Meteorological Institute
#
# This file is part of SNAP. SNAP is free software: you can
# redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

from distutils.core import setup
import os

version = os.getenv("VERSION", "0.5")

setup(
    name="Snappy",
    version=version,
    description="SNAP GUI in python",
    author="Heiko Klein",
    author_email="Heiko.Klein@met.no",
    url="https://gitlab.met.no/emep/snap",
    packages=["Snappy", "Snappy.EEMEP", "METNO"],
    package_dir={"Snappy": "Snappy", "Snappy.EEMEP": "Snappy/EEMEP"},
    package_data={"Snappy": ["resources/*"], "Snappy.EEMEP": ["resources/*"]},
    scripts=[
        "snapPy",
        "snap4rimsterm",
        "snapCombineInverse",
        "snapRunnerNpps",
        "eemepModelRunner",
    ],
)
