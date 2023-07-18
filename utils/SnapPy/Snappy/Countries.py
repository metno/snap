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
import csv
import os
import re


def get_country_list(name):
    """
    get a list of countries for a region. Currently implemented regions: europe
    """
    filename = os.path.join(os.path.dirname(__file__), "resources", f"{name}.csv")

    countries = []
    point = re.compile(r"POINT\s*\((.*)\)")
    with open(filename, newline="") as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            if row["id"].startswith("#"):
                continue
            m = re.search(point, row["point"])
            if m:
                lon, lat = m[1].split(sep=" ")
                countries.append(Country(row["name"], float(lat), float(lon)))
    return countries


class Country:
    def __init__(self, name, lat: float, lon: float) -> None:
        self.name = name
        self.lat = lat
        self.lon = lon

    def __str__(self) -> str:
        return ",".join([self.name, f"POINT({self.lon:.3f} {self.lat:.3f})"])

    def __repr__(self) -> str:
        return f"Country({self})"


def _to_diana_txt_file(filename, name):
    with open(filename, "wt") as fh:
        fh.write(
            """# -*- coding: utf-8 -*-
[COLUMNS
Lon:r  Lat:r Name:s ]

[DATA]
"""
        )
        for cc in get_country_list(name):
            fh.write(f'{cc.lon} {cc.lat} "{cc.name}"\n')


if __name__ == "__main__":
    print(get_country_list("europe"))
