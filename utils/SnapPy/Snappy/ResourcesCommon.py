# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2020   Norwegian Meteorological Institute
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

import os
import subprocess
import sys
from functools import lru_cache

"""
Common Resource-definitions for EEmep and Snap, parent abstract interface
"""


@lru_cache(maxsize=1)
def getLustreDir():
    """
    Get current lustre directory
    """
    lustredir = "/lustre/storeB"
    store = os.getenv("STORE", None)
    if store:
        lustredir = os.path.join(os.sep, "lustre", store)
    else:
        lustredirenv = _getLustreMappEnv()
        if os.path.isdir(lustredirenv):
            lustredir = lustredirenv
    return lustredir


def _getLustreMappEnv():
    sh_script = os.path.join(
        os.path.dirname(__file__), "resources/lustredir_serviceenv.sh"
    )
    lustredir = "/no_such_file"
    try:
        proc = subprocess.run(
            ["/bin/sh", sh_script],
            check=True,
            stdout=subprocess.PIPE,
            timeout=10,
            encoding="utf-8",
        )
        for line in proc.stdout.splitlines():
            if line.rstrip():
                lustredir = line.rstrip()  # lustredir in last line
    except subprocess.SubprocessError as se:
        print(se, file=sys.stderr)

    return lustredir

def getLustreStore():
    lustredir = getLustreDir()
    return lustredir[len("/lustre/"):]


if __name__ == "__main__":
    # test code
    print(getLustreDir())
    assert getLustreDir().startswith("/lustre/store")
    assert len(getLustreDir()) < 15
