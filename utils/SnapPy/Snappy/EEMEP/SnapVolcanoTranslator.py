# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2025   Norwegian Meteorological Institute
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
import copy
import datetime
import os
import subprocess
from logging import getLogger

from Snappy.EEMEP.SnapAshResources import SnapAshResources
from Snappy.EEMEP.VolcanoRun import VolcanoXML

logger = getLogger(__name__)


class SnapVolcanoTranslator:
    def __init__(self, volcano_file):
        """Run a snap-run with the input parameters from a volcano.xml file. The run will be started
        in the background as a qsub script in a SGE queue defined in resources/snapash.job

        :param volcano_file: volcano.xml file-path
        """
        self.volcano = VolcanoXML(volcano_file)
        if self.volcano.snap_model_setup is None:
            logger.warning(f"no snap_model_setup in file: '{volcano_file}'")
            return

        os.makedirs(name=self.volcano.outputdir, exist_ok=True)

        defs = {}
        defs["volcano_name"] = self.volcano.name
        defs["runtime"] = self.volcano.runTimeHours
        defs["simulationstart"] = datetime.datetime.now()
        defs["interpolation"] = ""
        defs["latitude"] = self.volcano.latlon[0]
        defs["longitude"] = self.volcano.latlon[1]
        classes = {
            f"ASH_{i+1}": x * 1e-2
            for i, x in enumerate([0.1, 0.5, 5.0, 20.0, 70.0, 4.4])
        }
        (start, lower, upper) = self.eruptions_to_txt(
            self.volcano.eruptions,
            classes,
            os.path.join(self.volcano.outputdir, "release.txt"),
        )

        defs["start"] = start
        defs["lowerlist"] = ",".join([str(x) for x in lower])
        defs["upperlist"] = ",".join([str(x) for x in upper])
        defs["radiuslist"] = ",".join([str(1000) for x in lower])

        reftime, metmodel = self.volcano.snap_model_setup
        files = SnapAshResources.getMeteorologyFiles(
            metmodel, start, defs["runtime"], reftime
        )
        defs["meteofiles"] = "\n".join([f"FIELD.INPUT= {x}" for x in files])

        snap_input = SnapAshResources.get_snap_input(metmodel)
        with open(os.path.join(self.volcano.outputdir, "snap.input"), "wt") as fh:
            fh.write(snap_input.format(**defs))

    @staticmethod
    def eruptions_to_txt(eruptions, classes: dict[str, float], releasefile: str):
        starttimes = {}
        endtimes = set()
        all_lower = set()
        all_upper = set()
        for erupt in eruptions:
            endtimes.add(erupt.end)
            if not erupt.start in starttimes:
                starttimes[erupt.start] = {
                    "end": erupt.end,
                    "lower": {
                        erupt.bottom: {
                            "upper": erupt.top,
                            "release": erupt.rate,
                            "m63": erupt.m63,
                        }
                    },
                }
            else:
                if starttimes[erupt.start]["end"] != erupt.end:
                    logger.error(
                        f"eruption start-times with different end-times: {erupt.start} {erupt.end}"
                    )
                    continue
                if erupt.bottom in starttimes[erupt.start]["lower"]:
                    logger.error(
                        f"eruption {erupt.start} with duplicated lower-values: {erupt.bottom}"
                    )
                    continue
                starttimes[erupt.start]["lower"][
                    erupt.bottom : {
                        "upper": erupt.top,
                        "release": erupt.rate,
                        "m63": erupt.m63,
                    }
                ]
        print(starttimes)
        start, startvals = next(iter(starttimes.items()))
        nullrelease = copy.deepcopy(startvals)
        for lower, lowvals in nullrelease["lower"].items():
            lowvals["release"] = 0.0

        snap_start = start.replace(minute=0, second=0, microsecond=0)
        if snap_start < start:
            # generate a 0 start for the first minutes
            starttimes[snap_start] = copy.deepcopy(nullrelease)
            starttimes[snap_start]["end"] = start

        # end all releases after last
        end = max(endtimes)
        starttimes[end] = copy.deepcopy(nullrelease)
        starttimes[end]["end"] = end

        with open(releasefile, "wt") as fh:
            fh.write("*runtime[h] lower[m] comp release[g/s]\n")
            last_end = snap_start
            for start, vals in sorted(starttimes.items()):
                endtimes.add(vals["end"])
                runtime = (start - snap_start).total_seconds() / 3600
                for lower, lowerVals in sorted(vals["lower"].items()):
                    all_lower.add(lower)
                    all_upper.add(lowerVals["upper"])
                    for pclass, frac in sorted(classes.items()):
                        rel = 1000 * frac * lowerVals["m63"] * lowerVals["release"]
                        fh.write(f"{runtime:.2f} {int(lower)} {pclass} {rel}\n")

        return (snap_start, all_lower, all_upper)

    def run(self) -> int:
        """
        The run method is a convenient method to actually run bsnap_naccident and
        snapAddToa on the results in the output-directory of volcano.xml.

        return returncode of snapAddToa, return-code of bsnap is ignored since that might
            fail due to missing meteo, but still give a good beginning.
        """
        with open(os.path.join(self.volcano.outputdir, "snap.outlog"), "w") as fh:
            logger.info("Starting bsnap_naccident snap.input")
            proc = subprocess.run(
                ["bsnap_naccident", "snap.input"],
                cwd=self.volcano.outputdir,
                stderr=fh,
                stdout=fh,
            )
        with open(os.path.join(self.volcano.outputdir, "snapAddToa.outlog"), "w") as fh:
            logger.info("Starting snapAddToa snap.nc")
            proc = subprocess.run(
                ["snapAddToa", "snap.nc"],
                cwd=self.volcano.outputdir,
                stderr=fh,
                stdout=fh,
            )
        return proc.returncode


def main() -> int:
    import argparse
    import sys

    # umask to make files group-writable for later cleanup
    os.umask(0o002)

    parser = argparse.ArgumentParser(
        description="run snap from a volcano.xml file",
        usage=f"{sys.argv[0]} [--translate_only] volcano.xml",
    )
    parser.add_argument(
        "--translate_only",
        help="translate volcano.xml to snap.input and release.txt",
        action=argparse.BooleanOptionalAction,
        default=False,
    )
    parser.add_argument(
        "volcano_filepath",
        help="volcano.xml filepath, with volcano definition",
        nargs=1,
    )
    args = parser.parse_args()

    svt = SnapVolcanoTranslator(args.volcano_filepath[0])
    if not args.translate_only:
        exit(svt.run())
    exit(0)


if __name__ == "__main__":
    main()
