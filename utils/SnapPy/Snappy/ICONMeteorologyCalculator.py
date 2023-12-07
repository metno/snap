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
'''
Created on Oct 24, 2016

@author: heikok
'''

from datetime import datetime, timedelta
import netCDF4
import numpy as np
import os
import subprocess

from Snappy.Resources import Resources, MetModel
import Snappy.MeteorologyCalculator

class ICONMeteorologyCalculator(Snappy.MeteorologyCalculator.MeteorologyCalculator):
    '''Calculate dwd icon-meteorology'''
    dir_template = "NRPA_LON{lon0}_LAT{lat0}_ICON"


    @staticmethod
    def get_valid_timesteps(filename):
        """Extract the valid timesteps usable for SNAP from a icon-netcdf file.

        :param filename: filename of the netcdf-file
        :return: (timesteps, times) a tuple of step-numbers and datetime-values for the valid timesteps
        """
        time_var = 'time'
        x_wind_var = 'x_wind_pl'
        with netCDF4.Dataset(filename, 'r') as nc:
            times = netCDF4.num2date(nc['time'][:], nc['time'].units, only_use_python_datetimes=True)
            x_wind = nc['x_wind_pl']

            valid_times = []
            for i, dt in enumerate(times):
                wind0 = np.ma.filled(x_wind[i,0,0,0], np.nan) # convert masked array to float
                if not np.isnan(wind0):
                    if len(valid_times):
                        stepH = (dt - times[valid_times[-1]]).total_seconds() / 3600
                        if (stepH <= 6):
                            valid_times.append(i)
                    else:
                        valid_times.append(i)
            return valid_times, times[valid_times]


    @staticmethod
    def getGlobalMeteoResources():
        '''retrieve the GlobalMeteoResources from internal resources'''
        gres = Resources()
        res = Snappy.MeteorologyCalculator.GlobalMeteoResource()
        res.indirs = gres.getMetGlobalInputDirs(MetModel.Icon0p25Global)
        # icon_20200427T00Z.nc
        res.pathglob = "icon_????????T??Z.nc"
        res.pathptime = "icon_%Y%m%dT%HZ.nc"
        res.path_grace_period_sec = 0 # files are written atomic (rsync)
        res.outputdir = gres.getSnapOutputDir()
        res.output_filename_pattern = gres.MET_FILENAME_PATTERN[MetModel.Icon0p25Global] # keeping filename for extracted data
        res.domainHeight = gres.ecDomainHeight # reuse domainHeight/Width
        res.domainWidth = gres.ecDomainWidth
        res.domainDeltaX = 0.25
        res.domainDeltaY = 0.25
        res.timeoffset = 0 # required offset between reference-time and first useful startup-time
        return res

    def __init__(self, res: Snappy.MeteorologyCalculator.GlobalMeteoResource, dtime: datetime, domainCenterX, domainCenterY):
        self.files = []
        self.optFiles = []
        # not utc-dependent for ICON-data
        super().__init__(res, dtime, domainCenterX, domainCenterY)
        self.globalOptionalFiles = []
        for hr in (6, 12, 18): # earlier hours
            hdate = self.date - timedelta(hours=hr)
            datefile = self.findGlobalData(res, hdate)
            self.add_expected_files(datefile[0], optional=True)
            self.globalOptionalFiles.append(datefile[1])

    def add_expected_files(self, date, optional=False):
        # only one file expected
        file = os.path.join(
            self.outputdir,
            self.res.output_filename_pattern.format(year=date.year,
                                                    month=date.month,
                                                    day=date.day,
                                                    UTC=date.hour,
                                                    resdir=Resources().directory)
        )
        if optional:
            self.optFiles.append(file)
        else:
            self.files.append(file)

        return

    def get_meteorology_files(self):
        # The Icon-Meteorology calcuator has the newest file as expected, and
        # older ones as optional, while snap expects the newest ones to be last,
        # so reversing the files
        return list(reversed(super().get_meteorology_files()))

    def must_calc(self):
        '''check if calculation is required or has been done earlier'''
        recalc = False
        for f in self.files + self.optFiles:
            if (not os.path.isfile(f)):
                recalc = True
        return recalc


    def calc(self, proc=None):
        '''run the calculation of ec-data if required.

        Args:
           proc -- A QProcess, which will be used to run a longer process in the background.
                  STDERR/STDOUT and signal-handler should be set. If proc is None, the
                  subprocess will be run in the current-process. If proc is set, the caller
                  needs to wait for the proc to finish before calling other methods of this object
'''
        if not self.must_calc():
            return

        command = f"""#! /bin/bash
echo "Preprocessing 2-5days icon meteorology, please wait ca. 5min"
echo "MET-Output-Directory: {self.outputdir}"
cd {self.outputdir} || exit 1

umask 000
touch running
cp {Resources().directory}/icon_fimex.cfg .
chmod 666 icon_fimex.cfg
cp {Resources().directory}/icon_sigma_hybrid.ncml .
chmod 666 icon_sigma_hybrid.ncml

"""
        precommand = """
echo "MET-Input: {file}"
if [ -e "{outputfile}" ]; then
    echo "{outputfile} exists, skipping..."
else
    tmpfile=out$$.nc4
    fimex -c icon_fimex.cfg \
        --input.file={file} \
        --interpolate.xAxisValues={xAxisValues} \
        --interpolate.yAxisValues={yAxisValues} \
        --extract.pickDimension.name=time \
        --extract.pickDimension.list={timeStepList} \
        --output.file=$tmpfile \
        --output.type=nc4 \
        && mv $tmpfile {outputfile}
fi
"""
        in_files = [self.globalfile] + self.globalOptionalFiles
        out_files = self.files + self.optFiles
        for i, file in enumerate(in_files):
            (timesteps, _) = ICONMeteorologyCalculator.get_valid_timesteps(file)
            timeStepList = ",".join([str(x) for x in timesteps])
            command += precommand.format(
                file=file,
                xAxisValues="{},{},...,{}".format(self.lon0,
                                                self.lon0+self.res.domainDeltaX,
                                                self.lon0+self.res.domainWidth),
                yAxisValues="{},{},...,{}".format(self.lat0,
                                                self.lat0+self.res.domainDeltaY,
                                                self.lat0+self.res.domainHeight),
                timeStepList=timeStepList,
                outputfile=out_files[i],
                outputdir=self.outputdir
            )
        command += f"rm {self.outputdir}/running\n"
        scriptFile = os.path.join(self.outputdir, "command.sh")
        with open(scriptFile, 'w') as script:
            script.write(command)

        if proc is None:
            subprocess.call(['/bin/bash', scriptFile])
        else:
            self.proc = proc # make sure proc lives long enough
            proc.start('/bin/bash', [scriptFile])

        return

if __name__ == "__main__":
    yesterday = datetime.today() - timedelta(days=1)
    yesterdaytime = datetime.combine(yesterday, datetime.min.time())
    for utc in (0, 6, 12, 18):
        dt = yesterdaytime + timedelta(hours=utc)
        file = f"/lustre/storeB/project/metproduction/products/icon/icon_{dt:%Y%m%dT%H}Z.nc"
        steps, times = ICONMeteorologyCalculator.get_valid_timesteps(file)
        print(file)
        print(steps)
        print([f"{x:%Y-%m-%dT%H}" for x in times])

    mydate = yesterdaytime
    print(ICONMeteorologyCalculator.findGlobalData(ICONMeteorologyCalculator.getGlobalMeteoResources(), mydate))
    try:
        ICONMeteorologyCalculator.findGlobalData(ICONMeteorologyCalculator.getGlobalMeteoResources(), datetime.strptime("2010-10-24T00", "%Y-%m-%dT%H"))
    except Exception as e:
        print(e.args[0])
#    print(EcMeteorologyCalculator(Resources(), datetime.strptime("2016-10-24T00", "%Y-%m-%dT%H"), 63, 42, None))
    met = ICONMeteorologyCalculator(ICONMeteorologyCalculator.getGlobalMeteoResources(), mydate, -159, 20) # hawaii
    print("recalc: ", met.must_calc())
    met.calc()
    if met.must_calc() is True:
        print("ERROR: recalc after calc required")
    else:
        print("calc output-file: ", met.get_meteorology_files())
