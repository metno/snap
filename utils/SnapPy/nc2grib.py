#! /usr/bin/env python3

import os
import subprocess
import sys
from Snappy.Resources import Resources

def convert_to_grib(snapNc, basedir, ident, isotopes):
    config = Resources().getGribWriterConfig(isotopes)
    xmlFile = "cdmGribWriterConfig.xml"
    basexmlFile = os.path.join(basedir, xmlFile)
    with open(basexmlFile, 'w') as xh:
        xh.write(config['xml'])
    errlog = open(os.path.join(basedir, "fimex.errlog"), "w")
    outlog = open(os.path.join(basedir, "fimex.outlog"), "w")
    tempfile = 'tmp.grib'
    basetempfile = os.path.join(basedir, tempfile)
    # fimex works in basedir, so it does not need the basefiles
    for appendix, params in config['extracts'].items():
        outFile = os.path.join(basedir, "{ident}_{type}".format(ident=ident, type=appendix))
        with open(outFile, 'wb') as gh:
            for param in params:
                if (os.path.exists(basetempfile)):
                    os.remove(basetempfile)
                procOptions = ['fimex', '--input.file={}'.format(snapNc), '--input.config={}'.format(config['ncml']),
                       # avoid problem with lat/lon variables
                       # in fimex grib-writer< 0.64
                       # '--extract.removeVariable=longitude',
                       # '--extract.removeVariable=latitude',
                       '--output.file={}'.format(tempfile),
                       '--output.type=grib', '--output.config={}'.format(xmlFile)]
                procOptions.append('--extract.selectVariables={}'.format(param))
                print(" ".join(procOptions))
                proc = subprocess.Popen(procOptions, cwd=basedir, stderr=errlog, stdout=outlog)
                if (proc.wait() != 0):
                    errlog.write("'{fimex}' in {dir} failed".format(fimex=' '.join(procOptions), dir=basedir))
                else:
                    # append tmp-file to final grib-file
                    with (open(basetempfile, 'rb')) as th:
                        while True:
                            data = th.read(16*1024*1024) # read max 16M blocks
                            if data:
                                gh.write(data)
                            else:
                                break
                if (os.path.exists(basetempfile)):
                    os.remove(basetempfile)

    errlog.close()
    outlog.close()


if __name__ == "__main__":
    ncfile = sys.argv[1]
    ident = sys.argv[2]
    isotopes = [int(sys.argv[3])]
    dirname = os.path.dirname(ncfile)
    convert_to_grib(ncfile, dirname, ident, isotopes)
