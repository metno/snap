# Convert a eEMEP Ash ColumnSource Emission File to a SNAP release.txt file

import datetime
import math

# 7BIN VAAC distribution (Gravset_mod.f90), last is e.g. size 30-100, log-normal-distribution
diameters = [0.1, 0.3, 1.0, 3.0, 10.0, 30.0, 100.0]
distrib   = [0.0, 0.1, 0.5, 5.0, 20.0, 70.0,   4.4]
classnames= [f"ASH_{i}" for i,_ in enumerate(diameters)]

def readEmepWriteSnap(infile, outfile, outsnap):
    FIELDS = {
        x: i for i, x in enumerate(
            'TYPE,VARIABLE,BASE[km],H[km],D[h],dM/dt[kBq/s],m63,START,END,DESCRIPTION'.split(',')
        )
    }
    data = []
    with open(infile, 'rt') as ifh:
        for line in ifh:
            line = line.strip()
            if line.startswith('#'):
                continue
            if not line:
                continue
            row = line.split(',')
            datarow = {}
            for x in ('START', 'END'):
                datarow[x] = datetime.datetime.strptime(row[FIELDS[x]].strip(), "%Y-%m-%d %H:%M:%S")
            for x in ('dM/dt[kBq/s]','m63'):
                datarow[x] = float(row[FIELDS[x]])
            for x in ('BASE[km]', 'H[km]'):
                ox = x.replace('km','m')
                datarow[ox] = round(float(row[FIELDS[x]])*1000)
            data.append(datarow)

    with open(outfile, 'wt') as ofh:
        ofh.write("* hour  height_lower  comp  release[Bq/s]\n")
        last_start = None
        last_end = None
        release_start = None
        release_hour_steps = []
        release_lower = []
        release_upper = []
        for row in data:
            if release_start is None:
                release_start = row['START']
                release_hour_steps.append(0)
                last_start = row['START']
                last_end = row['END']
            if last_start < row['START']:
                if last_end != row['START']:
                    if last_end > row['START']:
                        raise Exception(f"last(END) '{last_end}' and new START {row['START']} not aligned")
                    # create an extra step with 0 emissions
                    step = (last_end - last_start).total_seconds() / (60 * 60)
                    release_hour_steps.append(step + release_hour_steps[-1])
                    last_start = last_end
                    last_end = row['START']
                    for rl in release_lower:
                        for cn in classnames:
                            ofh.write(f"{release_hour_steps[-1]} {rl} {cn} 0\n")
                step = (row['START'] - last_start).total_seconds() / (60 * 60)
                release_hour_steps.append(step + release_hour_steps[-1])
                last_start = last_end
                last_end = row['START']
            # write the current emission
            if not row['BASE[m]'] in release_lower:
                release_lower.append(row['BASE[m]'])
            if not row['H[m]'] in release_upper:
                release_upper.append(row['H[m]'])
            for i, cn in enumerate(classnames):
                emis = row["dM/dt[kBq/s]"] * 1000 * distrib[i] / 100 # % -> frac; kBq -> Bq
                ofh.write(f"{release_hour_steps[-1]} {row['BASE[m]']} {cn} {emis:.0f}\n")

    with open(outsnap, 'wt') as ofh:
        ofh.write(f"TIME.START= {release_start: %Y %m %d %H}\n")
        ofh.write(f"RELEASE.FILE= {outfile}\n")
        ofh.write(f"RELASE.HEIGHTLOWER.M = {', '.join([str(x) for x in release_lower])}\n")
        ofh.write(f"RELASE.HEIGHTUPPER.M = {', '.join([str(x) for x in release_lower])}\n")
        ofh.write(f"RELASE.HEIGHTRADIUS.M = {', '.join(['1000' for _ in release_lower])}\n")
        comps = [f"'{x}'" for x in classnames]
        ofh.write(f"RELEASE.COMPONENTS= {', '.join(comps)}\n\n")
        for i, cn in enumerate(classnames):
            if i < 2:
                radius = diameters[i] / 2
            else:
                radius = .5 * 10**((math.log(diameters[i], 10) + math.log(diameters[i-1], 10))/2)
            if radius <= 0.05:
                radius = 0.06 # in snap r <= 0.05 is gaseous
            ofh.write(f"COMPONENT= {cn}\n")
            ofh.write("DRY.DEP.ON\n")
            ofh.write("WET.DEP.ON\n")
            ofh.write("RADIOACTIVE.DECAY.OFF\n")
            ofh.write(f"FIELD.IDENTIFICATION= {i+1:02d}\n")
            ofh.write(f"RADIUS.MICROMETER={radius:.2f}\n")
            ofh.write("DENSITY.G/CM3=1\n\n") # rephra between 0.4 and 1.5

if __name__ == "__main__":
    readEmepWriteSnap("/lustre/storeB/project/fou/kl/eva/eemep/TestRuns_Eyja/eemep_Eyjafjoll_ondemand_20230413/columnsource_emission_eya.csv",
                      "release.txt",
                      "snap_eyja.input")
