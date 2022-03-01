#! /usr/bin/env python3

import datetime
import subprocess
from Snappy.Resources import Resources
import os
import re
import sys
from time import gmtime, strftime

def get_isotope_release(res, reldict):
    errors = ""
    for tag in ('releaseTime', 'radius', 'lowerHeight', 'upperHeight'):
        if not tag in reldict:
            errors += "Cannot interprete {}: {}".format(tag, reldict[tag])

    source_tmpl = '''
MAX.PARTICLES.PER.RELEASE= 2000
TIME.RELEASE.PROFILE.STEPS
RELEASE.HOUR= 0, {releaseTime}
RELEASE.RADIUS.M= {radius}, {radius}
RELEASE.LOWER.M= {lowerHeight}, {lowerHeight}
RELEASE.UPPER.M= {upperHeight}, {upperHeight}
'''
    source_term = source_tmpl.format(releaseTime=reldict['releaseTime'],
                                        radius=reldict['radius'],
                                        lowerHeight=reldict['lowerHeight'], upperHeight=reldict['upperHeight'])

    isotopes = {'relI131': 'I131',
                'relXE133': 'Xe133',
                'relCS137': 'Cs137'}
    for rel, iso in isotopes.items():
        emis = 0.
        try:
            emis = float(reldict[rel])
        except:
            pass
        if (emis > 0.):
            source_term += "RELEASE.BQ/SEC.COMP= {rel}, {rel}, '{iso}'\n".format(rel=reldict[rel], iso=iso)

    # add Cs137, I131 and Xe133
    source_term += res.isotopes2snapinput([169, 158, 148])

    return (source_term, errors)

def runsnap(npps):
    now = datetime.datetime.now()
    res = Resources()
    nPPs = res.readNPPs()
    reldict = {
        'releaseTime': 96,
        'radius': 50,
        'lowerHeight': 50,
        'upperHeight': 500,
        'relI131': 1.39e13,
        'relXE133': 1.0e13,
        'relCS137': 2.6e11
    }
    (term, errors) = get_isotope_release(res, reldict)
    if (len(errors) > 0):
        print(errors, file=sys.stderr)
    nppdir = {}
    for tag in npps:
        assert tag in nPPs
        lat = float(nPPs[tag]['lat'])
        lon = float(nPPs[tag]['lon'])
        curtime = gmtime()
        nppdir[tag] = os.path.join(res.getSnapOutputDir(), "{0}_{1}".format(tag, strftime("%Y-%m-%dT%H%M%S", curtime)))
        simStart = strftime("%Y-%m-%d_%H:%M:%S", curtime)
        print(f'outputdir for {simStart}: {nppdir[tag]}')
        os.mkdir(nppdir[tag])
        sourceTerm = f"""
TITLE={tag} {now:%Y %m %d %H}
SIMULATION.START.DATE={simStart}
SET_RELEASE.POS= P=   {lat},   {lon}
TIME.START= {now:%Y %m %d %H}
TIME.RUN = 52h
STEP.HOUR.OUTPUT.FIELDS= 3
"""
        sourceTerm += term
        with open(os.path.join(nppdir[tag], "snap.input"),'w') as fh:
            fh.write(sourceTerm)
            files = res.getECMeteorologyFiles(now, 52, 'best')
            fh.write(res.getSnapInputMetDefinitions('nrpa_ec_0p1', files))
        with open(f"{nppdir[tag]}/snap.stdout.log", 'wt') as stdout:
            with open(f"{nppdir[tag]}/snap.stderr.log", 'wt') as stderr:
                subprocess.run(['/usr/bin/bsnap_naccident', 'snap.input'],
                                cwd=nppdir[tag],
                                stdout=stdout,
                                stderr=stderr
                                )
        # and plot
        prod_dir = os.path.join(nppdir[tag], 'prod')
        os.mkdir(prod_dir)
        with open(os.path.join(prod_dir, "diana.setup"),'wt') as fh:
            di_setup = f"""
%include /etc/diana/setup/diana.setup-COMMON
<FIELD_FILES>
filegroup=SNAP
m=SNAP.current t=fimex format=netcdf f={nppdir[tag]}/snap.nc
</FIELD_FILES>
"""
            fh.write(di_setup)

        hours = ",".join(["{x} | {y}".format(x=x, y=x+3) for x in range(0,48,3)])
        component = 'Cs137'
        dianaIn = os.path.join(prod_dir, "diana.in")
        dianaInTmpl = ""
        with open(res.getBSnapInputFile(), 'rt') as fh:
            for line in fh:
                if line.startswith('AREA'):
                    # zoom into area (lon_min:lon_max:lat_min:lat_max in radians)
                    dianaInTmpl += "AREA proj4string=\"+proj=latlon +R=6371000 +no_defs\" rectangle=-0.38:1.0:0.55:1.35\n"
                else:
                    dianaInTmpl += line
        with open(dianaIn, 'wt') as fh:
            fh.write(dianaInTmpl.format(hours=hours, component=component))
        # env-vars for bdiana without display
        os.environ['QT_QPA_PLATFORMTHEME']=''
        os.environ['QT_QPA_FONTDIR']='/usr/share/fonts/truetype'
        os.environ['QT_QPA_PLATFORM']='offscreen'

        subprocess.run(['bdiana', '-i', 'diana.in', '-s', 'diana.setup'],
                        cwd=prod_dir
                        )
        

    # montage
    opts = ['montage', '-mode', 'concatenate', '-tile', f"4x{len(npps)}"]
    for tag in npps:
        opts += ['-label', tag]
        for t in [12,24,36,48]:
            opts += [f"{nppdir[tag]}/prod/snap_{t}h.png"]
    opts.append(f'/home/VGLSHARE/SNAP4DSA/plots_{now:%Y-%m-%dT%H}0000Z.png')
    print('running montage with:\n'+" ".join(opts))
    subprocess.run(opts)




if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"usage: {sys.argv[0]} NPP1 [NPP2 NPP3 ...]", file=sys.stderr)
        sys.exit(2)
    
    npps = sys.argv[1:]
    runsnap(npps)