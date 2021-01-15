#! /usr/bin/env python3
import argparse
import pathlib
import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
import re


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Outputs plots over time spent on the various computations for SNAP"
    )
    parser.add_argument("logfile", type=pathlib.Path)
    parser.add_argument("--mode", choices=["system_clock", "cpu", "wtime"], default="cpu")

    args = parser.parse_args()
    logfile = args.logfile

    treg = re.compile("\([\w ]+\)")

    def timesplit(s: str):
        sys, cpu, *rest = re.split(treg, s)
        wtime = None
        if len(rest) > 1:
            wtime = rest.pop(0)
        if args.mode == "system_clock":
            t = sys
        elif args.mode == "cpu":
            t = cpu
        else:
            t = wtime

        hour, minute, rest = t.split(":")
        second, ms = rest.split(".")

        return int(hour) * 3600 + int(minute) * 60 + int(second) + int(ms) / 1000

    isteps = []
    nparts = []

    output_time = []
    timeloop = []
    metinput_time = []
    particleloop = []

    with logfile.open("r") as f:
        for line in f:
            line = line.strip()
            # Start of iteration
            if line.startswith("istep,nplume,npart"):
                _txt, istep, nplume, npart = line.split()
                istep = int(istep)

                if istep > 0:
                    isteps.append(istep)
                    nparts.append(int(npart))
                    output_time.append(current_output_time)
                    timeloop.append(current_timeloop_time)
                    metinput_time.append(current_metinput_time)
                    particleloop.append(current_particleloop_time)
                    assert istep == len(output_time)

                current_output_time = 0
                current_timeloop_time = 0
                current_metinput_time = 0
                current_particleloop_time = 0
            elif line.startswith("timer: output/accumulation:"):
                line = line[len("timer: output/accumulation:") :]
                current_output_time = timesplit(line)
            elif line.startswith("timer: Reading MET input:"):
                line = line[len("timer: Reading MET input:") :]
                current_metinput_time = timesplit(line)
            elif line.startswith("timer: Particle loop:"):
                line = line[len("timer: Particle loop:") :]
                current_particleloop_time = timesplit(line)
            elif line.startswith("timer: time_loop:"):
                line = line[len("timer: time_loop:") :]
                current_timeloop_time = timesplit(line)

    timeloop = np.array(timeloop)
    particleloop = np.array(particleloop)
    outputs = np.array(output_time)
    met_input = np.array(metinput_time)
    isteps = np.array(isteps)
    nparts = np.array(nparts)

    fig = plt.figure()
    gs = mpl.gridspec.GridSpec(2, 2)

    ax2 = fig.add_subplot(gs[0, :])
    ax2.stackplot(
        isteps,
        particleloop,
        outputs,
        met_input,
        labels=["Particle loop", "accumulation/output", "MET input"],
    )
    ax2.plot(isteps, timeloop, label="total time")
    ax2.set_ylabel("Time per iteration [s]")
    ax2.legend()
    ax2.set_xlim((isteps[0], isteps[-1]))

    ax3 = fig.add_subplot(gs[1, :])
    factor = 1e6 / nparts
    ax3.stackplot(
        isteps,
        particleloop * factor,
        outputs * factor,
        met_input * factor,
        labels=["Particle loop", "accumulation/output", "MET input"],
    )
    ax3.plot(isteps, timeloop * factor, label="total time")
    ax3.set_ylabel("Time per iteration per particle [µs]")
    ax3.set_ylim([0, 2])
    ax3.set_xlim((isteps[0], isteps[-1]))
    ax3.legend()

    plt.show()
