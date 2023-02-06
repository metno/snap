#! /usr/bin/env python3

import argparse
import datetime
import pathlib
from collections import defaultdict, OrderedDict, deque

import matplotlib.pyplot as plt
import numpy as np


def ftest_split(line: str) -> (str, float, float, float):
    name, a, b, c = line.split()
    return (name, float(a), float(b), float(c))


def main(logfile: pathlib.Path):
    content = logfile.read_text()
    lines = deque(content.split("\n"))

    massbalance = defaultdict(OrderedDict)
    while len(lines) > 0:
        line = lines.popleft()
        line = line.strip()
        if line.startswith("fldout."):
            _, year, month, day, hour = line.split()
            dt = datetime.datetime(
                year=int(year), month=int(month), day=int(day), hour=int(hour)
            )
            accprec = lines.popleft()
            hbl = lines.popleft()
            avghbl = lines.popleft()
            prec = lines.popleft()

            while lines[0].strip().startswith("component"):
                comp = lines.popleft()
                _, comp = comp.split(": ")
                try:
                    # Skip all ftest lines
                    while True:
                        ftest = ftest_split(lines[0])
                        lines.popleft()
                except ValueError as _e:
                    pass

                buckets = dict()
                while lines[0].strip().startswith("Bq"):
                    line = lines.popleft()
                    name, value = [l.strip() for l in line.split(":")]
                    try:
                        value, *_ = [l.strip() for l in value.split(" ")]
                    except ValueError as _e:
                        pass
                    buckets[name] = float(value)

                massbalance[comp][dt] = buckets

    fig, axs = plt.subplots(2, len(massbalance), sharex=True)
    if len(axs.shape) == 1:
        axs = axs.reshape((2, 1))

    for i, (k, v) in enumerate(massbalance.items()):
        times = v.keys()
        abl_inside = []
        abl_outside = []
        added = []
        domain = []
        lost = []
        wetdep = []
        drydep = []

        for v in v.values():
            abl_inside.append(v["Bq,particles in    abl"])
            abl_outside.append(v["Bq,particles above abl"])
            added.append(v["Bq,particles added"])
            domain.append(v["Bq,particles (domain)"])
            lost.append(v["Bq,particles lost (misc)"])
            keyname = "Bq,particles wet dep"
            if keyname in v:
                wetdep.append(v[keyname])
            keyname = "Bq,particles dry dep"
            if keyname in v:
                drydep.append(v[keyname])

        lost, added, abl_inside, abl_outside, domain = (
            np.array(lost),
            np.array(added),
            np.array(abl_inside),
            np.array(abl_outside),
            np.array(domain),
        )
        plots = [lost, abl_inside, abl_outside, domain]
        labels = ["Lost (misc)", "Inside ABL", "Outside ABL", "Outside domain"]
        if len(wetdep) > 0:
            wetdep = np.array(wetdep)
            plots.append(wetdep)
            labels.append("Deposited (wet)")
        if len(drydep) > 0:
            drydep = np.array(drydep)
            plots.append(drydep)
            labels.append("Deposited (dry)")

        plots = list(reversed(plots))
        labels = list(reversed(labels))

        axs[0, i].stackplot(times, *plots, labels=labels)
        axs[0, i].plot(times, added, label="Total added", linewidth=4, color="k")

        axs[0, i].set_title(f"{comp}")
        axs[0, i].legend()
        # axs[0, i].set_xlabel("Time")
        axs[0, i].set_ylabel("Activity [Bq]")

        axs[1, i].set_ylim((0, 1))
        axs[1, i].plot(
            times, added / added, label="Total added", linewidth=4, color="k"
        )
        if len(drydep) > 0:
            axs[1, i].plot(times, drydep / added, label="Deposited (dry)")
        if len(wetdep) > 0:
            axs[1, i].plot(times, wetdep / added, label="Deposited (wet)")
        axs[1, i].plot(times, abl_outside / added, label="Outside ABL")
        axs[1, i].plot(times, abl_inside / added, label="Inside ABL")
        axs[1, i].plot(times, domain / added, label="Outside domain")
        axs[1, i].plot(times, lost / added, label="Lost (misc)")

        axs[1, i].legend()
        axs[1, i].set_xlabel("Time")
        axs[1, i].set_ylabel("Percent of added activity")

    plt.show()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("logfile", type=pathlib.Path)

    args = parser.parse_args()
    main(args.logfile)
