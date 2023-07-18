#! /usr/bin/env python3
import numpy as np
import zipfile
import csv
import io


class DoseCoefficientsICRP:
    def __init__(self, path):
        with zipfile.ZipFile(path) as zipf:
            with zipf.open("CSV/TableA_1.csv") as tableA:
                self.particulates = DoseCoefficientsICRP._parse_particulates(tableA)
            with zipf.open("CSV/TableB_1.csv") as tableB:
                self.gases = DoseCoefficientsICRP._parse_gases(tableB)
            with zipf.open("CSV/TableC_1.csv") as tableC:
                self.noble_gases = DoseCoefficientsICRP._parse_noble_gases(tableC)

    @staticmethod
    def _parse_particulates(path):
        for _ in range(8):
            path.readline()
        fieldnames = [
            "Nuclide",
            "T12",
            "Type",
            "f1 inhalation",
            "e (1um)",
            "e (5um)",
            "blank",
            "f1 ingestion",
            "e ingestion",
        ]
        with io.TextIOWrapper(path, encoding="utf-8") as f:
            reader = csv.DictReader(f, fieldnames=fieldnames)

            contents = []
            prev = None
            for row in reader:
                if prev is not None:
                    for fillforward in fieldnames:
                        if row[fillforward] == "":
                            row[fillforward] = prev[fillforward]
                prev = row
                if "-" not in row["Nuclide"]:
                    continue
                contents.append(row)

        nuclides = [row["Nuclide"] for row in contents]
        types = [row["Type"] for row in contents]
        e = [float(row["e (1um)"]) for row in contents]
        return (nuclides, types, e)

    @staticmethod
    def _parse_gases(path):
        for _ in range(4):
            path.readline()
        fieldnames = ["Nuclide/Chemical form", "T12", "e (Sv Bq-1)"]
        with io.TextIOWrapper(path, encoding="utf-8") as f:
            reader = csv.DictReader(f, fieldnames=fieldnames)

            contents = []
            prev = None
            for row in reader:
                if prev is not None:
                    for fillforward in fieldnames:
                        if row[fillforward] == "":
                            row[fillforward] = prev[fillforward]
                prev = row
                if row["e (Sv Bq-1)"] == "":
                    continue
                contents.append(row)

        nuclides = []
        chemical_forms = []
        prev = None
        for row in contents:
            iso, *rest = row["Nuclide/Chemical form"].split(" ")
            chem_form = " ".join(rest).strip()
            chemical_forms.append(chem_form)
            if iso == "":
                nuclides.append(prev)
            else:
                prev = iso
                nuclides.append(iso)

        e = [float(row["e (Sv Bq-1)"]) for row in contents]

        return (nuclides, chemical_forms, e)

    @staticmethod
    def _parse_noble_gases(path):
        for _ in range(6):
            path.readline()
        fieldnames = ["Nuclide", "T12", "Sv d-1 per Bq m-3"]
        with io.TextIOWrapper(path, encoding="utf-8") as f:
            reader = csv.DictReader(f, fieldnames=fieldnames)

            contents = []
            prev = None
            for row in reader:
                if prev is not None:
                    for fillforward in fieldnames:
                        if row[fillforward] == "":
                            row[fillforward] = prev[fillforward]
                prev = row
                if row["Sv d-1 per Bq m-3"] == "":
                    continue
                contents.append(row)

        nuclides = [row["Nuclide"] for row in contents]
        e = [row["Sv d-1 per Bq m-3"] for row in contents]
        e = [float(e) / 24 for e in e]

        return nuclides, e

    def DPUI(self, iso, typ):
        """Dose Per Unit Intake, [Sv / h per Bq/m3]

        The following coefficients are from
        Annals of the ICRP, ICRP Publication 119,
        Compendium of Dose Coefficients based on ICRP Publication 60

        Assumes breathing volume of 1m3 / h corresponding to resting
        """
        if "-" not in iso:
            n = 0
            while n < len(iso) and not iso[n].isdecimal():
                n += 1
            iso = iso[:n] + "-" + iso[n:]

        breathing_volume_per_hour = 1
        if typ == "particulate":
            candidates = [p for p in zip(*self.particulates) if p[0] == iso]
            if len(candidates) == 0:
                return None
            e_max = np.max([c[2] for c in candidates])
            return e_max * breathing_volume_per_hour
        elif typ == "gas":
            candidates = [p for p in zip(*self.particulates) if p[0] == iso]
            if len(candidates) == 0:
                return None
            e_max = np.max([c[2] for c in candidates])
            return e_max * breathing_volume_per_hour
        elif typ == "noble":
            candidates = [c for c in zip(*self.noble_gases) if c[0] == iso]
            if len(candidates) == 0:
                return None
            return candidates[0][1]
        else:
            raise ValueError(f"Unknown type requested: {typ}")


if __name__ == "__main__":
    # Download from https://www.sciencedirect.com/science/article/pii/S0146645313000110
    # Direct link: https://ars.els-cdn.com/content/image/1-s2.0-S0146645313000110-mmc1.zip
    path = "1-s2.0-S0146645313000110-mmc1.zip"
    d = DoseCoefficientsICRP(path)

    print(d.DPUI("Cs-137", "particulate"))
    print(d.DPUI("I-131", "gas"))
    print(d.DPUI("Xe133", "noble"))
    print(d.DPUI("Te-131", "particulate"))
