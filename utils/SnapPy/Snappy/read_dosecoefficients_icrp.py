#! /usr/bin/env python3
import pandas as pd
import numpy as np
import pathlib
import zipfile


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
        particulates = pd.read_csv(path, skiprows=5)
        particulates.columns = particulates.columns.str.strip()
        particulates.drop(columns=["Unnamed: 6"], inplace=True)

        # Forward fill to get nuclide for all species
        particulates.fillna(method="ffill", inplace=True)
        # Drop label "Hydrogen/Beryllium/..."
        particulates = particulates[particulates["Nuclide"].str.contains("-")]

        return particulates

    @staticmethod
    def _parse_gases(path):
        gases = pd.read_csv(path, skiprows=3)
        gases = gases[~np.isnan(gases["e (Sv Bq-1)"])]
        nuclides = []
        chemical_forms = []
        prev = None
        for (irow, row) in gases.iterrows():
            iso, *rest = row[0].split(" ")
            chem_form = " ".join(rest).strip()
            chemical_forms.append(chem_form)
            if iso == "":
                nuclides.append(prev)
            else:
                prev = iso
                nuclides.append(iso)

        gases["Nuclide"] = nuclides
        gases["Chemical form"] = nuclides

        return gases

    @staticmethod
    def _parse_noble_gases(path):
        n_gases = pd.read_csv(path, skiprows=5)
        n_gases = n_gases[~np.isnan(n_gases["(Sv d-1 per Bq m-3)"])]
        n_gases["Sv h-1 per Bq m-3"] = n_gases["(Sv d-1 per Bq m-3)"] / 24

        return n_gases

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
            candidates = self.particulates[self.particulates["Nuclide"] == iso]
            return np.max(candidates["e (1 ?m)"]) * breathing_volume_per_hour
        elif typ == "gas":
            candidates = self.gases[self.gases["Nuclide"] == iso]
            return np.max(candidates["e (Sv Bq-1)"]) * breathing_volume_per_hour
        elif typ == "noble":
            candidates = self.noble_gases[self.noble_gases["Nuclide"] == iso]
            return np.max(candidates["Sv h-1 per Bq m-3"])
        else:
            raise ValueError(f"Unknown type requested: {typ}")


if __name__ == "__main__":
    # Download from https://www.sciencedirect.com/science/article/pii/S0146645313000110
    # Direct link: https://ars.els-cdn.com/content/image/1-s2.0-S0146645313000110-mmc1.zip
    path = "1-s2.0-S0146645313000110-mmc1.zip"
    d = DoseCoefficientsICRP(path)

    print(d.DPUI("Cs-137", "particulate"))
    print(d.DPUI("Cs-137", "particulate"))
    print(d.DPUI("I-131", "gas"))
    print(d.DPUI("Xe133", "noble"))
