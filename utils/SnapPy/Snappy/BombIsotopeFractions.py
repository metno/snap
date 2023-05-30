import os
import csv
import re

class BombIsotopeFractions:
    # fractions{isotope} = {0: frac0, 10: frac10, 20: frac20} with 0-40 in hours and frac0-frac40 fractions of total fission-products in Bq
    _fractions = None
    _timesteps = [0,10,20,30,40]
    def __new__(cls):
        '''BombIsotopeFractions singleton data'''
        if cls._fractions is None:
            cls._fractions = dict()
            directory = os.path.join(os.path.dirname(__file__), "resources")
            with open(
                os.path.join(directory, "bomb-isotope-distribution_Tovedal.csv"), mode="r", encoding="UTF-8", newline=''
            ) as fh:
                csvreader = csv.reader(fh, delimiter=',')
                for i in range(2):
                    next(csvreader)
                header = next(csvreader)
                offset = 9
                for i,hrs in enumerate(cls._timesteps):
                    if f"t={hrs}" not in header[offset+i]:
                        raise Exception(f"error in header for hour {hrs}: {header[offset+i]}")
                for row in csvreader:
                    if '-' in row[0]:
                        isotope = row[0].replace('-', '') # without - as ususal in snappy
                        stepfraction = {}
                        for i,hrs in enumerate(cls._timesteps):
                            stepfraction[hrs] = float(row[offset+i])/100.
                        cls._fractions[isotope] = stepfraction
        obj = object.__new__(cls)
        return obj
    
    def isotopes(self):
        '''
            list over isotopes as ['Cs137', 'Cs134', ...]
        '''
        return self._fractions.keys()
    
    def fraction(self, isotope: str, hrs: int) -> float:
        '''
            @param isotope is a isotope name like Cs137 or Cs-137
            @param hrs since bomb, intra/extrapolated
            return a fraction of the total activity
        '''
        isotope = isotope.replace("-", "")
        stepfracs = self._fractions[isotope]
        if hrs < 0:
            return 0
        if hrs > self._timesteps[-1]:
            return stepfracs[self._timesteps[-1]]
        if hrs == self._timesteps[0]:
            return stepfracs[self._timesteps[0]]

        for i, nhr in enumerate(self._timesteps):
            if nhr >= hrs:
                phr = self._timesteps[i-1]
                hfrac = (hrs-phr)/(nhr-phr)
                nfrac = stepfracs[nhr]
                pfrac = stepfracs[phr]
                frac = pfrac + hfrac*(nfrac-pfrac)
                return frac


if __name__ == "__main__":
    bfracs = BombIsotopeFractions()
    assert('Cs137' in bfracs.isotopes())
    assert(bfracs.fraction('Cs137',0) == 0.0002/100)
    assert(len(bfracs.isotopes()) > 10)
    for hr in range(0,48):
        tot = 0
        for iso in bfracs.isotopes():
            tot += bfracs.fraction(iso, hr)
        assert(tot > .99)
        assert(tot < 1.01)
