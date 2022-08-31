import os

class Isotopes:
    _isoByName = None
    _isoById = None
    def __new__(cls):
        '''Isotopes singleton data'''
        if cls._isoById is None:
            cls._isoById = dict()
            cls._isoByName = dict()
            directory = os.path.join(os.path.dirname(__file__), "resources")
            with open(
                os.path.join(directory, "isotope_list.txt"), mode="r", encoding="UTF-8"
            ) as isoFH:
                for line in isoFH:
                    if line.strip() != "":
                        isoId = int(line[0:4])
                        el = line[4:7].strip()
                        iso = line[8:13].strip()
                        isoType = int(line[13:14])
                        decay = float(line[14:])
                        isotope = {
                            "id": isoId,
                            "isotope": "{0}{1}".format(el, iso),
                            "type": isoType,
                            "decay": decay,
                        }
                        cls._isoById[isoId] = isotope
                        cls._isoByName[isotope["isotope"]] = isotope
        obj = object.__new__(cls)
        return obj
    
    def byId(self, id):
        '''
            id is a number in the isotopes file
            return a dict with isotopte-name, type=0,1,2 (nobelgas, gas, aerosol) and decayrate (/s)
        '''
        return self._isoById[id]

    def byName(self, name):
        '''
            @param name is a isotope name like Cs137
            return a dict with isotopte-name, type=0,1,2 (nobelgas, gas, aerosol) and decayrate (/s)
        '''
        return self._isoByName[name]


if __name__ == "__main__":
    isotopes = Isotopes()
    print(isotopes.byName('Cs137'))
    print(isotopes.byName('I131'))