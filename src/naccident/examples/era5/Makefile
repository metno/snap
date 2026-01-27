#Makefile to clean code or run full procedure.
date := 2026-01-02
utc := 06
clean:
	rm -f *.grib
	rm -f snap.log
	rm -f *.nc

run:
	make clean

	python3 download_ml.py 2026-01-02 06
	python3 download_sfc.py 2026-01-02 06

	cat *.grib > all.grib

	fimex --input.file=all.grib --input.config=cdmGribReaderConfigEC_Era5.xml --output.file=converted.nc --output.type=nc4

	bsnap_naccident snap.input


run_bomb:
	make clean

	python3 download_ml.py $(date) $(utc)
	python3 download_sfc.py $(date) $(utc)

	cat *.grib > all.grib

	fimex --input.file=all.grib --input.config=cdmGribReaderConfigEC_Era5.xml --output.file=converted.nc --output.type=nc4

	bsnap_naccident snap_bomb.input

run_NPP:
	make clean

	python3 download_ml.py $(date) $(utc)
	python3 download_sfc.py $(date) $(utc)

	cat *.grib > all.grib

	fimex --input.file=all.grib --input.config=cdmGribReaderConfigEC_Era5.xml --output.file=converted.nc --output.type=nc4

	bsnap_naccident snap_NPP.input