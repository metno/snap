from pathlib import Path

import pytest
from Snappy.Utils import restrictDomainSizeAndResolution

TESTDATA_DIR = Path(__file__).resolve().parents[3] / "src" / "test" / "snap_testdata"


def _find_source_file(pattern: str) -> Path:
    if not TESTDATA_DIR.exists():
        pytest.skip(f"test-data directory not available: {TESTDATA_DIR}")
    matches = sorted(TESTDATA_DIR.glob(pattern))
    if not matches:
        pytest.skip(
            f"no test-data file found for pattern {pattern!r} in {TESTDATA_DIR}"
        )
    return matches[0]


LATLON_FILE = _find_source_file("meteo20260113_00_sellafield.nc")
LAMBERT_FILE = _find_source_file("meps_det_2_5km_20251125T06Z_ringhals.nc")


def _require_file(path: Path) -> None:
    if not path.exists():
        pytest.skip(f"file not available: {path}")


def test_latlon():
    _require_file(LATLON_FILE)
    # Sellafield coordinates: 54.5N, 3.6W
    assert restrictDomainSizeAndResolution(str(LATLON_FILE), -4, 54, 0, 0) == ""


def test_latlon_domain_size():
    _require_file(LATLON_FILE)
    assert restrictDomainSizeAndResolution(str(LATLON_FILE), -4, 54, 0, 20).startswith(
        "FIMEX.INTERPOLATION=nearest|+proj=latlon +R=6371000 +no_defs|-5"
    )


def test_latlon_supersampling():
    _require_file(LATLON_FILE)
    assert restrictDomainSizeAndResolution(
        str(LATLON_FILE), -4, 54, 0.05, 20
    ).startswith(
        """FIELD.OUTPUT_RESOLUTION_FACTOR= 2
FIMEX.INTERPOLATION=nearest|+proj=latlon +R=6371000 +no_defs|-4"""
    )


def test_latlon_supersampling_only():
    _require_file(LATLON_FILE)
    assert restrictDomainSizeAndResolution(
        str(LATLON_FILE), -4, 54, supersampling=5
    ).startswith("FIELD.OUTPUT_RESOLUTION_FACTOR= 5")


def test_lambert():
    _require_file(LAMBERT_FILE)
    # ringhals coordinates: 12.1E, 57.3N
    assert restrictDomainSizeAndResolution(str(LAMBERT_FILE), 12, 57, 0, 0) == ""


def test_lambert_domain_size():
    _require_file(LAMBERT_FILE)
    assert restrictDomainSizeAndResolution(str(LAMBERT_FILE), 12, 57, 0, 20).startswith(
        "FIMEX.INTERPOLATION=nearest|+proj=lcc +lat_0=63.3 +lon_0=15 +lat_1=63.3 +lat_2=63.3 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs|-21"
    )


def test_lambert_supersampling():
    _require_file(LAMBERT_FILE)
    assert restrictDomainSizeAndResolution(
        str(LAMBERT_FILE), 12, 57, 1.25, 20
    ).startswith(
        """FIELD.OUTPUT_RESOLUTION_FACTOR= 2
FIMEX.INTERPOLATION=nearest|+proj=lcc +lat_0=63.3 +lon_0=15 +lat_1=63.3 +lat_2=63.3 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs|-19"""
    )


def test_lambert_supersampling_only():
    _require_file(LAMBERT_FILE)
    assert restrictDomainSizeAndResolution(
        str(LAMBERT_FILE), 12, 57, supersampling=5
    ).startswith("FIELD.OUTPUT_RESOLUTION_FACTOR= 5")
