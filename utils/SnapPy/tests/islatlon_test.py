"""Tests for lat/lon parsing functions."""

import pytest
from Snappy.Utils import parseLat, parseLon


class TestParseLat:
    """Test latitude parsing."""

    def test_parse_lat_decimal_negative(self):
        assert abs(parseLat(-3.54) - (-3.54)) < 1e-3

    def test_parse_lat_decimal_string(self):
        assert abs(parseLat("-3.54") - (-3.54)) < 1e-3

    def test_parse_lat_with_south(self):
        assert abs(parseLat("3.54 S") - (-3.54)) < 1e-3

    def test_parse_lat_dms_south(self):
        assert abs(parseLat("3:5:3 S") - (-3.0841)) < 1e-3

    def test_parse_lat_dms_with_symbols_south(self):
        assert abs(parseLat("3 °5' 3\" S") - (-3.0841)) < 1e-3

    def test_parse_lat_dms_with_symbols_north(self):
        assert abs(parseLat("60°5'5\"N") - 60.084722) < 1e-3

    def test_parse_lat_with_special_chars(self):
        assert abs(parseLat("8°20′2+″S+") - (-8.333)) < 1e-3

    def test_parse_lat_compact_north(self):
        assert abs(parseLat("N6451") - 64.85) < 1e-3

    def test_parse_lat_compact_south(self):
        # Reventador PSN: S0004 W07739
        assert abs(parseLat("S0004") - (-0.0666)) < 1e-3

    def test_parse_lat_invalid_range(self):
        with pytest.raises(ValueError):
            parseLat("195")


class TestParseLon:
    """Test longitude parsing."""

    def test_parse_lon_decimal_negative(self):
        assert abs(parseLon(-3.54) - (-3.54)) < 1e-3

    def test_parse_lon_decimal_string(self):
        assert abs(parseLon("-3.54") - (-3.54)) < 1e-3

    def test_parse_lon_with_west(self):
        assert abs(parseLon("3.54 W") - (-3.54)) < 1e-3

    def test_parse_lon_dms_west(self):
        assert abs(parseLon("3:5:3 W") - (-3.0841)) < 1e-3

    def test_parse_lon_dms_west_compact(self):
        assert abs(parseLon("10:5:5W") - (-10.084722)) < 1e-3

    def test_parse_lon_dm_west(self):
        assert abs(parseLon("10:5W") - (-10.08333)) < 1e-3

    def test_parse_lon_dms_with_symbols_west(self):
        assert abs(parseLon("3 °5' 3\" W") - (-3.0841)) < 1e-3

    def test_parse_lon_dm_with_symbols_west(self):
        assert abs(parseLon("10°4'W") - (-10.06666)) < 1e-3

    def test_parse_lon_compact_west(self):
        assert abs(parseLon("W01947") - (-19.7833333)) < 1e-3

    def test_parse_lon_compact_south_west(self):
        # Reventador PSN: S0004 W07739
        assert abs(parseLon("W07739") - (-77.65)) < 1e-3

    def test_parse_lon_invalid_range(self):
        with pytest.raises(ValueError):
            parseLon("370")
