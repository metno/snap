import datetime as dt
import os

from Snappy.Resources import MetModel
from Snappy.EEMEP.Resources import Resources


class SnapAshResources:
    @staticmethod
    def get_snap_input(metmodel: str) -> str:
        """get the contents of the snap.input for a certain metmodel"""
        with open(
            os.path.join(
                os.path.dirname(__file__), f"resources/snapash.input_{metmodel}.tmpl"
            )
        ) as fh:
            input_string = fh.read()
        return input_string

    @staticmethod
    def getMeteorologyFiles(
        metmodel, dtime: dt.datetime, run_hours: int, fixed_run="best"
    ):
        if metmodel == MetModel.NrpaEC0p1:
            res = Resources()
            return res.getECMeteorologyFiles(dtime, run_hours, fixed_run)
        else:
            raise RuntimeError(f"metmodel {metmodel} not implemented")
