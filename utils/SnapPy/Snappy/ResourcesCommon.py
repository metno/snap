import enum
import os
import subprocess
import sys

"""
Common Resource-definitions for EEmep and Snap, parent abstract interface
"""


class LustreDir(enum.Enum):
    LUSTREDIR = "/lustre/storeX"
    STORE = "storeX"
    MET_PRODUCTION_DIR = "/lustre/metproductionX"
    MET_ARCHIVE_DIR = "/lustre/arkivX"
    LF_PROD_DIR = "/lustre/metproductionLF"  # for transition, to be removed when all metprodction-data is actually on metproductionX


class ResourcesCommon:
    """
    common resources for snap and eemep, setting/using the following variables:

    self._lustredir
    """

    def initLustreDirs(self):
        """
        Initialize the lustre directories, either from environment variable, from
        lustredir_serviceenv.sh/kepler or default value. The result is cached in self._lustredir
        for later use.
        :raises: ValueError if any of the directories does not exist or is not a directory
        """
        if not hasattr(self, "_lustredir"):
            lustredir = "/lustre/storeB"
            store = os.getenv("STORE", None)
            metproductiondir = os.getenv("MET_PRODUCTION_DIR", None)
            lfprod_dir = os.getenv("LF_PROD_DIR", None)
            archivedir = os.getenv("MET_ARCHIVE_DIR", None)
            if store:
                lustredir = os.getenv(
                    "LUSTREDIR", os.path.join(os.sep, "lustre", store)
                )
                if not metproductiondir:
                    raise ValueError(
                        "MET_PRODUCTION_DIR environment variable is required when STORE is set"
                    )
                if not lfprod_dir:
                    raise ValueError(
                        "LF_PROD_DIR environment variable is required when STORE is set"
                    )
                if not archivedir:
                    raise ValueError(
                        "MET_ARCHIVE_DIR environment variable is required when STORE is set"
                    )
            else:
                lustredirenv = self._getLustreMappEnv()
                if os.path.isdir(lustredirenv):
                    lustredir = lustredirenv
                metproductiondirenv = self._getLustreMappEnv(
                    LustreDir.MET_PRODUCTION_DIR
                )
                if os.path.isdir(metproductiondirenv):
                    metproductiondir = metproductiondirenv
                # hack on vgl-servers to find archivedir and lfprod_dir, since not set in serviceenv.sh
                if lustredir.endswith("A"):
                    store = "storeA"
                    archivedir = "/lustre/storeA/immutable/archive"
                    if os.path.isdir("/lustre/metproductionA"):
                        lfprod_dir = "/lustre/metproductionA"
                    else:
                        lfprod_dir = "/lustre/metproduction"
                else:
                    store = "storeB"
                    archivedir = "/lustre/arkivB"
                    lfprod_dir = "/lustre/metproductionB"

            self._lustredir = {
                LustreDir.LUSTREDIR: lustredir,
                LustreDir.STORE: store,
                LustreDir.MET_PRODUCTION_DIR: metproductiondir,
                LustreDir.MET_ARCHIVE_DIR: archivedir,
                LustreDir.LF_PROD_DIR: lfprod_dir,
            }
            # test if all _lustredir are directories or links to directories, if not, raise ValueError
            for dir_type, dir_path in self._lustredir.items():
                if dir_type.name.endswith("DIR") and not os.path.isdir(dir_path):
                    raise ValueError(
                        f"{dir_type.value} directory {dir_path} does not exist or is not a directory"
                    )
            return

    def _getLustreDir(self, dir_type: LustreDir = LustreDir.LUSTREDIR) -> str:
        """Get the cached directory name of dir_type, either from environment variable,
        from lustredir_serviceenv.sh or default value.
        The result is cached in self._lustredir for later use.

        :param dir_type: defaults to LustreDir.LUSTREDIR
        :return: string with the directory name of dir_type
        """
        self.initLustreDirs()
        return self._lustredir[dir_type]

    def formatWithLustreDirs(self, template: str) -> str:
        """Format the given template string with the lustre directories, replacing
        {LUSTREDIR}, {MET_PRODUCTION_DIR}, {MET_ARCHIVE_DIR} and {LF_PROD_DIR}.

        :param template: string with placeholders for lustre directories
        :return: formatted string with lustre directories
        """
        return template.format(
            LUSTREDIR=self._getLustreDir(LustreDir.LUSTREDIR),
            MET_PRODUCTION_DIR=self._getLustreDir(LustreDir.MET_PRODUCTION_DIR),
            MET_ARCHIVE_DIR=self._getLustreDir(LustreDir.MET_ARCHIVE_DIR),
            LF_PROD_DIR=self._getLustreDir(LustreDir.LF_PROD_DIR),
        )

    def lustreTemplateDirs(self, dirs):
        return [self.formatWithLustreDirs(x) for x in dirs]

    @staticmethod
    def _getLustreMappEnv(dir_type: LustreDir = LustreDir.LUSTREDIR) -> str:
        service = "lustredir_serviceenv.sh"
        if dir_type == LustreDir.LUSTREDIR:
            pass
        elif dir_type == LustreDir.MET_PRODUCTION_DIR:
            service = "metproductiondir_serviceenv.sh"
        else:
            raise ValueError(
                f"Unsupported dir_type {dir_type}, only LUSTREDIR and MET_PRODUCTION_DIR are supported"
            )

        sh_script = os.path.join(os.path.dirname(__file__), f"resources/{service}")
        lustredir = "/no_such_file"
        try:
            proc = subprocess.run(
                ["/bin/sh", sh_script],
                check=True,
                stdout=subprocess.PIPE,
                timeout=10,
                encoding="utf-8",
            )
            for line in proc.stdout.splitlines():
                if line.rstrip():
                    lustredir = line.rstrip()  # lustredir in last line
        except subprocess.SubprocessError as se:
            print(se, file=sys.stderr)

        return lustredir


if __name__ == "__main__":
    # test code
    rc = ResourcesCommon()
    print(rc._getLustreDir())
    assert rc._getLustreDir().startswith("/lustre/store")
    assert len(rc._getLustreDir()) < 15
