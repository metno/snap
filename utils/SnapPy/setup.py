#!/usr/bin/env python3

from distutils.core import setup
import os

version=os.getenv('VERSION', "0.4")

setup(name='Snappy',
      version=version,
      description='SNAP GUI in python',
      author='Heiko Klein',
      author_email='Heiko.Klein@met.no',
      url='https://gitlab.met.no/emep/snap',
      packages=['Snappy', 'Snappy.EEMEP'],
      package_dir={'Snappy': 'Snappy',
                   'Snappy.EEMEP': 'Snappy/EEMEP'},
      package_data={'Snappy': ['resources/*'],
                    'Snappy.EEMEP': ['resources/*']},
      scripts=['snapPy', 'snap4rimsterm', 'snapCombineInverse'],
)

