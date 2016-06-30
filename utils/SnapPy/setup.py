#!/usr/bin/env python3

from distutils.core import setup
import os

version=os.getenv('VERSION', "0.4")

setup(name='Snappy',
      version=version,
      description='SNAP GUI in python',
      author='Heiko Klein',
      author_email='Heiko.Klein@met.no',
#      url='https://www.python.org/sigs/distutils-sig/',
      packages=['Snappy'],
      package_dir={'Snappy': 'Snappy'},
      package_data={'Snappy': ['resources/*']},
      scripts=['snapPy', 'snap4rimsterm'],
)

