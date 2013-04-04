#!/bin/sh
#
# Script for running SNAP and creating forward trajectories for NRPA.
# Input file from NRPA: nrpa.input
#--------------------------------------------------------------------
# set -x
echo " "
echo "*** Today is: *** " ; date
echo "*** Directory is: *** " ; pwd
echo " "
echo "*** Maps of forward trajectories for NRPA***"
echo "Remove old files"
T
echo "Convert files from .eps -> .png" 
convert Traj_001.eps Traj_001.png
convert Traj_002.eps Traj_002.png
convert Traj_003.eps Traj_003.png 
echo "Remove .eps" 
rm *.eps
echo "*** END ***"
echo " "

