#! /bin/sh

cd /home/heikok/Programme/Models/SNAP/SnapRunner
gnome-terminal -e 'perl snapRunner.pl' --title 'SNAP runner log'&
sleep 2;
gnome-open http://localhost:8081/snaprunner/&
