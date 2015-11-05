#! /bin/bash
# SNAP-beregning


rm prod/*.png
SETUP=/etc/diana/setup

clear;
echo ""
echo -n "Hvilket kartutsnitt vil du ha (f.eks: N.halvkule). Dersom du ønsker Globalt, trykk enter : " ;
read p
p=$p

if test "$p" = "" 
then 
p=Globalt
fi

bdiana -i snap.in -s diana.setup p=$p
mkdir prod/
mv snap_* prod/
./sendmail.sh prod/*.png
clear
echo "SNAP-beregning er ferdig.."
sleep 1;

echo ""
echo -n "Vil du starte DIANA for å se på kjøringen [j/n]? : "
read s

if test $s = "j"; then
diana.bin -s diana.setup &
else
exit 0;
fi

#laget av statsmeteorolog Bjart Eriksen, VA, MET
