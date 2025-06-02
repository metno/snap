#!/bin/bash
if [[ "$1" == "-y" ]]; then
    echo "Creating debian package"
elif [[ "$1" == "-h" ]]; then
    cat -- "$0" | sed -n '/INSTRUCTIONS$/,/INSTRUCTIONS_END$/p'
    exit 0
else
    echo "Usage: $0 [-h|-y]"
    exit 0
fi

set -e

#INSTRUCTIONS
# 1 - Update version, changelog, etc. in this script
#     Then run this script with -y option (you have to have devscripts installed)
#
# 2 - Upload to internal package repository using dupload
#     (you need a properly configured .dupload.conf in your home directory, copy this from https://gitlab.met.no/it/sd/klient/linux/internrepo/-/raw/master/.dupload.conf)
#
#     Check first that it looks plausible:
#     $ dupload --no --to jammy dist/snap-py_<version>-1_amd64.changes
#     then remove --no option
#
#     Wait for confirmation email that package has been accepted
#
# 3 - Check that it works on one machine
#     $ ssh -X ppi-vglserver-b1.met.no
#     $ sudo apt-get update
#     $ sudo apt-get install snap-py
#     $ snapPy
#     If something goes wrong, downgrade to last version again using
#     $ sudo apt-get install snap-py=<version-number>
#
# 4 - Roll out to all machines with ansible (jammy/noble)
#     # Setup
#     $ git clone git@gitlab.met.no:met/mapp/desktop/vgl-ansible.git
#     $ sudo apt-get install ansible
#     # Rollout
#     $ cd vgl-ansible
#     #Roll out to selected host
#     $ ansible-playbook -i hosts --tags snap --limit ppi-vglserver-b1.met.no install.yml
#     #Roll out to all hosts
#     $ ansible-playbook -i hosts --tags snap install.yml
#
#     It may take a bit of time before the package is available for ansible (10 minutes)
#
# 5 - Inform meteorologists that a new version is available
#INSTRUCTIONS_END

if [ ! -f "Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip" ]; then
    wget https://ars.els-cdn.com/content/image/1-s2.0-S0146645313000110-mmc1.zip --output-document Snappy/resources/1-s2.0-S0146645313000110-mmc1.zip
fi

HOST=$(lsb_release --codename --short)
export VERSION=2.5.2
CHANGELOG="minute resolution for eemep"
export DEBEMAIL=${USER}@met.no
rm --force debian
ln --symbolic debian.$HOST debian
dch --package snap-py --newversion ${VERSION}-1 --upstream "$CHANGELOG"
dch --release ''
python3 setup.py sdist
cd dist || exit 1
tar xvfz Snappy-${VERSION}.tar.gz
cd Snappy-${VERSION} || exit 1
cp -Hr ../../debian .
rm ../../debian
mv ../Snappy-${VERSION}.tar.gz ../snap-py_${VERSION}.orig.tar.gz
debuild -us -uc -sa
