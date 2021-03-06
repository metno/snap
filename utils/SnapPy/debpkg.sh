#!/bin/bash
if [[ "$1" == "-y" ]]; then
    echo "Creating debian package"
elif [[ "$1" == "-h" ]]; then
    cat $0 | sed -n '/INSTRUCTIONS$/,/INSTRUCTIONS_END$/p'
    exit 0
else
    echo "Usage: $0 [-h|-y]"
    exit 0
fi

#INSTRUCTIONS
# 1 - Update version, changelog, etc. in this script
#     Then run this script with -y option
#
# 2 - Upload to internal package repository using dupload
#     (you need a properly configured .dupload.conf in your home directory, something like)
#
#     package config;
#     $default_host = "xenial";
#     $cfg{'xenial'} = {
#        fqdn => "xenialrepo.met.no",
#        method => "scpb",
#        incoming => "/incoming/xenial/main",
#        dinstall_runs => 1,
#        preupload=> {
#                changes=>'sed -i "s/^Distribution:.*$/Distribution: xenial/g" %1',
#        },
#      };
#
#     Check first that it looks plausible:
#     $ dupload --no --to bionic dist/snap-py_<version>-1_amd64.changes
#     then remove --no option
#
#     Wait for confirmation email that package has been accepted
#
# 3 - Check that it works on one machine
#     $ ssh -X vglserver2
#     $ sudo apt-get update
#     $ sudo apt-get install snap-py
#     $ snapPy
#     If something goes wrong, downgrade to last version again using
#     $ sudo apt-get install snap-py=<version-number>
#
# 4 - Roll out to all machines with ansible
#     # Setup
#     $ git clone git@gitlab.met.no:ansible/workstation.git
#     $ sudo apt-get install ansible
#     # Rollout
#     $ cd workstation/vgl
#     #IMPORTANT: snap-py is in group misc - only reinstall the misc packages
#     #Roll out to e.g. vglserver2
#     $ ansible-playbook -i hosts -t misc -l vglserver2 install.yml
#     #Roll out to all hosts
#     $ ansible-playbook -i hosts -t misc install.yml
#
#     It may take a bit of time before the package is available for ansible (10 minutes)
#
# 5 - Inform meteorologists that a new version is available
#INSTRUCTIONS_END

HOST=bionic
VERSION=1.6.23
CHANGELOG="ensure jobs only read from a single store"
rm -f debian
ln -s debian.$HOST debian
dch -v ${VERSION}-1 -U "${CHANGELOG}"
dch -r ''
VERSION=$VERSION python3 setup.py sdist
cd dist
tar xvfz Snappy-${VERSION}.tar.gz
cd Snappy-${VERSION}
cp -Hr ../../debian .
rm ../../debian
mv ../Snappy-${VERSION}.tar.gz ../snap-py_${VERSION}.orig.tar.gz
debuild -us -uc -sa

