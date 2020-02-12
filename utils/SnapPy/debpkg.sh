#!/bin/bash

cat $0 | sed -n '/INSTRUCTIONS$/,/INSTRUCTIONS_END$/p'

#INSTRUCTIONS
# 1 - Update version, changelog, etc. in this script
# 2 - Upload to internal package repository using dupload 
#     (you need a properly configured .dupload.conf in your home directory, something like)
#
#     package config;
#     $default_host = "bionic";
#     $cfg{'xenialold'} = { 
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
#     $ dupload --no --to xenialold dist/snap-py_1.6.10-1_amd64.changes
#     then remove --no option
# 3 - Check that it works on one machine
#     # Setup
#     $ git clone git@gitlab.met.no:ansible/workstation.git
#     $ sudo apt-get install ansible
#     # Rollout
#     $ cd workstation/vgl
#     #IMPORTANT: snap-py is in group misc - only reinstall the misc packages
#     $ ansible-playbook -i hosts -t misc -l vglserver2 install.yml
#     $ ssh -X vglserver2 snap-py
#     If something goes wrong, downgrade to last version again using 
#     $ ssh vglserver2
#     $ sudo apt-get install snap-py=<version-number>
# 4 - Roll out to all machines with ansible
#     $ ansible-playbook -i hosts -t misc install.yml
#INSTRUCTIONS_END


VERSION=1.6.10
CHANGELOG="Better handling of multiple users and emission altitude"
dch -v ${VERSION}-1 -U "${CHANGELOG}"
dch -r ''
VERSION=$VERSION python3 setup.py sdist
cd dist
tar xvfz Snappy-${VERSION}.tar.gz 
cd Snappy-${VERSION}
cp -r ../../debian .
mv ../Snappy-${VERSION}.tar.gz ../snap-py_${VERSION}.orig.tar.gz
debuild -us -uc -sa



