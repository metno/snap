#! /bin/sh

set -e

. /usr/lib/mapp-config/mapp-services.sh

LUSTREDATA=$(mapp_service_var_with_default lustre /lustre/storeB)

echo $LUSTREDATA
exit 0