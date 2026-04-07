#! /bin/sh

set -e

. /usr/lib/mapp-config/mapp-services.sh

DATA=$(mapp_service_var_with_default metproduction /lustre/metproductionB)

echo $DATA
exit 0
