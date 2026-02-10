#!/bin/bash

echo "Loading dependencies"
. /u/local/Modules/default/init/modules.sh
module load R/4.1.0-BIO
module load bcftools
source "/u/home/l/lkrocken/.bashrc"

file=$1

## these parameters change based on what file is being run
param1=$2
param2=$3
param3=$4
param4=$5
param5=$6
param6=$7
param7=$8
param8=${9}
param9=${10}
param10=${11}
param11=${12}
param12=${13}
param13=${14}
param14=${15}

Rscript $file $param1 $param2 $param3 $param4 $param5 $param6 $param7 $param8 $param9 $param10 $param11 $param12 $param13 $param14
