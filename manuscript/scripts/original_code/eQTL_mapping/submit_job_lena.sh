#!/bin/bash

echo "Loading dependencies"
. /u/local/Modules/default/init/modules.sh
module load R/4.1.0-BIO

JobType=$1
file=$2

## these parameters change based on what file is being run
param1="$3"
param2="$4"
param3="$5"
param4="$6"
param5="$7"
param6=$8
param7=$9
param8=${10}
param9=${11}
param10=${12}
param11=${13}
param12=${14}
param13=${15}

if ([ $JobType -ne 12 ]); then

    Rscript "$file" "$param1" "$param2" "$param3" "$param4" $param5 $param6 $param7 $param8 $param9 $param10 $param11 $param12 $param13

fi

if ([ $JobType -eq 12 ]); then

    java -jar $file -input $param1 -pvalue_table $param2 -output $param3

fi
