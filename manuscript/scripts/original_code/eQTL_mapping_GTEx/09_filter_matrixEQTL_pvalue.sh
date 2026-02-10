#!/bin/bash

context_name=$1
cohort=$2
input_dir=$3
output_dir=$4

head -n 1 ${input_dir}/${context_name} > ${output_dir}/${context_name}
awk '{if($5<=5e-01) print $0}' ${input_dir}/${context_name} >> ${output_dir}/${context_name}
