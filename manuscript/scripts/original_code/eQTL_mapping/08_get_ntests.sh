#!/bin/bash

# Get number of SNPs tested per gene in each tissue
tissue_name=$1
nSNPs=$2
cohort=$3
input_dir=$4
output_dir=$5

if ([ $nSNPs -eq 1 ]); then
awk '{print $2}' ${input_dir}/${tissue_name}.${cohort}.mean_norm_res_exp.all_pairs.txt | sort | uniq -c | awk '{print $2,$1}' > ${output_dir}/${cohort}.n_SNPs_per_gene_${tissue_name}.txt
fi

if ([ $nSNPs -eq 2 ]); then
awk '{print $1}' ${input_dir}/${tissue_name}.${cohort}.mean_norm_res_exp.all_pairs.txt | sort | uniq -c | awk '{print $2,$1}' > ${output_dir}/${cohort}.n_genes_per_SNPs_specific.txt
fi

if ([ $nSNPs -eq 3 ]); then
awk '{print $2}' ${input_dir}/${tissue_name}.${cohort}.mean_norm_res_exp.shared.all_pairs.txt | sort | uniq -c | awk '{print $2,$1}' > ${output_dir}/${cohort}.n_SNPs_per_gene_${tissue_name}.txt
fi

if ([ $nSNPs -eq 4 ]); then
awk '{print $1}' ${input_dir}/${tissue_name}.${cohort}.mean_norm_res_exp.shared.all_pairs.txt | sort | uniq -c | awk '{print $2,$1}' > ${output_dir}/${cohort}.n_genes_per_SNPs_shared.txt
fi
