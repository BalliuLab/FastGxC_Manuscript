#!/bin/bash

# qrsh -l h_data=32G,h_rt=12:00:00

echo "Loading dependencies"

echo "Loading dependencies"
. /u/local/Modules/default/init/modules.sh
module load R/4.2.2-BIO 


JobType=$1
work_dir=$2
echo $JobType

# Residualized expression files
if ([ $JobType -eq 1 ]); then
    i=$3
    cohort=$4
    echo Running context $i for cohort $cohort
    my_script=$work_dir/scripts/eQTL_mapping/01_residualize_expression_for_covariates.R
    R --vanilla --slave -f $my_script --args $i $cohort $work_dir
fi

# Merge GTEx expression files
if ([ $JobType -eq 2 ]); then
    cohort=$4
    my_script=$work_dir/scripts/eQTL_mapping/02_merge_expression_files.R ${work_dir} $cohort
    R --vanilla --slave -f $my_script
fi

# Decompose expression files and compute PCA
if ([ $JobType -eq 3 ]); then
my_script=$work_dir/scripts/eQTL_mapping/GTEx_03_decompose_expression.R
R --vanilla --slave -f $my_script
fi


# Andrew can you add here the scripts for processing the vcf files? Maybe merge with the next script that filters the duplicated SNPs. 
# if ([ $JobType -eq 4 ]); then
# echo "Under construction"
# fi

# (Andrew) Delete duplicated snps in 2 snp files
if ([ $JobType -eq 5 ]); then
my_script=$work_dir/scripts/eQTL_mapping/GTEx_05_remove_duplicated_snps.R
R --vanilla --slave -f $my_script
fi

# Compute MAF per context
if ([ $JobType -eq 6 ]); then
my_script=$work_dir/scripts/eQTL_mapping/GTEx_06_compute_MAF_per_context.R
R --vanilla --slave -f $my_script
fi

# Run MatrixEQTL by context
if ([ $JobType -eq 7 ]); then
exp_scale=$3
i=$4
my_script=$work_dir/scripts/eQTL_mapping/GTEx_07_run_MatrixEQTL_by_context.R
R --vanilla --slave -f $my_script --args $exp_scale $i
fi


# Get number of SNPs tested per gene in each context
if ([ $JobType -eq 8 ]); then
context_name=$3
nSNPs=$4

input_dir=$work_dir/results/eQTL_mapping/MatrixEQTL
output_dir=$work_dir/results/eQTL_mapping/TreeQTL

if ([ $nSNPs -eq 1 ]); then
awk '{print $2}' ${input_dir}/${context_name}.v8.EUR.normalized_and_residualized_expression.all_pairs.txt | sort | uniq -c | awk '{print $2,$1}' > ${output_dir}/n_SNPs_per_gene_${context_name}.txt
fi

if ([ $nSNPs -eq 2 ]); then
awk '{print $1}' ${input_dir}/${context_name}.v8.EUR.normalized_and_residualized_expression.all_pairs.txt | sort | uniq -c | awk '{print $2,$1}' > ${output_dir}/n_genes_per_SNPs_Heterogeneous.txt
fi

if ([ $nSNPs -eq 3 ]); then
awk '{print $2}' ${input_dir}/${context_name}.v8.EUR.normalized_and_residualized_expression_homogeneous.all_pairs.txt | sort | uniq -c | awk '{print $2,$1}' > ${output_dir}/n_SNPs_per_gene_${context_name}.txt
fi

if ([ $nSNPs -eq 4 ]); then
awk '{print $1}' ${input_dir}/${context_name}.v8.EUR.normalized_and_residualized_expression_homogeneous.all_pairs.txt | sort | uniq -c | awk '{print $2,$1}' > ${output_dir}/n_genes_per_SNPs_Homogeneous.txt
fi


fi

# Filter MatrixEQTL results for eQTL p-value <= threshold
if ([ $JobType -eq 9 ]); then
context_name=$3

input_dir=$work_dir/results/eQTL_mapping/MatrixEQTL
output_dir=$work_dir/results/eQTL_mapping/MatrixEQTL_FDRthreshold

head -n 1 ${input_dir}/${context_name} > ${output_dir}/${context_name}
awk '{if($5<=5e-01) print $0}' ${input_dir}/${context_name} >> ${output_dir}/${context_name}

fi

# Run TreeQTL
if ([ $JobType -eq 10 ]); then
exp_scale=$3
my_script=$work_dir/scripts/eQTL_mapping/GTEx_10_run_TreeQTL.R
R --vanilla --slave -f $my_script --args $exp_scale
fi
