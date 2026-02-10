#!/bin/bash

# work_dir=/u/project/zaitlenlab/bballiu/FastGxE/
work_dir=/u/home/b/bballiu/FastGxC
JobType=1
# 1: Residualized GTEx expression files for covariates
# 2: Merge GTEx expression files
# 3: Decompose GTEx expression files and compute PCA
# 4: Preprocess the vcf files
# 5: Filter duplicated SNPs from genotype file
# 6: Compute MAF per tissue in GTEx
# 7: Run MatrixEQTL per tissue in GTEx
# 8: Get number of SNPs tested per gene and genes tested per SNP in each tissue in GTEx
# 9: Filter MatrixEQTL results in GTEx for p-value <= threshold
#10: Run TreeQTL in GTEx

# GTEx: Residualized expression files for covariates
if ([ $JobType -eq 1 ]); then
for i in $(seq 1 49); do
qsub -N j.$i -o ${work_dir}/logfiles/resexp.${i}.o -e ${work_dir}/logfiles/resexp.${i}.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType $i
done
fi


# GTEx: Merge expression files
if ([ $JobType -eq 2 ]); then
qsub -N merge -o ${work_dir}/logfiles/merge.o -e ${work_dir}/logfiles/merge.e -l h_data=64G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType
fi


# GTEx: Decompose expression files and compute PCA
if ([ $JobType -eq 3 ]); then
qsub -N decomp -o ${work_dir}/logfiles/decomp.o -e ${work_dir}/logfiles/decomp.e -l h_data=80G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType
fi

# Andrew can you add here the scripts for processing the vcf files? Maybe merge with the next script that filters the duplicated SNPs. 
# if ([ $JobType -eq 4 ]); then
# echo "Under construction"
# fi

# GTEx: Filter duplicated SNPs from genotype file
if ([ $JobType -eq 5 ]); then
qsub -N remove_dup_snps -o ${work_dir}/logfiles/remove_dup_snps.o -e ${work_dir}/logfiles/remove_dup_snps.e -l h_data=96G,h_rt=4:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType
fi

# GTEx: Compute MAF per tissue
if ([ $JobType -eq 6 ]); then
qsub -N MAF -o ${work_dir}/logfiles/MAF.o -e ${work_dir}/logfiles/MAF.e -l h_data=80G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType
fi

# GTEx: Run MatrixEQTL by context
if ([ $JobType -eq 7 ]); then
for exp_scale in $(seq 1 4); do
# 1: normalized_expression
# 2: normalized_and_residualized_expression
# 3: normalized_expression_heterogeneous
# 4: normalized_and_residualized_expression_heterogeneous
for i in $(seq 1 49); do
qsub -N eQTL.${exp_scale}.${i} -o ${work_dir}/logfiles/eQTL.${exp_scale}.${i}.o -e ${work_dir}/logfiles/eQTL.${exp_scale}.${i}.e -l h_data=64G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType $exp_scale $i
done
done

for exp_scale in $(seq 5 6); do
# 5: normalized_expression_homogeneous
# 6: normalized_and_residualized_expression_homogeneous
i=1
qsub -N eQTL.${exp_scale}.${i} -o ${work_dir}/logfiles/eQTL.${exp_scale}.${i}.o -e ${work_dir}/logfiles/eQTL.${exp_scale}.${i}.e -l h_data=64G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType $exp_scale $i
done

fi

# GTEx: Get number of SNPs tested per gene and number of genes tested per SNP in each tissue
if ([ $JobType -eq 8 ]); then

# Number of SNPs tested per gene for full and het
nSNPs=1
cd ${work_dir}/results/eQTL_mapping/MatrixEQTL

ls -l *.v8.EUR.normalized_and_residualized_expression.all_pairs.txt | awk '{print $9}' > tmp_MatrixEQTL_files

cat tmp_MatrixEQTL_files | while read MatrixEQTL_file ; do
tissue_name=$(echo $MatrixEQTL_file | tr "." " " | awk '{print $1}')
qsub -N nrTests.${tissue_name}.SNPs -o ${work_dir}/logfiles/nrTests.${tissue_name}.SNPs.o -e ${work_dir}/logfiles/nrTests.${tissue_name}.SNPs.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType $tissue_name $nSNPs
done

rm tmp_MatrixEQTL_files

# Number of genes tested per SNPs for full and het
nSNPs=2
tissue_name="Whole_Blood"
qsub -N nrTests.Genes -o ${work_dir}/logfiles/nrTests.Genes.o -e ${work_dir}/logfiles/nrTests.Genes.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType $tissue_name $nSNPs

# Number of SNPs tested per gene and genes tested per SNPs for hom
tissue_name="AverageTissue"
for nSNPs in 3 4; do
qsub -N nrTests.${tissue_name}.${nSNPs} -o ${work_dir}/logfiles/nrTests.${tissue_name}.${nSNPs}.o -e ${work_dir}/logfiles/nrTests.${tissue_name}.${nSNPs}.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType $tissue_name $nSNPs
done

fi

# GTEx: Filter MatrixEQTL results for p-value <= threshold
if ([ $JobType -eq 9 ]); then
cd ${work_dir}/results/eQTL_mapping/MatrixEQTL
ls -l *.all_pairs.txt | awk '{print $9}' > tmp_MatrixEQTL_files

cat tmp_MatrixEQTL_files | while read file ; do
qsub -N FilterQTLs.${file} -o ${work_dir}/logfiles/FilterQTLs.${file}.o -e ${work_dir}/logfiles/FilterQTLs.${file}.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType ${file}
done

rm tmp_MatrixEQTL_files

fi

# GTEx: Run TreeQTL
if ([ $JobType -eq 10 ]); then
for exp_scale in $(seq 1 6); do
qsub -N treeQTL.${exp_scale} -o ${work_dir}/logfiles/treeQTL.${exp_scale}.o -e ${work_dir}/logfiles/treeQTL.${exp_scale}.e -l h_data=80G,h_rt=20:00:00,highp $work_dir/scripts/eQTL_mapping_GTEx/GTEx_submit_job.sh $JobType $exp_scale
done
fi

