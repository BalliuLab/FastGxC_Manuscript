#!/bin/bash

jobType=$1
logfiles="/u/project/bballiu/bballiu/FastGxC/scripts/genome_feature_enrichment_sc/logfiles/"
scripts_dir="/u/project/bballiu/bballiu/FastGxC/scripts/genome_feature_enrichment_sc/"

if [ $jobType -eq 1 ]; then
  rm ${logfiles}snp_sets.*
  file="__main__run_tissue_specific_enrich_genomfeatures.R"
  qsub -N snp_sets -o ${logfiles}snp_sets.o -e ${logfiles}snp_sets.e -l h_data=64G,h_rt=2:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}$file $jobType 
fi

if [ $jobType -eq 2 ]; then
  rm ${logfiles}*_bg_matching.*
  file="__main__run_tissue_specific_enrich_genomfeatures.R"
  for tissue in "B" "CD4" "CD8" "NK" "pDC" "cDC" "cMono" "ncMono";do
      qsub -N ${tissue}_bg_matching -o ${logfiles}${tissue}_bg_matching.o -e ${logfiles}${tissue}_bg_matching.e -l h_data=64G,h_rt=10:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}$file $jobType $tissue
  done
fi

if [ $jobType -eq 21 ]; then
  rm ${logfiles}CLUES.*_bg_matching.*
  file="__main__run_tissue_specific_enrich_genomfeatures.R"
  for tissue in "B" "CD4" "CD8" "NK" "pDC" "cDC" "cMono" "ncMono";do
      qsub -N CLUES.${tissue}_bg_matching -o ${logfiles}CLUES.${tissue}_bg_matching.o -e ${logfiles}CLUES.${tissue}_bg_matching.e -l h_data=64G,h_rt=10:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}$file $jobType $tissue
  done
fi

if [ $jobType -eq 3 ]; then
  rm ${logfiles}merge_matching.*
  file="__main__run_tissue_specific_enrich_genomfeatures.R"
  qsub -N merge_matching -o ${logfiles}merge_matching.o -e ${logfiles}merge_matching.e -l h_data=64G,h_rt=2:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}$file $jobType 
fi

if [ $jobType -eq 4 ]; then
  rm ${logfiles}*_cont_table.*
  file="__main__run_tissue_specific_enrich_genomfeatures.R"
  for iter in "HET.each_tissue" "HET.single_tissue" "TBT.each_tissue" "TBT.single_tissue";do
      qsub -N ${iter}_cont_table -o ${logfiles}${iter}_cont_table.o -e ${logfiles}${iter}_cont_table.e -l h_data=64G,h_rt=10:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}$file $jobType $iter
  done
fi

if [ $jobType -eq 5 ]; then
  rm ${logfiles}fisher_fdr.*
  file="__main__run_tissue_specific_enrich_genomfeatures.R"
  qsub -N fisher_fdr -o ${logfiles}fisher_fdr.o -e ${logfiles}fisher_fdr.e -l h_data=64G,h_rt=2:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}$file $jobType 
fi

