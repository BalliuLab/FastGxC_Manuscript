#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger 
#%%%%%%%%%%%%%%% April 24, 2024
#%%%%%%%%%%%%%%% merge imputed VCF files, convert to plink, and filter out SNPs based on R2 and MAF
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#!/bin/bash

bashrc=$1
workdir=$2
outdir=$3
qced_fam=$4
MAF_filter=$5
r2_filter=$6
onek1k_dir=$7
outfile_prefix=$8

## load the bashrc file so tha we have the required tools
source $bashrc

## list of all imputed vcf files
ls ${workdir}chr*dose.vcf.gz > ${workdir}all_vcfs.txt

## use bcftools to concatenate all imputed vcfs
bcftools concat -f ${workdir}all_vcfs.txt -Oz -o ${outdir}${outfile_prefix}_imputed.vcf

echo "finished concatenating all vcf files"

## Keep only individuals in final fam file
cat $qced_fam | cut -d " " -f 1-2 | awk '{print $2}' > ${outdir}keep_samples.txt
bcftools view -S ${outdir}keep_samples.txt -Oz -o ${onek1k_dir}/genotypes/${outfile_prefix}_imputed.vcf.gz ${outdir}${outfile_prefix}_imputed.vcf 

echo "finished subsetting by individuals who passed genotype QC"

## filter merged vcf for R2 > 0.8 and write out gzipped vcf
bcftools view -i "R2>${r2_filter}" -Oz -o ${outdir}${outfile_prefix}_imputed_r2${r2_filter}.vcf.gz ${onek1k_dir}${outfile_prefix}_imputed.vcf.gz

echo "finished subsetting and filtering vcf for R2"


## filter for maf > 0.05 and keep only individuals in final fam file
plink2 --vcf ${outdir}${outfile_prefix}_imputed_r2${r2_filter}.vcf.gz --maf $MAF_filter --make-bed --out ${outdir}${outfile_prefix}_imputed_r2${r2_filter}_MAF_${MAF_filter}prc