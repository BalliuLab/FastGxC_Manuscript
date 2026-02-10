#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% March 19, 2024
#%%%%%%%%%%%%%%% Merge CLUES immvar and all_clues, filter by R2 and MAF
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#!/bin/bash

geno_file_dir=$1 # directory with all genotype files
outdir=$2 # scratch output directory 
CLUES_ASN_ids=$3 ## list of asian individual IDs
CLUES_EUR_ids=$4 ## list of european individual IDs
MAF_filter=$5
ImpRsq_filter=$6
bashrc=$7
work_dir=$8 # FastGxC output directory
ref_data=$9
scripts_dir=${10}
outfile_prefix_clues_asn=${11}
outfile_prefix_clues_eur=${12}

source ${bashrc}

#### parameters
sd_threshold=3

### merge clues and immvar vcfs  
ls ${geno_file_dir}*.vcf.gz > ${outdir}vcf_list.txt
bcftools merge -l ${outdir}vcf_list.txt -Oz -o ${outdir}immvar_clues_hg19_raw.vcf.gz 
echo "finished merging clues and immvar"

# filter merged vcf file for R2 > 0.8
bcftools view -i "R2>${ImpRsq_filter}" -Oz -o ${outdir}/filtered_r2${ImpRsq_filter}_immvar_clues_hg19.vcf.gz ${outdir}immvar_clues_hg19_raw.vcf.gz

echo "finished filtering merged vcf for R2 above $ImpRsq_filter"

# convert vcf files to plink
plink2 --vcf ${outdir}/filtered_r2${ImpRsq_filter}_immvar_clues_hg19.vcf.gz --max-alleles 2 --make-bed --out ${outdir}immvar_clues_hg19_r2${ImpRsq_filter}

echo "finished vcf converting to plink"

cat ${CLUES_ASN_ids} | awk '{print 0,$1}' > ${outdir}keep_asn_inds.txt
cat ${CLUES_EUR_ids} | awk '{print 0,$1}' > ${outdir}keep_eur_inds.txt

#filter for maf > 0.05 per cohort, write out cohort specific plink files
plink \
    --bfile ${outdir}immvar_clues_hg19_r2${ImpRsq_filter} \
    --keep ${outdir}keep_asn_inds.txt \
    --maf $MAF_filter\
    --make-bed \
    --out ${outdir}${outfile_prefix_clues_asn}_tmp

plink \
    --bfile ${outdir}immvar_clues_hg19_r2${ImpRsq_filter} \
    --keep ${outdir}keep_eur_inds.txt \
    --maf $MAF_filter\
    --make-bed \
    --out ${outdir}${outfile_prefix_clues_eur}_tmp

echo "finished splitting plink files by ancestry and filtering for MAF above $MAF_filter"

### LD parameters
window_size=200
step_size=50
r2=0.25

plink2 --bfile ${outdir}${outfile_prefix_clues_asn}_tmp --indep-pairwise $window_size $step_size $r2 --pca --out ${outdir}/${outfile_prefix_clues_asn}_pca_tmp
plink2 --bfile ${outdir}${outfile_prefix_clues_eur}_tmp --indep-pairwise $window_size $step_size $r2 --pca --out ${outdir}/${outfile_prefix_clues_eur}_pca_tmp

awk '{print $2"\t""ASN""\t"$3"\t"$4"\t"$5"\t"$6"\t"$7"\t"$8"\t"$9}' ${outdir}/${outfile_prefix_clues_asn}_pca_tmp.eigenvec | sed '1d' | sort -n -k 1 > ${outdir}/${outfile_prefix_clues_asn}_pca_res.txt
awk '{print $2"\t""EUR""\t"$3"\t"$4"\t"$5"\t"$6"\t"$7"\t"$8"\t"$9}' ${outdir}/${outfile_prefix_clues_eur}_pca_tmp.eigenvec | sed '1d' | sort -n -k 1 > ${outdir}/${outfile_prefix_clues_eur}_pca_res.txt

ancestry="ASN"
Rscript ${scripts_dir}CLUES_genotype_ancestry_PCA.R ${outdir} ${outdir}/${outfile_prefix_clues_asn}_pca_res.txt $outfile_prefix_clues_asn $sd_threshold $ancestry
ancestry="EUR"
Rscript ${scripts_dir}CLUES_genotype_ancestry_PCA.R ${outdir} ${outdir}/${outfile_prefix_clues_eur}_pca_res.txt $outfile_prefix_clues_eur $sd_threshold $ancestry

#filter for people who passed PCA threshold checks
plink \
    --bfile ${outdir}${outfile_prefix_clues_asn}_tmp \
    --keep "$outdir/${outfile_prefix_clues_asn}_PCA_samples.txt" \
    --maf $MAF_filter\
    --make-bed \
    --out ${outdir}${outfile_prefix_clues_asn}

plink \
    --bfile ${outdir}${outfile_prefix_clues_eur}_tmp \
    --keep "$outdir/${outfile_prefix_clues_eur}_PCA_samples.txt" \
    --maf $MAF_filter\
    --make-bed \
    --out ${outdir}${outfile_prefix_clues_eur}

echo "finished final PCA on both CLUES cohorts"

rm ${outdir}vcf_list.txt
rm ${outdir}/immvar_clues_hg19_raw.vcf.gz
rm ${outdir}/filtered_r2${ImpRsq_filter}_immvar_clues_hg19.vcf.gz
rm ${outdir}immvar_clues_hg19_r2${ImpRsq_filter}*
rm ${outdir}/${outfile_prefix_clues_asn}_pca_tmp*
rm ${outdir}/${outfile_prefix_clues_eur}_pca_tmp*
rm ${outdir}${outfile_prefix_clues_asn}_tmp*
rm ${outdir}${outfile_prefix_clues_eur}_tmp*

