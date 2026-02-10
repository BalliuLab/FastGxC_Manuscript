#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger and Bruna Balliu
#%%%%%%%%%%%%%%% March 19, 2024
#%%%%%%%%%%%%%%% Genotype filtering, QC, pre-imputation processing, and PCA for OneK1K cohort
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#!/bin/bash

work_dir=$1
raw_gen_dir=$2
gene_expression_ID=$3
rename_chr_file=$4
ref_data=$5
scripts_dir=$6
bashrc=$7
outfile_prefix=$8

## source bashrc file to have access to tools on hoffman
source $bashrc

#######################################################
################# PARAMETERS ##########################
#######################################################
maf_filter=0.01
hwe_filter=1e-6
geno_filter=0.05
mind_filter=0.05
sd_threshold=3

### LD parameters
window_size=200
step_size=50
r2=0.25

qced_file_dir="$work_dir/qced_genotypes/"
qced_file_name=${qced_file_dir}/$outfile_prefix
final_qced_file_name=${qced_file_name}\_basicQC\_sexCheck

oneKG_metadata="$ref_data/1KG_Phase1_hg19/1kg_phase1_hg19_metadata.tsv"

mkdir -p $qced_file_dir


################# ################# ################# ################# 
########## Preimputation QC
################# ################# ################# ################# 

#### Keep only the individuals with transcriptomic data
echo 
echo "keeping only individuals with expression data"
echo 
plink \
    --bfile $raw_gen_dir/$outfile_prefix \
    --keep $gene_expression_ID \
    --make-bed \
    --out ${qced_file_name}_withExp

echo 
echo "filtering for maf, hwe, genotype rate, etc."
echo

##### Apply basic QC filters
plink \
    --bfile ${qced_file_name}_withExp\
    --set-hh-missing \
    --biallelic-only \
    --snps-only just-acgt \
    --maf $maf_filter \
    --hwe $hwe_filter \
    --geno $geno_filter \
    --mind $mind_filter \
    --write-snplist \
    --out ${qced_file_name}_withExp

cat ${qced_file_name}_withExp.irem ${qced_file_name}_withExp.nosex > ${qced_file_name}_withExp_ids_2_remove

###### Get clean data 
echo
echo "extracting only snps that passed qc filters"
echo

plink \
    --bfile ${qced_file_name}_withExp \
    --set-hh-missing \
    --extract ${qced_file_name}_withExp.snplist \
    --remove ${qced_file_name}_withExp_ids_2_remove \
    --make-bed \
    --out ${qced_file_name}_basicQC

#rm ${qced_file_name}_withExp*


###### Remove individuals that failed sex check
echo
echo "filtering by LD"
echo

plink \
    --bfile ${qced_file_name}_basicQC \
    --indep-pairwise $window_size $step_size $r2 \
    --out ${qced_file_name}_basicQC

echo
echo "sex check"
echo

plink \
    --bfile ${qced_file_name}_basicQC \
    --extract ${qced_file_name}_basicQC.prune.in \
    --check-sex \
    --out ${qced_file_name}_basicQC


grep PROBLEM ${qced_file_name}_basicQC.sexcheck | awk '{print $1, $2}' > ${qced_file_name}_basicQC_ids_2_remove

echo
echo "remove people who failed sex check"
echo

plink \
    --bfile ${qced_file_name}_basicQC \
    --remove ${qced_file_name}\_basicQC\_ids\_2\_remove \
    --make-bed \
    --out $final_qced_file_name


#####  convert plink file to vcf
echo
echo "convert sex checked bedfile to vcf"
echo

plink --bfile $final_qced_file_name --recode vcf --out $final_qced_file_name


######### Rename vcf chr names to hg19 dbsnp names
echo
echo "rename vcf chr names to hg19 dbsnp names"
echo

bcftools annotate --rename-chrs $rename_chr_file ${final_qced_file_name}.vcf -Oz -o ${final_qced_file_name}_mod.vcf
mv ${final_qced_file_name}_mod.vcf ${final_qced_file_name}.vcf

# add the "chr" prefix from the start of each line
awk '{if($0 !~ /^#/) print "chr"$0; else print $0}' ${final_qced_file_name}.vcf > ${final_qced_file_name}\_chr.vcf
# zip and index
bgzip $final_qced_file_name\_chr.vcf
tabix $final_qced_file_name\_chr.vcf.gz

##### Fix strand issues 

echo
echo "fix strands"
echo

bcftools +fixref $final_qced_file_name\_chr.vcf.gz -Oz -o $final_qced_file_name\_fixref.vcf.gz -- -d -f $ref_data/hg19/hg19.fa -i $ref_data/dbsnp/dbSNP_hg19.vcf.gz -m top

##### Remove the "chr" prefix from the start of each line
final=$final_qced_file_name\_final
zcat $final_qced_file_name\_fixref.vcf.gz | awk '{gsub(/^chr/,""); print}' > ${final}.vcf
bgzip $final.vcf
tabix $final.vcf.gz

## split vcf by chomosome
echo
echo "split cleaned vcf by chromosome to submit for imputation"
echo

bcftools index ${final}.vcf.gz
bcftools index -s ${final}.vcf.gz | cut -f 1 | while read C; do bcftools view -O z -o ${qced_file_dir}split.${C}.vcf.gz ${final}.vcf.gz "${C}" ; done


###### Upload these vcf files to Michigan Imputation server
###### Setting the following parameters for the server  


######### Convert back to plink
echo
echo "convert back to plink"
echo

plink2 --vcf $final_qced_file_name\_fixref.vcf.gz --vcf-half-call m --make-bed --out $final_qced_file_name


####### Remove all unneeded files

####################################################################### 
########## PCA with and w/o 1000G
####################################################################### 

################# Subset 1KG to just the sites in OneK1K data and merge
echo
echo "subset 1000G to sites in OneK1K and merge"
echo

awk 'BEGIN{OFS="\t"}{print $1,$4}' $final_qced_file_name.bim  > ${workdir}OneK1K_genotypes.txt
bcftools view $ref_data/1KG_Phase1_hg19/1kg_phase1_all_hg19.vcf.gz -R ${workdir}OneK1K_genotypes.txt -Oz -o $ref_data/1KG_Phase1_hg19/1kg_phase1_all_hg19_filter_OneK1K.vcf.gz
tabix $ref_data/1KG_Phase1_hg19/1kg_phase1_all_hg19_filter_OneK1K.vcf.gz
bcftools merge -m id $ref_data/1KG_Phase1_hg19/1kg_phase1_all_hg19_filter_OneK1K.vcf.gz ${final}.vcf.gz --threads 8 -Oz -o $final_qced_file_name\_1000G.vcf.gz

echo
echo "PCA with merged samples"
echo

#### pca with reference + and our samples together  
plink2 --vcf $final_qced_file_name\_1000G.vcf.gz \
    --max-alleles 2 \
    --snps-only just-acgt \
    --rm-dup force-first --allow-extra-chr --maf $maf_filter --geno $geno_filter\
    --vcf-half-call m \
    --set-all-var-ids @:#[b37]$r,$a \
    --indep-pairwise $window_size $step_size $r2 \
    --make-bed --out $final_qced_file_name\_1000G_pruned

plink2 --bfile $final_qced_file_name\_1000G_pruned --maf $maf_filter --snps-only just-acgt --pca --allow-extra-chr --out $final_qced_file_name\_1000G_pca

echo
echo "get individuals within 3SD of PC1 and PC2"
echo

#  Create file with PCA outcome for plotting 
awk '{print $2}' $final_qced_file_name\_1000G_pca.eigenvec >  ${final_qced_file_name}_ids2keep
awk -F "\t" '{print $1"\t"$6}' $oneKG_metadata | grep -wf  ${final_qced_file_name}_ids2keep > ${final_qced_file_name}_tmp1
grep -v 'HG0' $final_qced_file_name\_1000G_pca.eigenvec | grep -v 'NA' | awk '{print $2}' |  awk '{print $0"\t""OneK1K"}' | sed '1d' > ${final_qced_file_name}_tmp2
cat ${final_qced_file_name}_tmp1 ${final_qced_file_name}_tmp2 | sort -n -k 1 > ${final_qced_file_name}_tmp3
awk '{print $2"\t"$3"\t"$4"\t"$5"\t"$6"\t"$7"\t"$8"\t"$9}' $final_qced_file_name\_1000G_pca.eigenvec | sed '1d' | sort -n -k 1 > ${final_qced_file_name}_tmp4
join -1 1 -2 1 ${final_qced_file_name}\_tmp3  ${final_qced_file_name}_tmp4 > $final_qced_file_name\_1000G_pca_res.txt

rm ${final_qced_file_name}_ids2keep ${final_qced_file_name}_tmp* 

## find individuals that are within +-3SD from mean of PC1 and PC2
Rscript ${scripts_dir}OneK1K_genotype_ancestry_PCA.R ${work_dir}/qced_genotypes/ $final_qced_file_name\_1000G_pca_res.txt $outfile_prefix $sd_threshold

echo
echo "extract individuals who passed PCA check"
echo

## subset by people who passed PCA thresholds
passed_individuals="${work_dir}/qced_genotypes/${outfile_prefix}_PCA_samples.txt"
plink \
    --bfile $final_qced_file_name \
    --keep $passed_individuals \
    --make-bed \
    --out $final_qced_file_name\_passPCA

rm $qced_bed_file_name\_sexCheck.*



################# ################# ################# ################# 
########## Heterozygosity rate 
################# ################# ################# ################# 

echo
echo "heterozygosity rate check"
echo

###### Remove individuals that failed heterozygosity rate check 
plink  --bfile $final_qced_file_name\_passPCA --extract ${qced_file_name}_basicQC.prune.in --het --out $final_qced_file_name\_het

Rscript ${scripts_dir}OneK1K_genotype_het.R ${work_dir}/qced_genotypes/ $final_qced_file_name\_het.het $outfile_prefix $sd_threshold

het_file="$work_dir/qced_genotypes/${outfile_prefix}_het_samples.txt"
plink \
    --bfile $final_qced_file_name\_passPCA \
    --keep $het_file \
    --chr 1-22 \
    --make-bed \
    --out $final_qced_file_name\_passHet


echo
echo "compute relatedness"
echo

###### Compute relatedness and remove people with estimated relatedness larger than 0.125
# https://www.kingrelatedness.com/Download.shtml
king -b $final_qced_file_name\_passHet.bed --kinship --degree 2

kinship_thresh=0.125
kinship_file="$work_dir/qced_genotypes/${outfile_prefix}_king_samples.txt"
awk -F"\t" '$9>0.125' king.kin > "$work_dir/qced_genotypes/${outfile_prefix}_tmp.txt"
awk -F"\t" '{print $1,$2}' "$work_dir/qced_genotypes/${outfile_prefix}_tmp.txt" | uniq > ${kinship_file}

plink \
    --bfile $final_qced_file_name\_passHet \
    --remove $kinship_file \
    --chr 1-22 \
    --make-bed \
    --out $final_qced_file_name\_finalPass


### remove all unneeded files:
rm ${qced_file_name}_withExp.irem ${qced_file_name}_withExp.nosex
rm ${qced_file_name}_withExp*
rm ${qced_file_name}_basicQC.*
rm ${qced_file_name}_basicQC_ids_2_remove
rm ${qced_file_name}_basicQC_sexCheck_1000G_pca.*
rm ${qced_file_name}_basicQC_sexCheck_1000G_pruned.*
rm ${qced_file_name}_basicQC_sexCheck.*
rm ${qced_file_name}_basicQC_sexCheck_chr*
rm ${qced_file_name}_basicQC_sexCheck_final.*
rm ${qced_file_name}_basicQC_sexCheck_fixref.*
rm ${qced_file_name}_basicQC_sexCheck_het.*
rm ${qced_file_name}_basicQC_sexCheck_passHet.*
rm ${qced_file_name}_basicQC_sexCheck_passPCA.*
rm OneK1K_hg19_tmp.txt
#rm ${qced_file_dir}split*