#!/bin/bash

work_dir="/u/project/bballiu/bballiu/FastGxC/"
scratch_workdir="/u/scratch/l/lkrocken/qc_outputs/"
scripts_dir="${work_dir}/scripts/sc_genotype_and_expression_qc/"
onek1k_dir="/u/project/bballiu/shared_datasets/OneK1K/"
clues_dir="/u/project/bballiu/shared_datasets/CLUES/"
outfile_prefix_onek1k="OneK1K_hg19"
outfile_prefix_clues_asn="CLUES_ASN_hg19"
outfile_prefix_clues_eur="CLUES_EUR_hg19"

logfiles="${scratch_workdir}/logfiles/"

mkdir -p ${scratch_workdir} ## create scratch_workdir directory if it doesn't exist
mkdir -p ${logfiles} ## create logfile directory

## General QC parameters
MAF_filter=0.05
ImpRsq_filter=0.8

## bashrc directory
bashrc="/u/home/l/lkrocken/.bashrc"

JobType=$1


# OneK1K - Genotype QC: preimputation QC
if ([ $JobType -eq 1 ]); then

  # remove existing logfiles
  rm ${logfiles}OneK1K_preImpQC.*

  work_dir="${scratch_workdir}OneK1K/genotype_qc/"
  raw_gen_dir="${onek1k_dir}/genotypes/"
  gene_expression_ID="${onek1k_dir}/expression/OneK1K_individuals_with_exp.txt"
  rename_chr_file="${onek1k_dir}/genotypes/OneK1K_rename_chr_hg19.txt"
  ref_data="/u/project/bballiu/shared_datasets/reference_datasets/"
  outfile_prefix="OneK1K_hg19"

  qsub -N OneK1K_genoQC -o ${logfiles}OneK1K_preImpQC.o -e ${logfiles}OneK1K_preImpQC.e -l h_data=64G,h_rt=5:00:00,highp ${scripts_dir}OneK1K_genotype_preimputationQC.sh $work_dir $raw_gen_dir $gene_expression_ID $rename_chr_file $ref_data $scripts_dir $bashrc $outfile_prefix

fi

# OneK1K: postimputation QC
if ([ $JobType -eq 2 ]); then

  # remove existing logfiles
  rm ${logfiles}OneK1K_postImpQC.*

  outfile_prefix="OneK1K_hg19"

	workdir="/u/project/bballiu/lkrocken/C-STEM/genotype_qc/OneK1K/imputed/"
  outdir="${scratch_workdir}OneK1K/genotype_qc/"
	fam_file="${scratch_workdir}OneK1K/genotype_qc/qced_genotypes/${outfile_prefix}_basicQC_sexCheck_finalPass.fam"
  

  mkdir -p ${outdir}

	qsub -N OneK1K_postImp_QC -o ${logfiles}OneK1K_postImpQC.o -e ${logfiles}OneK1K_postImpQC.e -l h_data=180G,h_rt=23:00:00,highp ${scripts_dir}OneK1K_genotype_postimputationQC.sh $bashrc $workdir $outdir $fam_file $MAF_filter $ImpRsq_filter $onek1k_dir $outfile_prefix
fi

# CLUES - Genotype QC: Separate genotypes by ancestry and filter by R2 and MAF
if ([ $JobType -eq 3 ]); then

  echo "running CLUES - Genotype QC: Separate genotypes by ancestry and filter by R2 and MAF"

  #remove existing logfiles
  rm ${logfiles}CLUES_genotypeQC.*

  geno_file_dir="${clues_dir}/genotypes/"
  outdir="${scratch_workdir}/CLUES/genotype_qc/"
  CLUES_ASN_ids="${clues_dir}CLUES_ASN_ids.txt"
  CLUES_EUR_ids="${clues_dir}CLUES_EUR_ids.txt"
  outfile_prefix_clues_asn="CLUES_ASN_hg19"
  outfile_prefix_clues_eur="CLUES_EUR_hg19"
  ref_data="/u/project/bballiu/shared_datasets/reference_datasets/"

  mkdir -p ${outdir} ## create output directory if it doesn't exist
  
  qsub -N CLUES_qc -o ${logfiles}CLUES_genotypeQC.o -e ${logfiles}CLUES_genotypeQC.e -l h_data=180G,h_rt=15:00:00,highp ${scripts_dir}CLUES_genotypes_postimputationQC.sh $geno_file_dir $outdir $CLUES_ASN_ids $CLUES_EUR_ids $MAF_filter $ImpRsq_filter $bashrc $work_dir $ref_data $scripts_dir $outfile_prefix_clues_asn $outfile_prefix_clues_eur 
fi

# CLUES expression: convert CLUES to seurat, split by  ancestry (can only be run after step 3 is run), and filter to people that passed genotype QC
if ([ $JobType -eq 4 ]); then
  
  echo "running CLUES - separate expression by ancestry"
  # remove existing logfiles
  rm ${logfiles}CLUES_ASN_expressionQC.*
  rm ${logfiles}CLUES_EUR_expressionQC.*

  run_UMAP=0
  raw_clues_anndata="${clues_dir}/expression/GSE174188_CLUES1_adjusted.h5ad"
  mkdir -p "${scratch_workdir}/CLUES/expression_qc/" ## create output directory if it doesn't exist

  ## Split by Asian ancestry
  raw_clues_asn_seurat="${scratch_workdir}/CLUES/expression_qc/${outfile_prefix_clues_asn}_raw.rds"
  clues_asn_fam="${scratch_workdir}/CLUES/genotype_qc/${outfile_prefix_clues_asn}.fam"
  qsub -N CLUES_ASN_expQC -o ${logfiles}CLUES_ASN_expressionQC.o -e ${logfiles}CLUES_ASN_expressionQC.e -l h_data=64G,h_rt=12:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}CLUES_expression_convert_seurat.R $bashrc $raw_clues_anndata $raw_clues_asn_seurat $clues_asn_fam $run_UMAP
  
  ## Split by European ancestry
  raw_clues_eur_seurat="${scratch_workdir}CLUES/expression_qc/${outfile_prefix_clues_eur}_raw.rds"
  clues_eur_fam="${scratch_workdir}/CLUES/genotype_qc/${outfile_prefix_clues_eur}.fam"
  qsub -N CLUES_EUR_expQC -o ${logfiles}CLUES_EUR_expressionQC.o -e ${logfiles}CLUES_EUR_expressionQC.e -l h_data=64G,h_rt=12:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}CLUES_expression_convert_seurat.R $bashrc $raw_clues_anndata $raw_clues_eur_seurat $clues_eur_fam $run_UMAP

fi

# OneK1K expression : convert OneK1K to seurat and filter to people that passed genotype QC
if ([ $JobType -eq 5 ]); then

  # remove existing logfiles
  rm ${logfiles}OneK1K_expressionQC.*

	raw_onek1k_seurat="${onek1k_dir}/expression/OneK1K_hg19_raw.rds"
	raw_umap_seurat="${scratch_workdir}OneK1K/expression_qc/${outfile_prefix_onek1k}_raw_umap.rds"
	onek1k_fam="${scratch_workdir}OneK1K/genotype_qc/${outfile_prefix_onek1k}_imputed_r20.8_MAF_0.05prc.fam"
  run_UMAP=0
  mkdir -p "${scratch_workdir}OneK1K/expression_qc/"

  qsub -N OneK1K_expQC -o ${logfiles}OneK1K_expressionQC.o -e ${logfiles}OneK1K_expressionQC.e -l h_data=64G,h_rt=12:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}OneK1K_expression_convert_seurat.R $bashrc $raw_onek1k_seurat $raw_umap_seurat $onek1k_fam $run_UMAP
  
fi

# CLUES/OneK1K: Compute pseudobulk per cohort for each cell type, filter gene expression, and inverse normalize
if ([ $JobType -eq 6 ]); then

  ## remove existing logfiles 
  rm ${logfiles}CLUES_ASN_pseudobulk.*
  rm ${logfiles}CLUES_EUR_pseudobulk.*
  rm ${logfiles}OneK1K_pseudobulk.*

  num_genes_exp=3
  samp_threshold=0.1
  sd_threshold=3
  gtf_file="/u/project/bballiu/shared_datasets/reference_datasets/hg19/Homo_sapiens.GRCh37.87.gtf"
  seed=1
  meQTL_dir="${scratch_workdir}/MatrixEQTL/"
  func="mean"
  functions_file="${scripts_dir}qc_functions.R"
  pca_bash_file="${scripts_dir}genotype_prune_and_PCA.sh"

  ## CLUES parameters
  raw_clues_asn_seurat="${scratch_workdir}/CLUES/expression_qc/${outfile_prefix_clues_asn}_raw.rds"
  raw_clues_eur_seurat="${scratch_workdir}CLUES/expression_qc/${outfile_prefix_clues_eur}_raw.rds"
  clues_cell_groupings="${clues_dir}/expression/CLUES_celltypes.csv"
  outdir_clues="${scratch_workdir}/CLUES/expression_qc/"

  ## OneK1K parameters
  raw_umap_seurat="${scratch_workdir}OneK1K/expression_qc/${outfile_prefix_onek1k}_raw_umap.rds"
  onek1k_cell_groupings="${onek1k_dir}/expression/OneK1K_celltypes.csv"
  outdir_onek1k="${scratch_workdir}/OneK1K/expression_qc/"

  ## compute pseudobulk for CLUES ASN
  cohort="CLUES_ASN"
  geno_file_plink="${scratch_workdir}CLUES/genotype_qc/${outfile_prefix_clues_asn}"
  qsub -N CLUES_ASN_pseudobulk -o ${logfiles}CLUES_ASN_pseudobulk.o -e ${logfiles}CLUES_ASN_pseudobulk.e -l h_data=160G,h_rt=12:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}expression_create_pseudobulk.R $bashrc $raw_clues_asn_seurat $clues_cell_groupings $outdir_clues $cohort $func $functions_file $gtf_file $num_genes_exp $samp_threshold $sd_threshold $geno_file_plink $meQTL_dir $seed $pca_bash_file
  
  ## compute pseudobulk for CLUES EUR
  cohort="CLUES_EUR"
  geno_file_plink="${scratch_workdir}CLUES/genotype_qc/${outfile_prefix_clues_eur}"
  qsub -N CLUES_EUR_pseudobulk -o ${logfiles}CLUES_EUR_pseudobulk.o -e ${logfiles}CLUES_EUR_pseudobulk.e -l h_data=160G,h_rt=12:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}expression_create_pseudobulk.R $bashrc $raw_clues_eur_seurat $clues_cell_groupings $outdir_clues $cohort $func $functions_file $gtf_file $num_genes_exp $samp_threshold $sd_threshold $geno_file_plink $meQTL_dir $seed $pca_bash_file

  ## compute pseudobulk for OneK1K
  cohort="OneK1K"
  geno_file_plink="${scratch_workdir}OneK1K/genotype_qc/${outfile_prefix_onek1k}_imputed_r20.8_MAF_0.05prc"
  qsub -N OneK1K_pseudobulk -o ${logfiles}OneK1K_pseudobulk.o -e ${logfiles}OneK1K_pseudobulk.e -l h_data=64G,h_rt=12:00:00,highp -pe shared 4 ${scripts_dir}submit_job.sh ${scripts_dir}expression_create_pseudobulk.R $bashrc $raw_umap_seurat $onek1k_cell_groupings $outdir_onek1k $cohort $func $functions_file $gtf_file $num_genes_exp $samp_threshold $sd_threshold $geno_file_plink $meQTL_dir $seed $pca_bash_file
  
fi

# CLUES/OneK1K : create snploc, geneloc, and genotype files for matrixEQTL
if ([ $JobType -eq 7 ]); then

  # remove existing logfiles
  rm ${logfiles}CLUES_ASN_snploc.*
  rm ${logfiles}CLUES_EUR_snploc.*
  rm ${logfiles}OneK1K_snploc.*

  #global parameters
  func="mean"

	# CLUES_ASN parameters
  plink_files="${scratch_workdir}CLUES/genotype_qc/${outfile_prefix_clues_asn}"
  cohort_name="CLUES_ASN"
  exp_files_dir="${scratch_workdir}CLUES/expression_qc/"
  meQTL_outdir="${work_dir}/data/${cohort_name}/MatrixEQTL_input/"

  mkdir -p ${meQTL_outdir}
  qsub -N CLUES_ASN_snploc -o ${logfiles}CLUES_ASN_snploc.o -e ${logfiles}CLUES_ASN_snploc.e -l h_data=120G,h_rt=12:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}create_geno_snp_gene_mat.R $bashrc $plink_files $cohort_name $exp_files_dir $meQTL_outdir $func

  # CLUES_EUR parameters
  plink_files="${scratch_workdir}CLUES/genotype_qc/${outfile_prefix_clues_eur}"
  cohort_name="CLUES_EUR"
  exp_files_dir="${scratch_workdir}CLUES/expression_qc/"
  meQTL_outdir="${work_dir}/data/${cohort_name}/MatrixEQTL_input/"

  mkdir -p ${meQTL_outdir}
  qsub -N CLUES_EUR_snploc -o ${logfiles}CLUES_EUR_snploc.o -e ${logfiles}CLUES_EUR_snploc.e -l h_data=120G,h_rt=12:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}create_geno_snp_gene_mat.R $bashrc $plink_files $cohort_name $exp_files_dir $meQTL_outdir $func

  # OneK1K parameters
  plink_files="${scratch_workdir}OneK1K/genotype_qc/${outfile_prefix_onek1k}_imputed_r20.8_MAF_0.05prc"
  cohort_name="OneK1K"
  exp_files_dir="${scratch_workdir}OneK1K/expression_qc/"
  meQTL_outdir="${work_dir}/data/${cohort_name}/MatrixEQTL_input/"

  mkdir -p ${meQTL_outdir}
  qsub -N OneK1K_snploc -o ${logfiles}OneK1K_snploc.o -e ${logfiles}OneK1K_snploc.e -l h_data=120G,h_rt=12:00:00,highp ${scripts_dir}submit_job.sh ${scripts_dir}create_geno_snp_gene_mat.R $bashrc $plink_files $cohort_name $exp_files_dir $meQTL_outdir $func

fi