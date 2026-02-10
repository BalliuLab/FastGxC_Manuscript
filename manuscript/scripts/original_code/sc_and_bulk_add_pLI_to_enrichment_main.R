# ============================================================================
# Lise Tucker code to bulk table to add pLI scores to FastGxC plot
# ============================================================================

library(tidyverse)
library(dplyr)
library(readr)
library(matrixStats)

cat("\n\n\nstarting __main__\n\n\n")

# file directories
bruna_project.dir <- "/u/project/bballiu/bballiu/FastGxC/"
lise_project.dir <- "/u/home/l/ltucker/project-bballiu/FastGxC_paper/"
git_input_dir <- "/u/project/bballiu/bballiu/FastGxC/FastGxC_Manuscript_public/manuscript/Input_Files/Figure6_Enrichment/"

# directories for output files
plot.dir <- "/u/scratch/l/ltucker/FastGxC_paper/FastGxC_gene_sets/plots/"
enrichment_outdir <- "/u/home/l/ltucker/project-bballiu/FastGxC_paper/Enrichment_figure/"

dir.create(plot.dir)
dir.create(enrichment_outdir)

source(paste0(bruna_project.dir,'scripts/lise_sc_and_bulk_add_pLI_to_enrichment_func.R'))

# choose run type
args = commandArgs(trailingOnly = T)
RunType = as.numeric(args[1])
cat(paste0("\nRunType: ", RunType,"\n"))

# 0 make eAssociation file for GTEx: COMPLETED 
if(RunType == 1){
  make_GTEx_eAssoc_file(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/TreeQTL"),
                output_file = paste0(enrichment_outdir, "eAssociations.GTEx.all_contexts.residualized_exp_types.txt")) # set, GENE
  print("done")
}

# 1 make GENE sets of interest FastGxC shared only and specific only
if(RunType == 1){
  # Single Cell: COMPLETED
  make_gene_sets_unique(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/Github_Data/eAssociations.scMeta.all_contexts.residualized_exp_types.txt"),
                output_dir = paste0(enrichment_outdir, "Enrichment.GENE_sets_FastGxC.sc.csv")) # set, GENE

  # GTEx: COMPLETED
  make_gene_sets_unique(input_dir = paste0(enrichment_outdir, "eAssociations.GTEx.all_contexts.residualized_exp_types.txt"),
                output_dir = paste0(enrichment_outdir, "Enrichment.GENE_sets_FastGxC.GTEx.csv")) # set, GENE
  print("done")
}

# 2 get tested GENEs per set
if(RunType == 2){
  print("2")
  # Single Cell: COMPLETED
  get_tested_genes(shared_all_pairs_file = paste0(bruna_project.dir, "results/eQTL_mapping/CLUES_ASN.CLUES_EUR.OneK1K_Metasoft/random_effect2/AverageContext.mean_norm_res_exp.shared.all_pairs.txt"),
                  output_dir = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_tested.sc.csv")) # set, GENE

  # GTEx: COMPLETED 
  get_tested_genes(shared_all_pairs_file = paste0(bruna_project.dir, "results/eQTL_mapping/MatrixEQTL/AverageTissue.v8.EUR.normalized_and_residualized_expression_homogeneous.all_pairs.txt"),
                  output_dir = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_tested.GTEx.csv")) # set, GENE
}

# 3 combine: gene sets of interest + tested gene: COMPLETED
if(RunType == 3){
  print("3")
  # Single Cell: COMPLETED
  combine_soi_tested_gene_based(soi_dir = paste0(enrichment_outdir, "Enrichment.GENE_sets_FastGxC.sc.csv"),
                     tested_dir = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_tested.sc.csv"),
                     output_dir = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_FastGxC.sc.csv")) # set, IS_BG, GENE

  # GTEx: COMPLETED
  combine_soi_tested_gene_based(soi_dir = paste0(enrichment_outdir, "Enrichment.GENE_sets_FastGxC.GTEx.csv"),
                    tested_dir = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_tested.GTEx.csv"),
                    output_dir = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_FastGxC.GTEx.csv")) # set, IS_BG, GENE
}

# 3.5 - create n_SNPs_per_gene_AverageContext.txt for GTEx
# input files of name: "n_SNPs_per_gene_<context>.txt"
if(RunType == 35){
  print("3.5")
  make_n_SNPs_per_gene_average(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/TreeQTL/"), # create a file n_SNPs_per_gene_AverageContext.txt
                     output_file = paste0(enrichment_outdir, "n_SNPs_per_gene_AverageContext_GTEx.txt")) 
}

# 4 Add gene lengths (sum of exon widths per gene) and number of SNPs per gene avg across contexts to SOI file
if(RunType == 4){
  print("4")
  # Single Cell: COMPLETED
  add_gene_lengths_and_snp_count_to_SOI_sc(input_snp_file = paste0(bruna_project.dir, "results/eQTL_mapping/CLUES_ASN.CLUES_EUR.OneK1K_Metasoft/random_effect2/n_SNPs_per_gene_AverageContext.txt"),
                     input_soi_file = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_FastGxC.sc.csv"),
                     output_file = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_FastGxC.sc.csv")) 
  
  # GTEx: COMPLETED
  add_gene_lengths_and_snp_count_to_SOI_GTEx(input_snp_file = paste0(enrichment_outdir, "n_SNPs_per_gene_AverageContext_GTEx.txt"),
                     input_soi_file = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_FastGxC.GTEx.csv"),
                     output_file = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_FastGxC.GTEx.csv")) 
      
}


# 5 add expression profiles to sets 
if(RunType == 5){
  print("5")
  # Single Cell: COMPLETED
  add_exp_profile_to_SOI_sc(input_dir= paste0(bruna_project.dir, "data/"),
                     input_soi_file= paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_FastGxC.sc.csv"),
                     output_file = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_exp_profile_FastGxC.sc.csv")) 

  # GTEx: COMPLETED
  add_exp_profile_to_SOI_GTEx(input_dir= paste0(bruna_project.dir, "data/GTEx_v8/MatrixEQTL_input/"),
                    input_soi_file= paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_FastGxC.GTEx.csv"),
                    output_file = paste0(enrichment_outdir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_exp_profile_FastGxC.GTEx.csv"))                   

}

# 6 add pLI scores to matched genes files
if(RunType == 6){
  print("6")
  # Single Cell: COMPLETED
  add_pLI_to_unmatched_genes_sc(input_matched_file = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_exp_profile_FastGxC.sc.csv"), # set, IS_SOI, GENE, n_SNPs, gene_size, exp_mean
                      output_file = paste0(enrichment_outdir,"sc_pLI_no_match_FastGxC.csv")) # set, IS_SOI, GENE, n_SNPs, gene_size, exp_mean, pLI

  # GTEx: COMPLETED
  add_pLI_to_unmatched_genes_GTEx(input_matched_file = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_exp_profile_FastGxC.GTEx.csv"), # set, IS_SOI, GENE, n_SNPs, gene_size, exp_mean
                      output_file = paste0(enrichment_outdir,"GTEx_pLI_no_match_FastGxC.csv")) # set, IS_SOI, GENE, n_SNPs, gene_size, exp_mean, pLI

}

# 7 match genes 
if(RunType == 7){
  print("7")
  # Single Cell: COMPLETED
  run_gene_match_second(genes_dir = paste0(enrichment_outdir,"sc_pLI_no_match_FastGxC.csv"), # set, IS_BG, GENE, n_SNPs, gene_size, mean_CLUES_EUR_exp, mean_CLUES_ASN_exp, mean_OneK1K_exp, mean_exp, median_exp
                      output_dir = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.GENEs_matched_FastGxC.sc.csv"), # set, IS_SOI, GENE, n_SNPs, gene_size, exp_mean
                      matching_seed = 246)

  # GTEx: COMPLETED
  run_gene_match_second(genes_dir = paste0(enrichment_outdir,"GTEx_pLI_no_match_FastGxC.csv"), # set, IS_BG, GENE, n_SNPs, gene_size, mean_CLUES_EUR_exp, mean_CLUES_ASN_exp, mean_OneK1K_exp, mean_exp, median_exp
                    output_dir = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.GENEs_matched_FastGxC.GTEx.csv"), # set, IS_SOI, GENE, n_SNPs, gene_size, exp_mean
                    matching_seed = 246)
}


# 8 plot initial pLI
if(RunType == 8){
  print("8")
  # Single Cell: COMPLETED
  plot_pLI_match_all(input_pLI_file = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.GENEs_matched_FastGxC.sc.csv"), # set, IS_SOI, GENE, n_SNPs, gene_size, exp_mean, pLI
                      output_file = paste0(enrichment_outdir,"sc_pLI_fixed_bg_set_plot.png"),
                      data_type="sc") 

  # GTEx: COMPLETED
  plot_pLI_match_all(input_pLI_file = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.GENEs_matched_pLI_first_FastGxC.GTEx.csv"),  # set, IS_SOI, GENE, n_SNPs, gene_size, exp_mean, pLI
                      output_file = paste0(enrichment_outdir,"GTEx_pLI_fixed_bg_set_plot.png"),
                      data_type="GTEx")

}

# 9 build contingency table
if(RunType == 9){
  print("9")
  # Single Cell: COMPLETED
  build_cont_table(input_pLI_file = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.GENEs_matched_FastGxC.sc.csv"),
                      output_cont_table = paste0(enrichment_outdir,"sc_FastGxC_cont_table.csv"))

  # GTEx: COMPLETED
  run_fishers_exact_test_and_fdr(input_dir = paste0(enrichment_outdir,"GTEx_FastGxC_cont_table.csv"), 
                      output_dir = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.fisher_results_fdr_FastGxC_GTEx.csv"))

}

# 10 Fischers' exact test
if(RunType == 10){
  print("10")
  # Single Cell: COMPLETED
  run_fishers_exact_test_and_fdr(input_dir = paste0(enrichment_outdir,"sc_FastGxC_cont_table.csv"), 
                      output_dir = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.fisher_results_fdr_FastGxC_sc.csv"))

  # GTEx: COMPLETED
  run_fishers_exact_test_and_fdr(input_dir = paste0(enrichment_outdir,"GTEx_FastGxC_cont_table.csv"), 
                      output_dir = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.fisher_results_fdr_FastGxC_GTEx.csv"))

}


# 11 Add pLI scores to existing fisher results files
if(RunType == 11){
  print("11")
  # Single Cell: COMPLETED
  update_enrichment_tables_sc(input_dir = paste0(git_input_dir,"Enrichment.Tissue_Agnostic.VEP.fisher_results_fdr_hom_het_only.sc.csv"), 
                      my_fisher_results = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.fisher_results_fdr_FastGxC_sc.csv"),
                      output_dir = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.VEP.fisher_results_fdr_hom_het_only_with_pLI_genes.sc.csv"))

  # GTEx: COMPLETED
  update_enrichment_tables_bulk(input_dir = paste0(git_input_dir,"Enrichment.Tissue_Agnostic.SNPs_Matched_by_MAF.additional_SNP_sets.VEP_Annotations.FinalFisherResults.csv"),
                        my_fisher_results = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.fisher_results_fdr_FastGxC_GTEx.csv"),
                        output_dir = paste0(enrichment_outdir,"Enrichment.Tissue_Agnostic.SNPs_Matched_by_MAF.additional_SNP_sets.VEP_Annotations_with_pLI_genes.FinalFisherResults.csv"),
                        cont_table_path = paste0(enrichment_outdir, "GTEx_FastGxC_cont_table.csv"))

}
