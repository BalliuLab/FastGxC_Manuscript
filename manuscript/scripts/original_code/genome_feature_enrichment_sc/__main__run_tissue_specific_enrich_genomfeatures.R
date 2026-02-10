library(tidyverse)
cat("starting __main__\n\n\n")

#%%%%%%%%%%%%%%%%%%%%%%%% file directories %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bruna_project.dir <- "/u/project/bballiu/bballiu/FastGxC/"
source(paste0(bruna_project.dir,'scripts/genome_feature_enrichment_sc/__funcs__run_tissue_specific_enrich_genomfeatures.R'))
output.dir <- paste0(bruna_project.dir,"results/genomic_features_enrichment/Tissue_Specific/")

#%%%%%%%%%%%%%%%%%%%%%%%% intermediate files %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#snp_sets.dir <- paste0(output.dir, "Enrichment.Tissue_Specific.SNP_sets.sc.csv")
#merged_snps.dir <- paste0(output.dir, "Enrichment.Tissue_Specific.All_Tissues_Merged.SNP_sets_with_matched_BG.sc.csv")

## OneK1K
#snp_sets.dir <- paste0(output.dir, "OneK1K.Enrichment.Tissue_Specific.SNP_sets.sc.csv")
#merged_snps.dir <- paste0(output.dir, "OneK1K.Enrichment.Tissue_Specific.All_Tissues_Merged.SNP_sets_with_matched_BG.sc.csv")

## CLUES
snp_sets.dir <- paste0(output.dir, "CLUES.Enrichment.Tissue_Specific.SNP_sets.sc.csv")
merged_snps.dir <- paste0(output.dir, "CLUES.Enrichment.Tissue_Specific.All_Tissues_Merged.SNP_sets_with_matched_BG.sc.csv")

#%%%%%%%%%%%%%%%%%%%%%%%% steps %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# choose run type
args = commandArgs(trailingOnly = T)
RunType = as.numeric(args[1])
cat(paste0("\nRunType: ", RunType,"\n"))

# make snp sets of interest
if(RunType == 1){
  make_snp_sets(input_dir = paste0(bruna_project.dir, "results/genomic_features_enrichment/eAssociations.CLUES.all_contexts.residualized_exp_types.txt"),
                output_dir = snp_sets.dir) # set, tissue, SNP
}

# add matched background snps per tissue
if(RunType == 2){
    args=commandArgs(TRUE)
    ti=args[2]
    ti_output=paste0(output.dir, "Enrichment.Tissue_Specific.",ti,".SNP_sets_with_matched_BG.csv")
    print(paste0("final output will be saved to",ti_output))
    
    add_matched_snps_per_tissue(tissue = ti,
                                snp_sets_dir = snp_sets.dir, 
                                output_dir = ti_output)
}

#### add matched snps per tissue for OneK1K or CLUES
if(RunType == 21){
  args=commandArgs(TRUE)
  ti=args[2]
  ti_output=paste0(output.dir, "CLUES.Enrichment.Tissue_Specific.",ti,".SNP_sets_with_matched_BG.csv")
  print(paste0("final output will be saved to",ti_output))
  
  add_matched_snps_per_tissue_mod(tissue = ti,
                              snp_sets_dir = snp_sets.dir, 
                              output_dir = ti_output)
  
}

# merged matched background snps across tissues
if(RunType == 3){
  merge_matched_snps_across_tissues(enr_folder = output.dir, 
                                    output_dir = merged_snps.dir) #set, tissue, IS_SOI, SNP
  
}

# merged matched background snps across tissues OneK1K
if(RunType == 31){
  merge_matched_snps_across_tissues_onek1k(enr_folder = output.dir, 
                                    output_dir = merged_snps.dir,
                                    tiss_index = 4) #set, tissue, IS_SOI, SNP
  
}

# run intersect and build 2x2 contingency table for fishers exact test
if(RunType == 4){
  # run_intersect_by_set(input_dir = merged_snps.dir,
  #                      enr_folder = output.dir, 
  #                      set_run = "HET.single_tissue")
  
  args=commandArgs(TRUE)
  print(paste0("parallel run: ",args[2]))
  run_intersect_by_set(input_dir = merged_snps.dir,
                       snps_loc = "/u/project/bballiu/bballiu/FastGxC/results/genomic_features_enrichment/CLUES_5prcMAF_snpsloc.txt",
                       enr_folder = output.dir,
                       set_run = args[2],
                       out_dir = paste0(output.dir, "CLUES.Enrichment.Tissue_Specific.ATAC_Intersect_Contingency_Table.sc.",args[2],".csv"))
  
}

# merge contingency tables, then run fishers exact test and then perform multiple testing correction
if(RunType == 5){
  run_fishers_exact_test_and_fdr(cont_dir = output.dir,
                                 out_dir = paste0(output.dir, "CLUES.Enrichment.Tissue_Specific.ENCODE_ATAC_Intersect.fisher_results_fdr.sc.csv"))
}





