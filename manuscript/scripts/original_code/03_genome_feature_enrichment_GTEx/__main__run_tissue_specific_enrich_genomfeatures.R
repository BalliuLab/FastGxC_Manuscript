library(tidyverse)
cat("starting __main__\n\n\n")

#%%%%%%%%%%%%%%%%%%%%%%%% file directories %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bruna_project.dir <- "/u/project/zaitlenlab/bballiu/FastGxE/"
source(paste0(bruna_project.dir,'scripts/__funcs__run_tissue_specific_enrich_genomfeatures.R'))
output.dir <- paste0(bruna_project.dir,"results/eQTL_mapping/Feature_Enrichment/Tissue_Specific/")

#%%%%%%%%%%%%%%%%%%%%%%%% intermediate files %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
snp_sets.dir <- paste0(output.dir, "Enrichment.Tissue_Specific.SNP_sets.csv")
merged_snps.dir <- paste0(output.dir, "Enrichment.Tissue_Specific.All_Tissues_Merged.SNP_sets_with_matched_BG.csv")

#%%%%%%%%%%%%%%%%%%%%%%%% steps %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# make snp sets of interest
if(0){
  make_snp_sets(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/Github_Data/eAssociations.v8.EUR.all_tissues.residualized_exp_types.txt"),
                output_dir = snp_sets.dir) # set, tissue, SNP
}

# add matched background snps per tissue
if(0){
    args=commandArgs(TRUE)
    ti=args[1]
    ti_output=paste0(output.dir, "Enrichment.Tissue_Specific.",ti,".SNP_sets_with_matched_BG.csv")
    print(paste0("final output will be saved to",ti_output))
    
    add_matched_snps_per_tissue(tissue = ti,
                                snp_sets_dir = snp_sets.dir, 
                                output_dir = ti_output)
    
}

# merged matched background snps across tissues
if(0){
  merge_matched_snps_across_tissues(enr_folder = output.dir, 
                                    output_dir = merged_snps.dir) #set, tissue, IS_SOI, SNP
  
}

# run intersect and build 2x2 contingency table for fishers exact test
if(0){
  # run_intersect_by_set(input_dir = merged_snps.dir,
  #                      enr_folder = output.dir, 
  #                      set_run = "HET.single_tissue")
  
  args=commandArgs(TRUE)
  print(paste0("parallel run: ",args[1]))
  run_intersect_by_set(input_dir = merged_snps.dir,
                       enr_folder = output.dir,
                       set_run = args[1],
                       out_dir = paste0(output.dir, "Enrichment.Tissue_Specific.ATAC_Intersect_Contingency_Table.",args[1],".csv"))
  
}

# merge contingency tables, then run fishers exact test and then perform multiple testing correction
if(1){
  run_fishers_exact_test_and_fdr(cont_dir = output.dir,
                                 out_dir = paste0(output.dir, "Enrichment.Tissue_Specific.ENCODE_ATAC_Intersect.fisher_results_fdr.csv"))
}





