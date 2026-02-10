library(tidyverse)
cat("\n\n\nstarting __main__\n\n\n")

# file directories
bruna_project.dir <- "/u/project/zaitlenlab/bballiu/FastGxE/"
output.dir <- paste0(bruna_project.dir,"results/eQTL_mapping/Feature_Enrichment/Tissue_Agnostic/")
source(paste0(bruna_project.dir,'scripts/__funcs__run_tissue_agnostic_enrich_genomfeatures.R'))

# choose run type
RunType = 5
cat(paste0("\nRunType: ", RunType,"\n"))

# 1 make snp sets of interest: COMPLETED
if(RunType == 1){
  make_snp_sets(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/Github_Data/eAssociations.v8.EUR.all_tissues.residualized_exp_types.txt"),
                output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNP_sets.csv")) # set, SNP
}

# 2 get tested snps per set: COMPLETED
if(RunType == 2){
  get_tested_snps(MatrixEQTL_dir = paste0(bruna_project.dir, "results/eQTL_mapping/MatrixEQTL/"),
                  output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_tested.csv")) # set, SNP
}

# 3 combine: snp sets of interest + tested snps: COMPLETED
if(RunType == 3){
  combine_soi_tested(soi_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNP_sets.csv"),
                     tested_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_tested.csv"),
                     output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv")) # set, IS_BG, SNP
}

# 4 make file that has AverageTissue MAF for all SNPs: COMPLETED
if(RunType == 4){
  get_average_maf(input_dir = paste0(bruna_project.dir, "data/GTEx_v8/misc/GTEx_v8_SNPs_by_Tissue_MAF.txt"),
                  output_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageTissue.csv")) # SNP, AverageTissue
}

# 5 do matching; returns a smaller version of file: NOT RUN
#if(RunType == 5){
if(1){
  # run_match(eqtls_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv"),
  #           maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageTissue.csv"),
  #           output_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
  #           matching_seed = 12345)
  
  # run_match_in_pieces(eqtls_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv"),
  #                     maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageTissue.csv"),
  #                     output_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"), # set, IS_SOI, SNP, MAF_avg
  #                     matching_seed = 12345,
  #                     num_pieces = 200)
  print("running match in pieces...")
  run_match_in_pieces(eqtls_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv"),
                      maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageTissue.csv"),
                      output_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"), # set, IS_SOI, SNP, MAF_avg
                      matching_seed = 246,
                      num_pieces = 300)
  print("done matching")
}

# 6 make plots to check how well matching worked: NOT RUN
if(RunType == 6){
  plot_match_stats(unmatched_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv"),
                   matched_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
                   maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageTissue.csv"),
                   plots_save_dir = paste0(output.dir,"plots/MatchingStats.Tissue_Agnostic.pdf"))
}

# 7 FINAL: 
# a) make genomic features file
# b) run intersect and build 2x2 contingency table for fishers exact test
# c) run fishers exact test and then perform multiple testing correction
#if(RunType == 7){
if(1){  
  # eukaryotic promoter database 
  if(1){
    gf.dir <- paste0(output.dir,"GenomicFeatures.EukPromoterDatabase.csv") # desc,chr,start,end
    ct.dir <- paste0(output.dir,"Enrichment.Tissue_Agnostic.EukPromoterDatabase.contingency_table.csv") # set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F
    fs.dir <- paste0(output.dir, "Enrichment.Tissue_Agnostic.EukPromoterDatabase.fisher_results_fdr.csv") # set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper
    
    # cat("\n\nmaking genomic features file...\n\n")
    # make_genomic_features_file(gen_feature_ID = "EukPromoterDatabase",
    #                            output_dir = gf.dir)
    
    cat("\n\nbuilding contingency table...\n\n")
    build_cont_table_by_intersect(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
                                  genomic_features_file_dir = gf.dir,
                                  output_dir = ct.dir,
                                  output_intervals_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.EukPromoterDatabase.Intersect_Intervals.csv"))
     
    cat("\n\nrunning fishers exact test\n\n")
    run_fishers_exact_test_and_fdr(input_dir = ct.dir,
                                   output_dir = fs.dir)
  }
  
  # encode regulatory build
  if(0){
    gf.dir <- paste0(output.dir,"GenomicFeatures.ENCODE_RegulatoryBuild.csv") # desc,chr,start,end
    ct.dir <- paste0(output.dir,"Enrichment.Tissue_Agnostic.ENCODE_RegulatoryBuild.contingency_table.csv") # set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F
    fs.dir <- paste0(output.dir, "Enrichment.Tissue_Agnostic.ENCODE_RegulatoryBuild.fisher_results_fdr.csv") # set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper
    
    # cat("\n\nmaking genomic features file...\n\n")
    # make_genomic_features_file(gen_feature_ID = "ENCODE_RegulatoryBuild",
    #                            output_dir = gf.dir)
    # 
    # cat("\n\nbuilding contingency table...\n\n")
    # build_cont_table_by_intersect(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
    #                               genomic_features_file_dir = gf.dir,
    #                               output_dir = ct.dir,
    #                               output_intervals_dir = "Enrichment.Tissue_Agnostic.ENCODE_RegulatoryBuild.Intersect_Intervals.csv")

    cat("\n\nrunning fishers exact test\n\n")
    run_fishers_exact_test_and_fdr(input_dir = ct.dir,
                                   output_dir = fs.dir)
  }
  
}

# final step: move "Enrichment.Tissue_Agnostic.fisher_results_fdr.csv" offline to plot! 

cat("\ndone with __main__\n")