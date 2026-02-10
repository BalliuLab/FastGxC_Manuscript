library(tidyverse)
cat("\n\n\nstarting __main__\n\n\n")

# file directories
bruna_project.dir <- "/u/project/bballiu/bballiu/FastGxC/"
#output.dir <- paste0(bruna_project.dir,"results/genomic_features_enrichment/Tissue_Agnostic/")
output.dir <- paste0(bruna_project.dir,"results/genomic_features_enrichment/Tissue_Agnostic_clean/")
source(paste0(bruna_project.dir,'scripts/genome_feature_enrichment_sc/__funcs__run_tissue_agnostic_enrich_genomfeatures.R'))

# choose run type
args = commandArgs(trailingOnly = T)
RunType = as.numeric(args[1])
cat(paste0("\nRunType: ", RunType,"\n"))

# 1 make snp sets of interest: COMPLETED
if(RunType == 1){
  make_snp_sets(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/Github_Data/eAssociations.scMeta.all_contexts.residualized_exp_types.txt"),
                output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNP_sets.sc.csv")) # set, SNP
  print("done")
}

# 1 make snp sets of interest FastGxC shared only and specific only OneK1K: COMPLETED
if(RunType == 11){
  make_snp_sets_unique(input_dir = paste0(bruna_project.dir, "results/genomic_features_enrichment/eAssociations.OneK1K.all_contexts.residualized_exp_types.txt"),
                output_dir = paste0(output.dir, "OneK1K.Enrichment.Tissue_Agnostic.SNP_sets_hom_het_only.sc.csv")) # set, SNP
  print("done")
}

# 1 make snp sets of interest FastGxC shared only and specific only: COMPLETED
if(RunType == 12){
  make_snp_sets_unique(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/Github_Data/eAssociations.scMeta.all_contexts.residualized_exp_types.txt"),
                output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNP_sets_hom_het_only.sc.csv")) # set, SNP
  print("done")
}

# 2 get tested snps per set: COMPLETED
if(RunType == 2){
  print("2")
  get_tested_snps(MatrixEQTL_dir = paste0(bruna_project.dir, "results/eQTL_mapping/CLUES_ASN.CLUES_EUR.OneK1K_Metasoft/random_effect2/"),
                  output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_tested.sc.csv")) # set, SNP
}

# 2 get tested snps per set for OneK1K: COMPLETED
if(RunType == 21){
  print("2")
  get_tested_snps_onek1k(MatrixEQTL_dir = paste0(bruna_project.dir, "/results/eQTL_mapping/sc_MatrixEQTL/"),
                  output_dir = paste0(output.dir, "OneK1K.Enrichment.Tissue_Agnostic.SNPs_tested.sc.csv")) # set, SNP
}

# 3 combine: snp sets of interest + tested snps: COMPLETED
if(RunType == 3){
  print("3")
  combine_soi_tested(soi_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNP_sets_hom_het_only.sc.csv"),
                     tested_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_tested.sc.csv"),
                     output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG_hom_het_only.sc.csv")) # set, IS_BG, SNP
}

# 4 make file that has AverageTissue MAF for all SNPs: COMPLETED
if(RunType == 4){
  print("4")
  get_median_maf(clues_asn_mafs = "/u/project/bballiu/bballiu/FastGxC/data/CLUES_ASN/misc/CLUES_ASN_SNPs_by_Context_MAF.txt", clues_eur_mafs = "/u/project/bballiu/bballiu/FastGxC/data/CLUES_EUR/misc/CLUES_EUR_SNPs_by_Context_MAF.txt", onek1k_mafs = "/u/project/bballiu/bballiu/FastGxC/data/OneK1K/misc/OneK1K_SNPs_by_Context_MAF.txt",
                  output_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageContext.sc.csv")) # SNP, AverageTissue
}

# 4 make file that has AverageTissue MAF for all SNPs: COMPLETED
if(RunType == 42){
  print("42")
  get_median_maf_clues(clues_asn_mafs = "/u/project/bballiu/bballiu/FastGxC/data/CLUES_ASN/misc/CLUES_ASN_SNPs_by_Context_MAF.txt", clues_eur_mafs = "/u/project/bballiu/bballiu/FastGxC/data/CLUES_EUR/misc/CLUES_EUR_SNPs_by_Context_MAF.txt",
                 output_dir = paste0(output.dir,"CLUES.MatchingInfo.SNPs_MAF.AverageContext.sc.csv")) # SNP, AverageTissue
}

# 4 make file that has AverageTissue MAF for all SNPs: COMPLETED
if(RunType == 4.5){
  print("4.5")
  get_average_maf_onek1k(input_dir = "/u/project/bballiu/bballiu/FastGxC/data/OneK1K/misc/OneK1K_SNPs_by_Context_MAF.txt",
                 output_dir = paste0(output.dir,"OneK1K.MatchingInfo.SNPs_MAF.AverageContext.sc.csv")) # SNP, AverageTissue
}

# 5 do matching; returns a smaller version of file: NOT RUN
if(RunType == 5){
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
    run_match_in_pieces(eqtls_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG_hom_het_only.sc.csv"),
                        maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageContext.sc.csv"),
                        output_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched_hom_het_only.sc.csv"), # set, IS_SOI, SNP, MAF_avg
                        matching_seed = 246,
                        num_pieces = 300)
    print("done matching")
  }
}

# 6 make plots to check how well matching worked: NOT RUN
if(RunType == 6){
  plot_match_stats(unmatched_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.sc.csv"),
                   matched_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.sc.csv"),
                   maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageContext.sc.csv"),
                   plots_save_dir = paste0(output.dir,"plots/MatchingStats.Tissue_Agnostic.sc.pdf"))
}

# 7 FINAL: 
# a) make genomic features file
# b) run intersect and build 2x2 contingency table for fishers exact test
# c) run fishers exact test and then perform multiple testing correction
if(RunType == 7){
if(1){ 
  ## VEP annotations file
  if(1){
    gf.dir <- paste0(output.dir,"../Tissue_Agnostic/GenomicFeatures.VEP.sc.csv") # desc,chr,start,end
    ct.dir <- paste0(output.dir,"Enrichment.Tissue_Agnostic.VEP.contingency_table_hom_het_only.sc.csv") # set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F
    fs.dir <- paste0(output.dir, "Enrichment.Tissue_Agnostic.VEP.fisher_results_fdr_hom_het_only.sc.csv") # set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper
    
    
    
    #cat("\n\nmaking genomic features file...\n\n")
    #make_genomic_features_file(gen_feature_ID = "VEP",
    #                           output_dir = gf.dir)
    
    #cat("\n\nbuilding contingency table...\n\n")
    build_cont_table_by_intersect_vep(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched_hom_het_only.sc.csv"),
                                  snps_loc_file = "/u/project/bballiu/bballiu/FastGxC/results/genomic_features_enrichment/Meta_5prcMAF_snpsloc.txt",
                                  genomic_features_file_dir = gf.dir,
                                  output_dir = ct.dir,
                                  output_intervals_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.VEP.Intersect_Intervals.sc.csv"))
    
    cat("\n\nrunning fishers exact test\n\n")
    run_fishers_exact_test_and_fdr(input_dir = ct.dir,
                                   output_dir = fs.dir)
  }
  
  # CLUES atac database 
  if(0){
    gf.dir <- paste0(output.dir,"GenomicFeatures.CLUES_atac.sc.csv") # desc,chr,start,end
    ct.dir <- paste0(output.dir,"Enrichment.Tissue_Agnostic.CLUES_atac.contingency_table.sc.csv") # set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F
    fs.dir <- paste0(output.dir, "Enrichment.Tissue_Agnostic.CLUES_atac.fisher_results_fdr.sc.csv") # set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper
    
    
    
    cat("\n\nmaking genomic features file...\n\n")
    make_genomic_features_file(gen_feature_ID = "CLUES_atac",
                                output_dir = gf.dir)
    
    cat("\n\nbuilding contingency table...\n\n")
    build_cont_table_by_intersect(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.sc.csv"),
                                  genomic_features_file_dir = gf.dir,
                                  output_dir = ct.dir,
                                  output_intervals_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.CLUES_atac.Intersect_Intervals.sc.csv"))
     
    cat("\n\nrunning fishers exact test\n\n")
    run_fishers_exact_test_and_fdr(input_dir = ct.dir,
                                   output_dir = fs.dir)
  }
  
  # eukaryotic promoter database 
  if(0){
    gf.dir <- paste0(output.dir,"GenomicFeatures.EukPromoterDatabase.csv") # desc,chr,start,end
    ct.dir <- paste0(output.dir,"Enrichment.Tissue_Agnostic.EukPromoterDatabase.contingency_table.csv") # set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F
    fs.dir <- paste0(output.dir, "Enrichment.Tissue_Agnostic.EukPromoterDatabase.fisher_results_fdr.csv") # set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper
    
    
    
    #cat("\n\nmaking genomic features file...\n\n")
    #make_genomic_features_file(gen_feature_ID = "EukPromoterDatabase",
    #                           output_dir = gf.dir)
    
    #cat("\n\nbuilding contingency table...\n\n")
    #build_cont_table_by_intersect(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
    #                              genomic_features_file_dir = gf.dir,
    #                              output_dir = ct.dir,
    #                              output_intervals_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.EukPromoterDatabase.Intersect_Intervals.csv"))
    
    #cat("\n\nrunning fishers exact test\n\n")
    #run_fishers_exact_test_and_fdr(input_dir = ct.dir,
    #                               output_dir = fs.dir)
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
}

# final step: move "Enrichment.Tissue_Agnostic.fisher_results_fdr.csv" offline to plot! 

cat("\ndone with __main__\n")