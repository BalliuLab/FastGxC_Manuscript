library(tidyverse)
library(data.table)

make_snp_sets <- function(input_dir, output_dir){
  
  # function call: 
  # make_snp_sets(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/Github_Data/eAssociations.v8.EUR.all_tissues.residualized_exp_types.txt"),
  #               output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_sets_of_interest.csv")) 
  read_csv(input_dir, col_types = cols(.default = col_character())) %>% 
    rename(SNP = snp) %>% 
    select(exp_type, SNP) %>% distinct %>% 
    mutate(set = case_when(exp_type == "mean_norm_res_exp_homogeneous" ~ "HOM",
                           exp_type == "mean_norm_res_exp_heterogeneous" ~ "HET",
                           exp_type == "mean_norm_res_exp" ~ "TBT")) %>% 
    select(set, SNP) %>% 
    write_csv(output_dir) # set, SNP
}

make_snp_sets_sh_only <- function(input_dir, output_dir){
  het = read_csv(input_dir, col_types = cols(.default = col_character())) %>%
        rename(SNP = snp) %>% select(exp_type, SNP) %>% distinct %>% 
        mutate(set = case_when(exp_type == "mean_norm_res_exp_homogeneous" ~ "HOM",
                           exp_type == "mean_norm_res_exp_heterogeneous" ~ "HET",
                           exp_type == "mean_norm_res_exp" ~ "TBT")) %>% 
    filter(exp_type != "mean_norm_res_exp") %>% group_by(SNP) %>% mutate(count = n()) %>% 
    filter((count == 1 & exp_type == "mean_norm_res_exp_heterogeneous") | count == 2) %>%
    select(set, SNP) %>% distinct
  
  hom = read_csv(input_dir, col_types = cols(.default = col_character())) %>%
    rename(SNP = snp) %>% select(exp_type, SNP) %>% distinct %>% 
    mutate(set = case_when(exp_type == "mean_norm_res_exp_homogeneous" ~ "HOM",
                           exp_type == "mean_norm_res_exp_heterogeneous" ~ "HET",
                           exp_type == "mean_norm_res_exp" ~ "TBT")) %>% 
    filter(exp_type != "mean_norm_res_exp") %>% group_by(SNP) %>% mutate(count = n()) %>% 
    filter(count == 1 & exp_type == "mean_norm_res_exp_homogeneous") %>% select(set, SNP) %>% distinct
  
  final = rbind(het, hom)
  final %>% write_csv(output_dir) # set, SNP
  
}

make_snp_sets_unique <- function(input_dir, output_dir){
  
  hom_het = read_csv(input_dir, col_types = cols(.default = col_character())) %>% 
    rename(SNP = snp) %>% 
    select(exp_type, SNP) %>% distinct %>% 
    mutate(set = case_when(exp_type == "mean_norm_res_exp_homogeneous" ~ "HOM",
                           exp_type == "mean_norm_res_exp_heterogeneous" ~ "HET",
                           exp_type == "mean_norm_res_exp" ~ "TBT")) %>% 
    filter(exp_type != "mean_norm_res_exp") %>% group_by(SNP) %>% mutate(count = n()) %>% filter(count == 1) %>%
    select(set, SNP) 
  
  fastgxc_cxc = read_csv(input_dir, col_types = cols(.default = col_character())) %>% 
    rename(SNP = snp) %>% 
    select(exp_type, SNP) %>% distinct %>% 
    mutate(set = case_when(exp_type == "mean_norm_res_exp_homogeneous" ~ "fastgxc_hom",
                           exp_type == "mean_norm_res_exp_heterogeneous" ~ "fastgxc_het",
                           exp_type == "mean_norm_res_exp" ~ "TBT")) %>% 
    group_by(SNP) %>% mutate(count = n()) %>% filter(count == 1) %>%
    select(set, SNP) 
  
  fastgxc_cxc_both = read_csv(input_dir, col_types = cols(.default = col_character())) %>% 
    rename(SNP = snp) %>% 
    select(exp_type, SNP) %>% distinct %>% filter(exp_type != "mean_norm_res_exp") %>% 
    mutate(set = case_when(exp_type == "mean_norm_res_exp_homogeneous" ~ "fastgxc_hom",
                           exp_type == "mean_norm_res_exp_heterogeneous" ~ "fastgxc_het",
                           exp_type == "mean_norm_res_exp" ~ "TBT")) %>% 
    group_by(SNP) %>% mutate(count = n()) %>% filter(count == 2) %>% mutate(set = "fastgxc_het") %>% 
    select(set, SNP) 
  
  final = rbind(hom_het, fastgxc_cxc, fastgxc_cxc_both)
  final %>% write_csv(output_dir) # set, SNP
}

get_tested_snps <- function(MatrixEQTL_dir, output_dir){
  # get_tested_snps(MatrixEQTL_dir = paste0(bruna_project.dir, "results/eQTL_mapping/MatrixEQTL/"),
  #                 output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_tested.csv")) # set, SNP
  
  het_tbt <- tribble(~SNP)
  for (f in list.files(path = MatrixEQTL_dir, pattern = "mean_norm_res_exp.specific.all_pairs.txt", full.names = T)){
    
    print(f)
    cat(paste0("nrows: ",nrow(het_tbt),"\n"))
    
    t <- read_tsv(f, col_types = cols(.default = col_character())) %>% select(SNP) %>% distinct
    het_tbt <- bind_rows(het_tbt, t) %>% distinct
    
    rm(t)
    gc()
    
  }
  het_tbt <- het_tbt %>% mutate(set = "HET_TBT") %>% select(set, SNP)
  
  hom <- read_tsv(paste0(MatrixEQTL_dir,"AverageContext.mean_norm_res_exp.shared.all_pairs.txt"), col_types = cols(.default = col_character())) %>% 
    select(SNP) %>% distinct %>% mutate(set = "HOM") %>% select(set, SNP)
  print(hom)
  
  print("writing file now...")
  bind_rows(hom, het_tbt) %>% write_csv(output_dir)
}

### function for just OneK1K data 
get_tested_snps_onek1k <- function(MatrixEQTL_dir, output_dir){
  # get_tested_snps(MatrixEQTL_dir = paste0(bruna_project.dir, "results/eQTL_mapping/MatrixEQTL/"),
  #                 output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_tested.csv")) # set, SNP
  
  het_tbt <- tribble(~SNP)
  for (f in list.files(path = MatrixEQTL_dir, pattern = ".OneK1K.mean_norm_res_exp.specific.all_pairs.txt", full.names = T)){
    
    print(f)
    cat(paste0("nrows: ",nrow(het_tbt),"\n"))
    
    t <- read_tsv(f, col_types = cols(.default = col_character())) %>% select(SNP) %>% distinct
    het_tbt <- bind_rows(het_tbt, t) %>% distinct
    
    rm(t)
    gc()
    
  }
  het_tbt <- het_tbt %>% mutate(set = "HET_TBT") %>% select(set, SNP)
  
  hom <- read_tsv(paste0(MatrixEQTL_dir,"AverageContext.OneK1K.mean_norm_res_exp.shared.all_pairs.txt"), col_types = cols(.default = col_character())) %>% 
    select(SNP) %>% distinct %>% mutate(set = "HOM") %>% select(set, SNP)
  print(hom)
  
  print("writing file now...")
  bind_rows(hom, het_tbt) %>% write_csv(output_dir)
}

combine_soi_tested <- function(soi_dir, tested_dir, output_dir){
  
  # combine_soi_tested(soi_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNP_sets.csv"),
  #                    tested_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_tested.csv"),
  #                    output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv")) # set, IS_BG, SNP
  
  tested <- read_csv(tested_dir, col_types = cols(.default = col_character())) # set (HOM, HET_TBT), SNP
  soi <- read_csv(soi_dir, col_types = cols(.default = col_character())) # set (HOM, HET, TBT), SNP
  
  soi %>% group_by(set) %>% group_modify(function(tib, key){
    
    print(key$set[1])
    
    if (key$set[1] == "HOM") bg_col = "HOM"
    if (key$set[1] == "HET") bg_col = "HET_TBT"
    if (key$set[1] == "TBT") bg_col = "HET_TBT"
    if (key$set[1] == "fastgxc_hom") bg_col = "HOM"
    if (key$set[1] == "fastgxc_het") bg_col = "HET_TBT"
    
    soi_temp <- tib %>% mutate(IS_BG = 0) %>% select(SNP,IS_BG) %>% distinct
    bg_temp <- tested %>% filter(set == bg_col) %>% select(SNP) %>% distinct
    
    joined_temp <- right_join(soi_temp, bg_temp, by = c("SNP")) %>% 
      mutate(IS_BG = replace_na(IS_BG, 1)) %>% 
      select(IS_BG, SNP)
    
    print(joined_temp)
    print(joined_temp %>% group_by(IS_BG) %>% summarize(n_SNPs = n()))
    cat("\n\n")
    
    return(joined_temp)
  }) %>% select(set, IS_BG, SNP) %>% 
    write_csv(output_dir)
}

get_average_maf <- function(input_dir, output_dir){
  read_tsv(input_dir) %>% select(SNP, AverageTissue) %>% write_csv(output_dir)
}

get_average_maf_onek1k <- function(input_dir, output_dir){
  read_tsv(input_dir) %>% select(SNP, AverageContext) %>% write_csv(output_dir)
}

### gets the median of the Average Context across all 3 cohorts (CLUES ASN, CLUES EUR, OneK1K)
get_median_maf <- function(clues_asn_mafs, clues_eur_mafs, onek1k_mafs, output_dir){
  library(data.table)
  library(dplyr)
  library(stringi)
  
  clues_asn = fread(clues_asn_mafs, sep = "\t", data.table = F)
  clues_eur = fread(clues_eur_mafs, sep = "\t", data.table = F)
  onek1k = fread(onek1k_mafs, sep = "\t", data.table = F)
  
  ### have to make sure the SNP IDs match across cohorts (don't run)
  #onek1k$SNP <- sapply(strsplit(onek1k$SNP, ":"), function(x) paste(x[1:2], collapse = ":"))
  #clues_asn$SNP = sapply(strsplit(clues_asn$SNP, ":"), function(x) paste(x[1:2], collapse = ":"))
  #clues_eur$SNP = sapply(strsplit(clues_eur$SNP, ":"), function(x) paste(x[1:2], collapse = ":"))
  
  
  total_avg_df = rbindlist(list(clues_asn, clues_eur, onek1k))
  total_avg_df = total_avg_df[rowSums(is.na(total_avg_df[,-1])) != (ncol(total_avg_df)-1), ]
  total_avg_df %>% group_by(SNP) %>% summarise(AverageContext = median(AverageContext, na.rm = T)) %>% write_csv(output_dir)
}

### gets the median of the Average Context across CLUES cohorts (CLUES ASN, CLUES EUR)
get_median_maf_clues <- function(clues_asn_mafs, clues_eur_mafs, output_dir){
  library(data.table)
  library(dplyr)
  library(stringi)
  
  clues_asn = fread(clues_asn_mafs, sep = "\t", data.table = F)
  clues_eur = fread(clues_eur_mafs, sep = "\t", data.table = F)
  
  ### have to make sure the SNP IDs match across cohorts (don't run)
  #onek1k$SNP <- sapply(strsplit(onek1k$SNP, ":"), function(x) paste(x[1:2], collapse = ":"))
  #clues_asn$SNP = sapply(strsplit(clues_asn$SNP, ":"), function(x) paste(x[1:2], collapse = ":"))
  #clues_eur$SNP = sapply(strsplit(clues_eur$SNP, ":"), function(x) paste(x[1:2], collapse = ":"))
  
  
  total_avg_df = rbindlist(list(clues_asn, clues_eur))
  total_avg_df = total_avg_df[rowSums(is.na(total_avg_df[,-1])) != (ncol(total_avg_df)-1), ]
  total_avg_df %>% group_by(SNP) %>% summarise(AverageContext = median(AverageContext, na.rm = T)) %>% write_csv(output_dir)
}

run_match <- function(eqtls_dir, maf_dir, output_dir, matching_seed){
  # run_match(eqtls_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv"), 
  #           maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageTissue.csv"),  # SNP, AverageTissue
  #           output_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
  #           matching_seed = 12345)
 
  
  library(MatchIt)
  set.seed(matching_seed)
  
  print("reading in files...")
  eqtls <- read_csv(eqtls_dir) # set, IS_BG, SNP
  maf <- read_csv(maf_dir) %>% rename(MAF_avg = AverageTissue) %>% select(SNP, MAF_avg)
  
  eqtls %>% group_by(set) %>% group_modify(function(tib, key){
    
    cat(paste0("\n***",key$set[1],"***\n"))
    tib %>% group_by(IS_BG) %>% summarize(n_SNPs = n(), .groups = "drop") %>% print(n=100)
    
    annotated <- inner_join(tib, maf, by = c("SNP")) %>% select(IS_BG, SNP, MAF_avg) %>% as.data.frame
    print("annotated")
    print(annotated)
    
    print("matching...")
    match.obj <- matchit(IS_BG ~ MAF_avg, 
                         data = annotated, 
                         method="nearest", 
                         ratio=1)
    
    print("match.obj")
    print(match.obj)
    print("summary(match.obj)")
    print(summary(match.obj))
    
    return.tib <- match.data(match.obj) %>% as_tibble 
    
    print(return.tib)
    return.tib %>% group_by(IS_BG) %>% summarize(n_SNPs = n(), .groups = "drop") %>% print(n=100)

    return(return.tib)
    print("done! next set running now...")
    
  }) %>% 
    write_csv(output_dir)
  
}

run_match_in_pieces <- function(eqtls_dir, maf_dir, output_dir, matching_seed, num_pieces){
  # run_match_in_pieces(eqtls_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv"),
  #                     maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageTissue.csv"),
  #                     output_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
  #                     matching_seed = 12345,
  #                     num_pieces = 300)
  
  library(MatchIt)
  set.seed(matching_seed)
  
  print("reading in files...")
  eqtls <- read_csv(eqtls_dir, col_types = cols(.default = col_character())) # set, IS_BG, SNP
  maf <- read_csv(maf_dir) %>% rename(MAF_avg = AverageContext) %>% select(SNP, MAF_avg)
  cat("\n\n")
  
  final_tib <- eqtls %>% group_by(set) %>% group_modify(function(tib, key){
    
    cat(paste0("\n***",key$set[1],"***\n"))
    
    #tib: IS_BG, SNP
    bg <- tib %>% filter(IS_BG == 1)
    soi <- tib %>% filter(IS_BG == 0)
    
    # assign randomly number from 1 - num_pieces
    bg$piece_n <- sample(x = 1:num_pieces, size = nrow(bg), replace = TRUE)
    soi$piece_n <- sample(x = 1:num_pieces, size = nrow(soi), replace = TRUE)
    
    matched_results <- tribble(~IS_SOI, ~SNP, ~MAF_avg)
    
    for(n in seq(num_pieces)){
      cat(paste0(n,"/",num_pieces," pieces matched\n"))
      
      piece_tib <- bind_rows(bg %>% filter(piece_n == n), 
                             soi %>% filter(piece_n == n)) %>% select(IS_BG, SNP)
      
      annotated <- inner_join(piece_tib, maf, by = c("SNP")) %>% select(IS_BG, SNP, MAF_avg)
      
      # flip IS_BG to IS_SOI for MatchIt package
      annotated <- annotated %>% mutate(IS_SOI = case_when(IS_BG == 1 ~ 0, IS_BG == 0 ~ 1)) %>% select(IS_SOI, SNP, MAF_avg)
      print(annotated %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
      
      print("matching...")
      match.obj <- matchit(IS_SOI ~ MAF_avg, 
                           data = as.data.frame(annotated), 
                           method="nearest", 
                           ratio=1)
      #print(match.obj)
      #print(summary(match.obj))
      
      matchdata <- match.data(match.obj) %>% as_tibble %>% select(IS_SOI, SNP, MAF_avg)
      print(matchdata %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
      
      matched_results <- bind_rows(matched_results, matchdata) #IS_SOI, SNP, MAF_avg
      cat("\n")
    }
    print("done with this set!")
    
    print(matched_results)
    print(matched_results %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
    
    return(matched_results)

  })
  
  print("final_tib")
  print(final_tib)
  
  final_tib %>% # set, IS_SOI, SNP, MAF_avg
    write_csv(output_dir)
}

plot_match_stats <- function(unmatched_dir, matched_dir, maf_dir, plots_save_dir){
  
  # plot_match_stats(unmatched_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.SNPs_SOI_BG.csv"),
  #                  matched_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
  #                  maf_dir = paste0(output.dir,"MatchingInfo.SNPs_MAF.AverageTissue.csv"),
  #                  plots_save_dir = paste0(output.dir,"plots/MatchingStats.Tissue_Agnostic.pdf"))

  
  library(ggplot2)
  library(cowplot)
  
  maf <- read_csv(maf_dir, col_types = cols(AverageContext = 'd', .default = col_character())) %>% rename(MAF_avg = AverageContext) %>% select(SNP, MAF_avg)
  
  unmatched <- read_csv(unmatched_dir) %>% # set, IS_BG, SNP
    mutate(IS_SOI = case_when(IS_BG == 1 ~ 0, IS_BG == 0 ~ 1)) %>%
    inner_join(maf, by = c("SNP")) %>% 
    select(set, IS_SOI, SNP, MAF_avg)
  
  matched <- read_csv(matched_dir, col_types = cols(SNP = 'c')) #set, IS_SOI, SNP, MAF_avg
  
  print(unmatched)
  print(unmatched %>% group_by(set, IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
  
  print(matched)
  print(matched %>% group_by(set, IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
  
  final_plot <- matched %>% group_by(set) %>% group_map(function(tib, key){
    
    print(key$set[[1]])
    
    tib$IS_SOI <- factor(tib$IS_SOI, c(1, 0))
    unmatched$IS_SOI <- factor(unmatched$IS_SOI, c(1, 0))
    
    p.matched <- tib %>% select(IS_SOI, MAF_avg) %>% 
      # plot
      ggplot(aes(x = MAF_avg, color = IS_SOI)) +
      geom_density()

    p.unmatched <- unmatched %>% 
      filter(set == key$set[[1]]) %>% 
      select(IS_SOI, MAF_avg) %>% 
      # plot
      ggplot(aes(x = MAF_avg, color = IS_SOI)) +
      geom_density()
    
    return(plot_grid(p.matched, p.unmatched, nrow = 1, labels = c("matched","unmatched")))
    
  }) %>% plot_grid(plotlist=., nrow = 3, labels = c("HET", "HOM", "TBT"))
  
  ggsave(plot = final_plot,
         filename = plots_save_dir,
         dpi = 300,
         height = 15, width = 15, units="in")
}

make_genomic_features_file <- function(gen_feature_ID, output_dir){
  #output format = desc,chr,start,end
  if(gen_feature_ID == "VEP"){
    vep_file = fread("/u/project/bballiu/bballiu/FastGxC/results/genomic_features_enrichment/Meta_input_snps_to_vep_no_header_condensed.txt", sep = "\t",
                     data.table = F)
    vep_file$desc = sapply(strsplit(vep_file$Consequence, ","), "[[", 1)
    vep_file = vep_file %>% mutate(desc = case_when(desc == "regulatory_region_variant" ~ Extra, .default = desc))
    vep_file$desc <- gsub("IMPACT=MODIFIER;BIOTYPE=", "", vep_file$desc)
    vep_file$desc <- gsub(";MINIMISED=1", "", vep_file$desc)
    vep_file$desc <- gsub("_variant", "", vep_file$desc)
    gen_features.all = vep_file[,c("chr", "start", "end", "desc")]
    
    gen_features.all %>% write_csv(output_dir) # desc,chr,start,end
  }
  
  if(gen_feature_ID == "CLUES_atac"){
    gen_features.all <- read_tsv("/u/project/bballiu/shared_datasets/reference_datasets/lupus_ATAC_beds/sorted_simple_atac_lineage_groups3.bed",
                                 col_names = c("chr", "start", "end", "desc"), col_types = cols()) %>% select(desc,chr,start,end) %>% 
                                  filter(desc %in% c("B", "T", "myeloid", "open", "nk"))
    gen_features.all <- gen_features.all %>% mutate(desc = case_when(
      ((desc == "myeloid")) ~ "Mye",
      ((desc == "nk")) ~ "NK",
      ((desc == "open")) ~ "Open",
      ((desc == "T")) ~ "T",
      ((desc == "B")) ~ "B"
    ))
    
    gen_features.all %>% write_csv(output_dir) # desc,chr,start,end
  }

  # SCREEN elements NEED TO FIX OUTPUT DIR
  if(gen_feature_ID == "ENCODE_SCREEN"){
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NOT RUN 
    gen_features.all <- read_tsv(paste0(results.dir,"/GRCh38-ccREs.bed"),
                                 col_names = c("chr","start","end","ID1","ID2","desc"), col_types = cols()) %>% select(desc,chr,start,end)
    gen_features.all <- gen_features.all %>% 
      mutate(desc = case_when(
        ((desc == "pELS,CTCF-bound") | (desc == "pELS")) ~ "Proximal Enhancer-Like Signature (pELS)",
        ((desc == "dELS,CTCF-bound") | (desc == "dELS")) ~ "Distal Enhancer-Like Signature (dELS)",
        ((desc == "PLS,CTCF-bound")  | (desc == "PLS"))   ~ "Promoter-Like Signature (PLS)",
        ((desc == "DNase-H3K4me3,CTCF-bound")  | (desc == "DNase-H3K4me3"))   ~ "DNase-H3K4me3",
        desc == "CTCF-only,CTCF-bound" ~ "CTCF-Bound Only",
        TRUE ~ "ERROR"
      ))
    
    print(gen_features.all)
    
    gen_features.all %>% write_csv(output_dir) # desc,chr,start,end
    
  }
  
  # encode regulatory build
  else if(gen_feature_ID == "ENCODE_RegulatoryBuild"){
    
    gen_features.all <- read_tsv("/u/project/zaitlenlab/bballiu/FastGxE/data/Regulatory_Elements/ENCODE_regulatory_elements/homo_sapiens_raw/homo_sapiens.GRCh38.Regulatory_Build.regulatory_features.20190329.gff",
                                 col_names = c("chr", "source", "type", "start", "end", "score", "strand", "phase", "attributes"),
                                 col_types = cols()) %>%
      rename(desc = type) %>% 
      filter(chr %in% c("X",seq(1,22))) %>% 
      mutate(chr = paste0("chr",chr))  %>% 
      select(desc,chr,start,end) %>% 
      filter(desc %in% c("promoter","promoter_flanking_region"))
                    
    print(gen_features.all)
    
    gen_features.all %>% write_csv(output_dir) # desc,chr,start,end
  }
  
  # Eukaryotic Promoter Database (new) 
  else if(gen_feature_ID == "EukPromoterDatabase"){
    
    #### following code not run in this context but shuold work
    EPD_dir <- "/u/project/zaitlenlab/bballiu/FastGxE/data/Regulatory_Elements/EukaryoticPromoterDatabase_hsEPDnew"
    
    beds.info <- tribble(~name, ~length, ~reference,
                         "CCAAT-box", 12, 8,
                         "GC-box",14,7,
                         "Initiator",8,3,
                         "TATA-box",15,4)
    
    all_elements <- bind_rows(lapply(list.files(path = EPD_dir, pattern = "*.bed", full.names = T), function(f){
      
      bed <- read_tsv(f,col_names = c("chrom", "pos_ignore", "pos","name","score_ignore","strand"),col_types = cols()) %>% select(chrom, pos, name, strand)
      
      bed.name <- unique(bed$name)
      bed.length <- beds.info %>% filter(name == bed.name) %>% select(length) %>% .[[1]]
      bed.ref<- beds.info %>% filter(name == bed.name) %>% select(reference) %>% .[[1]]
      
      bed_intervals <- bed %>% group_by(strand) %>% group_modify(function(tib, key){
        
        if(key$strand[[1]] == "+"){
          return.tib <- tib %>%
            mutate(chrom_start = pos - (bed.ref - 1)) %>%
            mutate(chrom_end = pos + (bed.length - bed.ref)) %>% 
            select(chrom, chrom_start, chrom_end, name)
          
        } else if(key$strand[[1]] == "-"){
          return.tib <- tib %>%
            mutate(chrom_start = pos - (bed.length - bed.ref)) %>%
            mutate(chrom_end = pos + (bed.ref - 1)) %>% 
            select(chrom, chrom_start, chrom_end, name)
        } 
        
        return(return.tib)
      }) %>% ungroup %>% select(name, chrom, chrom_start, chrom_end)
      return(bed_intervals)
    }))

    all_elements %>% 
      rename(desc = name, 
             chr = chrom,
             start = chrom_start,
             end = chrom_end) %>% 
      select(desc,chr,start,end) %>% 
      write_csv(output_dir)
    
  }
  else{
    print("unrecognized gen_feature_ID") 
  }
}

call_bedtools_intersect <- function(tib.bed1, tib.bed2){
  ######### tib.bed1 and tib.bed2 = 3 columns: chr, start, end
  ######### returns (unique) rows in tib.bed1 that overlap with tib.bed2 at least once
  
  a.file <- tempfile()
  b.file <- tempfile()
  out.file <- tempfile()
  
  options(scipen=99)
  
  #write bed formatted tibbles to tempfile
  write_tsv(tib.bed1, path = a.file, col_names=FALSE)
  write_tsv(tib.bed2, path = b.file, col_names=FALSE)
  
  #sort bed files; bash = sort -k1,1 -k2,2n in.bed > in.sorted.bed f
  a.file.sorted <- tempfile()
  b.file.sorted <- tempfile()
  try(system2("sort",
              args = c("-k1,1","-k2,2n",a.file),
              stdout = a.file.sorted))
  try(system2("sort",
              args = c("-k1,1","-k2,2n",b.file),
              stdout = b.file.sorted))
  
  # call bedtools intersect on sorted temp bed files
  try(system2("intersectBed",
              args = c("-a",a.file.sorted,"-b",b.file.sorted,"-u","-sorted"),
              stdout = out.file))
  
  # read in bedtools intersect results
  tibble.return <- read_tsv(out.file, col_names = c("chr","start","end"),col_types = cols())
  
  # delete temp files
  unlink(a.file);unlink(b.file);unlink(out.file)
  
  return(tibble.return)
}

#### FIX THIS FUNCTION - currently the intersect intervals file is empty
build_cont_table_by_intersect <- function(input_dir, genomic_features_file_dir, output_dir, output_intervals_dir){

  # build_cont_table_by_intersect(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
  #                               genomic_features_file_dir = gf.dir,
  #                               output_dir = ct.dir,
  #                               output_intervals_dir = "Enrichment.Tissue_Agnostic.EukPromoterDatabase.Intersect_Intervals.csv")
  
  gen_features.all <- read_csv(genomic_features_file_dir) # desc,chr,start,end
  print(gen_features.all %>% select(desc) %>% distinct)
  
  # ~200 MBs
  snps_loc <- read_tsv("/u/project/bballiu/bballiu/FastGxC/results/genomic_features_enrichment/Meta_5prcMAF_snpsloc.txt") %>% #snp (rsIDs), chr, pos
    rename(SNP = snp, SNP_pos = pos, SNP_chr = chr) %>% select(SNP, SNP_chr, SNP_pos) %>% distinct
  
  eqtls.matched <- read_csv(input_dir) %>% # set, IS_SOI, SNP, MAF_avg
    inner_join(snps_loc, by = c("SNP")) %>% 
    select(set, IS_SOI, SNP, SNP_chr, SNP_pos)
  
  intersect_intervals <- tribble(~set, ~IS_SOI, ~regulatory_element, ~chr, ~start, ~end)
  
  eqtls.matched %>% group_by(set) %>% group_modify(function(tib1, key1){
    
    # tib1: IS_SOI, SNP, SNP_chr, SNP_pos
    cat(paste0("***",key1$set[[1]],"***\n"))
    # contingency table
    #                 IN SNP SET    NOT IN SNP SET
    # IN FEATURE          A               B
    # NOT IN FEATURE      C               D
    #                     E               F
    
    soi <- tib1 %>% filter(IS_SOI == 1) %>% select(SNP, SNP_chr, SNP_pos) %>% distinct %>% mutate(chr = SNP_chr, start = SNP_pos, end = SNP_pos) %>% select(chr, start, end)
    bg  <- tib1 %>% filter(IS_SOI == 0) %>% select(SNP, SNP_chr, SNP_pos) %>% distinct %>% mutate(chr = SNP_chr, start = SNP_pos, end = SNP_pos) %>% select(chr, start, end)
    
    cont.E <- nrow(soi)
    cont.F <- nrow(bg)
    #print(paste0("E and F (should be super similar; count of gene-snp pairs in SNP set and in BG matched set) = ",cont.E," and ",cont.F))
    
    set_cont_table <- gen_features.all %>% #desc,chr,start,end
      group_by(desc) %>% group_modify(function(tib2, key2){
        print(key2$desc[1])
        
        soi_intersect <- call_bedtools_intersect(soi, tib2) #"chr","start","end"
        bg_intersect <- call_bedtools_intersect(bg, tib2) #"chr","start","end"
        
        int_temp <- bind_rows(
          soi_intersect %>% mutate(IS_SOI = 1, set = key1$set[1], regulatory_element = key2$desc[1]) %>% select(set, IS_SOI, regulatory_element, chr, start, end), 
          bg_intersect  %>% mutate(IS_SOI = 0, set = key1$set[1], regulatory_element = key2$desc[1]) %>% select(set, IS_SOI, regulatory_element, chr, start, end)
        )
        intersect_intervals <- bind_rows(intersect_intervals, int_temp)  
        print(head(intersect_intervals))
        
        cont.A <- nrow(soi_intersect)
        cont.B <- nrow(bg_intersect)
        cont.C <- cont.E - cont.A
        cont.D <- cont.F - cont.B
        trib_return <- tribble(
          ~cont_A, ~cont_B, ~cont_C, ~cont_D, ~cont_E, ~cont_F, 
          cont.A,  cont.B,   cont.C,  cont.D,  cont.E,  cont.F
        )
        print(trib_return)
        return(trib_return)
        
      }) %>% select(desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F)
    return(set_cont_table)
    
  }) %>% select(set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F) %>% 
    write_csv(output_dir)
  
  intersect_intervals %>% 
    write_csv(output_intervals_dir)
}

build_cont_table_by_intersect_vep <- function(input_dir, snps_loc_file, genomic_features_file_dir, output_dir, output_intervals_dir){
  
  # build_cont_table_by_intersect(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.SNPs_Matched.csv"),
  #                               genomic_features_file_dir = gf.dir,
  #                               output_dir = ct.dir,
  #                               output_intervals_dir = "Enrichment.Tissue_Agnostic.EukPromoterDatabase.Intersect_Intervals.csv")
  
  gen_features.all <- read_csv(genomic_features_file_dir) # desc,chr,start,end
  print(gen_features.all %>% select(desc) %>% distinct)
  
  # ~200 MBs
  snps_loc <- read_tsv(snps_loc_file) %>% #snp (rsIDs), chr, pos
    rename(SNP = snp, SNP_pos = pos, SNP_chr = chr) %>% select(SNP, SNP_chr, SNP_pos) %>% distinct
  
  ######## merge across fastgxc_hom and fastgxc_het
  eqtls.matched = read_csv(input_dir) %>% 
    mutate(set = case_when( set == "fastgxc_hom" ~ "fastgxc_only",
                            set == "fastgxc_het" ~ "fastgxc_only",
                            TRUE ~ set))
  
  eqtls.matched <- read_csv(input_dir) %>% mutate(set = case_when( set == "fastgxc_hom" ~ "fastgxc_only",
                                                                   set == "fastgxc_het" ~ "fastgxc_only",
                                                                   TRUE ~ set)) %>% # set, IS_SOI, SNP, MAF_avg
    inner_join(snps_loc, by = c("SNP")) %>% 
    select(set, IS_SOI, SNP, SNP_chr, SNP_pos)
  
  intersect_intervals <- tribble(~set, ~IS_SOI, ~regulatory_element, ~chr, ~start, ~end)
  
  eqtls.matched %>% group_by(set) %>% group_modify(function(tib1, key1){
    
    # tib1: IS_SOI, SNP, SNP_chr, SNP_pos
    cat(paste0("***",key1$set[[1]],"***\n"))
    # contingency table
    #                 IN SNP SET    NOT IN SNP SET
    # IN FEATURE          A               B
    # NOT IN FEATURE      C               D
    #                     E               F
    
    soi <- tib1 %>% filter(IS_SOI == 1) %>% select(SNP, SNP_chr, SNP_pos) %>% distinct %>% mutate(chr = SNP_chr, start = SNP_pos, end = SNP_pos) %>% select(chr, start, end)
    bg  <- tib1 %>% filter(IS_SOI == 0) %>% select(SNP, SNP_chr, SNP_pos) %>% distinct %>% mutate(chr = SNP_chr, start = SNP_pos, end = SNP_pos) %>% select(chr, start, end)
    
    cont.E <- nrow(soi)
    cont.F <- nrow(bg)
    #print(paste0("E and F (should be super similar; count of gene-snp pairs in SNP set and in BG matched set) = ",cont.E," and ",cont.F))
    
    set_cont_table <- gen_features.all %>% #desc,chr,start,end
      group_by(desc) %>% group_modify(function(tib2, key2){
        print(key2$desc[1])
        
        cur_snps = paste0(tib2$chr, "_", tib2$start, "_", tib2$end)
        soi_snps = paste0(gsub("chr", "", soi$chr), "_", soi$start, "_", soi$end)
        
        soi_intersect <- call_bedtools_intersect(soi, tib2) #"chr","start","end"
        bg_intersect <- call_bedtools_intersect(bg, tib2) #"chr","start","end"
        
        int_temp <- bind_rows(
          soi_intersect %>% mutate(IS_SOI = 1, set = key1$set[1], regulatory_element = key2$desc[1]) %>% select(set, IS_SOI, regulatory_element, chr, start, end), 
          bg_intersect  %>% mutate(IS_SOI = 0, set = key1$set[1], regulatory_element = key2$desc[1]) %>% select(set, IS_SOI, regulatory_element, chr, start, end)
        )
        intersect_intervals <- bind_rows(intersect_intervals, int_temp)  
        print(head(intersect_intervals))
        
        cont.A <- nrow(soi_intersect)
        cont.B <- nrow(bg_intersect)
        cont.C <- cont.E - cont.A
        cont.D <- cont.F - cont.B
        trib_return <- tribble(
          ~cont_A, ~cont_B, ~cont_C, ~cont_D, ~cont_E, ~cont_F, 
          cont.A,  cont.B,   cont.C,  cont.D,  cont.E,  cont.F
        )
        print(trib_return)
        return(trib_return)
        
      }) %>% select(desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F)
    return(set_cont_table)
    
  }) %>% select(set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F) %>% 
    write_csv(output_dir)
  
  intersect_intervals %>% 
    write_csv(output_intervals_dir)
}

run_fishers_exact_test_and_fdr <- function(input_dir, output_dir){
  # run_fishers_exact_test_and_fdr(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.contingency_table.csv"),
  #                                output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.fisher_results_fdr.csv")) # set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper

  cont_tables <- read_csv(input_dir) # set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F
  
  fisher.all <- cont_tables %>% group_by(set, desc) %>% group_modify(function(tib, key){
    
    print(paste0("KEY: ", key))
    if(nrow(tib) != 1){
      print("MORE THAN ONE CONTINGENCY ROW PER SET AND DESC?")
      return(NA)
    }
      
    cont.matrix <- matrix(c(tib$cont_A[1], tib$cont_C[1], tib$cont_B[1], tib$cont_D[1]), nrow=2)
    print(cont.matrix)
    fisher.results <- fisher.test(cont.matrix)
    print(fisher.results)
    return(tribble(~ odds_ratio, ~ p.value, ~ conf_int.lower, ~ conf_int.upper,
                   unname(fisher.results$estimate),
                   fisher.results$p.value,
                   fisher.results$conf.int[1],
                   fisher.results$conf.int[2]))
  })
  
  # FDR correction
  fisher.all.fdr_corrected <- fisher.all %>% group_by(set) %>% 
    group_modify(function(tib, key){
      return(tib %>% mutate(p.adjusted.BH = p.adjust(p.value, method = "BH")))
    }) %>% ungroup %>% 
    select(set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper)

  fisher.all.fdr_corrected %>% write_csv(output_dir)
  
}
