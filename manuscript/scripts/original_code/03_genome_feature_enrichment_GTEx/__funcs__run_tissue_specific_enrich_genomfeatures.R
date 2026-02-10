library(tidyverse)

make_snp_sets <- function(input_dir, output_dir){
  
  # make_snp_sets(input_dir = paste0(bruna_project.dir, "results/eQTL_mapping/Github_Data/eAssociations.v8.EUR.all_tissues.residualized_exp_types.txt"),
  #               output_dir = snp_sets.dir) # set, tissue, SNP
  
  eqtls.all <- read_csv(input_dir) %>% #exp_type,tissue,gene,snp
    rename(SNP = snp) %>% 
    select(exp_type, tissue, SNP)          

  het <- eqtls.all %>% filter(exp_type == "normalized_and_residualized_expression_heterogeneous") %>% select(tissue, SNP) %>% distinct
  tbt <- eqtls.all %>% filter(exp_type == "normalized_and_residualized_expression") %>% select(tissue, SNP) %>% distinct
  
  rm(eqtls.all)
  gc()
  
  writeLines("*** making snp sets... ***\n")
  
  bind_rows(
    # snps with het effects in each individual tissue
    het %>%
      select(tissue, SNP) %>% distinct %>%
      mutate(set = "HET.each_tissue") %>%
      select(set, tissue, SNP),

    # snps with tbt effects in each individual tissue
    tbt %>%
      select(tissue, SNP) %>% distinct %>%
      mutate(set = "TBT.each_tissue") %>%
      select(set, tissue, SNP),

    # snps with het effects found only in a single tissue
    het %>%
      select(tissue, SNP) %>% distinct %>%
      group_by(SNP) %>% filter(n() == 1) %>% ungroup %>%
      mutate(set = "HET.single_tissue") %>%
      select(set, tissue, SNP),

    # snps with tbt effects found only in a single tissue
    tbt %>%
      select(tissue, SNP) %>% distinct %>%
      group_by(SNP) %>% filter(n() == 1) %>% ungroup %>%
      mutate(set = "TBT.single_tissue") %>%
      select(set, tissue, SNP)
    
    ### consider adding snps with effects found in few tissues
    
  ) %>% write_csv(output_dir) #set, tissue, SNP
  
  
}

add_matched_snps_per_tissue <- function(tissue_run, snp_sets_dir, output_dir){
  print(paste0("***** ",tissue_run," ******"))
  # add_matched_snps_per_tissue(tissue = args[1],
  #                             snp_sets_dir = snp_sets.dir, 
  #                             output_dir = soi_bg.dir)
  
  
  library(MatchIt)
  
  print("reading in files...")
  maf <- read_csv("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/Feature_Enrichment/Tissue_Agnostic/MatchingInfo.SNPs_MAF.AverageTissue.csv") %>% 
    rename(MAF_avg = AverageTissue) %>% select(SNP, MAF_avg)
  #SNP	gene	beta	t-stat	p-value	FDR
  tested <- read_tsv(paste0("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/MatrixEQTL/",
                            tissue_run, ".v8.EUR.normalized_and_residualized_expression_heterogeneous.all_pairs.txt"), col_types = cols()) %>% select(SNP) %>% distinct
  
  snp_sets <- read_csv(snp_sets_dir) %>% # tissue, set, SNP
    filter(tissue == tissue_run) %>% select(set, SNP)
  
  final <- snp_sets %>% group_by(set) %>% group_modify(function(tib, key){
    
    cat(paste0("\n\nrunning ",key$set[1],"...\n"))
    
    soi_temp <- tib %>% mutate(IS_SOI = 1) %>% select(IS_SOI, SNP) %>% distinct
    joined <- right_join(soi_temp, tested, by = c("SNP")) %>% 
      mutate(IS_SOI = replace_na(IS_SOI, 0)) %>% 
      select(IS_SOI, SNP)
    
    print("adding random n...")
    # add new column (piece_n) that has random numbers for piece by piece matching
    #num_pieces <- 50
    print(joined %>% group_by(IS_SOI) %>% summarize(n_SNPs = n()))
    n_rows_soi <- nrow(joined %>% filter(IS_SOI == 1))
    n_rows_bg  <- nrow(joined %>% filter(IS_SOI == 0))
    # pick smaller of two number, and then how many times to split up to get 5000 SNPs per match
    if(min(n_rows_soi,n_rows_bg) > 5000) {
      num_pieces = round(min(n_rows_soi,n_rows_bg) / 5000)
    }else {
      num_pieces = 1
    }
    print(paste0("num pieces split = ",num_pieces))
    
    joined <- joined %>% group_by(IS_SOI) %>% group_modify(function(tt, kk){
      tt$piece_n <- sample(x = 1:num_pieces, size = nrow(tt), replace = TRUE)
      return(tt)
    }) %>% select(IS_SOI, SNP, piece_n)
    
    n_results <- tribble(~IS_SOI, ~SNP, ~MAF_avg)
    
    print("starting matching loop...")
    for(n in seq(num_pieces)){
      cat(paste0(n,"/",num_pieces," pieces matched\n"))
      
      joined.n <- joined %>% filter(piece_n == n) %>% select(IS_SOI, SNP)
      
      annotated <- inner_join(joined.n, maf, by = c("SNP")) %>% select(IS_SOI, SNP, MAF_avg)
      
      print(annotated %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
      
      print("matching...")
      match.obj <- matchit(IS_SOI ~ MAF_avg, 
                           data = as.data.frame(annotated), 
                           method="nearest", 
                           ratio=1)
      
      matchdata <- match.data(match.obj) %>% as_tibble %>% select(IS_SOI, SNP, MAF_avg)
      print(matchdata %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
      
      n_results <- bind_rows(n_results, matchdata) #IS_SOI, SNP, MAF_avg
      cat("\n")
    }
    print("n_results")
    print(n_results)
    print(n_results %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
    return(n_results)
  }) #set, IS_SOI, SNP, MAF_avg
  
  print("final")
  print(final)
  final %>% write_csv(output_dir)
  
}

merge_matched_snps_across_tissues <- function(enr_folder, output_dir){
  
  # merge_matched_snps_across_tissues(enr_folder = output.dir, 
  #                                   output_dir = paste0(output.dir, "Enrichment.Tissue_Specific.All_Tissues_Merged.SNP_sets_with_matched_BG.csv"))

  merged <- bind_rows(lapply(list.files(path = enr_folder, pattern = "*.SNP_sets_with_matched_BG.csv", full.names = F), 
                   function(f){
                     print(f)
                     f.tib <- read_csv(paste0(enr_folder, f), col_type = cols()) %>% #set, IS_SOI, SNP, MAF_avg
                       mutate(tissue = strsplit(f, "[.]")[[1]][3]) %>% 
                       select(set, tissue, IS_SOI, SNP)
                     #print(f.tib %>% group_by(tissue, IS_SOI) %>% summarize(n_snps = n(), .groups = "drop"))
                     return(f.tib)
                   }))
  
  print("merged")
  print(merged)
  print(merged %>% group_by(set, tissue, IS_SOI) %>% summarize(n_snps = n(), .groups = "drop"))
  
  merged %>% select(set, tissue, IS_SOI, SNP) %>% write_csv(output_dir)
}

run_intersect_by_set <- function(input_dir, enr_folder, set_run, out_dir){

  # run_intersect_by_set(input_dir = merged_snps.dir,
  #                      enr_folder = output.dir,
  #                      set_run = args[1],
  #                      out_dir = paste0(output.dir, "Enrichment.Tissue_Specific.ATAC_Intersect_Contingency_Table.",args[1],".csv"))
  
  print("reading in files...")
  GTEx_tissues <- read_csv(paste0(enr_folder, "Enrichment.Tissue_Specific.GTEx_Tissues_with_matches.csv"), col_types = cols())
  ENCODE_tissues <- read_csv(paste0(enr_folder, "Enrichment.Tissue_Specific.ENCODE_Tissues_with_matches.csv"), col_types = cols())
  snps_loc <- read_tsv("/u/project/zaitlenlab/bballiu/FastGxE/data/GTEx_v8/MatrixEQTL_input/GTEx_v8_WGS_838Indiv_Analysis_Freeze_EUR_SNPs_1prcMAF_snpsloc.txt", col_types = cols()) %>% #snp (rsIDs), chr, pos
    rename(SNP = snp, SNP_pos = pos, SNP_chr = chr) %>% select(SNP, SNP_chr, SNP_pos) %>% distinct
  snps <- read_csv(input_dir, col_types = cols()) %>% # set, tissue, IS_SOI, SNP
    filter(set == set_run) %>%
    inner_join(snps_loc, by = "SNP") %>% rename(chr = SNP_chr) %>% mutate(start = SNP_pos, end = SNP_pos) %>% 
    select(tissue, IS_SOI, SNP, chr, start, end)
  #print(snps %>% group_by(tissue, IS_SOI) %>% summarize(n_snps = n(), .groups = "drop"), n=1000)
  
  intersect_pairwise_results <- tribble(~ENCODE_tissue, ~ENCODE_ac_num, ~GTEx_tissue, ~ct.A, ~ct.B, ~ct.C, ~ct.D, ~ct.E, ~ct.F)
  
  cat('\nstarting nested loop...\n\n')
  
  for(encode.t in 1:nrow(ENCODE_tissues)){
    
    encode.ac_num <- ENCODE_tissues[encode.t, "File_Accession", drop = TRUE]
    encode.name <- ENCODE_tissues[encode.t, "Tissue_Name", drop = TRUE]
    print(paste0("***running encode: ",encode.name," ", encode.ac_num, " ; ", encode.t, "/",nrow(ENCODE_tissues)," ***"))
    
    atac_dir <- paste0("/u/project/zaitlenlab/bballiu/FastGxE/data/Regulatory_Elements/ENCODE_ATAC-seq/filtered_beds_p-0.01_sorted/",encode.ac_num, "_filtered_sorted.bed")
    
    for(gtex.t in 1:nrow(GTEx_tissues)){
      
      gtex.name <- GTEx_tissues[gtex.t,"Tissue_Name",drop = TRUE]
      print(paste0("gtex: ",gtex.name))
      
      # contingency table
      #                 IN SNP SET    NOT IN SNP SET
      # IN FEATURE          A               B
      # NOT IN FEATURE      C               D
      #                     E               F
      
      soi <- snps %>% filter(tissue == gtex.name) %>% filter(IS_SOI == 1) %>% distinct %>% select(chr, start, end)
      bg <- snps %>% filter(tissue == gtex.name) %>% filter(IS_SOI == 0) %>% distinct %>% select(chr, start, end)
      
      a.file <- tempfile()
      write_tsv(soi, path = a.file, col_names=FALSE)
      a.file.sorted <- tempfile()
      try(system2("sort",
                  args = c("-k1,1","-k2,2n",a.file),
                  stdout = a.file.sorted))
      out.file <- tempfile()
      try(system2("intersectBed",
                  args = c("-a",a.file.sorted,"-b",atac_dir,"-u","-sorted"),
                  stdout = out.file))
      cont.A <- nrow(read_tsv(out.file, col_names = c("chr","start","end"),col_types = cols()))
      unlink(a.file);unlink(a.file.sorted);unlink(out.file);
      
      a.file <- tempfile()
      write_tsv(bg, path = a.file, col_names=FALSE)
      a.file.sorted <- tempfile()
      try(system2("sort",
                  args = c("-k1,1","-k2,2n",a.file),
                  stdout = a.file.sorted))
      out.file <- tempfile()
      try(system2("intersectBed",
                  args = c("-a",a.file.sorted,"-b",atac_dir,"-u","-sorted"),
                  stdout = out.file))
      cont.B <- nrow(read_tsv(out.file, col_names = c("chr","start","end"),col_types = cols()))
      unlink(a.file);unlink(a.file.sorted);unlink(out.file);
      
      cont.E <- nrow(soi)
      cont.F <- nrow(bg)
      cont.C <- cont.E - cont.A
      cont.D <- cont.F - cont.B
      
      this_pair_result <- tribble(~ENCODE_tissue, ~ENCODE_ac_num, ~GTEx_tissue, ~ct.A, ~ct.B, ~ct.C, ~ct.D, ~ct.E, ~ct.F,
                                  encode.name, encode.ac_num, gtex.name, cont.A, cont.B, cont.C, cont.D, cont.E, cont.F)
      print(this_pair_result)
      
      intersect_pairwise_results <- bind_rows(intersect_pairwise_results,this_pair_result)
      cat("\n\n")
    }
    
    print(gc())
    cat("\n\n\n")
    
  }
  
  print("done done done")
  print("intersect_pairwise_results")
  print(intersect_pairwise_results, n = 100)
  
  print("writing to file")
  print(out_dir)
  intersect_pairwise_results %>% write_csv(out_dir)

}

run_fishers_exact_test_and_fdr <- function(cont_dir, out_dir){
  # run_fishers_exact_test_and_fdr(cont_dir = output.dir,
  #                                out_dir = paste0(output.dir, "Enrichment.Tissue_Specific.ENCODE_ATAC_Intersect.fisher_results_fdr.csv"))
  
  print("merging cont tables..")
  g_clean=read_csv(paste0(cont_dir,"Enrichment.Tissue_Specific.GTEx_Tissues_clean.txt"),col_types=cols(),col_names = c("tissue")) %>% pull(tissue)
  e_clean=read_csv(paste0(cont_dir,"Enrichment.Tissue_Specific.ENCODE_Tissues_clean.txt"),col_types=cols(),col_names = c("tissue")) %>% pull(tissue)
  cont_tables <- bind_rows(lapply(list.files(path = cont_dir, pattern = "Enrichment.Tissue_Specific.ATAC_Intersect_Contingency_Table.*", full.names = F),
         function(f){
           f.set <- paste0(strsplit(f, "[.]")[[1]][4],".",strsplit(f, "[.]")[[1]][5])
           # ENCODE_tissue, ENCODE_ac_num, GTEx_tissue, ct.A, ct.B, ct.C, ct.D, ct.E, ct.F
           f.tib <- read_csv(paste0(cont_dir,f),col_types=cols()) %>% 
             filter(ENCODE_ac_num %in% e_clean) %>% 
             filter(GTEx_tissue %in% g_clean) %>% 
             mutate(set = f.set) %>% 
             select(set, ENCODE_tissue, ENCODE_ac_num, GTEx_tissue, ct.A, ct.B, ct.C, ct.D, ct.E, ct.F)
           return(f.tib)
         }))
  
  print("running fisher tests now...")
  fisher.all <- cont_tables %>% group_by(set, ENCODE_tissue, ENCODE_ac_num, GTEx_tissue) %>% group_modify(function(tib, key){
    
    cont.matrix <- matrix(c(tib$ct.A[1], tib$ct.C[1], tib$ct.B[1], tib$ct.D[1]), nrow=2)
    fisher.results <- fisher.test(cont.matrix)
    ret.tib <- tribble(~ odds_ratio, ~ p.value, ~ conf_int.lower, ~ conf_int.upper,
                       unname(fisher.results$estimate),
                       fisher.results$p.value,
                       fisher.results$conf.int[1],
                       fisher.results$conf.int[2])

    return(ret.tib)
  }) %>% ungroup %>% select(set, ENCODE_tissue, ENCODE_ac_num, GTEx_tissue, odds_ratio, p.value, conf_int.lower, conf_int.upper)
  
  print("running FDR correction...")
  fisher.all.fdr_corrected <- fisher.all %>% group_by(set) %>%
    group_modify(function(tib, key){
      return(tib %>% mutate(p.adjusted.BH = p.adjust(p.value, method = "BH")))
    }) %>% ungroup %>%
    select(set, ENCODE_tissue, ENCODE_ac_num, GTEx_tissue, odds_ratio, p.adjusted.BH, p.value, conf_int.lower, conf_int.upper)

  #print(fisher.all.fdr_corrected[,c("set","ENCODE_tissue","GTEx_tissue","odds_ratio","p.adjusted.BH","p.value")], n = 1000)
  
  print("writing final output...")
  print(out_dir)
  
  fisher.all.fdr_corrected %>% write_csv(out_dir)
  
}


