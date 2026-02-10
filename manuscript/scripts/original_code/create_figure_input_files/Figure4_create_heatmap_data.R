# make single cell prep files for Figure 4 heatmap: 
if(1){
  
  eqtls=read_csv("/Users/lkrockenberger/Google Drive/My Drive/FastGxC_Manuscript/manuscript/Input_Files/Figure3_sc_Performance/eAssociations.scMeta.all_contexts.residualized_exp_types.all.stats.txt", col_types = cols(.default = col_character()))
  eqtls$beta = as.numeric(eqtls$beta)
  eqtls$beta = as.numeric(eqtls$t.stat)
  eqtls$p.value = as.numeric(eqtls$p.value)
  
  eqtls %>% select(exp_type) %>% distinct
  
  # fastgxe: not done - run again after looking at bulk file for reference
  if(1){
    fastgxe_eqtls = eqtls %>% 
      filter(exp_type %in% c("mean_norm_res_exp_heterogeneous")) %>% 
      # filter(exp_type%in%c("normalized_and_residualized_expression_heterogeneous","normalized_and_residualized_expression_homogeneous")) %>% 
      mutate(snp_gene=paste0(snp ,"_",gene)) %>% 
      select(snp_gene,tissue,beta)
    
    
    fastgxe_mat = fastgxe_eqtls %>%
      select(snp_gene,tissue,beta) %>%
      pivot_wider(names_from=tissue,
                  values_from=beta)
    
    #fastgxe_mat[is.na(fastgxe_mat)] = 0
    
    cor(x=fastgxe_mat %>% select(-snp_gene), 
        use = "complete.obs", #"pairwise.complete.obs",
        method = "spearman") %>%
      write.table(file = "manuscript/Input_Files/Figure4_Correlation_Heatmap/fastgxe_hetonly_cor_spearman_sc.txt")
    
    cor(x=fastgxe_mat %>% select(-snp_gene), 
        use = "pairwise.complete.obs",
        method = "pearson") %>%
      write.table(file = "manuscript/Input_Files/Figure4_Correlation_Heatmap/fastgxe_hetonly_cor_pearson_sc.txt")
  }
  
  # tbt
  if(1){
    print("filtering...")
    tbt_eqtls = eqtls %>% 
      filter(exp_type%in%c("mean_norm_res_exp")) %>% 
      mutate(snp_gene=paste0(snp,"_",gene)) %>% 
      select(snp_gene,tissue,beta)
    
    print("making matrix..")
    tbt_mat = tbt_eqtls %>%
      select(snp_gene,tissue,beta) %>%
      pivot_wider(names_from=tissue,
                  values_from=beta)
    
    #tbt_mat[is.na(tbt_mat)] = 0
    
    print("running cor...")
    cor(x=tbt_mat %>% select(-snp_gene), 
        use = "complete.obs",#"pairwise.complete.obs",
        method = "pearson") %>%
      write.table(file = "manuscript/Input_Files/Figure4_Correlation_Heatmap/tbt_cor_pearson_sc.txt")
    
    # old
    if(0){
      # tbt_eqtls = eqtls %>% filter(exp_type%in%c("normalized_and_residualized_expression")) %>%
      #   mutate(snp_gene=paste0(snp,"_",gene_ensembl)) %>%
      #   select(snp_gene,tissue,beta)
      # tbt_mat = tbt_eqtls %>%
      #   select(snp_gene,tissue,beta) %>%
      #   pivot_wider(names_from=tissue,
      #               values_from=beta)
      # tbt_mat[is.na(tbt_mat)] = 0
      # cor(x=tbt_mat %>% select(-snp_gene),
      #     use = "pairwise.complete.obs",
      #     method = "spearman") %>% # try also: method = "pearson" (default) LINEAR
      #   write.table(file = "manuscript/input_files/Figure3_GTEx_Performance/new_cor.spearman.tbt.txt")
    }
  }
  
}

# make bulk prep files for Figure 4 heatmap: 
if(1){
  
  eqtls=read_csv("/Users/lkrockenberger/Google Drive/My Drive/FastGxE_data/eAssociations.v8.EUR.all_tissues.residualized_exp_types.all_stats.txt", col_types = cols(.default = col_character()))
  eqtls$beta = as.numeric(eqtls$beta)
  eqtls$beta = as.numeric(eqtls$t.stat)
  eqtls$p.value = as.numeric(eqtls$p.value)
  
  eqtls %>% select(exp_type) %>% distinct
  
  # fastgxe: not done - run again after looking at bulk file for reference
  if(1){
    fastgxe_eqtls = eqtls %>% 
     filter(exp_type%in%c("normalized_and_residualized_expression_heterogeneous","normalized_and_residualized_expression_homogeneous")) %>% 
      mutate(snp_gene=paste0(snp ,"_",gene_ensembl)) %>% 
      select(snp_gene,tissue,beta)
    
    
    fastgxe_mat = fastgxe_eqtls %>%
      select(snp_gene,tissue,beta) %>%
      pivot_wider(names_from=tissue,
                  values_from=beta)
    
    fastgxe_mat[is.na(fastgxe_mat)] = 0
    
    cor(x=fastgxe_mat %>% select(-snp_gene), 
        use = "pairwise.complete.obs",
        method = "spearman") %>%
      write.table(file = "manuscript/Input_Files/Figure4_Correlation_Heatmap/fastgxe_hetonly_cor_spearman.txt")
    
    cor(x=fastgxe_mat %>% select(-snp_gene), 
        use = "pairwise.complete.obs",
        method = "pearson") %>%
      write.table(file = "manuscript/Input_Files/Figure4_Correlation_Heatmap/fastgxe_hetonly_cor_pearson.txt")
  }
  
  # tbt
  if(1){
    print("filtering...")
    tbt_eqtls = eqtls %>% 
      filter(exp_type%in%c("normalized_and_residualized_expression")) %>% 
      mutate(snp_gene=paste0(snp,"_",gene_ensembl)) %>% 
      select(snp_gene,tissue,beta)
    
    print("making matrix..")
    tbt_mat = tbt_eqtls %>%
      select(snp_gene,tissue,beta) %>%
      pivot_wider(names_from=tissue,
                  values_from=beta)
    
    tbt_mat[is.na(tbt_mat)] = 0
    
    print("running cor...")
    cor(x=tbt_mat %>% select(-snp_gene), 
        use = "pairwise.complete.obs",
        method = "pearson") %>%
      write.table(file = "manuscript/Input_Files/Figure4_Correlation_Heatmap/tbt_cor_pearson_sc.txt")
    
    # old
    if(0){
      # tbt_eqtls = eqtls %>% filter(exp_type%in%c("normalized_and_residualized_expression")) %>%
      #   mutate(snp_gene=paste0(snp,"_",gene_ensembl)) %>%
      #   select(snp_gene,tissue,beta)
      # tbt_mat = tbt_eqtls %>%
      #   select(snp_gene,tissue,beta) %>%
      #   pivot_wider(names_from=tissue,
      #               values_from=beta)
      # tbt_mat[is.na(tbt_mat)] = 0
      # cor(x=tbt_mat %>% select(-snp_gene),
      #     use = "pairwise.complete.obs",
      #     method = "spearman") %>% # try also: method = "pearson" (default) LINEAR
      #   write.table(file = "manuscript/input_files/Figure3_GTEx_Performance/new_cor.spearman.tbt.txt")
    }
  }
  
}



