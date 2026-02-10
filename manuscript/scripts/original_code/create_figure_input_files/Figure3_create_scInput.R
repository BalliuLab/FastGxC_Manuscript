#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% 
#%%%%%%%%%%%%%%% FastGxC Create Manuscript Figure Input
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% Sunday, June 30 2024
#%%%%%%%%%%%%%%% 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls())

## imports
library(data.table)
library(dplyr)
library(tidyverse)

## functions
fig3input = function(workdir, outdir, all_stats = F){
  eAssoc_files = list.files(workdir, pattern = "eAssoc")
  if(all_stats){
    final_df = data.frame(matrix(nrow = 0, ncol = 7))
    names(final_df) = c("exp_type", "tissue", "gene", "snp", "beta", "t.stat", "p.value")
  }else{
    final_df = data.frame(matrix(nrow = 0, ncol = 4))
    names(final_df) = c("exp_type", "tissue", "gene", "snp")
  }
  for(file in eAssoc_files){
    cur_exp_type = strsplit(file, "\\.")[[1]][3]
    if(grepl("specific", file)){
      cur_exp_type = paste0(strsplit(file, "\\.")[[1]][3], "_heterogeneous")
    }
    if(grepl("shared", file)){
      cur_exp_type = paste0(strsplit(file, "\\.")[[1]][3], "_homogeneous")
    }
    context = strsplit(file, "\\.")[[1]][2]
    cur_df = fread(paste0(workdir, file), sep=' ', data.table = F)
    tmp = data.frame(exp_type = cur_exp_type, tissue = context, gene = cur_df$gene, snp = cur_df$SNP)
    if(all_stats){
      tmp = data.frame(exp_type = cur_exp_type, tissue = context, gene = cur_df$gene, snp = cur_df$SNP, beta = cur_df$beta, t.stat = cur_df$t.stat, p.value = cur_df$p.value)
    }
    final_df = rbind(final_df, tmp)
  }
  if(all_stats){
    fwrite(final_df, file = paste0(outdir, "eAssociations.scMeta.all_contexts.residualized_exp_types.all.stats.txt"), sep = ",")
  }else{
    fwrite(final_df, file = paste0(outdir, "eAssociations.scMeta.all_contexts.residualized_exp_types.txt"), sep = ",")
  }
  return(final_df)
}


#### create single cell input file with exp type, tissue, gene, snp: DONE
if(1){
  workdir = "/Users/lkrockenberger/Documents/Balliu_Lab/FastGxC/sc_results/"
  outdir = "/Users/lkrockenberger/Google Drive/My Drive/FastGxC_Manuscript/manuscript/Input_Files/Figure3_sc_Performance/"
  fig3_df = fig3input(workdir, outdir)
}

#### create single cell input file with exp type, tissue, gene, snp, beta, t.stat (might be SE), p.value: DONE
if(1){
  workdir = "/Users/lkrockenberger/Documents/Balliu_Lab/FastGxC/sc_results/"
  outdir = "/Users/lkrockenberger/Google Drive/My Drive/FastGxC_Manuscript/manuscript/Input_Files/Figure3_sc_Performance/"
  fig3_df = fig3input(workdir, outdir, T)
}

##### create single cell input file for eGenes
if(1){
  workdir = "/Users/lkrockenberger/Documents/Balliu_Lab/FastGxC/sc_results/"
  outdir = "/Users/lkrockenberger/Google Drive/My Drive/FastGxC_Manuscript/manuscript/Input_Files/Figure3_sc_Performance/"
  files = list.files(workdir, pattern = "eGenes", full.names = T)
  
  fread_func = function(file, exp_type){
    df = fread(file, sep = "\t", data.table = F)
    df$exp_type = exp_type
    return(df)
  }
  egenes = rbind(a = fread_func(files[2], exp_type = "mean_norm_res_exp.specific"), b = fread_func(files[3], "mean_norm_res_exp"))
  egenes = egenes %>% pivot_longer(cols = c("B", "CD4", "CD8", "NK", "cDC", "cMono", "ncMono", "pDC"), values_to = "value") %>% filter(value == 1) %>% select(gene, name,exp_type) %>% 
            rename(tissue = "name")
  sh_egenes = fread(files[1], sep = "\t", data.table = F)
  egenes = rbind(egenes, tmp_df = data.frame(gene = sh_egenes$family, tissue = "AverageContext", exp_type = "mean_norm_res_exp.shared")) %>% 
            select(c("exp_type", "tissue", "gene"))
  fwrite(egenes, file = paste0(outdir, "eGenes.scMeta.all_contexts.residualized_exp_types.txt"), sep = ",")
  
}

##### create bulk input file for eGenes (hoffman)
if(1){
  workdir = "/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/TreeQTL/"
  outdir = "/u/scratch/l/lkrocken/"
  files = list.files(workdir, pattern = "eGenes", full.names = T)
  files = files[grepl("normalized_and_residualized", files)]
  
  fread_func = function(file, exp_type){
    df = fread(file, sep = "\t", data.table = F)
    df$exp_type = exp_type
    return(df)
  }
  egenes = rbind(a = fread_func(files[1], exp_type = "normalized_and_residualized_expression_heterogeneous"), b = fread_func(files[3], "normalized_and_residualized_expression"))
  egenes = egenes %>% pivot_longer(cols = names(a)[c(-1,-length(names(a)))], values_to = "value") %>% filter(value == 1) %>% select(gene, name,exp_type) %>% 
    rename(tissue = "name")
  sh_egenes = fread(files[2], sep = "\t", data.table = F)
  egenes = rbind(egenes, tmp_df = data.frame(gene = sh_egenes$family, tissue = "AverageContext", exp_type = "normalized_and_residualized_expression_homogeneous")) %>% 
    select(c("exp_type", "tissue", "gene"))
  fwrite(egenes, file = paste0(outdir, "eGenes.v8.EUR.all_tissues.residualized_exp_types_test.txt"), sep = ",")
  
}

#### create sample size and cell number file method,tissue,n_samples: DONE
if(1){
  clues_asn_cell_file = "/Users/lkrockenberger/Documents/Balliu_Lab/FastGxC/cell_samp_size_files/CLUES_ASN_donor_celltype_cell.txt"
  clues_eur_cell_file = "/Users/lkrockenberger/Documents/Balliu_Lab/FastGxC/cell_samp_size_files/CLUES_EUR_donor_celltype_cell.txt"
  onek1k_cell_file = "/Users/lkrockenberger/Documents/Balliu_Lab/FastGxC/cell_samp_size_files/OneK1K_donor_celltype_cell.txt"
  outfile = "/Users/lkrockenberger/Google Drive/My Drive/FastGxC_Manuscript/data/CLUES_OneK1K/CLUES_OneK1K_NSamples_by_Context_and_Method.csv"
    
  num_cell_samp_size = data.frame()
  num_cell_samp_size = rbind(num_cell_samp_size, cbind(fread(clues_asn_cell_file, sep = ",", data.table = F), cohort = "CLUES_ASN"))
  num_cell_samp_size = rbind(num_cell_samp_size, cbind(fread(clues_eur_cell_file, sep = ",", data.table = F), cohort = "CLUES_EUR"))
  num_cell_samp_size = rbind(num_cell_samp_size, cbind(fread(onek1k_cell_file, sep = ",", data.table = F), cohort = "OneK1K"))
  
  n_samp_cells = num_cell_samp_size %>% group_by(final_celltypes) %>% mutate(n_cells = n()) %>% 
    group_by(final_celltypes) %>% mutate(n_samples = length(unique(donor_id)))
  shared_cells = nrow(n_samp_cells)
  shared_samples = length(unique(n_samp_cells$donor_id))
  n_samp_cells = unique(n_samp_cells[,c("final_celltypes", "n_cells", "n_samples")])
  names(n_samp_cells) = c("tissue", "n_cells", "n_samples")
  n_samp_cells = cbind(method = "mean_norm_res_exp", n_samp_cells)
  n_samp_cells = rbind(n_samp_cells, cbind(method = "mean_norm_res_exp_heterogeneous", n_samp_cells[,c("tissue", "n_cells", "n_samples")]))
  n_samp_cells = rbind(data.frame(method = "mean_norm_res_exp_homogeneous", tissue = "AverageContext", n_cells = shared_cells, n_samples = shared_samples), n_samp_cells)
  fwrite(n_samp_cells, outfile, sep = ",")
}
