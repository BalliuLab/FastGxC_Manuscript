##########################################################################################
### Script that adds up shared and specific estimated effects to get FastGxC main effect
### Lena Krockenberger 
### January 16, 2026
##########################################################################################

library(data.table)
library(dplyr)

outdir = "/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/Added_Results_CGreviews/"
combine_fastgxc_cxc = function(file, shared_effects, cxc_files){
  cur_df = fread(file, sep = " ")
  cur_df[shared_effects, beta := beta + i.beta, on = .(gene, SNP)] ### add shared effect onto specific effect only for where gene,SNP pair match otherwise leave row unchanged
  context = strsplit(basename(file), "\\.")[[1]][2]
  cur_df = cbind(tissue = context, cur_df)
  
  cxc_file = cxc_files[grepl(paste0("\\.", context, "\\."), cxc_files)]
  cxc_df = fread(cxc_file, sep = " ")
  
  ### get ids in cxc that match with fastgxc
  fastgxc_ids = paste(cur_df$gene, cur_df$SNP, sep = "_")
  cxc_ids = paste(cxc_df$gene, cxc_df$SNP, sep = "_")
  
  keep = cxc_ids %in% fastgxc_ids
  cxc_df = cxc_df[keep, ]
  cur_df = cur_df[match(cxc_ids[keep], fastgxc_ids), ]
  
  ### merged
  total_df = cbind(cur_df, beta_cxc = cxc_df$beta, t.stat_cxc = cxc_df$t.stat, p.value_cxc = cxc_df$t.stat)
  total_df
  
}

combine = function(file, shared_effects, cxc_files){
  cur_df = fread(file, sep = " ")
  cur_df[shared_effects, beta := beta + i.beta, on = .(gene, SNP)] ### add shared effect onto specific effect only for where gene,SNP pair match otherwise leave row unchanged
  context = strsplit(basename(file), "\\.")[[1]][2]
  cur_df = cbind(tissue = context, cur_df)
}

#### single cell output
shared_sc_file = list.files("/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/CLUES_ASN.CLUES_EUR.OneK1K_Metasoft/random_effect2/", pattern = "eAssoc.*shared", full.names = T)
specific_sc_files = list.files("/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/CLUES_ASN.CLUES_EUR.OneK1K_Metasoft/random_effect2/", pattern = "eAssoc.*specific", full.names = T)
cxc_sc_files = list.files("/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/CLUES_ASN.CLUES_EUR.OneK1K_Metasoft/random_effect2/", pattern = "eAssoc.*mean_norm_res_exp.txt", full.names = T)
shared_effects = fread(shared_sc_file, sep=" ")
sc_combined_fastgxc = bind_rows(lapply(specific_sc_files, combine, shared_effects))
sc_combined_fastgxc_cxc = bind_rows(lapply(specific_sc_files, combine_fastgxc_cxc, shared_effects, cxc_sc_files))

fwrite(sc_combined_fastgxc, file = paste0(outdir, "eAssociations.scMeta.all_contexts.residualized.all.stats.txt"), sep = "\t", )
fwrite(sc_combined_fastgxc_cxc, file = paste0(outdir, "eAssociations.scMeta.fastgxc_cxc.all_contexts.residualized.all.stats.txt"), sep = "\t")

### GTEx output
shared_file = list.files("/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/TreeQTL/", pattern = "eAssoc.*normalized_and_residualized_expression.*homogeneous", full.names = T)
specific_files = list.files("/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/TreeQTL/", pattern = "eAssoc.*normalized_and_residualized_expression.*heterogeneous", full.names = T)
cxc_files = list.files("/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/TreeQTL/", pattern = "eAssoc.*normalized_and_residualized_expression.txt", full.names = T)
shared_effects = fread(shared_file, sep=" ")
combined_fastgxc = bind_rows(lapply(specific_files, combine, shared_effects))
combined_fastgxc_cxc = bind_rows(lapply(specific_files, combine_fastgxc_cxc, shared_effects, cxc_files))

fwrite(combined_fastgxc, file = paste0(outdir, "eAssociations.v8.EUR.all_tissues.residualized.all_stats.txt"), sep = "\t", )
fwrite(combined_fastgxc_cxc, file = paste0(outdir, "eAssociations.v8.EUR.fastgxc_cxc.all_tissues.residualized.all_stats.txt"), sep = "\t", )

### combine GTEx and single cell
fastgxc_cxc_gtex = fread("/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/Added_Results_CGreviews/eAssociations.v8.EUR.fastgxc_cxc.all_tissues.residualized.all_stats.txt", sep = "\t")
fastgxc_cxc_sc = fread("/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/Added_Results_CGreviews/eAssociations.scMeta.fastgxc_cxc.all_contexts.residualized.all.stats.txt", sep = "\t")
fastgxc_cxc_gtex$cohort = "GTEx"
fastgxc_cxc_sc$cohort = "PBMC"
combined = rbind(fastgxc_cxc_gtex, fastgxc_cxc_sc)

## save combined data frame to FastGxC input files
fwrite(combined, file = "/u/project/bballiu/bballiu/FastGxC/FastGxC_Manuscript_public/manuscript/Input_Files/Figure4_Correlation_Heatmap/FastGxC_added_CxC_effect_sizes.txt", sep = "\t")





