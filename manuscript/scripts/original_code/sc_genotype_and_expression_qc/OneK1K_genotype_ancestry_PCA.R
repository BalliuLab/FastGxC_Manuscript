## pca of onek1k merged with 1000G
library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)

rm(list = ls())
args = commandArgs(trailingOnly = T)
outdir = args[1]
merged_Onek1k_1K = args[2]
outfile_prefix = args[3]
sd_threshold=as.numeric(args[4])

print(outdir)
print(merged_Onek1k_1K)

onek1k_merged=fread(merged_Onek1k_1K, sep = " ", header = F, data.table = F)
names(onek1k_merged) = c("IID", "ancestry", paste0("PC", seq(1,7)))

merged_eur_only = onek1k_merged[onek1k_merged$ancestry == "EUR",]
merged_onek1k_only = onek1k_merged[onek1k_merged$ancestry == "OneK1K",]
mean_pc1 = mean(merged_eur_only$PC1)
mean_pc2 = mean(merged_eur_only$PC2)
sd_pc1 = sd(merged_eur_only$PC1)
sd_pc2 = sd(merged_eur_only$PC2)
upper_pc1 = mean_pc1 + sd_threshold*(sd_pc1)
lower_pc1 = mean_pc1 - sd_threshold*(sd_pc1)
upper_pc2 = mean_pc2 + sd_threshold*(sd_pc2)
lower_pc2 = mean_pc2 - sd_threshold*(sd_pc2)

pass_pc_check = merged_onek1k_only[merged_onek1k_only$PC1 >= lower_pc1 & merged_onek1k_only$PC1 <= upper_pc1, ] 
pass_pc_check = pass_pc_check[pass_pc_check$PC2 >= lower_pc2 & pass_pc_check$PC2 <= upper_pc2, ]

#final_individuals = data.frame(str_split_fixed(pass_pc_check$IID, "_", 2))
final_individuals = data.frame(col1 = 0 , col2 = pass_pc_check$IID)
PCA_thresholds = data.frame(names = c("upper_pc1", "lower_pc1", "upper_pc2", "lower_pc2"), values = c(upper_pc1, lower_pc1, upper_pc2, lower_pc2))

#final_individuals = data.frame(0, pass_pc_check$IID)
fwrite(final_individuals, paste0(outdir, outfile_prefix, "_PCA_samples.txt"), sep = "\t", col.names = F)
fwrite(PCA_thresholds, paste0(outdir, outfile_prefix, "_PCA_thresholds.txt"), sep = "\t", col.names = F)