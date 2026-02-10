## pca of onek1k merged with 1000G
library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)

rm(list = ls())
args = commandArgs(trailingOnly = T)
outdir = args[1]
pca_res = args[2]
outfile_prefix = args[3]
sd_threshold=as.numeric(args[4])
ancestry=args[5]

pca_res_df=fread(pca_res, sep = "\t", header = F, data.table = F)
names(pca_res_df) = c("IID", "ancestry", paste0("PC", seq(1,7)))

eur_samples = pca_res_df[pca_res_df$ancestry == ancestry,]
mean_pc1 = mean(eur_samples$PC1)
mean_pc2 = mean(eur_samples$PC2)
sd_pc1 = sd(eur_samples$PC1)
sd_pc2 = sd(eur_samples$PC2)
upper_pc1 = mean_pc1 + sd_threshold*(sd_pc1)
lower_pc1 = mean_pc1 - sd_threshold*(sd_pc1)
upper_pc2 = mean_pc2 + sd_threshold*(sd_pc2)
lower_pc2 = mean_pc2 - sd_threshold*(sd_pc2)

pass_pc_check = pca_res_df[pca_res_df$PC1 >= lower_pc1 & pca_res_df$PC1 <= upper_pc1, ] 
pass_pc_check = pass_pc_check[pass_pc_check$PC2 >= lower_pc2 & pass_pc_check$PC2 <= upper_pc2, ]

#final_individuals = data.frame(str_split_fixed(pass_pc_check$IID, "_", 2))
final_individuals = data.frame(col1 = 0 , col2 = pass_pc_check$IID)
PCA_thresholds = data.frame(names = c("upper_pc1", "lower_pc1", "upper_pc2", "lower_pc2"), values = c(upper_pc1, lower_pc1, upper_pc2, lower_pc2))

#final_individuals = data.frame(0, pass_pc_check$IID)
fwrite(final_individuals, paste0(outdir, outfile_prefix, "_PCA_samples.txt"), sep = "\t", col.names = F)
fwrite(PCA_thresholds, paste0(outdir, outfile_prefix, "_PCA_thresholds.txt"), sep = "\t", col.names = F)