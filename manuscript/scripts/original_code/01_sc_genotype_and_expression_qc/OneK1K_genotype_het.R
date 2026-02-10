library(dplyr)
library(data.table)
library(stringr)

#### calculate sd for heterozygosity
args = commandArgs(trailingOnly = T)
outdir = args[1]
het_file = args[2]
outfile_prefix=args[3]
sd_threshold=as.numeric(args[4])

heterozygosity = fread(het_file, sep = " ", header = T)

het_mean = mean(heterozygosity$F)
het_sd = sd(heterozygosity$F)
lower_cutoff = het_mean - sd_threshold*(het_sd)
upper_cutoff = het_mean + sd_threshold*(het_sd)
filtered_het = heterozygosity[heterozygosity$F >= lower_cutoff & heterozygosity$F <= upper_cutoff,]
fwrite(filtered_het[,c("FID", "IID")], file = paste0(outdir, outfile_prefix, "_het_samples.txt"), sep = "\t", col.names = F)
