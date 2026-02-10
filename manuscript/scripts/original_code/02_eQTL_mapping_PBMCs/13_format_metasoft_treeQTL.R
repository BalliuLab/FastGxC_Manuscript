#################################################################
######### R script to format metasoft output into TreeQTL format
######### Lena Krockenberger
######### April 3, 2024
#################################################################

library(data.table)
library(stringi)
library(dplyr)
library(tidyr)

args = commandArgs(trailingOnly = T)
workdir = args[1]
index = as.numeric(args[2])

fixed = "fixed_effect"
random2 = "random_effect2"
#workdir = "/u/home/l/lkrocken/project-bballiu/FastGxC/metasoft_files/"
outdir_fixed = paste0(workdir, fixed, "/")
outdir_random2 = paste0(workdir, random2, "/")
dir.create(outdir_fixed, showWarnings = F)
dir.create(outdir_random2,  showWarnings = F)


files = list.files(workdir, pattern = "output")

read_file = function(file, context, outdir_fixed, outdir_random2, file_suffix){
    fixed_col = 3
    random2_col = 9

    fixed_beta_col = 4
    random_beta_col = 7

    fixed_se_col = 5
    random_se_col = 8

    df = fread(file, sep = "\t", header = T, fill = T, data.table = F)
    gene_snp= stri_split_fixed(str = df[,1], pattern = ":", n = 2)
    gene= unlist(lapply(gene_snp, '[[', 1))
    snp =  unlist(lapply(gene_snp, '[[', 2))

    ## create data frames
    fixed = data.frame(SNP = snp, gene = gene, beta = df[,fixed_beta_col], 'SE' = df[,fixed_se_col], 'p-value' = df[,fixed_col], FDR = 0)
    fixed = fixed %>% drop_na(p.value)
    fixed <- fixed[order(fixed$p.value),]
    ntests_fixed = fixed %>% group_by(gene) %>% mutate(n  = n()) %>% distinct(gene, n)
    random2 = data.frame(SNP = snp, gene = gene, beta = df[,random_beta_col], 'SE' = df[,random_se_col], 'p-value' = df[,random2_col], FDR = 0)
    random2 = random2 %>% drop_na(p.value)
    random2 <- random2[order(random2$p.value),]
    ntests_random2 = random2 %>% group_by(gene) %>% mutate(n  = n()) %>% distinct(gene, n)

    ### write out data frames
    fwrite(fixed, paste0(outdir_fixed, context, file_suffix), sep = "\t", quote = F)
    fwrite(random2, paste0(outdir_random2, context, file_suffix), sep = "\t", quote = F)

    fwrite(ntests_fixed, paste0(outdir_fixed, "n_SNPs_per_gene_", context, ".txt"), sep = "\t", quote = F, col.names = F)
    fwrite(ntests_random2, paste0(outdir_random2, "n_SNPs_per_gene_", context, ".txt"), sep = "\t", quote = F, col.names = F)

}


file = files[index]
print(file)
context = strsplit(file, "\\.")[[1]][2]
file_suffix = gsub(paste0("output.", context), "", file)
file_suffix = gsub("_betase.tsv", "", file_suffix)
if(grepl("Average", context)){
    file_suffix = gsub("specific", "shared", file_suffix)
}

read_file(paste0(workdir, file), context, outdir_fixed, outdir_random2, file_suffix)
print(paste0("finished file"))


