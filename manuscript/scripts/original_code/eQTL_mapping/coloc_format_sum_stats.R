###################################################################
############# script to format metasoft output for colocalization
############# Lena Krockenberger
############# June 12, 2024
##################################################################

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

args = commandArgs(trailingOnly = T)
workdir = "/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/CLUES_ASN.CLUES_EUR.OneK1K_Metasoft/random_effect2/" #args[1] ## directory containing all input files
ref_file = "/u/scratch/l/lkrocken/qc_outputs/OneK1K/genotype_qc/reference_snps_all_cohort.txt" #args[2]

## only need to run this once to make reference SNP file 
if(0){
    ## reference files
    onek1k = "/u/scratch/l/lkrocken/qc_outputs/OneK1K/genotype_qc/OneK1K_hg19_imputed_r20.8_MAF_0.05prc.bim"
    clues_asn = "/u/scratch/l/lkrocken/qc_outputs/CLUES/genotype_qc/CLUES_ASN_hg19.bim"
    clues_eur = "/u/scratch/l/lkrocken/qc_outputs/CLUES/genotype_qc/CLUES_EUR_hg19.bim"
    references = c(onek1k, clues_asn, clues_eur)

    ref_df = rbindlist(lapply(references, fread, sep = "\t", data.table = F))
    ref_df = unique(ref_df[,c(1,4,5,6)])
    names(ref_df) = c("chr", "pos", "alt", "ref")
    ref_df$snp_id = paste0(ref_df$chr, ":", ref_df$pos)
    fwrite(ref_df, file = ref_file, sep = "\t", quote = F)
}

reformat = function(ref_df, cur_file){
    print(paste0("reformatting file: ", cur_file))
    cur_df = fread(cur_file, sep = "\t", data.table = F)
    matched_indices =  match(cur_df$SNP, ref_df$snp_id)
    cur_ref = ref_df[matched_indices,]
    cur_df = cbind(cur_df, cur_ref[,c("chr", "pos", "alt", "ref")])
    #Chr, pos, EA, NEA, beta, se, pval, gene
    cur_df = cur_df[,c("chr", "pos", "alt", "ref", "beta", "SE", "p.value", "gene")]
    return(cur_df)
}

ref_df = fread(ref_file, sep = "\t", data.table = F)
## for all of the input files, add alt and ref alleles
input_files = list.files(workdir, pattern = "all_pairs.txt", full.names = F)
for(file in input_files){
    formatted_df = reformat(ref_df, paste0(workdir, file))
    fwrite(formatted_df, file = paste0(workdir, "colocalization/coloc.", file), sep = "\t", quote = F)
}

