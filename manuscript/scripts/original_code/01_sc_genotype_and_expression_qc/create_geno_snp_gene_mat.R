#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% May 12, 2024
#%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% script to create snploc, geneloc, and genotype matrices for Matrix eQTL
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suppressPackageStartupMessages(library(genio))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(biomaRt))
suppressPackageStartupMessages(library(dplyr))

args = commandArgs(trailingOnly = T)
plink_files = args[1] ## genotype files in plink format
cohort_name = args[2] ## OneK1K CLUES_ASN or CLUES_EUR
exp_files_dir = args[3] ## scratch directory with expression files
meQTL_outdir = args[4] ## matrix EQTL output directory
func = args[5] ## are we using mean or sum expression

#####################################################################
######### create genotype matrices per celltype and full
#####################################################################

plink_info = read_plink(plink_files)
genotypes = data.frame(plink_info$X, check.names = F)
snp_ids = rownames(genotypes)
snp_ids = gsub("^X", "", snp_ids)
snp_ids = gsub("\\.", ":", snp_ids)
snp_ids = gsub(">", ":", snp_ids)
snp_ids = gsub("<", ":", snp_ids)
genotypes = cbind(data.frame(SNP = snp_ids), genotypes)

#### write out non_subsetted genotype matrix
fwrite(genotypes, file = paste0(meQTL_outdir, cohort_name, "_5prcMAF_genotypes.txt"), sep = "\t", row.names = F, col.names = T, quote = F)

all_individual_files = list.files(exp_files_dir, pattern = "passed")
all_individual_files = all_individual_files[grepl(cohort_name, all_individual_files)]
#### write out subsetted genotype matrix per celltype
for(file in all_individual_files){
    cur_file = paste0(exp_files_dir, file)
    celltype = gsub(paste0(cohort_name, "_"), "", file) 
    celltype = gsub("_passed_exp_PCA.txt", "", celltype) 
    
    ## read in individuals who passed epxression QC
    individuals = fread(cur_file, sep = "\t", header = F, data.table = F)$V2
    ## subset genotype matrix by individuals 
    cur_genotypes = genotypes[, c("SNP", individuals)]

    #fwrite(cur_genotypes, file = paste0(meQTL_outdir, cohort_name, "_", celltype,"_5prcMAF_genotypes.txt"), sep = "\t", row.names = F, col.names = T, quote = F)
}

print("finished creating genotypes matrices")

#####################################################################
######### create snploc matrix
#####################################################################

all_bim = plink_info$bim
#snp_ids = gsub("<", ":", all_bim$id)
#snp_ids = gsub(">", ":", snp_ids)
snpsloc = data.frame(snp = snp_ids, chr = paste0("chr", all_bim$chr), pos = all_bim$pos)

snpsloc = snpsloc %>% distinct(snp, .keep_all=T)

fwrite(snpsloc, file = paste0(meQTL_outdir, cohort_name, "_5prcMAF_snpsloc.txt"), sep = "\t", row.names = F, col.names = T, quote = F)

print("finished creating snpsloc matrix")

#####################################################################
######### create geneloc matrix
#####################################################################

all_exp_files = list.files(exp_files_dir, pattern = paste0(func, "_invNorm"))
all_exp_files = all_exp_files[grepl(cohort_name, all_exp_files)]

all_genes = data.frame()
for(file in all_exp_files){
    cur_file = paste0(exp_files_dir, file)
    celltype = gsub(paste0(cohort_name, "_"), "", file) 
    celltype = gsub(paste0("_", func, "_invNorm.txt"), "", celltype) 

    cur_exp = fread(cur_file, sep = "\t", header = T, data.table = F)
    cur_genes = cur_exp$gene_id

    ensembl = useMart(biomart="ENSEMBL_MART_ENSEMBL", host="grch37.ensembl.org", path="/biomart/martservice",
                    dataset="hsapiens_gene_ensembl")
    gene_table <- getBM(attributes=c('ensembl_gene_id', 'chromosome_name', 'start_position','end_position'),
                                    values = cur_genes, mart=ensembl)
    gene_table = gene_table[gene_table$ensembl_gene_id %in% cur_genes,]
    rownames(gene_table) = gene_table$ensembl_gene_id
    gene_table = gene_table[cur_genes,]
    names(gene_table) = c("geneid", "chr", "s1", "s2")
    #fwrite(gene_table, file = paste0(meQTL_outdir, cohort_name, "_", celltype, "_geneloc.txt"), sep = "\t", row.names = F, col.names = T, quote = F)

    all_genes = rbind(all_genes, gene_table)
}
## also write out one large geneloc matrix
all_genes = all_genes %>% distinct()
all_genes$chr = paste0("chr", all_genes$chr)
fwrite(all_genes, file = paste0(meQTL_outdir, cohort_name, "_geneloc.txt"), sep = "\t", row.names = F, col.names = T, quote = F)

print("finished creating geneloc matrices")

