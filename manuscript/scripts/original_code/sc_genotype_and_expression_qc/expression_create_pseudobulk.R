
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% April 25, 2024
#%%%%%%%%%%%%%%% Compute pseudobulk for each cell type, filter gene expression, and inverse normalize
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(Seurat))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(biomaRt))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Matrix.utils))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(edgeR))
suppressPackageStartupMessages(library(GenomicFeatures))
suppressPackageStartupMessages(library(RNOmni))


args = commandArgs(trailingOnly = T)
input_file = args[1] ## seurat obj file
cell_type_file = args[2] ## cell types csv file
outdir = args[3] ## output directory
cohort = args[4] ## cohort
func = args[5] ## whether to mean or sum pseudobulk
functions_file = args[6]
gtf_file = args[7] # gtf file for gene lengths
num_genes_exp = as.numeric(args[8]) # number of contexts that gene needs to be expressed in
samp_threshold= as.numeric(args[9]) # minimum percentage of samples expressed for gene
sd_threshold = as.numeric(args[10]) # number of standard deviations away from mean for exp PC outliers
geno_file_plink = args[11] # plink input name with genotype data
meQTL_dir = args[12] # output matrixEQTL directory
seed = as.numeric(args[13]) # seed to run PCA for QTL
pca_bash_file = args[14]

source(functions_file)

## create matrix EQTL directories if they do not exist:
meQTL_exp_outdir = paste0(meQTL_dir, cohort, "/expression_matrices/")
print(meQTL_exp_outdir)
dir.create(meQTL_exp_outdir, recursive = T, showWarnings = F)
meQTL_cov_outdir = paste0(meQTL_dir, cohort, "/expression_covariates/")
print(meQTL_cov_outdir)
dir.create(meQTL_cov_outdir, recursive = T, showWarnings = F)

clues_columns = c("ind_cov", "Sex", "Age", "cg_cov", "SLE_status") ## columns to subset clues metadata
onek1k_columns = c("donor_id", "sex", "age", "cell_type") ## columns to subset onek1k metadata
seurat_obj = readRDS(input_file)
cell_type_conversion = fread(cell_type_file, sep = ",", data.table = F)

cur_metadata = seurat_obj@meta.data
if(grepl("CLUES", cohort)){
    cur_metadata = cur_metadata[,clues_columns]
    names(cur_metadata) = c("donor_id", "sex", "age", "cell_type", "SLE_status")
    metadata_cols = c("sex", "age","SLE_status")
}else if (grepl("OneK1K", cohort)) {
    cur_metadata = cur_metadata[, onek1k_columns]
    names(cur_metadata) = c("donor_id", "sex", "age", "cell_type")
    metadata_cols = c("sex", "age")
}
cur_metadata$cell_barcode = rownames(seurat_obj@meta.data)


# create metadata table
merged_meta = merge(cur_metadata, cell_type_conversion, by.x = "cell_type", by.y = "Old labels")
merged_meta = merged_meta[merged_meta$Drop == "no",]
all_celltypes = unique(merged_meta$final_celltypes)
rownames(merged_meta) = merged_meta$cell_barcode
filtered = subset(x = seurat_obj, cells = rownames(merged_meta))
filtered@assays$RNA@counts = filtered@assays$RNA@data
filtered@meta.data = merged_meta

### ADD FLAG FOR THIS
#saveRDS(filtered, paste0(outdir, cohort, "_hg19_annotatedCells.rds"))

merged_meta_final = merged_meta %>% distinct(donor_id, .keep_all = T)
#fwrite(merged_meta_final, file = paste0(outdir, cohort, "_metadata.txt"), sep = "\t", row.names = F, col.names = T, quote = F)
rownames(merged_meta_final) = merged_meta_final$donor_id
merged_meta_final$donor_id = NULL
merged_meta_final = merged_meta_final[, metadata_cols]
## convert sex to binary
merged_meta_final = merged_meta_final %>% mutate(sex = if_else(sex == "Female", 0, 1)) 
## if one of the CLUES cohorts, set SLE status to binary as well
if(grepl("CLUES", cohort)){
    ## convert SLE Status to binary
    merged_meta_final = merged_meta_final %>% mutate(SLE_status = if_else(SLE_status == "Healthy", 0, 1))
}


## get donor ids and cell type labels and compute pseudobulk for each cell type
pseudobulk_df = filtered@meta.data[, c("donor_id", "final_celltypes")]
pseudobulk_list=compute_pseudobulk(filtered, all_celltypes, "sum", pseudobulk_df)
if(func == "mean"){
  for(cell in all_celltypes){
    cur_pseudobulk = pseudobulk_list[[cell]]
    for(i in 1:ncol(cur_pseudobulk)){
      cur_id = colnames(cur_pseudobulk)[i]
      id = gsub(paste0("_", cell), "", cur_id)
      num_cells = nrow(pseudobulk_df[pseudobulk_df$donor_id == id & pseudobulk_df$final_celltypes == cell,])
      cur_pseudobulk[,cur_id] = cur_pseudobulk[,cur_id]/num_cells
    }
    pseudobulk_list[[cell]] = cur_pseudobulk
    print(paste0("finished mean pseudobulk for cell: ", cell))
  }
}


## get gene lengths
# First, import the GTF-file 
txdb <- makeTxDbFromGFF(gtf_file,format="gtf")
# then collect the exons per gene id
exons.list.per.gene <- exonsBy(txdb,by="gene")
# then for each gene, reduce all the exons to a set of non overlapping exons, calculate their lengths (widths) and sum then
exonic.gene.sizes <- sum(width(reduce(exons.list.per.gene)))

## filter genes and inverse normalize
## Final expression files should be individuals in columns and genes in rows
total_gene_df = data.frame(matrix(nrow = 0, ncol = 2))
inv_norm_list = list()
for(i in 1:length(all_celltypes)){
  cur_pseudobulk=pseudobulk_list[[all_celltypes[i]]]
  filtered_exp = filter_exp(cur_pseudobulk, exonic.gene.sizes, samp_threshold)
  inv_norm_exp = inverse_norm_trans(filtered_exp)
  total_gene_df = rbind(total_gene_df, data.frame(genes = rownames(inv_norm_exp), celltype = all_celltypes[i]))
  inv_norm_exp = data.frame(inv_norm_exp, check.names = F)
  inv_norm_exp = cbind(gene_id = rownames(inv_norm_exp), inv_norm_exp)
  inv_norm_list[[all_celltypes[i]]] = inv_norm_exp
  
}

# get list of genes that are expressed in more than 3 celltypes
final_genes = total_gene_df %>% group_by(genes) %>% mutate(n_tiss = n())
final_genes = final_genes[final_genes$n_tiss >= num_genes_exp,]

final_pca_df = data.frame()
final_individual_df = data.frame()
for (cell in all_celltypes){

    ## subset inverse normalized expression by genes that pass thresholds
    cur_pseudobulk = inv_norm_list[[cell]]
    cur_pseudobulk = cur_pseudobulk[cur_pseudobulk$gene_id %in% final_genes$genes,]
    
    # run PCA and save one file with first 6 PCs, cohort, cell type
    pseudobulk_pca = cur_pseudobulk[,-1]
    pseudobulk_pca = t(pseudobulk_pca)
    pseudobulk_pca = prcomp(pseudobulk_pca)

    cur_pca = pseudobulk_pca$x[,1:6]
    donor_ids = gsub(paste0("_", cell), "", rownames(cur_pca))
    cur_pca = cbind(data.frame(donor_id = donor_ids), cur_pca)
    cur_pca = cbind(data.frame(cohort = cohort), cur_pca)
    cur_pca = cbind(data.frame(celltype = cell), cur_pca)
    final_pca_df = rbind(final_pca_df, cur_pca)

    # subset by people who are outliers based on expression PCs
    keep_individuals = get_mean_sd(cur_pca, cell, sd_threshold, outdir)
    names(cur_pseudobulk) = gsub(paste0("_", cell), "", names(cur_pseudobulk))
    cur_pseudobulk = cur_pseudobulk[, c("gene_id", keep_individuals)]
    print(paste0(cell, " ", dim(cur_pseudobulk)))
    #fwrite(cur_pseudobulk, file = paste0(outdir, cohort, "_", cell, "_", func, "_invNorm.txt"), sep = "\t", row.names = F, col.names = T, quote = F)
    fwrite(cur_pseudobulk, file = paste0(meQTL_exp_outdir, cell, ".", cohort, ".",func, "_normalized_expression.txt"), sep = "\t", row.names = F, col.names = T, quote = F)

    #####################################################
    ## add to individual dataframe 
    cur_celltype_donor = pseudobulk_df %>% filter(donor_id %in% keep_individuals) %>% filter(final_celltypes %in% cell)
    cur_celltype_donor$cell = rownames(cur_celltype_donor)
    final_individual_df = rbind(final_individual_df, cur_celltype_donor)
    
    ## create individual file
    individual_df = data.frame(fam = 0, id = keep_individuals)
    individual_file = paste0(outdir, cohort, "_", cell, "_", func, "_passed_exp_geno_qc.txt")
    fwrite(individual_df, individual_file, sep = "\t", col.names = F, row.names = F, quote = F)
    geno_file = paste0(geno_file_plink, "_", cell, "_pca.eigenvec")

    ### run bash script to thin SNPs and compute genotype PCs
    command = paste0("sh ", pca_bash_file, " ", geno_file_plink, " ", cell, " ", individual_file)
    cat(paste0("running command: ", command, "\n"))
    system(command)
    #####################################################

    ## read in genotype PCs
    eigenvectors = fread(geno_file, data.table = F)
    rownames(eigenvectors) = eigenvectors$IID
    ## keep 6 genotype PCs and make known covariates metadata table for all individuals 
    eigenvectors = eigenvectors[,c(paste0("PC", seq(1,6)))]
    names(eigenvectors) = c(paste0("genotype_PC", seq(1,6)))
    known_covariates = cbind(eigenvectors[,c(1:6)], merged_meta_final[keep_individuals,]) 

    ## calculate number of hidden covariates 
    pca_qtl_df = t(cur_pseudobulk[,-1])
    qtl_pcs = prcomp(pca_qtl_df)
    set.seed(seed)
    num_exp_pcs = PCAForQTL::runBE(pca_qtl_df,B=20,alpha=0.05,mc.cores = 1)$numOfPCsChosen
    hidden_covariates = data.frame(qtl_pcs$x[,c(1:num_exp_pcs)])
    names(hidden_covariates) = c(paste0("exp_PC", seq(1, num_exp_pcs)))
    cell_covariate_metadata = cbind(known_covariates,hidden_covariates)
    final_cell_covariate_metadata = data.frame(transpose(cell_covariate_metadata))
    names(final_cell_covariate_metadata) = rownames(cell_covariate_metadata)
    final_cell_covariate_metadata = cbind(ID = names(cell_covariate_metadata), final_cell_covariate_metadata)
    fwrite(final_cell_covariate_metadata, file = paste0(outdir, cohort, "_", cell, "_", func, "_covariates.txt"), sep = "\t", row.names = F, col.names = T, quote = F)
    fwrite(final_cell_covariate_metadata, file = paste0(meQTL_cov_outdir, cell, "_covariates.txt"), sep = "\t", row.names = F, col.names = T, quote = F)
    print(paste0("finished writing covariates and normalized expression for cell: ", cell))
}

fwrite(final_pca_df, file = paste0(outdir, cohort, "_exp_PCs.txt"), sep = "\t", row.names = F, col.names = T, quote = F)
fwrite(final_individual_df, file = paste0(outdir, cohort, "_donor_celltype_cell.txt"))


