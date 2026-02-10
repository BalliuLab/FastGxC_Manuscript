#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% April 11, 2024
#%%%%%%%%%%%%%%% QC pipeline functions 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(Seurat))
suppressPackageStartupMessages(library(biomaRt))
suppressPackageStartupMessages(library(Matrix.utils))
suppressPackageStartupMessages(library(edgeR))
suppressPackageStartupMessages(library(GenomicFeatures))
suppressPackageStartupMessages(library(RNOmni))

compute_pseudobulk <- function(seurat_obj, all_celltypes, func, pseudobulk_df){

    print(func)
    pseudobulk_matrix = seurat_obj@assays$RNA@counts[, rownames(pseudobulk_df)]
    pseudobulk_sample <- aggregate.Matrix(t(pseudobulk_matrix), 
                                    groupings = pseudobulk_df, fun = func)
    pseudobulk_sample = t(pseudobulk_sample)

    output_pseudobulk_list = list()

    for(cell in all_celltypes){
        cur_cols = grepl(paste0("_",cell), colnames(pseudobulk_sample))
        cur_pseudobulk = pseudobulk_sample[,cur_cols]
        output_pseudobulk_list[[cell]] = cur_pseudobulk
    }
    return(output_pseudobulk_list)

}
###############################################################
### expression qc functions
###############################################################

## function takes in raw counts expression matrix and normalizes/filters genes
##      normalizes by correcting for library size, computing cpm, and tpm
## input parameters:
##      pseudobulk_sample - a dataframe with columns as samples or individuals and rows as genes
##      exonic.gene.sizes - a list of genes and their lengths (needed for library size correction)
##      samp_threshold - the required percentage of samples each gene must be expressed for
## outputs:
##      1. returns a matrix of genes in rows and samples in columns of normalized expression
filter_exp = function(pseudobulk_sample, exonic.gene.sizes, samp_threshold){

    # get gene lengths of genes in expression matrix
    gene_lengths = which(!is.na(exonic.gene.sizes[rownames(pseudobulk_sample)]))
    nonNA_genes = names(gene_lengths)
    pseudobulk_sample = pseudobulk_sample[nonNA_genes,]
    cur_pseudobulk = DGEList(pseudobulk_sample,
                             genes=data.frame(genes = rownames(pseudobulk_sample), length=gene_lengths))
    cur_pseudobulk = calcNormFactors(cur_pseudobulk)
    cur_pseudobulk_cpm = cpm(cur_pseudobulk)
    cur_pseudobulk_rpkm = rpkm(cur_pseudobulk)
    cur_pseudobulk_tpm = t( t(cur_pseudobulk_rpkm) / colSums(cur_pseudobulk_rpkm) ) * 1e6

    ### get genes with mean pseudobulk expression > 0 in at least 10% of samples
    gene_samp_perc = rowSums(cur_pseudobulk$counts > 0)/ncol(cur_pseudobulk$counts)
    keep = names(gene_samp_perc[gene_samp_perc >= samp_threshold])
    cur_pseudobulk_tpm<-cur_pseudobulk_tpm[keep,]

    return(cur_pseudobulk_tpm)
}

## function to perform inverse normal transform
inverse_norm_trans = function(filtered_exp){
    inv_norm_transf = t(apply(filtered_exp, 1, RNOmni::RankNorm))
    return(inv_norm_transf)
}

###############################################################
### functions for regressing out covariates
###############################################################

### run PCA 
#get_exp_pcs <- function(data){
#    pcs = data.frame(prcomp(t(data))$x)
#    return(pcs)
#}

###############################################################
### function for expression PC outlier correction
###############################################################

## function takes in expression PCs and determines if there are any outlier individuals 
## input parameters:
##      df - a dataframe with columns labeled "celltype", "cohort", "donor_id", and 6 PC columns "PC1" .... "PC6"
##          celltype is whatever tissue, context, or celltype for each row (can be the same for each row)
##          cohort is the cohort name and can be the same for every row
##          donor_id contains individual or sample names
##          PC1 to PC6 are the first 6 expression PCs
##      cell - the current cell type, tissue, or context of interest (must be included in the df table under celltype column)
##      sd_threshodl - the number of standard deviations away from the mean of each PC (this is used to determine outlier individuals)
##      outdir - the directory to save a file with the list of individuals who are not ouliers
## outputs:
##      1. prints a plot of PC1 vs PC2 with error thresholds
##      2. returns a vector of individuals who passed outlier detection
##      3. writes out a file of individuals who passed outlier detection
get_mean_sd <- function(df, cell, sd_threshold, outdir){
    cohort = unique(df$cohort)
    cur_df = df[df$celltype == cell,]
    mean_pc1 = mean(cur_df$PC1)
    mean_pc2 = mean(cur_df$PC2)
    sd_pc1 = sd(cur_df$PC1)
    sd_pc2 = sd(cur_df$PC2)
    upper_pc1 = mean_pc1 + (sd_threshold*sd_pc1)
    lower_pc1 = mean_pc1 - (sd_threshold*sd_pc1)
    upper_pc2 = mean_pc2 + (sd_threshold*sd_pc2)
    lower_pc2 = mean_pc2 - (sd_threshold*sd_pc2)

    plot = ggplot(cur_df, aes(PC1, PC2, color = cohort))+ geom_point() + theme_bw()+
        geom_vline(xintercept=upper_pc1)+geom_vline(xintercept=lower_pc1)+
        geom_hline(yintercept=upper_pc2)+geom_hline(yintercept=lower_pc2) + ggtitle(paste0(cohort, "-", cell))
    print(plot)
    
    passed_individuals = cur_df[cur_df$PC1 <= upper_pc1 & cur_df$PC1 >= lower_pc1,]
    passed_individuals = passed_individuals[passed_individuals$PC2 >= lower_pc2 & passed_individuals$PC2 <= upper_pc2,]
    print(dim(cur_df))
    print(dim(passed_individuals))
    output_inds = data.frame(fam_id = 0, donor_id = passed_individuals$donor_id)
    fwrite(output_inds, file = paste0(outdir, cohort, "_", cell, "_passed_exp_PCA.txt"), sep = "\t", row.names = F, col.names = F, quote = F)
    return(passed_individuals$donor_id)
}






