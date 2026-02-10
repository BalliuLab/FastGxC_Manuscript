#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% March 19, 2024
#%%%%%%%%%%%%%%% Preprocessing for CLUES expression data
#%%%%%%%%%%%%%%% Convert CLUES anndata to seurat and calculate umap
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Seurat))
suppressPackageStartupMessages(library(SeuratDisk))
suppressPackageStartupMessages(library(anndata))
suppressPackageStartupMessages(library(reticulate))
suppressPackageStartupMessages(library(sceasy))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(nbHelpers))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(biomaRt))
suppressPackageStartupMessages(library(genio))
suppressPackageStartupMessages(library(Matrix.utils))

args = commandArgs(trailingOnly = T)
clues_file = args[1] ## clues raw anndata file
output_file = args[2] ## clues raw seurat output file
fam = args[3]
run_UMAP = as.numeric(args[4])

# read clues into anndata and convert to Seurat object
clues_data = read_h5ad(clues_file)
raw_counts = clues_data$raw$X
dimnames(raw_counts) = list(clues_data$raw$obs_names,clues_data$raw$var$gene_ids)
clues_seurat = CreateSeuratObject(transpose_dgRMatrix(raw_counts))
clues_seurat@meta.data = clues_data$obs

merged_clues_fam = fread(fam, sep = " ", header = F, data.table = F)
total_ids = merged_clues_fam$V2
clues_seurat = subset(x = clues_seurat, cells = rownames(clues_seurat@meta.data[clues_seurat@meta.data$ind_cov %in% total_ids,]))

# ## run umap
if(run_UMAP){
  clues_seurat <- FindVariableFeatures(clues_seurat)
  clues_seurat <- ScaleData(clues_seurat)
  clues_seurat <- RunPCA(clues_seurat, features = VariableFeatures(object = clues_seurat))
  clues_seurat <- FindNeighbors(clues_seurat, dims = 1:10)
  clues_seurat <- FindClusters(clues_seurat, resolution = 0.5)
  clues_seurat <- RunUMAP(clues_seurat, dims = 1:10)
}

saveRDS(clues_seurat, file = output_file)




