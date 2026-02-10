#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% March 19, 2024
#%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Calculate UMAP for OneK1K expression data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Seurat))

args = commandArgs(trailingOnly = T)
onek1k_file = args[1] ## onek1k raw seurat object
output_file = args[2] ## onek1k raw seurat output file
fam = args[3] ## fam file to subset expression 
run_UMAP = as.numeric(args[4]) ## flag to run UMAP or not

# read seurat object
onek1k_data = readRDS(onek1k_file)

onek1k_fam = fread(fam, sep = "\t", header = F, data.table = F)
total_ids = onek1k_fam$V2
onek1k_seurat = subset(x = onek1k_data, cells = rownames(onek1k_data@meta.data[onek1k_data@meta.data$donor_id %in% total_ids,]))

## run umap
if(run_UMAP){
  onek1k_seurat <- FindVariableFeatures(onek1k_seurat)
  onek1k_seurat <- ScaleData(onek1k_seurat)
  onek1k_seurat <- RunPCA(onek1k_seurat, features = VariableFeatures(object = onek1k_seurat))
  onek1k_seurat <- FindNeighbors(onek1k_seurat, dims = 1:10)
  onek1k_seurat <- FindClusters(onek1k_seurat, resolution = 0.5)
  onek1k_seurat <- RunUMAP(onek1k_seurat, dims = 1:10)
}

saveRDS(onek1k_seurat, file = output_file)




