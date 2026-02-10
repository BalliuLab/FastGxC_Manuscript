#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger and Brunilda Balliu 
#%%%%%%%%%%%%%%% May 15th 2024, Los Angeles, California
#%%%%%%%%%%%%%%% Script to decompose expression and run PCA
#%%%%%%%%%%%%%%% For Hoffman
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# qrsh -l h_data=16G,h_rt=12:00:00,highp
# module load R/3.6.1

#%%%%%%%%%%%%%%% Arguments
args = commandArgs(trailingOnly = T)
work_dir = args[1]
cohort = args[2]
pseudobulk= args[3]
data_dir = args[4]
SNP_file_name = args[5]
functions_file = args[6]


#%%%%%%%%%%%%%%% R libraries and functions
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(magrittr))

source(functions_file)


#%%%%%%%%%%%%%%% Directories 

#%%%%%%%%%%%%%%% Read genotype matrix, SNPs in rows, samples in columns
genos=data.frame(fread(file = SNP_file_name, nrows = 5),row.names = 1, check.names = F)

#%%%%%%%%%%%%%%% Merge expression files across contexts
# Names of expression files
exp_files=list.files(path = data_dir, pattern = paste0(pseudobulk,"_norm_res_exp.txt"))

# Context names
#contexts=gsub(pattern =paste(data_dir,"\\.",cohort, ".", pseudobulk,"_norm_res_exp.txt",sep = "|"),replacement = "",x = exp_files)
contexts = unlist(lapply(strsplit(exp_files, "\\."), "[[", 1))
exp_files = paste0(data_dir, exp_files)

# Read expression matrix for Context t and merge with other Contexts 
exp_all=data.frame(fread(input = exp_files[1], header = T), check.names = F,stringsAsFactors = F)
colnames(exp_all)[-1] = paste(colnames(exp_all)[-1],contexts[1], sep = " - ")
print(paste("Finished merging context",1))

for(i in 2:length(exp_files)){
  
  # Read expression matrix for Context t
  exp_t=data.frame(fread(input = exp_files[i], header = T), check.names = F,stringsAsFactors = F)
  colnames(exp_t)[-1] = paste(colnames(exp_t)[-1],contexts[i], sep = " - ")
  
  # Merge with other Contexts
  exp_all = merge(x = exp_all, y = exp_t, by="gene_id", all = TRUE)
  
  print(paste("Finished merging context",i))
}

# Transpose merged expression matrix to have genes in the columns 
exp_all=t(data.frame(exp_all,row.names = 1,check.names = F))
print("Finished transposing merged file")

# Print number of genes and samples
sprintf("There are %s samples and %s genes. The max number of missing samples for a gene is  %s. The max number of missing genes for a sample is  %s.", nrow(exp_all), ncol(exp_all),max(colSums(is.na(exp_all))),max(rowSums(is.na(exp_all))))

#%%%%%%%%%%%%%%% Sample and context names
indv_contexts=matrix(unlist(strsplit(rownames(exp_all), split = " - ")), ncol = 2,byrow = T)
exp_all = data.frame(id=indv_contexts[,1],context=indv_contexts[,2], exp_all)


#%%%%%%%%%%%%%%% Decompose expression into homogeneous and heterogeneous context expression
print("Decomposing data")
shared_exp_file_name= paste0(data_dir, "AverageContext.", cohort, ".", pseudobulk,"_norm_res_exp.shared.txt")
spec_exp_file_name= paste0(data_dir, contexts,".", cohort, ".", pseudobulk,"_norm_res_exp.specific.txt")
decompose(expression=exp_all, shared_exp_file_name, spec_exp_file_name, genos)

