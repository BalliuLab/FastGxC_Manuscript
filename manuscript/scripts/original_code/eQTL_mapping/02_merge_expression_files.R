#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%% April 7th 2020, Los Angeles, California
#%%%%%%%%%%%%%%% Script to merge GTEx expression files across tissues
#%%%%%%%%%%%%%%% For Hoffman
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Change "normalized_expression" to "normalized_and_residualized" to merge the files residualized for covariates

# qrsh -l h_data=16G,h_rt=12:00:00,highp
# module load R/3.6.1

# R libraries and functions
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(reshape2))

args = commandArgs(trailingOnly = T)
work_dir = args[1] # Location of data files
cohort = args[2]


data.dir = paste0(work_dir, "data/", cohort, "/MatrixEQTL_input/")

# Names of GTEx expression files
exp_files=list.files(path = data.dir, pattern = ".mean_normalized_and_residualized_expression.txt",full.names = T)

# Tissue names
#tissue_names=gsub(pattern =paste(data.dir,"/",".v8.EUR.normalized_expression.txt",sep = "|"),replacement = "",x = exp_files)
context_names=gsub(pattern =paste(data.dir,"/","\\.",cohort,".mean_normalized_and_residualized_expression.txt",sep = "|"),replacement = "",x = exp_files)

# Read expression matrix for tissue t and merge with other tissues 
print(paste("Finished merging context",1))
exp_all=data.frame(fread(input = exp_files[1], header = T), check.names = F,stringsAsFactors = F)
colnames(exp_all)[-1] = paste(colnames(exp_all)[-1],context_names[1], sep = " - ")
for(i in 2:length(exp_files)){

  # Read expression matrix for tissue t
  exp_t=data.frame(fread(input = exp_files[i], header = T), check.names = F,stringsAsFactors = F)
  colnames(exp_t)[-1] = paste(colnames(exp_t)[-1],context_names[i], sep = " - ")
  
  # Merge with other tissues
  exp_all = merge(x = exp_all, y = exp_t, by="gene_id", all = TRUE)
  
  print(paste("Finished merging context",i))
}

# Transpose merged expression matrix to have genes in the columns 
print("Finished transposing merged file")
exp_all=t(data.frame(exp_all,row.names = 1,check.names = F))

# Save merged expression file
fwrite(x = data.table(exp_all,keep.rownames = T),
       file = paste0(data.dir, cohort, ".all_contexts.mean_normalized_and_residualized_expression_merged.txt"), 
       append = F, quote = F, sep = '\t', row.names = F, col.names = T)
print("Finished writing merged file")
