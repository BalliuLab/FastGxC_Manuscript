#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%% April 2nd 2020
#%%%%%%%%%%%%%%% Script to residualise expression for covariates
#%%%%%%%%%%%%%%% For Hoffman
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

args=commandArgs(trailingOnly = T) 
i=as.numeric(args[1])
cohort=args[2]
work_dir=args[3]
pseudobulk=args[4]
input.dir = args[5] #input.dir = paste0(work_dir, "data/", cohort, "/expression_matrices/")
data.dir = args[6] #data.dir = paste0(work_dir, "data/", cohort, "/MatrixEQTL_input/")
cov.dir = args[7] #cov.dir = paste0(work_dir, "data/", cohort, "/expression_covariates/")

# R libraries and functions
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(reshape2))

# Location of data files.

exp_files=list.files(path = input.dir, pattern = paste0(".", pseudobulk, "_normalized_expression.txt"))#,full.names = T)
context = strsplit(exp_files, "\\.")[[i]][1]
exp_files = paste0(input.dir, exp_files)

# Read expression and covariate matrix for tissue t
exp_t=t(data.frame(fread(input = exp_files[i], header = T), row.names = 1, check.names = F,stringsAsFactors = F))
cov_t=t(data.frame(fread(input = paste0(cov.dir,context,"_covariates.txt"), header = T), row.names = 1, check.names = F,stringsAsFactors = F))

# Keep only joint samples
joint_samples=intersect(rownames(cov_t),rownames(exp_t))
exp_t_noNAs=exp_t[joint_samples,]
cov_t=cov_t[joint_samples,]

# Residualize expression
exp_t_res=residuals(lm(formula = exp_t_noNAs~., data = data.frame(cov_t,check.names = F)))
exp_t[rownames(exp_t_res),colnames(exp_t_res)]=exp_t_res

# Save residualized expression
exp_t=data.table(t(exp_t),keep.rownames = T)
colnames(exp_t)[1]="gene_id"
fwrite(x = exp_t, file = paste0(data.dir, context, ".", cohort, ".", pseudobulk, "_norm_res_exp.txt"), append = F, quote = F, sep = '\t', row.names = F, col.names = T)



