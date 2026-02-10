#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%% April 2nd 2020
#%%%%%%%%%%%%%%% Script to residualize GTEx expression for covariates
#%%%%%%%%%%%%%%% For Hoffman
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

args=commandArgs(TRUE) 
i=as.numeric(args[1])

# R libraries and functions
library(data.table)
library(reshape2)


# Location of data files.
data.dir = '/u/home/b/bballiu/FastGxE/data/GTEx_v8/MatrixEQTL_input/'
cov.dir = '/u/home/b/bballiu/FastGxE/data/GTEx_v8/expression_covariates/'

exp_files=list.files(path = data.dir, pattern = ".v8.EUR.normalized_expression.txt",full.names = T)
tissue_names=gsub(pattern =paste(data.dir,"/",".v8.EUR.normalized_expression.txt",sep = "|"),replacement = "",x = exp_files)

# Read expression and covariate matrix for tissue t
exp_t=t(data.frame(fread(input = exp_files[i], header = T), row.names = 1, check.names = F,stringsAsFactors = F))
cov_t=t(data.frame(fread(input = paste0(cov.dir,tissue_names[i],".v8.EUR.covariates.txt"), header = T), row.names = 1, check.names = F,stringsAsFactors = F))

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
fwrite(x = exp_t, file = gsub(x=exp_files[i],pattern = ".v8.EUR.normalized_expression.txt",replacement = ".v8.EUR.normalized_and_residualized_expression.txt"), append = F, quote = F, sep = '\t', row.names = F, col.names = T)



