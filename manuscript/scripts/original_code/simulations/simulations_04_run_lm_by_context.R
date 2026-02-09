#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% January 29, 2025
#%%%%%%%%%%%%%%% FastGxC / CxC analysis of simulated data for different scenarios
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## gene-snpE104 8 - 0.0225807152
## scenario 15 - betaT49 - -0.0129920855 (causing bias in that)

# Libraries
library(lme4)
library(lmerTest)
library(mvtnorm)
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)

library(TreeQTL)
library(mppa)


# Arguments
args=commandArgs(TRUE)
i=as.numeric(args[1])
work_dir=args[2]
missing_data=as.numeric(args[3])
N=as.numeric(args[4])
nT=as.numeric(args[5])
I=as.numeric(args[6])
method=args[7]

# i=11
# work_dir="/u/project/bballiu/bballiu/FastGxC/"
# missing_data=1
# N=698
# nT=49
# I = 1e03
# method = "CxC"

print(paste("Started", method, "analysis of simulated data"))

data_dir=paste0(work_dir,"/simulation_study/simulated_data/")
res_dir=paste0(work_dir,"/simulation_study/simulation_results/")

dir.create(res_dir)
dir.create(paste0(res_dir, "linearModel_",method))
dir.create(paste0(res_dir, method))


if(missing_data==0) prefix4NA=NULL
if(missing_data==1) prefix4NA="_with05prcNAs" 
if(missing_data==2) prefix4NA="_with50prcNAs" 

if(method=="FastGxC") { 
  prefix4specific="_specific"
} else {
  prefix4specific=NULL
}



setwd(work_dir)

source(file = paste0(work_dir,'/scripts/00_functions.R'))

# Parameters
tissues = paste0("T",1:nT) 

# Load expression and genotype files
geno_file_name=paste0(data_dir,'genotype_data','_N', N, "_nG", I,'.txt')
genos = data.frame(fread(file = geno_file_name, sep = '\t'),row.names = 1)

exp_file_name=paste0(data_dir,'scenario_',i,'_N',N,'_nC',nT, '_nG', I ,'_expression_data.txt')
expression = data.frame(fread(file = exp_file_name, sep = '\t'),check.names = F) %>% rename(context=Tissue)
rownames(expression) = paste(expression$id, expression$context, sep = " - ")

# Create data with missing values
if(missing_data!=0){

  # Missing data design for OneK1K (mean missing % of 6.5%) and GTEx (mean missing % of 62%) 
  if(missing_data==1) study_design=read.table(file = 'data/OneK1K/OneK1K_study_design.txt', header = T,row.names = 1) 
  if(missing_data==2) study_design=read.table(file = 'data/GTEx_v8/GTEx_v8_study_design.txt', header = T)# Missing data design
  
  # Expands rows (individuals) and columns (contexts) to match numbers in simulations
  if(ncol(study_design)<nT) study_design <- duplicate_columns(study_design, target_cols = nT)
  if(nrow(study_design)<N) study_design <- duplicate_rows(study_design, target_rows = N)
  
  # Keep nr individuals and contexts to match numbers in simulations
  study_design=study_design[1:N,1:nT] 
  
  rownames(study_design) = paste0("ind",1:N)
  colnames(study_design) = paste0("T",1:nT)
  
  study_design = reshape2::melt(data = data.table(study_design,keep.rownames = T)) %>% 
    rename(id=rn,context=variable, missing = value) %>% mutate(missing = ifelse(test = missing==0,yes = NA,no = 1))
  
  rownames(study_design) = paste(study_design$id, study_design$context, sep = " - ")
  expression_merged = merge(x = expression,y = study_design) %>% as.data.frame(check.names = F) 
  rownames(expression_merged) = paste(expression_merged$id, expression_merged$context, sep = " - ")
  expression = expression_merged[rownames(expression),]
  expression[,paste0("E",1:I)]  = expression[,paste0("E",1:I)] * expression$missing
  rownames(expression) = paste(expression$id, expression$context, sep = " - ")
  expression = expression %>% select(-missing) 
}
expression = expression %>% group_by(context) %>% mutate(across(.cols = c(paste0("E",1:I)), .fns = ~ . - mean(., na.rm = TRUE)))
expression = data.frame(expression)
rownames(expression) = paste(expression$id, expression$context, sep = " - ")

### check that individuals in expression match individual in genotypes
## ADD LINE HERE

if(method=="FastGxC") {
  shared_exp_file_name=paste0(data_dir,'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,'_shared_expression.txt')
  spec_exp_file_name=paste0(data_dir,'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA, "_",tissues,'_specific_expression.txt')
  decompose(expression = expression, shared_exp_file_name=shared_exp_file_name, spec_exp_file_name=spec_exp_file_name, genos=genos) # Decompose and save gene expression
}

# Run lm for each context
for(j in 1:nT){
  # Output file name
  output_file_name_cis = paste0(res_dir, "linearModel_", method,'/scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA, "_",tissues[j],prefix4specific,'_expression.txt');

  if(method=="CxC")  {
    expression_j = expression %>% filter(context==paste0("T",j))  %>% select(-context) %>% data.frame(row.names = 1) %>% as.matrix()
  }
  
  if(method=="FastGxC") {
    expression_file_name=paste0(data_dir,'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA, "_",tissues[j],'_specific_expression.txt')
    expression_j = fread(expression_file_name, sep = "\t", data.table = F)
    rownames(expression_j) = expression_j$geneID
    expression_j = expression_j %>% select(-geneID) 
    expression_j = t(expression_j)
  }
  
  betas = bind_rows(lapply(seq(1,I), function(n){
    model = lm(expression_j[,paste0("E", n)]~t(genos)[,paste0("g",n)])
    df = data.frame(SNP= paste0("g",n), gene = paste0("E",n), beta =  summary(model)$coefficients[2,1], "p-value" = summary(model)$coefficients[2,4])
  }))

  fwrite(betas, file = output_file_name_cis, sep = "\t")
  
    #df = data.frame(tissue = paste0("beta_", nT), lm_estimates = median(betas$beta))
}

# Multiple testing correction
eQTL_res = data.frame(gene=colnames(expression[,-c(1,2)]), SNP=rownames(genos))
for(j in 1:nT){
  # Output file name
  output_file_name_cis = paste0(res_dir, "linearModel_",method,'/scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA, "_",tissues[j],prefix4specific,'_expression.txt');
  y = fread(input = output_file_name_cis) %>% select(gene, SNP, beta,`p.value`)
  colnames(y) = c("gene", "SNP", "beta", "p-value")
  colnames(y)[3:4] = paste(colnames(y)[3:4], tissues[j],sep = "_")
  eQTL_res = merge(x = eQTL_res, y = y)
}  
eQTL_res$specific_p=apply(eQTL_res[,grepl(pattern = "p-value",x = colnames(eQTL_res))],MARGIN = 1, simes.test)

# Run MatrixEQTL for shared expression 
if(method=="FastGxC"){
  
  # Output file name
  output_file_name_cis = paste0(res_dir, "linearModel_FastGxC",'/scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,'_shared_expression.txt');

  expression_file_name=paste0(data_dir,'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,'_shared_expression.txt')
  expression_j = fread(expression_file_name, sep = "\t", data.table = F)
  rownames(expression_j) = expression_j$geneID
  expression_j = expression_j %>% select(-geneID) 
  expression_j = t(expression_j)
  
  betas = bind_rows(lapply(seq(1,I), function(n){
    model = lm(expression_j[,paste0("E", n)]~t(genos)[,paste0("g",n)])
    df = data.frame(SNP= paste0("g",n), gene = paste0("E",n), beta =  summary(model)$coefficients[2,1], "p-value" = summary(model)$coefficients[2,4])
  }))

  fwrite(betas, file = output_file_name_cis, sep = "\t")

shared_eQTL_res = fread(input = output_file_name_cis) %>% select(gene, SNP, `beta`, `p.value`)
colnames(shared_eQTL_res) = c("gene", "SNP", "beta", "p-value")

colnames(shared_eQTL_res)[3:4] = paste(colnames(shared_eQTL_res)[3:4],'shared',sep = "_")


eQTL_res = merge(x = eQTL_res, y = shared_eQTL_res)


unlink(paste0(data_dir,'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,"_T", 1:nT,'_specific_expression.txt'))
unlink(paste0(data_dir,'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,'_shared_expression.txt'))
unlink(paste0(res_dir,'linearModel_FastGxC/scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,"_T", 1:nT,'_specific_expression.txt'))
unlink(paste0(res_dir,'linearModel_FastGxC/scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,'_shared_expression.txt'))

}

# Save all results
fwrite(x = eQTL_res, file = paste0(res_dir, method,'/',method, "_res_",'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,'.txt'),quote = F,sep = "\t")

print(paste("Finished", method, "analysis of simulated data"))


