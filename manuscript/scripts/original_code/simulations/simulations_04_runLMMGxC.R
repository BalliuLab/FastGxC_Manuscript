#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu
#%%%%%%%%%%%%%%% December 8th 2022
#%%%%%%%%%%%%%%% LMM GxC analysis of simulated data for different scenarios
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(lme4)
library(lmerTest)
library(mvtnorm)
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)
library(MatrixEQTL)
library(TreeQTL)
library(mppa)

# Parameters
args=commandArgs(TRUE)
i=as.numeric(args[1])
work_dir=args[2]
missing_data=as.numeric(args[3])
N=as.numeric(args[4])
nT=as.numeric(args[5])
I=as.numeric(args[6])
method=args[7]


# i=20
# work_dir=getwd()
# missing_data=1
# N=698
# nT=5
# I = 1000
# method = "LMM_GxC"

print(paste0("Started ",method," analysis of simulated data"))

data_dir=paste0(work_dir,"/simulation_study/simulated_data/")
res_dir=paste0(work_dir,"/simulation_study/simulation_results/")

dir.create(res_dir)
dir.create(paste0(res_dir, method))

if(missing_data==0) prefix=NULL
if(missing_data==1) prefix="_with05prcNAs" 
if(missing_data==2) prefix="_with50prcNAs" 

setwd(work_dir)

source(file = paste0(work_dir,'/scripts/00_functions.R'))

# Parameters
tissues = paste0("T",1:nT)  

# Run analysis
exp_file_name=paste0(data_dir,'scenario_',i,'_N',N,"_nC",nT, "_nG", I ,'_expression_data.txt')

expression = data.frame(fread(file = exp_file_name, sep = '\t'),check.names = F)
rownames(expression) = paste(expression$id, expression$Tissue, sep = " - ")

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
  study_design = reshape2::melt(data = data.table(study_design,keep.rownames = T)) %>% rename(id=rn,Tissue=variable, missing = value) %>% mutate(missing = ifelse(test = missing==0,yes = NA,no = 1))
  
  rownames(study_design) = paste(study_design$id, study_design$Tissue, sep = " - ")
  expression_merged = merge(x = expression,y = study_design) %>% as.data.frame(check.names = F)
  rownames(expression_merged) = paste(expression_merged$id, expression_merged$Tissue, sep = " - ")
  expression = expression_merged[rownames(expression),]
  expression[,paste0("E",1:I)]  = expression[,paste0("E",1:I)] * expression$missing
  rownames(expression) = paste(expression$id, expression$Tissue, sep = " - ")
  expression = expression %>% select(-missing) 

}
expression = expression %>% group_by(Tissue) %>% mutate(across(.cols = c(paste0("E",1:I)), .fns = ~ . - mean(., na.rm = TRUE)))
expression = data.frame(expression)
rownames(expression) = paste(expression$id, expression$Tissue, sep = " - ")

geno_file_name=paste0(data_dir,'genotype_data','_N', N, "_nG", I,'.txt')
genos = data.frame(fread(file = geno_file_name, sep = '\t'),row.names = 1)

betas = std_errors = p_values = data.frame(matrix(data = NA, nrow = I, ncol = 2+(2*(nT-1)), dimnames = list(NULL,c("Intercept",paste0("T",2:nT),"G",paste0("T",2:nT,":G")))), check.names = F)
p_values$"LRT_TxG" = NA
p_values$"LRT_G" = NA
p_values$"r2" = NA

for(j in 1:I){
  print(j)
  data_mat =expression[,c(1,2,2+j)] 
  data_mat=merge(x = data_mat, y = data.table(t(genos[j,paste0("ind",1:N)]),keep.rownames = T) %>% rename(id = rn), by = "id")
  colnames(data_mat)[3:4] =  c("E","G")
  data_mat$Tissue = factor(x = data_mat$Tissue,levels = paste0("T",1:nT))
  
  if(method=='LMM_GxC'){
    mylm00=lmer(formula = E ~ (1|id) + Tissue, data = data_mat, REML = F)
    mylm0=lmer(formula = E ~ (1|id) + Tissue + G, data = data_mat, REML = F)
    mylm1=lmer(formula = E~ (1|id) + Tissue + G + Tissue:G, data = data_mat, REML = F)
    betas[j,] = coef(summary(mylm1))[,"Estimate"]
    std_errors[j,] = coef(summary(mylm1))[,"Std. Error"]
    p_values[j,1:(2+(2*(nT-1)))] = coef(summary(mylm1))[,"Pr(>|t|)"]
    # p-value for significance of sp-eQTL
    p_values[j,"LRT_TxG"] = anova(mylm0,mylm1, test="LRT")[2,"Pr(>Chisq)"]
    # p-value for significance of eQTL 
    p_values[j,"LRT_G"] = anova(mylm00,mylm1, test="LRT")[2,"Pr(>Chisq)"]
    # r2 for proportion of variance explained by TxC
    p_values[j,"r2"] = anova(mylm0,mylm1, test="LRT")[2, "Chisq"]/anova(mylm0,mylm1, test="LRT")[2, "deviance"]
    # r2 for proportion of variance explained by any genetic effect 
    p_values[j,"r2_global"] = anova(mylm00,mylm1, test="LRT")[2, "Chisq"]/anova(mylm0,mylm1, test="LRT")[2, "deviance"]
  }
  
  if(method=='LM_GxC'){
    mylm00=lm(formula = E ~ Tissue, data = data_mat)
    mylm0=lm(formula = E ~ Tissue + G, data = data_mat)
    mylm1=lm(formula = E~ Tissue + G + Tissue:G, data = data_mat)
    betas[j,] = coef(summary(mylm1))[,"Estimate"]
    std_errors[j,] = coef(summary(mylm1))[,"Std. Error"]
    p_values[j,1:(2+(2*(nT-1)))] = coef(summary(mylm1))[,"Pr(>|t|)"]
    # p-value for significance of sp-eQTL
    p_values[j,"LRT_TxG"] = anova(mylm0,mylm1, test="LRT")[2,"Pr(>Chi)"]
    # p-value for significance of eQTL
    p_values[j,"LRT_G"] = anova(mylm00,mylm1, test="LRT")[2,"Pr(>Chi)"]
    
  }
}


fwrite(x = p_values, file = paste0(res_dir, method,'/',method, "_res_",'scenario_',i,'_N',N, "_nC", nT, "_nG", I, prefix, '.txt'), quote = F,sep = "\t")
fwrite(x = betas, file = paste0(res_dir, method,'/',method, "betas_res_",'scenario_',i,'_N',N, "_nC", nT, "_nG", I, prefix, '.txt'), quote = F,sep = "\t")
fwrite(x = std_errors, file = paste0(res_dir, method,'/',method, "std_errors_res_",'scenario_',i,'_N',N, "_nC", nT, "_nG", I, prefix, '.txt'), quote = F,sep = "\t")

print(paste0("Finished ",method," analysis of simulated data"))
