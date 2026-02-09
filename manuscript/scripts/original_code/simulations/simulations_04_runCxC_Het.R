#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu
#%%%%%%%%%%%%%%% December 8th 2022
#%%%%%%%%%%%%%%% CxC-Het analysis of simulated data for different scenarios
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Libraries
library(data.table)
library(dplyr)
library(tidyr)
library(meta)

# Arguments
args=commandArgs(TRUE)
i=as.numeric(args[1])
work_dir=args[2]
missing_data=as.numeric(args[3])
N=as.numeric(args[4])
nT=as.numeric(args[5])
I=as.numeric(args[6])
method=args[7]


# i=1
# work_dir="/u/project/bballiu/bballiu/FastGxC/"
# missing_data=0
# N=698
# nT=49
# I = 1e03
# method = "CxC_Het"

print(paste("Started", method, "analysis of simulated data"))


# Parameters and other settings
data_dir=paste0(work_dir,"/simulation_study/simulated_data/")
res_dir=paste0(work_dir,"/simulation_study/simulation_results/")

dir.create(res_dir)
dir.create(paste0(res_dir, method))

if(missing_data==0) prefix4NA=NULL
if(missing_data==1) prefix4NA="_with05prcNAs" 
if(missing_data==2) prefix4NA="_with50prcNAs" 

source(file = paste0(work_dir,'/scripts/00_functions.R'))

tissues = paste0("T",1:nT) 

eQTL_res_1 = fread(input = paste0(res_dir, "MatrixEQTL_CxC",'/scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA, "_",tissues[1],'_expression.txt')) 
eQTL_res_betas = eQTL_res_1 %>% select(gene, SNP, `beta`)
eQTL_res_se = eQTL_res_1 %>% mutate(se=`beta`/`t-stat`) %>% select(gene, SNP, `se`)
colnames(eQTL_res_betas)[3] = colnames(eQTL_res_se)[3] = tissues[1]

for(j in 2:nT){
  # Output file name
  output_file_name_cis = paste0(res_dir, "MatrixEQTL_CxC",'/scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA, "_",tissues[j],'_expression.txt');
  eQTL_res_j=fread(input = output_file_name_cis)
  
  eQTL_res_betas_j = eQTL_res_j %>% select(gene, SNP, `beta`)
  eQTL_res_se_j = eQTL_res_j %>% mutate(se=`beta`/`t-stat`) %>% select(gene, SNP, `se`)
  
  eQTL_res_betas = merge(x = eQTL_res_betas, y = eQTL_res_betas_j)
  eQTL_res_se = merge(x = eQTL_res_se, y = eQTL_res_se_j)
  
  
  colnames(eQTL_res_betas)[j+2] = colnames(eQTL_res_se)[j+2] = tissues[j]
}  


all_meta_res = bind_rows(lapply(1:nrow(eQTL_res_betas), function(iter) { 
  meta_res=try(metagen(TE = unlist(eQTL_res_betas[iter,-c(1,2)]), seTE = unlist(eQTL_res_se[iter,-c(1,2)]), comb.fixed = TRUE, comb.random = TRUE, prediction=TRUE, sm="SMD"), silent=T)
  if(all(class(meta_res)!="try-error")) data.frame(eQTL_res_betas[iter,1:2],Q=meta_res$Q,pval.Q=meta_res$pval.Q)
}))


fwrite(x = all_meta_res, file = paste0(res_dir, method,'/',method, "_res_",'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,'.txt'),quote = F,sep = "\t")


print(paste("Finished", method, prefix4NA, "analysis of simulated data"))
