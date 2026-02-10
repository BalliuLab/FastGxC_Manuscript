#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Simulate data for different scenarios
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Libraries
library(lme4)
library(lmerTest)
library(mvtnorm)
library(meta)
library(reshape2)
library(dplyr)
library(data.table)


# Arguments
args=commandArgs(TRUE)
work_dir=args[1]
scratch_work_dir=paste0(work_dir, "/simulation_study_package/")
I=as.numeric(args[2])
N = as.numeric(args[3])
nT=as.numeric(args[4])
i=as.numeric(args[5])
 

data_dir=paste0(scratch_work_dir,"/simulated_data/")
source(file = paste0(work_dir,'/scripts/00_functions.R'))

print(paste("number of genes/iterations",I))
print(paste("Nr individuals",N))
print(paste("number of coontexts",nT))
print(paste("scenario number",i))

# Load simulation study parameters
Scenarios=read.table(file = paste0(data_dir,'/scenarios','_nC',nT,'.txt'), header = T, sep = '\t')

# Parameters
maf = as.numeric(Scenarios[i,"maf"]) # minor allele frequency of genotype
v_g = 2 * maf * (1-maf) # variance of genotype
v_e = 1 # variance of expression error
w_corr = as.numeric(Scenarios[i,"w_corr"]) # correlation of contexts within an individual
hsq = as.numeric(unlist(Scenarios[i,grepl(pattern = "hsq",x = colnames(Scenarios))]))  # heritability explained by genotype effect in each context
betas=sqrt((hsq*v_e)/((1-hsq)*v_g))  # genotype effect in each context
alpha=as.numeric(Scenarios[i,"alpha"])

### Simulate genotypes
geno_file_name=paste0(data_dir,'/genotype_data','_N', N, "_nG", I,'.txt')
genos = fread(file = geno_file_name,sep = '\t',header = T) %>% data.frame(row.names = 1)

### Simulate gene expression 
expression = simulate_expression_data(I=I, N=N, nT=nT, betas=betas, v_e = v_e, w_corr = w_corr, alpha = alpha, genos = t(genos))

exp_file_name=paste0(data_dir,'/scenario_',i,'_N', N,"_nC",nT, "_nG", I ,'_expression_data.txt')
fwrite(x=expression, file = exp_file_name, append = F, quote = F, sep = '\t')
