#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu
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

# Parameters
args=commandArgs(TRUE)
work_dir=args[1]
I=as.numeric(args[2])
N = as.numeric(args[3])

data_dir=paste0(work_dir,"/simulation_study/simulated_data/")

source(file = paste0(work_dir,'/scripts/00_functions.R'))

print(paste("number of iters",I))
print(paste("Nr individuals",N))


# Load simulation study parameters
Scenarios=read.table(file = list.files(path=data_dir, pattern="scenarios_nC",  full.names=T)[1], header = T, sep = '\t')

### Simulate genotypes
maf = as.numeric(Scenarios[1,"maf"]) # minor allele frequency of genotype

genos = sapply(1:I, function(a){
  set.seed(30091987)
  rbinom(N, 2, maf)
})
colnames(genos) = paste0("g",1:I)
rownames(genos) = paste0("ind",1:N)

geno_file_name=paste0(data_dir,'/genotype_data','_N', N, "_nG", I,'.txt')

fwrite(x=data.table(t(genos),keep.rownames = T) %>% {setnames(., old = "rn", new = "id")[]}, file = geno_file_name, sep = '\t')

