#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Lena Krockenberger
#%%%%%%%%%%%%%%% March 14, 2025
#%%%%%%%%%%%%%%% Metasoft/Metatissue analysis of simulated data for different scenarios
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Libraries
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)


# Arguments
args=commandArgs(TRUE)
i=as.numeric(args[1])
work_dir=args[2]
missing_data=as.numeric(args[3])
N=as.numeric(args[4])
nT=as.numeric(args[5])
I=as.numeric(args[6])
method=args[7]
metasoft_dir=args[8]

# i=11
# work_dir="/u/project/bballiu/bballiu/FastGxC/"
# missing_data=1
# N=698
# nT=49
# I = 1e03
# method = "Metasoft_CxC"
# metasoft_dir = "/u/project/bballiu/bballiu/FastGxC/simulation_study/external_software/METASOFT/"

print(paste("Started", method, "analysis of simulated data"))

data_dir=paste0(work_dir,"/simulation_study/simulated_data/")
res_dir=paste0(work_dir,"/simulation_study/simulation_results/")

dir.create(res_dir)
dir.create(paste0(res_dir, "format_", method))
dir.create(paste0(res_dir, method))


if(missing_data==0) prefix4NA=NULL
if(missing_data==1) prefix4NA="_with05prcNAs" 
if(missing_data==2) prefix4NA="_with50prcNAs" 

setwd(work_dir)

# Parameters
tissues = paste0("T",1:nT) 
if(method == "Metasoft_CxC"){
  filedir_prefix = "CxC"
}else if (method == "Metasoft_FastGxC") {
  filedir_prefix = "FastGxC"
}else{
  print("Incorrect method specification. ")
}

## Format files for Metasoft
cur_file = fread(paste0(res_dir, filedir_prefix, "/", filedir_prefix, "_res_scenario_", i, "_N", N, "_nC", nT, "_nG", I, prefix4NA, ".txt"), sep = "\t", data.table = F)
cur_file_tmp = cur_file %>% mutate(SNP = paste0(cur_file$SNP,":", cur_file$gene))
columns = if(method == "Metasoft_FastGxC") c(paste0("T", 1:nT), "shared") else c(paste0("T", 1:nT))
SE_cols = if(method == "Metasoft_FastGxC") c(paste0("SE_T", 1:nT), "SE_shared") else c(paste0("SE_T", 1:nT))
df_with_se <- cur_file_tmp %>%
  mutate(
    !!!setNames(
      lapply(columns, function(i) {
        cur_file_tmp[[paste0("beta_", i)]] / cur_file_tmp[[paste0("t-stat_", i)]]
      }),
      SE_cols
    )
  )
### if method is FastGxC, then sign match specific to shared beta
if(method == "Metasoft_FastGxC"){
  cols_to_adjust = c(paste0("beta_", columns))
  df_with_se = df_with_se %>%
    mutate(across(all_of(cols_to_adjust), ~ ifelse(sign(.) != sign(beta_shared), -., .)))
}


cols_to_select <- as.vector(rbind(
  paste0("beta_", columns),
  paste0("SE_", columns)
))
df_with_se = df_with_se %>% select(c("SNP", cols_to_select))

outfile = paste0(res_dir, "format_", method, "/", filedir_prefix, "_res_scenario_", i, "_N", N, "_nC", nT, "_nG", I, prefix4NA, ".txt")
fwrite(df_with_se, file = outfile, col.names = F, sep = "\t")

## run metasoft
filepath_metasoft=paste0(metasoft_dir, "/Metasoft.jar")
filepath_metasoft_pvals=paste0(metasoft_dir, "/HanEskinPvalueTable.txt")
logfile_path = paste0(work_dir, "/logfiles/sim.", method, '.',i,'.',N,".",nT, ".txt")
metasoft_output_file = paste0(paste0(res_dir, method, '/',method, "_res_",'scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix4NA,'.txt'))
command = paste0("java -jar ", filepath_metasoft, " -input " , outfile, " -pvalue_table ", filepath_metasoft_pvals, " -output ", metasoft_output_file, " -log ", logfile_path, " -mvalue -mvalue_p_thres 1 -mvalue_method mcmc")
print(paste0("running command: ", command))
system(command)

print(paste("Finished", method, "analysis of simulated data"))
