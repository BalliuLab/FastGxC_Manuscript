############################################################
## Lena Krockenberger - January 25, 2025
## Create study design for OneK1K
############################################################


library(data.table)
library(dplyr)

data_dir = "/u/project/bballiu/bballiu/FastGxC/data/OneK1K/MatrixEQTL_input/"
exp_files = list.files(data_dir, pattern = "exp.txt")
outdir = "/u/scratch/l/lkrocken/OneK1K_study_design.txt"

onek1k_study_design = data.frame()
names(onek1k_study_design) = c("ids")
names_vector = c()
for(i in 1:length(exp_files)){
    cur_file = fread(paste0(data_dir,exp_files[i]), sep = "\t", data.table = F)
    ids = names(cur_file)[-1]
    cur_context = strsplit(exp_files[i], "\\.")[[1]][1]
    cur_study_design = data.frame(ids = ids, context = 1.0)
    names(cur_study_design) = c("ids", cur_context)
    
    if(i == 1){
        onek1k_study_design = cur_study_design
        names_vector = c("ids", cur_context)
        names(onek1k_study_design) = names_vector
    }else{
        names_vector = c(names_vector, cur_context)
        onek1k_study_design = full_join(onek1k_study_design, cur_study_design, by = "ids")
        names(onek1k_study_design) = names_vector
    }
}
onek1k_study_design[is.na(onek1k_study_design)] <- 0.0
fwrite(onek1k_study_design, outdir, sep = "\t", quote = F)