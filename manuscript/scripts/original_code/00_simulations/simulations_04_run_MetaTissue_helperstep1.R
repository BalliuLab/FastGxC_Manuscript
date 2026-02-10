

# setup ------------------------------------------------------------

# parameters
args = commandArgs(TRUE)
i = as.numeric(args[1])
work_dir = args[2]
missing_data = as.numeric(args[3])
N = as.numeric(args[4])
nT = as.numeric(args[5])
I = as.numeric(args[6])
method=args[7]


# # sample parameters for debugging
# i=20
# work_dir='/u/project/bballiu/bballiu/FastGxC' 
# missing_data=2
# N=698
# nT=8
# I = 1000
# method = "MetaTissue"


# translate missing data
if(missing_data==0) prefix4NA=""
if(missing_data==1) prefix4NA="_with05prcNAs" 
if(missing_data==2) prefix4NA="_with50prcNAs" 



# libraries & functions
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)

# helper fxns
source(file = paste0(work_dir,'/scripts/00_functions.R'))




# MetaTissue inputs & outputs --------------------------------------------------------


# geno & expr inputs
setwd(work_dir)
print(paste("started MetaTissue step 0 (data reformatting)"))

data_dir=paste0(work_dir, "/simulation_study/simulated_data/") 

# outputs

simnameout <- paste0('scenario_', i, "_N", N, "_nC", nT, "_nG", I, prefix4NA)

res_dir <- paste0(work_dir, "/simulation_study/simulation_results/")
dir_metatissue_fmt <- paste0(res_dir, "/metatissue_tmp/", simnameout)
dir.create(dir_metatissue_fmt, recursive = T, showWarnings = F)

if (file.exists( paste0(dir_metatissue_fmt, "/ind.txt") )) {
  warning(paste0("metatissue dir not empty. was sim data for ", simnameout,
                 " already converted to metatissue fmt? (overwriting)"))
}





# load simulated data (geno & expr) ------------------------------------------------------


simnameout <- paste0('scenario_', i, "_N", N, "_nC", nT, "_nG", I, prefix4NA)

filepath_expr <-
  paste0(data_dir, '/scenario_', i, "_N", N, "_nC", nT, "_nG", I, '_expression_data.txt')
filepath_geno <-
  paste0(data_dir, '/genotype_data_N', N, "_nG", I, '.txt')

expression <- data.frame(fread(file = filepath_expr, sep = "\t"), check.names = F)
rownames(expression) <- paste(expression$id, expression$Tissue, sep = " - ")

genos <- data.frame(fread(file = filepath_geno, sep = "\t"), row.names = 1)

# impose missing data structure
if(missing_data!=0){
  
  # Missing data design for OneK1K (mean missing % of 6.5%) and GTEx (mean missing % of 62%)
  if(missing_data==1) study_design=read.table(
    file = 'data/OneK1K/OneK1K_study_design.txt', header = T,row.names = 1)
  if(missing_data==2) study_design=read.table(
    file = 'data/GTEx_v8/GTEx_v8_study_design.txt', header = T)# Missing data design
  
  # Expands rows (individuals) and columns (contexts) to match numbers in simulations
  if(ncol(study_design)<nT) study_design <- duplicate_columns(study_design, target_cols = nT)
  if(nrow(study_design)<N) study_design <- duplicate_rows(study_design, target_rows = N)
  
  # Keep nr individuals and contexts to match numbers in simulations
  study_design=study_design[1:N,1:nT]
  
  rownames(study_design) = paste0("ind",1:N)
  colnames(study_design) = paste0("T",1:nT)
  study_design = reshape2::melt(
    data = data.table(study_design,keep.rownames = T)) %>%
    rename(id=rn,Tissue=variable, missing = value) %>%
    mutate(missing = ifelse(test = missing==0,yes = NA,no = 1))
  
  # will join by rownames ("Using rn as id variables" warning)
  rownames(study_design) = paste(study_design$id, study_design$Tissue, sep = " - ")
  expression_merged = merge(x = expression,y = study_design) %>% as.data.frame(check.names = F)
  rownames(expression_merged) = paste(expression_merged$id, expression_merged$Tissue, sep = " - ")
  expression = expression_merged[rownames(expression),]
  expression[,paste0("E",1:I)]  = expression[,paste0("E",1:I)] * expression$missing
  rownames(expression) = paste(expression$id, expression$Tissue, sep = " - ")
  expression = expression %>% select(-missing)
  
}






# format expr and geno files for metasoft ------------------------------------------------------
# - see http://genetics.cs.ucla.edu/metatissue/install_step1.html
# - left joins enforce the same order w/ genos

# donor x tissue indicator matrix (observed yes/no)
# left joins before enforce the same order w/ genos    
MT_tissueinfo <-
  pivot_wider(data = expression,
              id_cols = id, names_from = Tissue, values_from = E1) %>%
  left_join(data.frame(id = names(genos)),
            MT_tissueinfo, by = 'id') %>%
  mutate_at(.vars = 2:ncol(.), function(x) { if_else(is.na(x), 0, 1) }) %>%
  rename(`#TISSUE` = "id")

write.table(x = MT_tissueinfo,
            file = paste0(dir_metatissue_fmt, "/tissueinfo.txt"),
            quote = F, sep = "\t", row.names = F, col.names = T)



# expression matrix & genelist per context (_t)
# - note that expression mat in is (tissue, donor x genes)
#   final needs to be genes x donor per context
# - change Tissue as factor to ensure expected numerical order
#  want 1, 2, .., 9, 10, 11, ... vs string order 1, 2, ..., 8, 9
expression$Tissue <-
  factor(expression$Tissue, levels = paste0("T", 1:nT))
invisible(
  expression %>%
    group_by(Tissue) %>%
    group_split() %>%
    lapply(function(expr_t) {
      
      tissue_id <- first(expr_t$Tissue)
      
      expr_t <-
        left_join(data.frame(id = names(genos)), expr_t, by = 'id') %>%
        select(-Tissue, -id)
      filter_sample_notmissing <- !apply(is.na(expr_t), 1, all)
      expr_t <-
        expr_t[filter_sample_notmissing, ] %>% t
      
      fp_out <- paste0(dir_metatissue_fmt, "/", tissue_id, ".txt")        
      write.table(
        x = expr_t, file = fp_out,
        quote = F, sep = "\t", col.names = names(genos)[filter_sample_notmissing], row.names = F
      )
      
      return(NULL)
      
    })
)

# filepaths of expr_t matrices above
sapply(levels(expression$Tissue), 
       function(tissue_id) { 
         paste0(dir_metatissue_fmt, "/", tissue_id, ".txt") }) %>%
  writeLines(., paste0(dir_metatissue_fmt, "/genelist.txt"))

# gene metadata (locations)
write.table(
  x = data.frame(gene = paste0("E", 1:I), chr = "chr1", start = seq(1, 1000 * I, by = 1000)),
  file = paste0(dir_metatissue_fmt, "/probeinfo.txt"),
  append = F, quote = F, sep = "\t", row.names = F, col.names = F
)



# genotypes in EIGENSTRAT format ------------------------------------------------------

# snps in rows
write.table(
  x = sapply(1:I, function(g) paste(genos[g, ], collapse = "")),
  file = paste0(dir_metatissue_fmt, "/geno.eigenstrat"),
  append = F, quote = F, sep = "\t", row.names = F, col.names = F
)

# snp metadata (locations, alt / ref)
write.table(
  x = data.frame(snp = paste0("g", 1:I),
                 chr = 1, gen_pos = 0,
                 physpos = seq(1, 1000 * I, by = 1000), REF = "A", ALT = "C"),
  file = paste0(dir_metatissue_fmt, "/snp.txt"),
  append = F, quote = F, sep = "\t", row.names = F, col.names = F
)

# covariate matrix donor x variable 
write.table(
  x = data.frame(ind = paste0("ind", 1:N), gender = "U", status = "Case"),
  file = paste0(dir_metatissue_fmt, "/ind.txt"),
  append = F, quote = F, sep = "\t", row.names = F, col.names = F
)





print(paste0("finished MetaTissue step 0 (reformatting raw simulated data)."))
print(paste0("files in ", dir_metatissue_fmt))