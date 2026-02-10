##############################################################
############# script to run Metasoft for FastGxC results
############# Lena Krockenberger
############# March 23, 2023
##############################################################

library(data.table)
library(dplyr)
args = commandArgs(trailingOnly = T)
input_dir= args[1]
exp_scale= as.numeric(args[2])
cohorts = args[3]
outdir = args[4]
functions_file = args[5]

source(functions_file)

## assumed that cohorts are passed in with . delimiter
cohorts = unlist(unname(strsplit(cohorts, "\\.")))
## context by context
if(exp_scale == 1) exp_suffix = paste0(".mean_norm_res_exp.all_pairs.txt")
## fastgxc
if(exp_scale == 2) exp_suffix = paste0(".mean_norm_res_exp.specific.all_pairs.txt")
if(exp_scale == 2) exp_suffix2 = paste0(".mean_norm_res_exp.shared.all_pairs.txt")

if(exp_scale %in% 1){
  all_files = list.files(input_dir, pattern = exp_suffix)
}else if (exp_scale %in% c(2,3)) {
  all_files = list.files(input_dir, pattern = exp_suffix)
  all_files = c(all_files, list.files(input_dir, pattern = exp_suffix2))
}

keep_all_files = c()
for(cohort in cohorts){
  cur_files = all_files[grepl(cohort, all_files)]
  keep_all_files = c(keep_all_files, cur_files)
}
all_files = keep_all_files
contexts_vec = unique(sapply(strsplit(all_files, "\\."), "[[", 1))
all_files = paste0(input_dir, all_files)

get_metasoft_files <- function(all_files, context, outdir, exp_suffix){
  out_file = paste0(outdir, context, exp_suffix)
  cur_files = all_files[grepl(context, all_files)]

  parse_sumstats_across_contexts(
  filelist_qtlsumstat = cur_files,
  fileprefix_out = out_file, 
  filter_tests_mincontexts = 1, # tested in at least one context, not sure what we're using for this paper
 # MatrixQTL geneloc. need if by chromosome
  run_by_chrom = F, #chroms_to_loop = paste0("chr", 1:22), # useful for high mem
  extract_value = "betase", index_QTL_vs_genesnp = T)
}

for(i in 1:length(contexts_vec)){
  print(paste0("computing file for context ", contexts_vec[i]))
  get_metasoft_files(all_files, contexts_vec[i], outdir, exp_suffix)
}
