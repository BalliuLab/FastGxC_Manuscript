##############################################################
############# script to run Metasoft for FastGxC GTEx results
############# Lena Krockenberger
############# March 18, 2025
##############################################################

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
args = commandArgs(trailingOnly = T)
input_dir= args[1] 
exp_scale= as.numeric(args[2]) 
outdir = args[3] 
functions_file = args[4]
geneloc_file = args[5]

#input_dir = "/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/MatrixEQTL/"
#exp_scale = 1
#outdir = "/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping/Metasoft_GTEx/"
#functions_file = "/u/project/bballiu/bballiu/FastGxC/scripts/eQTL_mapping/00_functions.R"
#geneloc_file = "/u/project/bballiu/bballiu/FastGxC/data/GTEx_v8/MatrixEQTL_input/GTEx_v8_geneloc.txt"

source(functions_file)

## assumed that cohorts are passed in with . delimiter
if(exp_scale == 1) exp_suffix = paste0(".v8.EUR.normalized_and_residualized_expression")
out_file = paste0("GTEx", exp_suffix, "_Metasoft_format.txt")

if(exp_scale %in% 1){
  all_files = list.files(input_dir, pattern = exp_suffix)
  specific = all_files[grepl("heterogeneous", all_files)]
  shared= all_files[grepl("homogeneous", all_files)]
  all_files = c(specific, shared)
}else {
  print("Error, incorrect scale input")
}
contexts_vec = sapply(strsplit(all_files, "\\."), "[[", 1)

parse_sumstats_across_contexts(
  filelist_qtlsumstat = all_files,
  fileprefix_out = out_file, 
  filter_tests_mincontexts = 1, # tested in at least one context, not sure what we're using for this paper
 # MatrixQTL geneloc. need if by chromosome
  contexts_vec = contexts_vec,
  exprmap = geneloc_file,
  run_by_chrom = T, chroms_to_loop = paste0("chr", 1:22), # useful for high mem
  extract_value = "betase", index_QTL_vs_genesnp = T)