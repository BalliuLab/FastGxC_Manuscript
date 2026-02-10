#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu
#%%%%%%%%%%%%%%% April 21st 2020
#%%%%%%%%%%%%%%% Script to compute MAF of SNP per context
#%%%%%%%%%%%%%%% For Hoffman
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suppressPackageStartupMessages(library(MatrixEQTL))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))

args = commandArgs(trailingOnly = T)
work_dir = args[1] ## Location of data files.
cohort = args[2]
exp_suffix = args[3]
exp_file_path = args[4]
SNP_file_name = args[5]

## Assign CONTEXT_NAME and scale
exp_file_names=list.files(exp_file_path, pattern = paste0(exp_suffix,".txt"),  all.files = FALSE,full.names = F)
CONTEXT_NAMES = c(gsub(x = exp_file_names, pattern = paste(exp_suffix,".txt",sep = "|"), replacement = ""),"AverageContext")

## Load genotypes

snps = SlicedData$new();
snps$fileDelimiter = "\t";      # the TAB character
snps$fileOmitCharacters = "NA"; # denote missing values;
snps$fileSkipRows = 1;          # one row of column labels
snps$fileSkipColumns = 1;       # one column of row labels
snps$fileSliceSize = 2000;      # read file in slices of 2,000 rows
snps$LoadFile(SNP_file_name);
#rownames(snps) = gsub(pattern = "_A|_T|_C|_G",replacement = "", x = snps$GetAllRowNames())
samples_g=as.character(snps$columnNames)

MAF=data.frame(matrix(NA, nrow = nrow(snps), ncol = length(CONTEXT_NAMES),
                      dimnames = list(snps$GetAllRowNames(),CONTEXT_NAMES)),check.names = F)

for(i in 1:length(CONTEXT_NAMES)){

  CONTEXT_NAME = CONTEXT_NAMES[i]
  if(CONTEXT_NAME=="AverageContext") {
    expression_file_name = paste0(exp_file_path,CONTEXT_NAME,exp_suffix,".shared.txt");
  } else{
    expression_file_name = paste0(exp_file_path,CONTEXT_NAME,exp_suffix,".txt");

  }
  expression_mat=as.matrix(data.frame(fread(input = expression_file_name, header = T, check.names = F),row.names = 1, check.names = F))
  expression_mat=expression_mat[!apply(is.na(expression_mat), 1, all),] # Filter genes with NA across all samples
  expression_mat=expression_mat[,!apply(is.na(expression_mat), 2, all)] # Filter samples with NA across all genes
  samples_t=as.character(colnames(expression_mat))

  maf.list = vector('list', length(snps))
  for(sl in 1:length(snps)) {
    slice = snps[[sl]];
    slice=slice[,which(samples_g %in% samples_t)]
    maf.list[[sl]] = rowMeans(slice,na.rm=TRUE)/2;
    maf.list[[sl]] = pmin(maf.list[[sl]],1-maf.list[[sl]]);
  }
  maf=unlist(maf.list)
  MAF[,CONTEXT_NAME]=maf
  print(i)
  outdir = paste0(work_dir,"/data/", cohort, "/misc/")
  dir.create(outdir, showWarnings = F)
  fwrite(x = data.table(MAF,keep.rownames = T) %>% {setnames(., old = "rn", new = "SNP")[]},
         file = paste0(outdir, cohort, "_SNPs_by_Context_MAF.txt"),
         append = F, sep = "\t", row.names = F, col.names = T)
}

print("Gedaan!")





