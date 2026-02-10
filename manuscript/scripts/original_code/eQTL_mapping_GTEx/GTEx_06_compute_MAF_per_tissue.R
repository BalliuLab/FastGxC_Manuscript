#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu
#%%%%%%%%%%%%%%% April 21st 2020
#%%%%%%%%%%%%%%% Script to compute MAF of SNP per tissue
#%%%%%%%%%%%%%%% For Hoffman
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(MatrixEQTL)
library(data.table)
library(dplyr)

## Location of data files.
project.dir = '/u/project/zaitlenlab/bballiu/FastGxE';

## Assign TISSUE_NAME and scale
exp_suffix = ".v8.EUR.normalized_expression"
exp_file_names=list.files(path = paste0(project.dir, "/data/GTEx_v8/MatrixEQTL_input/"),
                          pattern = paste0(exp_suffix,".txt"),  all.files = FALSE,full.names = F)
TISSUE_NAMES = c(gsub(x = exp_file_names, pattern = paste(exp_suffix,".txt",sep = "|"), replacement = ""),"AverageTissue")

## Load genotypes
SNP_file_name = paste0(project.dir, "/data/GTEx_v8/MatrixEQTL_input/GTEx_v8_WGS_838Indiv_Analysis_Freeze_EUR_SNPs_1prcMAF.txt");

snps = SlicedData$new();
snps$fileDelimiter = "\t";      # the TAB character
snps$fileOmitCharacters = "NA"; # denote missing values;
snps$fileSkipRows = 1;          # one row of column labels
snps$fileSkipColumns = 1;       # one column of row labels
snps$fileSliceSize = 2000;      # read file in slices of 2,000 rows
snps$LoadFile(SNP_file_name);
#rownames(snps) = gsub(pattern = "_A|_T|_C|_G",replacement = "", x = snps$GetAllRowNames())
samples_g=as.character(snps$columnNames)

MAF=data.frame(matrix(NA, nrow = nrow(snps), ncol = length(TISSUE_NAMES),
                      dimnames = list(snps$GetAllRowNames(),TISSUE_NAMES)),check.names = F)

for(i in 1:length(TISSUE_NAMES)){

  TISSUE_NAME = TISSUE_NAMES[i]
  if(TISSUE_NAME=="AverageTissue") {
    expression_file_name = paste0(project.dir, "/data/GTEx_v8/MatrixEQTL_input/",TISSUE_NAME,exp_suffix,"_homogeneous.txt");
  } else{
    expression_file_name = paste0(project.dir, "/data/GTEx_v8/MatrixEQTL_input/",TISSUE_NAME,exp_suffix,".txt");

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
  MAF[,TISSUE_NAME]=maf
  print(i)
  fwrite(x = data.table(MAF,keep.rownames = T) %>% {setnames(., old = "rn", new = "SNP")[]},
         file = paste0(project.dir,"/data/GTEx_v8/misc/GTEx_v8_SNPs_by_Tissue_MAF.txt"),
         append = F, sep = "\t", row.names = F, col.names = T)
}

print("Gedaan!")





