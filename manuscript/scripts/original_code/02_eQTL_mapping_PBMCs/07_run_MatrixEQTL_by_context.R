#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu
#%%%%%%%%%%%%%%% April 20th 2020
#%%%%%%%%%%%%%%% Script to run Matrix EQTL by content
#%%%%%%%%%%%%%%% For Hoffman
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# qrsh -l h_data=16G,h_rt=12:00:00,highp
# module load R/3.6.1

## Libraries and functions
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(MatrixEQTL))
suppressPackageStartupMessages(library(TreeQTL))

## Arguments
args=commandArgs(trailingOnly = T)
work_dir = args[1] ## Location of data files.
cohort = args[2]
exp_scale=as.numeric(args[3])
i=as.numeric(args[4])
num_contexts = as.numeric(args[5])
outdir = args[6]
data_dir = args[7]
SNP_file_name = args[8]
snps_location_file_name = args[9]
gene_location_file_name = args[10]
MAF_file = args[11]


setDTthreads(1)
print(paste0("data.table getDTthreads(): ",getDTthreads()))

## Assign CONTEXT_NAME and scale
if(exp_scale == 1) exp_suffix = paste0(".", cohort, ".mean_norm_res_exp") ## context by context 
if(exp_scale == 2) exp_suffix = paste0(".", cohort, ".mean_norm_res_exp.specific") ## FastGxC specific
if(exp_scale == 3) exp_suffix = paste0(".", cohort, ".mean_norm_res_exp.shared") ## FastGxC shared

## Get context name
all_files=list.files(path = data_dir, pattern = paste0(exp_suffix,".txt"), all.files = FALSE,full.names = F)

if((exp_scale %in% 1:2) & length(all_files)!=num_contexts) stop(sprintf("Expecting gene expression files for %i contexts but got %i.", num_contexts, length(all_files)))

CONTEXT_NAME = gsub(x = list.files(path = data_dir,
                                  pattern = paste0(exp_suffix,".txt"),
                                  all.files = FALSE,full.names = F),
                   pattern = paste(exp_suffix,".txt",sep = "|"),
                   replacement = "")[i]

sprintf("Running analysis for %s context on %s data", CONTEXT_NAME, gsub(pattern = "_", replacement = " ", gsub(pattern = "\\.",replacement = " ",x = exp_suffix)))

## Settings

# Linear model to use, modelANOVA, modelLINEAR, or modelLINEAR_CROSS
useModel = modelLINEAR; # modelANOVA, modelLINEAR, or modelLINEAR_CROSS

# Gene expression file name
expression_file_name = paste0(data_dir,CONTEXT_NAME,exp_suffix,".txt");

# Covariates file name
# Set to character() for no covariates
covariates_file_name = character() #paste(work_dir, "/data/GTEx_v8/expression_covariates/",CONTEXT_NAME,"v8.EUR.covariates.txt", sep="");

# Output file name
output_file_name_cis = paste0(outdir, CONTEXT_NAME,exp_suffix,".all_pairs.txt");
output_file_name_tra = tempfile();

# Only associations significant at this level will be saved
pvOutputThreshold_cis = 1; #if((exp_scale == 1)|(exp_scale == 3)|(exp_scale == 5)) pvOutputThreshold_cis = 0.05 else
pvOutputThreshold_tra = 0;

# Error covariance matrix
# Set to numeric() for identity.
errorCovariance = numeric();

# Distance for local gene-SNP pairs
cisDist = 1e6;

## Load gene expression data
expression_mat=as.matrix(data.frame(fread(input = expression_file_name, header = T),row.names = 1, check.names = F))
expression_mat=expression_mat[!apply(is.na(expression_mat), 1, all),] # Filter genes with NA across all samples
expression_mat=expression_mat[,!apply(is.na(expression_mat), 2, all)] # Filter samples with NA across all genes
#expression_mat=expression_mat[intersect(rownames(expression_mat),names(which(rowSums(genes_by_context)!=1))), ] # Filter genes expressed only in single context (already did this in exp pipeline)
gene = SlicedData$new();
gene$CreateFromMatrix(expression_mat)

## Load covariates
cvrt = SlicedData$new();
cvrt$fileDelimiter = "\t";      # the TAB character
cvrt$fileOmitCharacters = "NA"; # denote missing values;
cvrt$fileSkipRows = 1;          # one row of column labels
cvrt$fileSkipColumns = 1;       # one column of row labels
if(length(covariates_file_name)>0) {
  cvrt$LoadFile(covariates_file_name);
}

## Load genotype data
snps = SlicedData$new();
snps$fileDelimiter = "\t";      # the TAB character
snps$fileOmitCharacters = "NA"; # denote missing values;
snps$fileSkipRows = 1;          # one row of column labels
snps$fileSkipColumns = 1;       # one column of row labels
snps$fileSliceSize = 2000;      # read file in slices of 2,000 rows
snps$LoadFile(SNP_file_name);
#rownames(snps) = gsub(pattern = "_A|_T|_C|_G",replacement = "", x = snps$GetAllRowNames())

# Match SNP and expression individuals
matched_snp_ids = match(colnames(gene), colnames(snps))
snps$ColumnSubsample(matched_snp_ids)


# Match SNP and expression individuals
# snps$ColumnSubsample(which(colnames(snps) %in% colnames(gene)))

## Run the analysis
snpspos = fread(input = snps_location_file_name, header = TRUE, stringsAsFactors = FALSE);

# Keep SNP with MAF>5% in each context
if(0){
  passSNPs=data.frame(fread(input = paste0(work_dir,"/data//GTEx_v8/misc/GTEx_v8_SNPs_MAFgeq5_each_context.txt"), header = T))
  snps$RowReorder(rownames(snps) %in% passSNPs$SNP);
  snpspos=snpspos[snpspos$snp %in% passSNPs$SNP,]

}

# Keep SNP with MAF>5% in the context of interest
if(1){
  MAF=data.frame(fread(input = MAF_file, header = T,check.names = F),check.names = F)
  passSNPs=MAF[(MAF[,CONTEXT_NAME]>=0.05),"SNP"]
  snps$RowReorder(rownames(snps) %in% passSNPs);
  snpspos=snpspos[snpspos$snp %in% passSNPs,]
}

genepos = fread(input = gene_location_file_name, header = TRUE, stringsAsFactors = FALSE);
genepos=genepos[genepos$geneid %in% rownames(expression_mat),] # keep positions only for tested genes

me = Matrix_eQTL_main(
  snps = snps,
  gene = gene,
  cvrt = cvrt,
  output_file_name     = output_file_name_tra,
  pvOutputThreshold     = pvOutputThreshold_tra,
  useModel = useModel,
  errorCovariance = errorCovariance,
  verbose = TRUE,
  output_file_name.cis = output_file_name_cis,
  pvOutputThreshold.cis = pvOutputThreshold_cis,
  snpspos = snpspos,
  genepos = genepos,
  cisDist = cisDist,
  pvalue.hist = FALSE,
  min.pv.by.genesnp = FALSE,
  noFDRsaveMemory = FALSE);

## Results:
cat('Analyse gedaan in: ', me$time.in.sec, ' seconden', '\n')
