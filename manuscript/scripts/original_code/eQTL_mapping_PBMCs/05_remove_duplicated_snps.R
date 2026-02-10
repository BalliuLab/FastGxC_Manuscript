#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu 
#%%%%%%%%%%%%%%% May 14, 2024
#%%%%%%%%%%%%%%% Script to remove duplicated SNPs
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(tibble))

args=commandArgs(trailingOnly=T)
work_dir = args[1]
cohort = args[2]
SNP_file_name = args[3]
snps_location_file_name = args[4]
outdir = args[5]


print("starting removed_duplicated_snps.R...")

print("reading in snps file and fixing column sample names...")
SNP_file <- read_delim(SNP_file_name,delim="\t") 
#SNP_file <- SNP_file %>% rename_all(gsub, pattern = '_(.*)', replacement = '') ### why do we need this?

print("reading in snps_loc file...")
snps_location_file <- read_delim(snps_location_file_name,delim="\t")

print("getting duplicated snps ...")

duplicated_snps_1 <- SNP_file %>% select(SNP) %>% group_by(SNP) %>% summarize(n_per_snp = n()) %>% filter(n_per_snp > 1) %>% pull(SNP)
sprintf("In genotype file, there are %i duplicated SNPs total",length(duplicated_snps_1))

duplicated_snps_2 <- snps_location_file %>% group_by(snp,chr,pos) %>% summarize(n_per_snp = n()) %>% filter(n_per_snp > 1) %>% pull(snp)
sprintf("In snps location file, there are %i duplicated SNPs total",length(duplicated_snps_2))

if(length(duplicated_snps_1) != length(duplicated_snps_2)) stop(sprintf("FOUND DIFFERENT NUMBER OF DUPLICATED SNPS!"))

# remove these snps from both files

print("writing new snps file...")
SNP_file.save_dir <- paste0(outdir, cohort, "_5prcMAF_genotypes.txt")
print(SNP_file.save_dir)

SNP_file %>%
  filter(!(SNP %in% duplicated_snps_1)) %>%
  write_delim(SNP_file.save_dir,
              append = FALSE,
              col_names=TRUE,
              delim = "\t")
print("done saving new snps file")


print("writing new snps location file...")
snps_loc_file.save_dir <-paste0(outdir, cohort, "_5prcMAF_snpsloc.txt")
print(snps_loc_file.save_dir)

snps_location_file %>%
  filter(!(snp %in% duplicated_snps_1)) %>%
  write_delim(snps_loc_file.save_dir,
              append = FALSE,
              col_names=TRUE,
              delim = "\t")
print("done saving new snps location file")

print('done!')

###
# 
# 
# tribble(
#   ~SNP,~"GTEX-111CU_GTEX-111CU",~"GTEX-111FC_GTEX-111FC",~"GTEX-111VG_GTEX-111VG",~"ABC__FFGF",~"ABAfC_F",
#   "rs554008981",0,1,2,2,1,
#   "rs201055865",2,0,1,1,2
#   ) 






