# ============================================================================
# Lise Tucker functions code to make table to add pLI scores to FastGxC figure 5
# sorry in advance to anyone trying to read this code. It gets kinda crazy after so many changes 
# ============================================================================
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)


# Job 0: Make eAssociation file for GTEx
make_GTEx_eAssoc_file <- function(input_dir, output_file){

  suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(stringr)
    library(purrr)
  })

  # Find all candidate eAssoc files
  eassoc_files <- list.files(
    input_dir,
    pattern = "^eAssoc_by_gene\\..*normalized_and_residualized_expression.*\\.txt$",
    full.names = TRUE
  )

  if (length(eassoc_files) == 0) {
    stop("No eAssoc_by_gene files found in input_dir")
  }

  # Helper to classify file type + extract tissue
  parse_filename_info <- function(fname){
    base <- basename(fname)
    
    # Determine exp_type - check most specific patterns first
    exp_type <- case_when(
      str_detect(base, "heterogeneous") ~ "mean_norm_res_exp_heterogeneous",
      str_detect(base, "homogeneous")   ~ "mean_norm_res_exp_homogeneous",
      TRUE                              ~ "mean_norm_res_exp"  # CxC files
    )
    
    # Determine tissue
    tissue <- case_when(
      str_detect(base, "homogeneous") ~ "AverageContext",
      TRUE ~ str_match(
        base,
        "^eAssoc_by_gene\\.([^\\.]+)\\."
      )[, 2]
    )
    
    tibble(
      exp_type = exp_type,
      tissue   = tissue
    )
  }

  # Read and combine all files
  message("Reading ", length(eassoc_files), " eAssociation files...")

  master_df <- map_dfr(eassoc_files, function(f){

    info <- parse_filename_info(f)

    read_table(
      f,
      col_types = cols(
        gene    = col_character(),
        SNP     = col_character(),
        beta    = col_double(),
        t.stat  = col_double(),
        p.value = col_double()
      )
    ) %>%
      transmute(
        exp_type = info$exp_type,
        tissue   = info$tissue,
        gene     = gene,
        snp      = SNP
      )
  })

  # Write output
  write_csv(master_df, output_file)

  message(
    "Wrote ",
    nrow(master_df),
    " eAssociations to ",
    output_file
  )

  invisible(master_df)
}


# Sets are 
# 1) CxC: which includes just all CxC
# 2) fastgxc_shared which includes only shared
# 3) fastgxc_specific which includes only specific
# 4) fastgxc_shared_specific which includes the genes that are in both shared and specific (not shared or specific only tho)

# Job 1
make_gene_sets_unique <- function(input_dir, output_dir){

  df <- read_csv(input_dir, col_types = cols(.default = col_character())) %>%
    rename(GENE = gene) %>%
    select(exp_type, GENE) %>%
    distinct()

  # ---- Define base gene lists ----
  cxc_genes <- df %>%
    filter(exp_type == "mean_norm_res_exp") %>%
    pull(GENE)

  hom_genes <- df %>%
    filter(exp_type == "mean_norm_res_exp_homogeneous") %>%
    pull(GENE)

  het_genes <- df %>%
    filter(exp_type == "mean_norm_res_exp_heterogeneous") %>%
    pull(GENE)

  # ---- Define FastGxC sets ----
  fastgxc_shared_specific <- intersect(hom_genes, het_genes)

  fastgxc_specific <- setdiff(het_genes, hom_genes)

  fastgxc_shared <- setdiff(hom_genes, het_genes)

  # ---- Assemble final output ----
  final <- bind_rows(
    tibble(set = "CxC", GENE = cxc_genes),
    tibble(set = "fastgxc_specific", GENE = fastgxc_specific),
    tibble(set = "fastgxc_shared", GENE = fastgxc_shared),
    tibble(set = "fastgxc_shared_specific", GENE = fastgxc_shared_specific)
  ) %>%
    distinct()

  write_csv(final, output_dir)  # set, GENE
}

# Job 2 (SC and GTEx) 
get_tested_genes <- function(shared_all_pairs_file, output_dir){
  library(tidyverse)

  # Read shared genes
  shared_genes <- fread(
    shared_all_pairs_file,
    select = "gene" 
  ) %>%
    distinct() %>%
    rename(GENE = gene)

  # Define sets
  cxc <- shared_genes %>% mutate(set = "CxC")
  fastgxc_specific <- shared_genes %>% mutate(set = "fastgxc_specific")
  fastgxc_shared <- shared_genes %>% mutate(set = "fastgxc_shared")
  fastgxc_shared_specific <- shared_genes %>% mutate(set = "fastgxc_shared_specific")

  # Write output 
  final <- bind_rows(cxc, fastgxc_specific, fastgxc_shared, fastgxc_shared_specific) %>% 
    distinct() %>%
    dplyr::select(set, GENE) 

  write_csv(final, output_dir)
}

# # Single Cell: COMPLETED
# combine_soi_tested_gene_based(soi_dir = paste0(sc_output.dir, "Enrichment.GENE_sets_FastGxC.sc.csv"),
#                     tested_dir = paste0(sc_output.dir, "Enrichment.Tissue_Agnostic.GENEs_tested.sc.csv"),
#                     output_dir = paste0(sc_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_FastGxC.sc.csv")) # set, IS_BG, GENE

# # # GTEx: COMPLETED
# combine_soi_tested_gene_based(soi_dir = paste0(GTEx_output.dir, "Enrichment.GENE_sets_FastGxC.GTEx.csv"),
#                   tested_dir = paste0(GTEx_output.dir, "Enrichment.Tissue_Agnostic.GENEs_tested.GTEx.csv"),
#                   output_dir = paste0(GTEx_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_FastGxC.GTEx.csv")) # set, IS_BG, GENE

# Job 3: Combine SOI!
combine_soi_tested_gene_based <- function(soi_dir, tested_dir, output_dir){
  tested <- read_csv(tested_dir, col_types = cols(.default = col_character()))
  soi <- read_csv(soi_dir, col_types = cols(.default = col_character()))
  
  soi %>% group_by(set) %>% group_modify(function(tib, key){
    
    print(key$set[1])
    
    soi_temp <- tib %>% mutate(IS_BG = 0) %>% dplyr::select(GENE, IS_BG) %>% distinct
    bg_temp <- tested %>% filter(set == key$set[1]) %>% mutate(IS_BG = 1) %>% dplyr::select(GENE, IS_BG) %>% distinct
    
    joined_temp <- bind_rows(soi_temp, bg_temp) %>% 
      dplyr::select(IS_BG, GENE)
    
    print(joined_temp)
    print(joined_temp %>% group_by(IS_BG) %>% summarize(n_GENEs = n()))
    cat("\n\n")
    
    return(joined_temp)
  }) %>% dplyr::select(set, IS_BG, GENE) %>% 
    write_csv(output_dir)
}

# Job 3.5 (get avg nSNPs per gene for GTEx)
make_n_SNPs_per_gene_average <- function(input_dir, output_file){
  
  # Get all n_SNPs_per_gene files
  files <- list.files(input_dir, pattern = "^n_SNPs_per_gene_.*\\.txt$", full.names = TRUE)
  
  # Read all files
  all_data <- rbindlist(lapply(files, function(f) {
    fread(f, header = FALSE, col.names = c("gene", "n_SNPs"))
  }))
  
  # Calculate average SNPs per gene using data.table syntax
  avg_snps <- all_data[, .(avg_n_SNPs = mean(n_SNPs)), by = gene]
  
  # Write output (tab-separated, no header, no row names)
  fwrite(avg_snps, file = output_file, sep = "\t", col.names = FALSE)
}

# Job 4
# # Single Cell: COMPLETED
# add_gene_lengths_and_snp_count_to_SOI_sc(input_snp_file = paste0(bruna_project.dir, "results/eQTL_mapping/CLUES_ASN.CLUES_EUR.OneK1K_Metasoft/random_effect2/n_SNPs_per_gene_AverageContext.txt"),
#                    input_soi_file = paste0(sc_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_FastGxC.sc.csv"),
#                    output_file = paste0(sc_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_FastGxC.sc.csv")) 

# Make sure to get as many gene sizes as possible
add_gene_lengths_and_snp_count_to_SOI_sc <- function(input_snp_file, input_soi_file, output_file){ 
  snps_per_gene <- read_tsv(input_snp_file, col_names = c("GENE", "n_SNPs"), col_types = cols(GENE = col_character(), n_SNPs = col_double())) # do I need to add col_types argument? (see next line for example)
  soi <- read_csv(input_soi_file, col_types = cols(.default = col_character()))

  # Read GTF and compute gene sizes (exonic length)
  hg_19_gtf <- "/u/project/bballiu/shared_datasets/reference_datasets/hg19/Homo_sapiens.GRCh37.87.gtf"
  
  gene_sizes <- read_tsv(
    hg_19_gtf,
    comment = "#",
    col_names = c("chr","source","feature","start","end",
                  "score","strand","frame","attribute"),
    col_types = cols(.default = col_character())
  ) %>%
    filter(feature == "exon") %>%
    mutate(
      start = as.numeric(start),
      end = as.numeric(end),
      gene_size = end - start + 1,
      GENE = str_extract(attribute, "ENSG[0-9]+")
    ) %>%
    group_by(GENE) %>%
    summarize(gene_size = sum(gene_size), .groups = "drop")
  
  # Add SNP counts to SOI table
  soi_snps <- soi %>%
  left_join(snps_per_gene, by = "GENE")

  # add gene sizes to table
  final <- soi_snps %>% left_join(gene_sizes, by = "GENE")

  # Write output
  final %>% write_csv(output_file)

}

# Job 4 GTEx
# # GTEx: COMPLETED
# add_gene_lengths_and_snp_count_to_SOI_GTEx(input_snp_file = paste0(GTEx_output.dir, "n_SNPs_per_gene_AverageContext_GTEx.txt"),
#                    input_soi_file = paste0(GTEx_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_FastGxC.GTEx.csv"),
#                    output_file = paste0(GTEx_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_FastGxC.GTEx.csv")) 
  
add_gene_lengths_and_snp_count_to_SOI_GTEx <- function(input_snp_file,
                                                      input_soi_file,
                                                      output_file){ 
  library(biomaRt)
  library(tidyverse)

  snps_per_gene <- read_tsv(
    input_snp_file,
    col_names = c("GENE", "n_SNPs"),
    col_types = cols(
      GENE = col_character(),
      n_SNPs = col_double()
    )
  )

  soi <- read_csv(input_soi_file, col_types = cols(.default = col_character()))

  # Read GTF and compute gene sizes (Ensembl IDs)
  hg_19_gtf <- "/u/project/bballiu/shared_datasets/reference_datasets/hg19/Homo_sapiens.GRCh37.87.gtf"
  
  gene_sizes_ensembl <- read_tsv(
    hg_19_gtf,
    comment = "#",
    col_names = c("chr","source","feature","start","end",
                  "score","strand","frame","attribute"),
    col_types = cols(.default = col_character())
  ) %>%
    filter(feature == "exon") %>%
    mutate(
      start = as.numeric(start),
      end   = as.numeric(end),
      gene_size = end - start + 1,
      ensembl_gene_id = str_extract(attribute, "ENSG[0-9]+")
    ) %>%
    filter(!is.na(ensembl_gene_id)) %>%
    group_by(ensembl_gene_id) %>%
    summarize(gene_size = sum(gene_size), .groups = "drop") %>%
    rename(GENE = ensembl_gene_id)

  # -----------------------------
  # Add SNP counts to SOI table
  # -----------------------------
  soi_snps <- soi %>%
    left_join(snps_per_gene, by = "GENE")

  # -----------------------------
  # First pass: add gene sizes using Ensembl IDs
  # -----------------------------
  soi_with_sizes <- soi_snps %>%
    left_join(gene_sizes_ensembl, by = "GENE")

  # -----------------------------
  # Identify non-Ensembl genes (assumed HUGO symbols)
  # -----------------------------
  hugo_genes <- soi_with_sizes %>%
    filter(is.na(gene_size)) %>%
    distinct(GENE) %>%
    pull(GENE)

  if (length(hugo_genes) > 0) {

    # -----------------------------
    # Use biomaRt to map HGNC symbols -> Ensembl IDs
    # -----------------------------
    mart <- useMart(
      biomart = "ENSEMBL_MART_ENSEMBL",
      dataset = "hsapiens_gene_ensembl",
      host = "https://grch37.ensembl.org"  
    )

    # new getBM call and also alias handling
    hugo_to_ensembl <- getBM(
    attributes = c("hgnc_symbol", "ensembl_gene_id"),
    filters    = "hgnc_symbol",
    values     = hugo_genes,
    mart       = mart
    )

    # Also try alias mapping for genes that didn't match
    unmapped <- setdiff(hugo_genes, hugo_to_ensembl$hgnc_symbol)

    if (length(unmapped) > 0) {
    alias_mapping <- getBM(
        attributes = c("external_synonym", "ensembl_gene_id", "hgnc_symbol"),
        filters    = "external_synonym", 
        values     = unmapped,
        mart       = mart
    ) %>%
        dplyr::select(GENE = external_synonym, ensembl_gene_id) %>%
        distinct()
    
    hugo_to_ensembl <- bind_rows(
        hugo_to_ensembl %>% rename(GENE = hgnc_symbol),
        alias_mapping
    )
    }

    cat("biomaRt returned mappings for", n_distinct(hugo_to_ensembl$GENE), 
        "out of", length(hugo_genes), "HUGO symbols\n")

    # Check which symbols failed to map
    unmapped_symbols <- setdiff(hugo_genes, hugo_to_ensembl$GENE)
    cat("Symbols that didn't map:", length(unmapped_symbols), "\n")
    head(unmapped_symbols, 20)

    # -----------------------------
    # Attach gene sizes via mapped Ensembl IDs
    # -----------------------------
    hugo_gene_sizes <- hugo_to_ensembl %>%
      left_join(
        gene_sizes_ensembl %>% dplyr::rename(ensembl_gene_id = GENE),
        by = "ensembl_gene_id"
      ) %>%
      dplyr::select(GENE, gene_size) %>%
      group_by(GENE) %>%
      summarize(
        gene_size = if (all(is.na(gene_size))) NA_real_ else max(gene_size, na.rm = TRUE),
        .groups = "drop"
      )
    # -----------------------------
    # Fill in missing gene sizes
    # -----------------------------
    final <- soi_with_sizes %>%
      left_join(hugo_gene_sizes, by = "GENE", suffix = c("", ".hugo")) %>%
      mutate(
        gene_size = coalesce(gene_size, gene_size.hugo)
      ) %>%
      dplyr::select(-gene_size.hugo)

  } else {
    final <- soi_with_sizes
  }

  final %>% write_csv(output_file)
}

# add_exp_profile_to_SOI_sc(input_dir= paste0(bruna_project.dir, "data/"),
#                     input_soi_file= paste0(sc_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_FastGxC.sc.csv"),
#                     output_file = paste0(sc_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_exp_profile_FastGxC.sc.csv")) 

add_exp_profile_to_SOI_sc <- function(input_dir, input_soi_file, output_file){ 
  soi <- read_csv(input_soi_file, col_types = cols(.default = col_character()))
  
  # ---- HELPERS ----
  # avg exp across individuals for each gene in a single expression file
  get_gene_means <- function(file_path) {
    df <- data.table::fread(file_path, data.table = FALSE)
    rownames(df) <- df[[1]]
    df[[1]] <- NULL
    rowMeans(df, na.rm = TRUE)
  }
  # average expression across all files in a cohort
  get_cohort_mean_exp <- function(exp_files) {
    if (length(exp_files) == 0) return(NULL)
    
    all_means <- lapply(exp_files, get_gene_means)
    all_genes <- unique(unlist(lapply(all_means, names)))
    
    combined <- sapply(
      all_means,
      function(x) x[match(all_genes, names(x))]
    )
    rownames(combined) <- all_genes
    
    setNames(rowMeans(combined, na.rm = TRUE), all_genes)
  }
  
  # ---- PRECOMPUTE EXPRESSION PER COHORT ----
  cohorts <- c("CLUES_EUR", "CLUES_ASN", "OneK1K")
  exp_lookup <- list()
  
  for (co in cohorts) {
    cohort_dir <- file.path(input_dir, co, "MatrixEQTL_input")
    
    files <- list.files(
      cohort_dir,
      pattern = "\\.mean_norm_res_exp\\.txt$",
      full.names = TRUE
    )
    
    exp_lookup[[co]] <- get_cohort_mean_exp(files)
  }
  
  # ---- FAST LOOKUP BY GENE ----
  lookup_exp_fast <- function(gene_vec, cohort) {
    exp_vec <- exp_lookup[[cohort]]
    if (is.null(exp_vec)) return(rep(NA_real_, length(gene_vec)))
    exp_vec[match(gene_vec, names(exp_vec))]
  }
  
  soi <- soi %>%
    mutate(
      mean_CLUES_EUR_exp = lookup_exp_fast(GENE, "CLUES_EUR"),
      mean_CLUES_ASN_exp = lookup_exp_fast(GENE, "CLUES_ASN"),
      mean_OneK1K_exp    = lookup_exp_fast(GENE, "OneK1K")
    )
  
  exp_matrix <- cbind(
    soi$mean_CLUES_EUR_exp,
    soi$mean_CLUES_ASN_exp,
    soi$mean_OneK1K_exp
  )
  
  soi$mean_exp   <- rowMeans(exp_matrix, na.rm = TRUE)
  soi$median_exp <- apply(exp_matrix, 1, median, na.rm = TRUE)
  
  write_csv(soi, output_file)
}

# add_exp_profile_to_SOI_GTEx(input_dir= paste0(bruna_project.dir, "data/GTEx_v8/MatrixEQTL_input/"),
#                   input_soi_file= paste0(GTEx_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_FastGxC.GTEx.csv"),
#                   output_file = paste0(GTEx_output.dir, "Enrichment.Tissue_Agnostic.GENEs_SOI_BG_snp_count_gene_size_exp_profile_FastGxC.GTEx.csv"))                   

add_exp_profile_to_SOI_GTEx <- function(input_dir, input_soi_file, output_file) {

  soi <- read_csv(input_soi_file, col_types = cols(.default = col_character()))

  # ---- HELPERS ----
  # average expression across individuals for each gene in a single file
  get_gene_means <- function(file_path) {
    df <- data.table::fread(file_path, data.table = FALSE)
    rownames(df) <- df[[1]]
    df[[1]] <- NULL
    rowMeans(df, na.rm = TRUE)
  }

  # average expression across all GTEx contexts
  get_GTEx_mean_exp <- function(exp_files) {
    if (length(exp_files) == 0) return(NULL)

    all_means <- lapply(exp_files, get_gene_means)
    all_genes <- unique(unlist(lapply(all_means, names)))

    combined <- sapply(
      all_means,
      function(x) x[match(all_genes, names(x))]
    )
    rownames(combined) <- all_genes

    setNames(rowMeans(combined, na.rm = TRUE), all_genes)
  }

  # ---- READ GTEx FILES ----
  # Matches: "<context>.v8.EUR.normalized_and_residualized_expression.txt"
  gtex_files <- list.files(
    input_dir,
    pattern = "\\.v8\\.EUR\\.normalized_and_residualized_expression\\.txt$",
    full.names = TRUE
  )

  gtex_exp <- get_GTEx_mean_exp(gtex_files)

  # ---- FAST LOOKUP ----
  lookup_exp_fast <- function(gene_vec, exp_vec) {
    if (is.null(exp_vec)) return(rep(NA_real_, length(gene_vec)))
    exp_vec[match(gene_vec, names(exp_vec))]
  }

  soi <- soi %>%
    mutate(
      mean_GTEx_exp = lookup_exp_fast(GENE, gtex_exp)
    )

  # keep same interface as sc version
  soi$mean_exp   <- soi$mean_GTEx_exp
  soi$median_exp <- soi$mean_GTEx_exp

  write_csv(soi, output_file)
}


# Job 6
add_pLI_to_unmatched_genes_sc <- function(input_matched_file, output_file){
  library(data.table)
  library(readr)
  
  ## Read input files
  gnomad_v4 <- fread("/u/home/l/ltucker/project-bballiu/gnomAD/gnomad.v4.1.constraint_metrics.tsv")
  gnomad_v2 <- fread("/u/home/l/ltucker/project-bballiu/gnomAD/gnomad.v2.1.1.lof_metrics.by_gene.txt.bgz")
  
  # Read matched genes and convert to data.table
  dt <- fread(input_matched_file)
  
  ## Prepare gnomAD lookups
  
  # v4.1: Filter to rows with valid Ensembl gene_id
  gnomad_v4_ensembl <- gnomad_v4[grepl("^ENSG", gene_id)]
  
  # Normalize booleans
  gnomad_v4_ensembl[, mane_select := tolower(mane_select) == "true"]
  gnomad_v4_ensembl[, canonical   := tolower(canonical) == "true"]
  
  # Order by transcript priority
  setorder(
    gnomad_v4_ensembl,
    gene_id,
    -mane_select,
    -canonical
  )
  
  # Collapse to ONE row per Ensembl gene_id
  v4_ensembl_lookup <- gnomad_v4_ensembl[
    !is.na(gene_id) & gene_id != "",
    .(pLI = lof.pLI[1]),
    by = gene_id
  ]
  setkey(v4_ensembl_lookup, gene_id)
  
  # v2.1.1 fallback
  v2_lookup <- gnomad_v2[
    !is.na(gene_id) & gene_id != "",
    .(pLI = first(pLI)),
    by = gene_id
  ]
  setkey(v2_lookup, gene_id)
  
  ## Add pLI column - 2-step matching (Ensembl only)
  dt[, pLI := NA_real_]
  
  # 1) Ensembl match via v4.1
  dt[
    !is.na(GENE),
    pLI := v4_ensembl_lookup[.SD, on = .(gene_id = GENE), pLI]
  ]
  v4_matches <- sum(!is.na(dt$pLI))
  
  # 2) Ensembl fallback (v2.1.1)
  dt[
    is.na(pLI) & !is.na(GENE),
    pLI := v2_lookup[.SD, on = .(gene_id = GENE), pLI]
  ]
  v2_matches <- sum(!is.na(dt$pLI)) - v4_matches
  
  no_matches <- sum(is.na(dt$pLI))
  

  # Helper to print counts per set / IS_BG
  print_counts <- function(dt, label) {
  cat("\n", label, "\n", sep = "")
  tmp <- dt[, .N, by = .(set, IS_BG)]
  tmp <- tmp[order(set, IS_BG)]
  print(tmp)
  }

  # Initial counts before any filtering
  print_counts(dt, "Initial counts (before NA filtering):")

  # NA filtering

  # First filter out NA pLI genes (for both IS_BG = 0 and IS_BG = 1)
  n_before_pli <- nrow(dt)
  dt <- dt[!is.na(pLI)]
  n_after_pli <- nrow(dt)

  cat(
    "\npLI NA filtering:\n",
    "  Rows before:", n_before_pli, "\n",
    "  Rows removed:", n_before_pli - n_after_pli, "\n",
    "  Rows remaining:", n_after_pli, "\n",
    sep = " "
  )

  # Save all IS_BG=1 genes with non-NA pLI to separate file (this is to create the Bruna specific csv file for her grant)
  dt_all_background <- dt[IS_BG == 1]

  cat(
    "\nIS_BG=1 genes with non-NA pLI:\n",
    "  Rows:", nrow(dt_all_background), "\n",
    sep = " "
  )

  fwrite(
    dt_all_background,
    "/u/home/l/ltucker/project-bballiu/FastGxC_paper/Enrichment_figure/sc_all_background.csv",
    sep = ",",
    quote = FALSE,
    na = ""
  )


  # Now filter out NA gene_size genes
  n_before_genesize <- nrow(dt)
  dt <- dt[!is.na(gene_size)]
  n_after_genesize <- nrow(dt)

  cat(
    "\ngene_size NA filtering:\n",
    "  Rows before:", n_before_genesize, "\n",
    "  Rows removed:", n_before_genesize - n_after_genesize, "\n",
    "  Rows remaining:", n_after_genesize, "\n",
    sep = " "
  )

  print_counts(dt, "Counts after NA filtering:")

  # --------------------------------
  # fastgxc global foreground pool. essentially remove all fastgxc eGenes from any of the fastgxc background sets
  # --------------------------------

  fastgxc_sets <- c("fastgxc_shared_specific", "fastgxc_specific", "fastgxc_shared")

  fastgxc_fg_genes <- unique(
  dt[set %in% fastgxc_sets & IS_BG == 0, GENE]
  )

  cat(
  "\nfastgxc global foreground pool:\n",
  "  Unique foreground genes:", length(fastgxc_fg_genes), "\n",
  sep = " "
  )

  # Remove from fastgxc backgrounds
  n_before_fastgxc_bg <- nrow(dt)
  dt <- dt[!(set %in% fastgxc_sets & IS_BG == 1 & GENE %in% fastgxc_fg_genes)]
  n_after_fastgxc_bg <- nrow(dt)

  cat(
  "  Background genes removed from fastgxc sets:",
  n_before_fastgxc_bg - n_after_fastgxc_bg, "\n"
  )

  print_counts(dt, "Counts after fastgxc background exclusion (including NA gene_size):")

  # --------------------------------
  # CxC self-background exclusion
  # --------------------------------
  cxc_fg_genes <- dt[set == "CxC" & IS_BG == 0, GENE]

  n_before_cxc_bg <- nrow(dt)
  dt <- dt[!(set == "CxC" & IS_BG == 1 & GENE %in% cxc_fg_genes)]
  n_after_cxc_bg <- nrow(dt)

  cat(
  "\nCxC background filtering:\n",
  "  Foreground genes:", length(unique(cxc_fg_genes)), "\n",
  "  Background genes removed:",
  n_before_cxc_bg - n_after_cxc_bg, "\n",
  sep = " "
  )
  print_counts(dt, "Counts after CxC background exclusion (including NA gene_size):")

  cat("\nWriting output to:\n", output_file, "\n")
  fwrite(dt, output_file, sep = ",", quote = FALSE, na = "")
  
  cat("Done.\n")
}


add_pLI_to_unmatched_genes_GTEx <- function(input_matched_file, output_file){
  library(data.table)
  
  ## Read input files
  gnomad_v4 <- fread("/u/home/l/ltucker/project-bballiu/gnomAD/gnomad.v4.1.constraint_metrics.tsv")
  gnomad_v2 <- fread("/u/home/l/ltucker/project-bballiu/gnomAD/gnomad.v2.1.1.lof_metrics.by_gene.txt.bgz")
  
  dt <- fread(input_matched_file)
  
  ## -----------------------------
  ## Prepare gnomAD lookups
  ## -----------------------------
  
  ## v4.1 Ensembl lookup
  gnomad_v4_ensembl <- gnomad_v4[grepl("^ENSG", gene_id)]
  gnomad_v4_ensembl[, mane_select := tolower(mane_select) == "true"]
  gnomad_v4_ensembl[, canonical   := tolower(canonical) == "true"]
  setorder(gnomad_v4_ensembl, gene_id, -mane_select, -canonical)
  
  v4_ensembl_lookup <- gnomad_v4_ensembl[
    !is.na(gene_id) & gene_id != "",
    .(pLI = lof.pLI[1]),
    by = gene_id
  ]
  setkey(v4_ensembl_lookup, gene_id)
  
  ## v4.1 HGNC symbol lookup
  gnomad_v4_hgnc <- gnomad_v4[!is.na(gene) & gene != ""]
  gnomad_v4_hgnc[, mane_select := tolower(mane_select) == "true"]
  gnomad_v4_hgnc[, canonical   := tolower(canonical) == "true"]
  setorder(gnomad_v4_hgnc, gene, -mane_select, -canonical)
  
  v4_hgnc_lookup <- gnomad_v4_hgnc[, .(pLI = lof.pLI[1]), by = gene]
  setkey(v4_hgnc_lookup, gene)
  
  ## v2.1.1 Ensembl fallback
  v2_ensembl_lookup <- gnomad_v2[
    !is.na(gene_id) & gene_id != "",
    .(pLI = first(pLI)),
    by = gene_id
  ]
  setkey(v2_ensembl_lookup, gene_id)
  
  ## v2.1.1 HGNC fallback (NEW)
  v2_hgnc_lookup <- gnomad_v2[
    !is.na(gene) & gene != "",
    .(pLI = first(pLI)),
    by = gene
  ]
  setkey(v2_hgnc_lookup, gene)
  
  ## -----------------------------
  ## Add pLI column
  ## -----------------------------
  
  dt[, pLI := NA_real_]
  dt[, is_ensembl := grepl("^ENSG", GENE)]
  
  # 1) HGNC match (v4.1)
  dt[!is_ensembl & !is.na(GENE),
     pLI := v4_hgnc_lookup[.SD, on = .(gene = GENE), pLI]]
  v4_hgnc_matches <- sum(!is.na(dt$pLI))
  
  # 2) HGNC fallback (v2.1.1) - NEW
  dt[is.na(pLI) & !is_ensembl & !is.na(GENE),
     pLI := v2_hgnc_lookup[.SD, on = .(gene = GENE), pLI]]
  v2_hgnc_matches <- sum(!is.na(dt$pLI)) - v4_hgnc_matches
  
  # 3) Ensembl match (v4.1)
  dt[is.na(pLI) & is_ensembl & !is.na(GENE),
     pLI := v4_ensembl_lookup[.SD, on = .(gene_id = GENE), pLI]]
  v4_ensembl_matches <- sum(!is.na(dt$pLI)) - v4_hgnc_matches - v2_hgnc_matches
  
  # 4) Ensembl fallback (v2.1.1)
  dt[is.na(pLI) & is_ensembl & !is.na(GENE),
     pLI := v2_ensembl_lookup[.SD, on = .(gene_id = GENE), pLI]]
  v2_ensembl_matches <- sum(!is.na(dt$pLI)) - v4_hgnc_matches - v2_hgnc_matches - v4_ensembl_matches
  
  no_matches <- sum(is.na(dt$pLI))
  
  # Remove helper column before output
  dt[, is_ensembl := NULL]
  
  ## -----------------------------
  ## Filtering diagnostics
  ## -----------------------------

  # Helper to print counts per set / IS_BG
  print_counts <- function(dt, label) {
  cat("\n", label, "\n", sep = "")
  tmp <- dt[, .N, by = .(set, IS_BG)]
  tmp <- tmp[order(set, IS_BG)]
  print(tmp)
  }

  # Initial counts before any filtering
  print_counts(dt, "Initial counts (before NA filtering):")

  # NA filtering

  # First filter out NA pLI genes (for both IS_BG = 0 and IS_BG = 1)
  n_before_pli <- nrow(dt)
  dt <- dt[!is.na(pLI)]
  n_after_pli <- nrow(dt)

  cat(
    "\npLI NA filtering:\n",
    "  Rows before:", n_before_pli, "\n",
    "  Rows removed:", n_before_pli - n_after_pli, "\n",
    "  Rows remaining:", n_after_pli, "\n",
    sep = " "
  )

  # Save all IS_BG=1 genes with non-NA pLI to separate file (for Bruna grant file)
  dt_all_background <- dt[IS_BG == 1]

  cat(
    "\nIS_BG=1 genes with non-NA pLI:\n",
    "  Rows:", nrow(dt_all_background), "\n",
    sep = " "
  )

  fwrite(
    dt_all_background,
    "/u/home/l/ltucker/project-bballiu/FastGxC_paper/Enrichment_figure/GTEx_all_background.csv",
    sep = ",",
    quote = FALSE,
    na = ""
  )

  # Now filter out NA gene_size genes
  n_before_genesize <- nrow(dt)
  dt <- dt[!is.na(gene_size)]
  n_after_genesize <- nrow(dt)

  cat(
    "\ngene_size NA filtering:\n",
    "  Rows before:", n_before_genesize, "\n",
    "  Rows removed:", n_before_genesize - n_after_genesize, "\n",
    "  Rows remaining:", n_after_genesize, "\n",
    sep = " "
  )

  print_counts(dt, "Counts after NA filtering:")

  # --------------------------------
  # fastgxc global foreground pool - get all fastgxc eGenes and remove them from all fastgxc background pools
  # --------------------------------
  fastgxc_sets <- c("fastgxc_shared_specific", "fastgxc_specific", "fastgxc_shared")

  fastgxc_fg_genes <- unique(
  dt[set %in% fastgxc_sets & IS_BG == 0, GENE]
  )

  cat(
  "\nfastgxc global foreground pool:\n",
  "  Unique foreground genes:", length(fastgxc_fg_genes), "\n",
  sep = " "
  )

  # Remove from fastgxc backgrounds
  n_before_fastgxc_bg <- nrow(dt)
  dt <- dt[!(set %in% fastgxc_sets & IS_BG == 1 & GENE %in% fastgxc_fg_genes)]
  n_after_fastgxc_bg <- nrow(dt)

  cat(
  "  Background genes removed from fastgxc sets:",
  n_before_fastgxc_bg - n_after_fastgxc_bg, "\n"
  )

  print_counts(dt, "Counts after fastgxc background exclusion:")

  # --------------------------------
  # CxC self-background exclusion
  # --------------------------------

  cxc_fg_genes <- dt[set == "CxC" & IS_BG == 0, GENE]

  n_before_cxc_bg <- nrow(dt)
  dt <- dt[!(set == "CxC" & IS_BG == 1 & GENE %in% cxc_fg_genes)]
  n_after_cxc_bg <- nrow(dt)

  cat(
  "\nCxC background filtering:\n",
  "  Foreground genes:", length(unique(cxc_fg_genes)), "\n",
  "  Background genes removed:",
  n_before_cxc_bg - n_after_cxc_bg, "\n",
  sep = " "
  )

  print_counts(dt, "Counts after CxC background exclusion:")

  # --------------------------------
  # For some sets the eGene pool is bigger than background pool so need to sample from eGene pool the number available in background pool
  # --------------------------------

  # Sample IS_BG = 0 to match IS_BG = 1
  sets_to_sample <- c("CxC", "fastgxc_shared_specific")
  
  set.seed(123)
  
  sampled_list <- list()
  
  for (s in sets_to_sample) {
    dt_set <- dt[set == s]
    dt_bg1 <- dt_set[IS_BG == 1]
    dt_bg0 <- dt_set[IS_BG == 0]
    
    n_bg1 <- nrow(dt_bg1)
    
    if (nrow(dt_bg0) < n_bg1) {
      stop(paste("Not enough", s, "IS_BG=0 genes to sample from"))
    }
    
    dt_bg0_sampled <- dt_bg0[sample(.N, n_bg1)]
    
    sampled_list[[length(sampled_list) + 1]] <- dt_bg1
    sampled_list[[length(sampled_list) + 1]] <- dt_bg0_sampled
  }
  
  # Keep all other sets unchanged
  dt_other <- dt[!set %in% sets_to_sample]
  
  # Recombine
  dt <- rbindlist(
    c(list(dt_other), sampled_list),
    use.names = TRUE
  )

  cat("\nWriting output to:\n", output_file, "\n")
  fwrite(dt, output_file, sep = ",", quote = FALSE, na = "")

  cat("Done.\n")
}


# Job 7!
run_gene_match_second <- function(genes_dir, output_dir, matching_seed){
  
  library(MatchIt)
  set.seed(matching_seed)
  
  print("reading in files...")
    genes <- read_csv(
    genes_dir,
    col_types = cols(.default = col_character())
    ) %>%
    mutate(
        n_SNPs    = as.numeric(n_SNPs),
        gene_size = as.numeric(gene_size),
        mean_exp  = as.numeric(mean_exp),
        IS_BG     = as.numeric(IS_BG),
        pLI       = as.numeric(pLI)
    )
  cat("\n\n")
  
  final_tib <- genes %>% group_by(set) %>% group_modify(function(tib, key){
      
      cat(paste0("\n***",key$set[1],"***\n"))
      
      # Count and report NAs before dropping (shouldn't be any because I filter above but it's a double check)
      na_counts <- tib %>%
        group_by(IS_BG) %>%
        summarize(
          n_dropped = sum(is.na(gene_size)),
          .groups = "drop"
        )
      cat("Rows dropped due to NA in gene_size:\n")
      print(na_counts)
      
      
      # Get SOI genes for this set
      SOI_genes <- tib %>% filter(IS_BG == 0) %>% pull(GENE)
      
      # Remove SOI genes from background pool (double check I do this in adding pLI code)
      tib_filtered <- tib %>% 
        filter(!(IS_BG == 1 & GENE %in% SOI_genes))
      
    annotated <- tib_filtered %>%
    mutate(IS_SOI = if_else(IS_BG == 1, 0, 1)) %>%
    dplyr::select(IS_SOI, GENE, n_SNPs, gene_size, mean_exp, pLI) # add pLI data

    print(
      annotated %>%
        group_by(IS_SOI) %>%
        summarize(
          n_genes = n(),
          mean_n_snps = mean(n_SNPs),
          mean_size   = mean(gene_size),
          mean_exp    = mean(mean_exp),
          .groups = "drop"
        ),
      n = 1000
    )

    print("matching...")
    match.obj <- matchit(
      IS_SOI ~ n_SNPs + gene_size + mean_exp,
      data   = as.data.frame(annotated),
      method = "nearest",
      ratio  = 1
    )

    matchdata <- match.data(match.obj) %>%
    as_tibble() %>%
    dplyr::select(IS_SOI, GENE, n_SNPs, gene_size, mean_exp, pLI)

    print("done with this set!")
    print(
      matchdata %>%
        group_by(IS_SOI) %>%
        summarize(
          n_genes = n(),
          mean_n_snps = mean(n_SNPs),
          mean_size   = mean(gene_size),
          mean_exp    = mean(mean_exp),
          .groups = "drop"
        ),
      n = 1000
    )
    
    return(matchdata)
  })

  print("final_tib")
  print(final_tib)

  final_tib %>%
    write_csv(output_dir)
}

# Job 9
build_cont_table <- function(input_pLI_file, output_cont_table){
  
  pli_data <- read_csv(input_pLI_file) %>%
    filter(!is.na(pLI) & pLI != "") %>%  # Remove rows with missing pLI
    mutate(pLI = as.numeric(pLI))        # Ensure pLI is numeric
  
  # Original comparisons: IS_SOI = 1 vs IS_SOI = 0 within each set
  cont_table <- pli_data %>% 
    group_by(set) %>% 
    group_modify(function(tib, key){
      
      soi <- tib %>% filter(IS_SOI == 1)
      bg  <- tib %>% filter(IS_SOI == 0)
      
      cont.E <- nrow(soi)
      cont.F <- nrow(bg)
      
      cont.A <- soi %>% filter(pLI > 0.9) %>% nrow()
      cont.B <- bg  %>% filter(pLI > 0.9) %>% nrow()
      cont.C <- cont.E - cont.A
      cont.D <- cont.F - cont.B
      
      tribble(
        ~desc, ~cont_A, ~cont_B, ~cont_C, ~cont_D, ~cont_E, ~cont_F,
        "pLI", cont.A,  cont.B,  cont.C,  cont.D,  cont.E,  cont.F
      )
      
    }) %>% 
    select(set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F)
  
  # Additional comparison 1: fastgxc_specific IS_SOI=1 vs fastgxc_shared IS_SOI=1
  fastgxc_specific_soi <- pli_data %>% filter(set == "fastgxc_specific", IS_SOI == 1)
  fastgxc_shared_soi <- pli_data %>% filter(set == "fastgxc_shared", IS_SOI == 1)
  
  cont.E_1 <- nrow(fastgxc_specific_soi)
  cont.F_1 <- nrow(fastgxc_shared_soi)
  cont.A_1 <- fastgxc_specific_soi %>% filter(pLI > 0.9) %>% nrow()
  cont.B_1 <- fastgxc_shared_soi %>% filter(pLI > 0.9) %>% nrow()
  cont.C_1 <- cont.E_1 - cont.A_1
  cont.D_1 <- cont.F_1 - cont.B_1
  
  comparison_1 <- tibble(
    set = "fastgxc_specific_vs_shared",
    desc = "pLI",
    cont_A = cont.A_1, cont_B = cont.B_1, cont_C = cont.C_1,
    cont_D = cont.D_1, cont_E = cont.E_1, cont_F = cont.F_1
  )
  
  # Additional comparison 2: fastgxc_specific IS_SOI=1 vs CxC IS_SOI=1
  cxc_soi <- pli_data %>% filter(set == "CxC", IS_SOI == 1)
  
  cont.E_2 <- nrow(fastgxc_specific_soi)
  cont.F_2 <- nrow(cxc_soi)
  cont.A_2 <- fastgxc_specific_soi %>% filter(pLI > 0.9) %>% nrow()
  cont.B_2 <- cxc_soi %>% filter(pLI > 0.9) %>% nrow()
  cont.C_2 <- cont.E_2 - cont.A_2
  cont.D_2 <- cont.F_2 - cont.B_2
  
  comparison_2 <- tibble(
    set = "fastgxc_specific_vs_CxC",
    desc = "pLI",
    cont_A = cont.A_2, cont_B = cont.B_2, cont_C = cont.C_2,
    cont_D = cont.D_2, cont_E = cont.E_2, cont_F = cont.F_2
  )
  
  # Combine all results
  cont_table <- bind_rows(cont_table, comparison_1, comparison_2)
  
  cont_table %>% write_csv(output_cont_table)
  
}


# Job 10
run_fishers_exact_test_and_fdr <- function(input_dir, output_dir){
  # run_fishers_exact_test_and_fdr(input_dir = paste0(output.dir,"Enrichment.Tissue_Agnostic.contingency_table.csv"),
  #                                output_dir = paste0(output.dir, "Enrichment.Tissue_Agnostic.fisher_results_fdr.csv")) # set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper

  cont_tables <- read_csv(input_dir) # set, desc, cont_A, cont_B, cont_C, cont_D, cont_E, cont_F
  
  fisher.all <- cont_tables %>% group_by(set, desc) %>% group_modify(function(tib, key){
    
    print(paste0("KEY: ", key))
    if(nrow(tib) != 1){
      print("MORE THAN ONE CONTINGENCY ROW PER SET AND DESC?")
      return(NA)
    }
      
    cont.matrix <- matrix(c(tib$cont_A[1], tib$cont_C[1], tib$cont_B[1], tib$cont_D[1]), nrow=2)
    print(cont.matrix)
    fisher.results <- fisher.test(cont.matrix)
    print(fisher.results)
    return(tribble(~ odds_ratio, ~ p.value, ~ conf_int.lower, ~ conf_int.upper,
                   unname(fisher.results$estimate),
                   fisher.results$p.value,
                   fisher.results$conf.int[1],
                   fisher.results$conf.int[2]))
  })
  
  # FDR correction
  fisher.all.fdr_corrected <- fisher.all %>% group_by(set) %>% 
    group_modify(function(tib, key){
      return(tib %>% mutate(p.adjusted.BH = p.adjust(p.value, method = "BH")))
    }) %>% ungroup %>% 
    select(set, desc, p.adjusted.BH, p.value, odds_ratio, conf_int.lower, conf_int.upper)

  fisher.all.fdr_corrected %>% write_csv(output_dir)
  
}


# Job 11: add results to existing enrichment files
update_enrichment_tables_sc <- function(input_dir, my_fisher_results,output_dir) {
  SETS_TO_KEEP <- c("fastgxc_shared", "fastgxc_shared_specific", "fastgxc_specific","CxC")

  # Renaming map (single-cell-specific)
  sc_rename <- c(
    "fastgxc_shared" = "HOM",
    "fastgxc_specific" = "HET",
    "fastgxc_shared_specific" = "HOM_HET_intersect",
    "CxC" = "TBT"
  )

  # Load data
  sc_fisher_results <- read.csv(input_dir)
  my_sc_fisher_results <- read.csv(my_fisher_results)

  # Filter to sets of interest
  my_sc_filtered <- my_sc_fisher_results[my_sc_fisher_results$set %in% SETS_TO_KEEP,]

  # Rename sets
  my_sc_filtered$set <- sc_rename[my_sc_filtered$set]

  # Append
  sc_fisher_results_updated <- rbind(sc_fisher_results, my_sc_filtered)

  # Write output
  write.csv(sc_fisher_results_updated, output_dir, row.names = FALSE)

}

update_enrichment_tables_bulk <- function(input_dir, my_fisher_results, output_dir,cont_table_path) {
  SETS_TO_KEEP <- c("fastgxc_shared", "fastgxc_shared_specific", "fastgxc_specific","CxC")

  # Renaming map (bulk-specific)
  bulk_rename <- c(
    "fastgxc_shared" = "HOM_only",
    "fastgxc_specific" = "HET_only",
    "fastgxc_shared_specific" = "HOM_HET_intersect",
    "CxC" = "TBT_only"
  )

  # Load data
  bulk_fisher_results <- read.csv(input_dir)
  my_bulk_fisher_results <- read.csv(my_fisher_results)
  bulk_cont_table <- read.csv(cont_table_path)

  # Filter to sets of interest
  my_bulk_filtered <- my_bulk_fisher_results[
    my_bulk_fisher_results$set %in% SETS_TO_KEEP, 
  ]

  bulk_cont_filtered <- bulk_cont_table[
    bulk_cont_table$set %in% SETS_TO_KEEP, 
  ]

  # Merge fisher results with contingency table
  my_bulk_merged <- merge(
    my_bulk_filtered,
    bulk_cont_filtered[, c("set", "desc", "cont_A", "cont_B", "cont_C", "cont_D")],
    by = c("set", "desc")
  )

  # Rename sets
  my_bulk_merged$set <- bulk_rename[my_bulk_merged$set]

  # Match existing bulk output format
  my_bulk_new_rows <- data.frame(
    set = my_bulk_merged$set,
    feature = my_bulk_merged$desc,
    odds_ratio = my_bulk_merged$odds_ratio,
    p.value = my_bulk_merged$p.value,
    conf_int.lower = my_bulk_merged$conf_int.lower,
    conf_int.upper = my_bulk_merged$conf_int.upper,
    ctA = my_bulk_merged$cont_A,
    ctB = my_bulk_merged$cont_B,
    ctC = my_bulk_merged$cont_C,
    ctD = my_bulk_merged$cont_D
  )

  # Append
  bulk_fisher_results_updated <- rbind(bulk_fisher_results, my_bulk_new_rows)

  # Write output
  write.csv(bulk_fisher_results_updated, output_dir, row.names = FALSE)

  invisible(bulk_fisher_results_updated)
}



#############################
##### Plotting Function
#############################

# Job 8 technically
plot_pLI_match_all <- function(input_pLI_file, output_file, data_type){
  library(data.table)
  library(ggplot2)
  library(RColorBrewer)

  dt <- fread(input_pLI_file)

  res <- dt[
    !is.na(set) & IS_SOI %in% c(0, 1),
    .(
      n_genes = sum(!is.na(pLI)),
      proportion_high_pLI = mean(pLI > 0.9, na.rm = TRUE)
    ),
    by = .(set, IS_SOI)
  ]

  # Label IS_SOI 
  res[, IS_SOI := factor(IS_SOI, levels = c(0, 1),
                        labels = c("IS_SOI = 0", "IS_SOI = 1"))]

  # Order Gene sets in consistent way
  set_order <- c("fastgxc_shared", "fastgxc_specific", "fastgxc_shared_specific", "CxC")
  res[, set := factor(set, levels = set_order)]

  p <- ggplot(
    res,
    aes(x = set, y = proportion_high_pLI, color = set, shape = IS_SOI)
  ) +
    geom_point(
      size = 3,
      position = position_dodge(width = 0.5)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_shape_manual(values = c("IS_SOI = 0" = 1, "IS_SOI = 1" = 16)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = paste0(data_type, " - Matched on: n_SNP, gene_length, mean_exp"),
      x = "Gene set",
      y = "Proportion of high-pLI genes (pLI > 0.9)",
      color = "Set",
      shape = ""
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )

  ggsave(output_file, p, width = 9, height = 5)

}













