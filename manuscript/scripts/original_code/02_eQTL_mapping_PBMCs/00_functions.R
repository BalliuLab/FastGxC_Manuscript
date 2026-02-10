#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Andrew Lu and Brunilda Balliu
#%%%%%%%%%%%%%%% May 27th 2021
#%%%%%%%%%%%%%%% Functions related to the FastGxE project
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%% 
#%%%%%%%%%%%%%%%%%% Functions related to the decomposition
#%%%%%%%%%%%%%%%%%% 

decompose=function(expression, shared_exp_file_name, spec_exp_file_name, genos){
  
  design = factor(expression$id)
  contexts=as.character(unique(expression$context))
  
  if (any(summary(as.factor(design)) == 1)) 
    stop("A multilevel analysis can not be performed when at least one some sample is not repeated.")
  
  X = scale(x = as.matrix(expression[,-c(1:2)]), center = T, scale = F)
  
  indiv.names = rownames(X)
  rownames(X) = as.character(design)
  
  X.mean.indiv = matrix(apply(X, 2, tapply, design, mean, na.rm = TRUE), 
                        nrow = length(unique(design)), 
                        ncol = dim(X)[2], 
                        dimnames = list(levels(as.factor(design)), colnames(X)))
  Xb = X.mean.indiv[as.character(design), ]
  Xw = X - Xb
  dimnames(Xw) = list(indiv.names, colnames(X))
  
  
  fwrite(x = data.table(t(X.mean.indiv[colnames(genos),]),keep.rownames = T) %>% {setnames(., old = "rn", new = "geneID")[]},  
         file = shared_exp_file_name, quote = F, row.names = F, 
         col.names = T, append = F, sep = '\t')
  print("Saved shared expression matrix")
  
  
  Xw = data.frame(id=expression$id,context=expression$context, Xw)
  
  for(j in 1:length(contexts)){
    
    wexp_t = data.frame(Xw[Xw$context == contexts[j],-2],row.names = 1)
    
    fwrite(x = data.table(t(wexp_t[colnames(genos),]),keep.rownames = T) %>% {setnames(., old = "rn", new = "geneID")[]}, 
           file = spec_exp_file_name[j],quote = F, row.names = F, 
           col.names = T, append = F, sep = '\t')
    
    print(paste0("Saving (specific) expression matrix for context: ",contexts[j]))
    
  }
  
}


#%%%%%%%%%%%%%%%%%% 
#%%%%%%%%%%%%%%%%%% Functions related to eQTL mapping
#%%%%%%%%%%%%%%%%%% 
if(1){

  # Modified treeQTL function to get eGenes in a multi-context experiment
get_eGenes_multi_tissue_mod = function (m_eqtl_out_dir, treeQTL_dir, tissue_names, level1 = 0.05, level2 = 0.05, level3 = 0.05, exp_suffix, cohort, num_contexts, metasoft) {
  pattern=paste0(exp_suffix,".all_pairs.txt")
  
  print(paste("Step 0.1: Computing summary statistics for each tissue"))
  m_eqtl_outfiles <- list.files(m_eqtl_out_dir, pattern = pattern, full.names = TRUE)
  if(length(m_eqtl_outfiles)!=num_contexts) stop(sprintf("Expecting %i MatrixEQTL files but got %i.", num_contexts, length(m_eqtl_outfiles)))
  
  ## NOTE: changed this such that there is no . before n_SNPs_per_gene to run for metasoft
  if(!metasoft){
    n_SNPs_per_gene_outfiles <- list.files(treeQTL_dir, pattern = paste0(cohort, ".n_SNPs_per_gene"), full.names = TRUE)
  }else{
    n_SNPs_per_gene_outfiles <- list.files(treeQTL_dir, pattern = paste0(cohort, "n_SNPs_per_gene"), full.names = TRUE)
  }
  n_SNPs_per_gene_outfiles=n_SNPs_per_gene_outfiles[!grepl(pattern = "AverageContext", x = n_SNPs_per_gene_outfiles)]
  if(length(n_SNPs_per_gene_outfiles)!=num_contexts) stop(sprintf("Expecting %i files with nr of SNPs per gene but got %i.", num_contexts,length(n_SNPs_per_gene_outfiles)))
  
  n_tissue <- length(tissue_names)
  for (i in 1:n_tissue) {
    cur_tissue_name <- tissue_names[i]
    
    print(paste("Computing summary statistics for tissue ", cur_tissue_name, sep = ""))
    n_SNPs_per_gene_this_tissue <- data.frame(fread(input = n_SNPs_per_gene_outfiles[i], header = F), stringsAsFactors = F,check.names = F)
    colnames(n_SNPs_per_gene_this_tissue)=c("family","n_tests")
    n_SNPs_per_gene_this_tissue <- n_SNPs_per_gene_this_tissue[n_SNPs_per_gene_this_tissue$n_tests > 0, ]
    
    gene_simes_cur_tissue <- get_eGenes(n_tests_per_gene = n_SNPs_per_gene_this_tissue, m_eqtl_out = m_eqtl_outfiles[i], method = "BH", level1 = 1, level2 = 1, silent = TRUE)
    gene_simes_cur_tissue <- merge(gene_simes_cur_tissue, n_SNPs_per_gene_this_tissue, by = "family", all = TRUE)
    gene_simes_cur_tissue$fam_p[which(is.na(gene_simes_cur_tissue$fam_p))] <- 1
    
    if (i == 1) {
      eGene_pvals <- gene_simes_cur_tissue[, c("family", "fam_p")]
      n_SNPs_per_gene_xT <- n_SNPs_per_gene_this_tissue
    } else {
      eGene_pvals <- merge(eGene_pvals, gene_simes_cur_tissue[, c("family", "fam_p")], by = "family", all = TRUE)
      n_SNPs_per_gene_xT <- merge(n_SNPs_per_gene_xT, n_SNPs_per_gene_this_tissue, by = "family", all = TRUE)
    }
    names(eGene_pvals)[i + 1] <- cur_tissue_name
    names(n_SNPs_per_gene_xT)[i + 1] <- cur_tissue_name
  }
  names(eGene_pvals)[1] <- "gene"
  remove(cur_tissue_name, n_SNPs_per_gene_this_tissue, gene_simes_cur_tissue)
  
  print("Step 0.2: Computing summary statistics across tissues")
  col_ind_pvals <- 2:(n_tissue + 1)
  eGene_pvals$simes_p <- apply(eGene_pvals[, col_ind_pvals], 1, TreeQTL:::get_simes_p)
  
  print("Step 1: Selecting eGenes across tissues")
  eGene_xT_qvals <- qvalue(eGene_pvals$simes_p, lambda = 0)$qvalue
  R_G <- sum(eGene_xT_qvals <= level1)
  print(paste("Number of cross-tissue eGenes = ", R_G))
  
  print("Step 2: Selecting tissues in which eGenes are active")
  q2_adj <- R_G * level2/nrow(eGene_pvals)
  ind_sel_simes <- which(eGene_xT_qvals <= level1)
  sel_eGenes_simes <- eGene_pvals[ind_sel_simes, ]
  rej_simes <- t(1 * apply(sel_eGenes_simes[, c(col_ind_pvals)], 1, TreeQTL:::qsel_by_fam, q2_adj))
  
  print("Step 3: Selecting SNPs associated to each gene in each tissue")
  sel_eGenes_simes$n_sel_tissues <- rowSums(rej_simes)
  sel_eGenes_simes$n_tested_tissues <- rowSums(!is.na(sel_eGenes_simes[, col_ind_pvals]))
  
  for (i in 1:n_tissue) {
    cur_tissue_name <- tissue_names[i]
    print(paste("Selecting SNPs for tissue", cur_tissue_name))
    sel_gene_names_this_tissue <- sel_eGenes_simes$gene[which(rej_simes[, i] == 1)]
    sel_gene_info <- n_SNPs_per_gene_xT[which(n_SNPs_per_gene_xT$family %in% sel_gene_names_this_tissue), c(1, i + 1)]
    names(sel_gene_info)[2] <- "n_tests"
    sel_gene_info <- merge(sel_gene_info, sel_eGenes_simes[, c("gene", "n_sel_tissues", "n_tested_tissues")], 
                           by.x = "family", by.y = "gene", all.x = TRUE, all.y = FALSE)
    n_sel_per_gene <- TreeQTL:::get_nsel_SNPs_per_gene_tissue_pair(sel_gene_info, cur_tissue_name, m_eqtl_outfiles[i], R_G, nrow(eGene_pvals), 
                                                                   level3 = level3)
    
    print(paste("Total number of associations for tissue", cur_tissue_name, "=", sum(n_sel_per_gene$n_sel_snp)))
    out_file_name <- paste0(treeQTL_dir,"/eAssoc_by_gene.", cur_tissue_name, exp_suffix,".txt")
    print(paste("Writing output file", out_file_name))
    get_eAssociations(data.frame(family = n_sel_per_gene$family, pval = NA, n_sel = n_sel_per_gene$n_sel_snp), NULL, 
                      m_eqtl_outfiles[i], out_file_name, by_snp = FALSE, silent = TRUE)
  }
  eGene_xT_sel <- data.frame(gene = sel_eGenes_simes$gene)
  eGene_xT_sel <- cbind(eGene_xT_sel, rej_simes)
  names(eGene_xT_sel)[2:(n_tissue + 1)] <- tissue_names
  eGene_xT_sel
}

# Modified treeQTL function to get Sime's p-values in a multi-context experiment
get_pvals_and_fam_p_mod = function(genes_by_tissue, snps_by_tissue, m_eqtl_out_dir, tissue_names, exp_suffix) {
  pattern=paste0(exp_suffix,".all_pairs.txt")
  m_eqtl_outfiles <- list.files(path = m_eqtl_out_dir, pattern = pattern, full.names = TRUE)
  n_tissue <- length(m_eqtl_outfiles)
  pvals_all_tissues <- data.table()
  
  for (i in 1:n_tissue) {
    print(paste("Reading output for tissue ", tissue_names[i], sep = ""))
    cur_data <- data.frame(fread(input = m_eqtl_outfiles[i], header = TRUE,  stringsAsFactors = FALSE))
    cur_data_table <- data.frame(cur_data %>% mutate(pair_names = paste(SNP,gene,sep = "*"))  %>% 
                                   select(pair_names,SNP,gene,p.value),check.names = F, stringsAsFactors = F)
    
    
    names(cur_data_table)[4] <- paste("p.value", i, sep = "_")
    cur_data_table <- data.table(cur_data_table)
    setkey(cur_data_table, pair_names)
    if (sum(duplicated(cur_data_table)) > 0) {
      cur_data_table <- unique(cur_data_table)
      print("Warning: Duplicate key in current output")
    }
    if (i == 1) {
      pvals_all_tissues <- cur_data_table
    } else {
      pvals_all_tissues <- merge(pvals_all_tissues, cur_data_table, by = c("pair_names", "SNP", "gene"), all = TRUE)
    }
  }
  
  print("Calculating p-values for each SNP-gene pair")
  genes_by_tissue <- data.table(genes_by_tissue)
  setkey(genes_by_tissue, gene)
  snps_by_tissue <- data.table(snps_by_tissue)
  setkey(snps_by_tissue, snp)
  pvals_all_tissues$n_tests_pair <- rowSums(genes_by_tissue[J(pvals_all_tissues$gene), 2:ncol(genes_by_tissue), with = FALSE] + 
                                              snps_by_tissue[J(pvals_all_tissues$SNP), 2:ncol(snps_by_tissue), with = FALSE] == 2)
  col_ind_ntests <- which(names(pvals_all_tissues) == "n_tests_pair")
  pvals_all_tissues$fam_p <- apply(pvals_all_tissues[, c(4:(n_tissue + 3), col_ind_ntests), with = FALSE], 1, TreeQTL:::get_simes_p_given_n_tests)
  pvals_all_tissues
}

# Modified treeQTL function to get eSNPs in a multi-context experiment
get_eSNPs_multi_tissue_mod = function(genes_by_tissue, snps_by_tissue, n_tests_per_SNP, m_eqtl_out_dir, tissue_names, level1 = 0.05, level2 = 0.05, level3 = 0.05, exp_suffix) {
  names(snps_by_tissue)[1] <- "snp"
  names(genes_by_tissue)[1] <- "gene"
  
  pvals_all_tissues <- get_pvals_and_fam_p_mod(genes_by_tissue, snps_by_tissue, m_eqtl_out_dir, tissue_names,exp_suffix)
  n_tissue <- length(tissue_names)
  print("Applying error control procedure")
  xT_meqtl_out <- data.frame(SNP = as.character(pvals_all_tissues$SNP), 
                             gene = as.character(pvals_all_tissues$gene), beta = NA, 
                             `t-stat` = NA, `p-value` = pvals_all_tissues$fam_p, FDR = NA)
  xT_meqtl_out <- xT_meqtl_out[order(xT_meqtl_out$p.value), ]
  names(xT_meqtl_out) <- c("SNP", "gene", "beta", "t-stat", "p-value", "FDR")
  m_eqtl_out_filename <- tempfile(tmpdir = getwd())
  write.table(xT_meqtl_out, m_eqtl_out_filename, quote = FALSE, row.names = FALSE)
  remove(xT_meqtl_out)
  
  eSNPs <- get_eSNPs(n_tests_per_SNP, m_eqtl_out_filename, method = "BH", level1 = level1, level2 = level2)
  print(paste("Number of eSNPs = ", nrow(eSNPs)))
  
  eAssoc_filename <- tempfile(tmpdir = getwd())
  get_eAssociations(eSNPs, n_tests_per_SNP, m_eqtl_out_filename, eAssoc_filename, by_snp = TRUE)
  level2_sel <- read.table(eAssoc_filename, header = TRUE, stringsAsFactors = FALSE)
  unlink(m_eqtl_out_filename)
  unlink(eAssoc_filename)
  R <- nrow(level2_sel)
  print(paste("Number of selected SNPxgene pairs =", R))
  level2_sel$pair_names <- paste(level2_sel$SNP, "*", level2_sel$gene, sep = "")
  sel_families <- pvals_all_tissues[which(pvals_all_tissues$pair_names %in% level2_sel$pair_names), ]
  n_sel_per_SNP <- table(sel_families$SNP)
  n_sel_per_SNP <- data.frame(SNP = names(n_sel_per_SNP), n_sel = as.numeric(n_sel_per_SNP))
  sel_families <- merge(sel_families, n_sel_per_SNP, by = "SNP", all.x = TRUE)
  names(n_tests_per_SNP) <- c("SNP", "n_genes_per_SNP")
  sel_families <- merge(sel_families, n_tests_per_SNP, by = "SNP", all.x = TRUE)
  sel_families$q_adj <- sel_families$n_sel * nrow(eSNPs) * level3/nrow(n_tests_per_SNP)/sel_families$n_genes_per_SNP
  sel_families$n_sel <- NULL
  sel_families$n_genes_per_SNP <- NULL
  max_recorded <- max(pvals_all_tissues[, 4:(n_tissue + 3), with = FALSE], na.rm = TRUE)
  if (min(sel_families$q_adj) >= max_recorded) {
    warning("Matrix eQTL output threshold may be too small for given levels")
  }
  col_ind_pvals <- which(sapply(names(sel_families), grep, fixed = TRUE, pattern = "p.value") == 1)
  col_ind_ntests <- which(names(sel_families) == "n_tests_pair")
  col_ind_qadj <- which(names(sel_families) == "q_adj")
  rej_by_fam_q <- apply(sel_families[, c(col_ind_pvals, col_ind_ntests, col_ind_qadj), with = FALSE], 1, TreeQTL:::bh_by_fam_q_adj)
  sel_pairs <- data.frame(matrix(t(rej_by_fam_q), nrow = ncol(rej_by_fam_q)))
  return_value <- data.frame(family = sel_families$pair_names, fam_p = sel_families$fam_p, as.matrix(sel_families[, col_ind_pvals, with = FALSE]) * sel_pairs)
  names(return_value) <- c("family", "fam_p", tissue_names)
  return_value$family <- as.character(return_value$family)
  return_value[is.na(return_value)] <- 0
  return_value[, 2] <- as.character(sapply(as.character(return_value$family), TreeQTL:::get_gene_name))
  return_value[, 1] <- as.character(sapply(as.character(return_value$family), TreeQTL:::get_snp_name))
  names(return_value)[1:2] <- c("SNP", "gene")
  return_value
}
}

#%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%% Functions related to the manuscript figures
#%%%%%%%%%%%%%%%%%%
if(1){
scientific_10 <- function(x) parse(text=gsub("e\\+*", "%*%10^", scales::scientific_format()(x))) 

# Function to make table with tissue colors and export to PNG
custom_tis_color <- function(...){
  formatter("span", style = function(x){
    print(x)
    
    picked_cols = x %>% as_tibble() %>% 
      rename(tissue = value) %>% 
      inner_join(gtex_colors.tib, by="tissue") %>% 
      pull(color_hex)
    
    style(display = "block",
          padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = x) 
  })}
export_formattable <- function(f, file, width = "100%", height = NULL, background = "white", delay = 0.2) {
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

# Function to plot eQTL examples
plot_eqtl_example = function(plot_gene, plot_snp, y_axis_tissue_lab=T){
  plttt = grabbed %>%
    filter(gene == plot_gene) %>% 
    filter(snp == plot_snp) %>% 
    mutate(tissue = fct_reorder(tissue, desc(tissue))) %>% 
    mutate(tissue = fct_relevel(tissue, "AverageTissue", after = Inf)) %>% 
    mutate(method = fct_relevel(method, "CxC")) %>% 
    mutate(is_shared = fct_relevel(is_shared, "shared")) %>%
    ggplot() +
    geom_point(aes(x = beta, y = tissue, color = tissue, shape = is_sign), size=3) +
    facet_wrap(~method) +
    ggtitle(paste0(plot_gene," - ",plot_snp)) +
    geom_vline(xintercept=c(0), linetype="dotted") +
    geom_errorbarh(aes(xmax = upper_int, xmin = lower_int, y = tissue), size = .5, height = .2, color = "gray50") +
    gtex_colors_scale +
    scale_shape_manual(values=c("significant" = 16, "not significant" = 1), 
                       labels=c("significant" = "< 0.05", "not significant" = "> 0.05"),
                       name="FDR p-value") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 20),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size=15),
          strip.background.y = element_blank(),
          strip.text.y = element_blank(),
          strip.text.x = element_text(size = 12),
          legend.spacing.x = unit(0.001, 'cm'),
          legend.position = "none")+
    xlab("eQTL effect size") + ylab("") +
    scale_y_discrete(labels=gtex_abbrev.vec)
  
  if(!y_axis_tissue_lab) plttt = plttt + theme(axis.title.y=element_blank(),
                                               axis.text.y=element_blank(),
                                               axis.ticks.y=element_blank())
  
  return(plttt)
}
}


#### function to create metasoft input files
parse_sumstats_across_contexts <-
  function(dir_qtlsumstats = NULL,
           extract_value = "pvalue",
           filelist_qtlsumstat = NULL,
           contexts_vec = NULL,
           fileprefix_out = NULL,
           sumstat_format = "MatrixQTL",
           filter_tests_mincontexts = 1,
           index_QTL_vs_genesnp = F,
           return_data = F,
           exprmap = NULL,
           run_by_chrom = F, chroms_to_loop = paste0("chr", 1:22)) {

    if (!xor(is.null(dir_qtlsumstats), is.null(filelist_qtlsumstat))) {
      stop("Error: specify either dir_qtlsumstats or filelist_qtlsumstat.")
    }

    if (!extract_value %in% c("pvalue", "beta", "se", "betase")) {
      stop("Error: extract_value must be = 'pvalue' (TreeQTL/TreeBH),
           'beta' (mashr/metasoft), 'se' (mashr/metasoft), or 'betase'.")
    }

    if (sumstat_format == "MatrixQTL") {
      print("attempting to join summary stats across contexts,")
      print("assuming input files have columns named gene, snp, and p-value.")

      if (run_by_chrom & is.null(exprmap)) {
        quit("ERROR: because MatixQTL doesn't export chromosome name, must supply genomap & exprmap.")
      }
    } else {
      quit("ERROR: only MatrixQTL currently supported.")
    }

    if (is.null(fileprefix_out)) {
      fileprefix_out <-
        paste0(dirname(dir_qtlsumstats), "/sumstats/stats")
    }

    dirout <- dirname(fileprefix_out)
    print(paste0("writing to ", dirout))
    if (!dir.exists(dirout)) {
      warning("Can't write to the 'fileout_prefix' specified. Creating directories:")
      print(dirout)
      dir.create(dirout, recursive = T)
    }

    if (is.null(filelist_qtlsumstat)) {
      filelist_qtlsumstat <-
        list.files(dir_qtlsumstats, full.names = TRUE)


      # [!!!] include shared?
      filelist_qtlsumstat <- filelist_qtlsumstat[
        grepl("shared.tsv$|specific.tsv$", filelist_qtlsumstat)
      ]
    }

    if (length(filelist_qtlsumstat) == 0) {
      stop("no summary stat files found. check specificiation of 'dir_qtlsumstats' or 'filelist_qtlsumstat'.")
    }

    # subset as needed?
    # filelist_qtlsumstat <- filelist_qtlsumstat[c(1, 3, 9)]

    if (is.null(contexts_vec)) {
      warning("contexts_vec not specified, attempting to infer context names:")
      contexts_vec <- sapply(
        filelist_qtlsumstat,
        function(f) {
          gsub("qtl_|_specific.tsv", "", basename(f))
        }
      )
      contexts_vec[contexts_vec == "shared.tsv"] <- "shared"
      warning(paste0(contexts_vec, collapse = "; "))
    }

    Ncontexts <- length(contexts_vec)
    print(paste0(Ncontexts, " contexts to summarize."))



    if (extract_value == "pvalue") { target_column <- "p-value" }
    if (extract_value == "beta") { target_column <- "beta" }
    if (extract_value %in% c("se", "betase")) { target_column <- c("t-stat", "beta") }

    # helper function for above
    # prob should unnest from this fxn and specify exprmap
    read_sumstat_matrixeqtl <- function(contextindex,
                                        joinkeys = c("gene", "SNP"),
                                        targetchrom = NULL,
                                        exprmap = NULL) {

      if (xor(is.null(targetchrom), is.null(exprmap))) {
        stop("ERROR: if aggregating by chromosome, need to specify exprmap for MatrixQTL.")
      }

      Nmax <- Inf # [!!!] change Nmax just for testing
      dt_qtlsummary <- data.table::fread(filelist_qtlsumstat[contextindex],
                                         select = c("gene", "SNP", target_column), nrows = Nmax
      )

      ###  COMMENTED THIS OUT TO SEE IF SNP KEEPS THE ALLELES
      #dt_qtlsummary$SNP = sapply(dt_qtlsummary$SNP, function(x) paste(strsplit(x, ":")[[1]][1:2], collapse = ':'))
      dt_qtlsummary = dt_qtlsummary %>% group_by(gene, SNP) %>% filter(n() == 1) %>% ungroup %>% as.data.table()

      if (extract_value == "se") {
        dt_qtlsummary[ , se := beta / `t-stat` ]
        dt_qtlsummary$beta <- NULL
        dt_qtlsummary$`t-stat` <- NULL
        target_column <- "se"
      }

      if (extract_value == "betase") {
        dt_qtlsummary[ , se := beta / `t-stat` ]
        dt_qtlsummary$`t-stat` <- NULL
        target_column <- "se"
      }

      print(paste0("processing: ", filelist_qtlsumstat[contextindex]))
      print(paste0("context: ", contexts_vec[contextindex]))

      if (!is.null(targetchrom)) {
        print(paste0("chr: ", targetchrom))
        # little inefficient [!!] but may need to do for mem reasons
        # also, matrixeqtl does not output chrom;
        dt_qtlsummary <- dt_qtlsummary[gene %in% exprmap[chr == targetchrom]$geneid]
      }


      data.table::setkeyv(dt_qtlsummary, joinkeys)
      dt_qtlsummary[, context := contexts_vec[contextindex]]
      return(dt_qtlsummary)
    }



    if (run_by_chrom) {

      if (typeof(exprmap) == "character" & file.exists(exprmap)) {

        exprmap <- data.table::fread(
          input = exprmap,
          header = T, stringsAsFactors = F
        )[, 1:4]

      }

      # looping through chromosomes
      # can do dt_qtlsummary[chr==targetchrom] for fastqtl, with added select="chr"
      lapply(
        chroms_to_loop,
        function(chromin) {
          big_summary <- data.table::rbindlist(
            lapply(1:length(filelist_qtlsumstat),
                   read_sumstat_matrixeqtl, targetchrom = chromin, exprmap = exprmap)
          )
          fileout_final <- paste0(fileprefix_out, "_", extract_value, "_", chromin, ".tsv")

          if (nrow(big_summary) == 0) {
            warning(paste0("no results found for ", chromin, ". skipping. (was chroms_to_loop specified correctly?)"))
            return(chromin)
          }


          if (extract_value == "se") { target_column <- "se" }
          if (extract_value == "betase") {
            target_column <- c("beta", "se")
          }

          big_summary <- data.table::dcast(
            big_summary, gene + SNP ~ context,
            value.var = target_column
          )

          if (extract_value == "betase") {
            column_order <- expand.grid(type = c("beta", "se"), context = contexts_vec)
            column_order <- c("gene", "SNP", apply(column_order, 1, function(x) { paste0(x[1], "_", x[2]) } ))
            big_summary <- big_summary[ , ..column_order]
          }
          if (index_QTL_vs_genesnp) {
            big_summary[ , gene := paste0(gene, ":", SNP)]
            big_summary[ , SNP := NULL]
            names(big_summary)[1] <- "QTL"
          }

          # since double the columns, increase threshold
          if (extract_value == "betase") { filter_tests_mincontexts <- filter_tests_mincontexts * 2 }

          # [!!!] filter rows in min # contexts = filter_tests_mincontexts
          num_contexts_present <- rowSums(is.na(big_summary[ , -(1:2)]))
          big_summary <- big_summary[num_contexts_present >= filter_tests_mincontexts, ]
          print(paste0("writing '", fileout_final, "' ..."))
          data.table::fwrite(big_summary, fileout_final, sep = "\t", na = "NA", quote = F)
          return(fileout_final)
        }
      )
    }


    if (!run_by_chrom) { # if not by chromosome
      big_summary <- data.table::rbindlist(
        lapply(1:length(filelist_qtlsumstat), read_sumstat_matrixeqtl)
      )
      fileout_final <- paste0(fileprefix_out, "_", extract_value, ".tsv")

      if (extract_value == "se") { target_column <- "se" }
      if (extract_value == "betase") { target_column <- c("beta", "se") }
      big_summary <- data.table::dcast(
        big_summary, gene + SNP ~ context,
        value.var = target_column
      )
      if (extract_value == "betase") {
        column_order <- expand.grid(type = c("beta", "se"), context = contexts_vec)
        column_order <- c("gene", "SNP", apply(column_order, 1, function(x) { paste0(x[1], "_", x[2]) } ))
        big_summary <- big_summary[ , ..column_order]
      }

      if (index_QTL_vs_genesnp) {
        big_summary[ , gene := paste0(gene, ":", SNP)]
        big_summary[ , SNP := NULL]
        names(big_summary)[1] <- "QTL"
      }
      # since double the columns, increase threshold
      if (extract_value == "betase") { filter_tests_mincontexts <- filter_tests_mincontexts * 2 }

      print(paste0("writing ", fileout_final, " ..."))
      data.table::fwrite(big_summary, fileout_final, sep = "\t", na = "NA", quote = F, col.names = F)

    }

    print("finished parsing summary statistics.")
    if (return_data) { return(big_summary) } else { return(NULL) }
  }



