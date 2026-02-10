library(tidyverse)

bruna_dir="/u/project/zaitlenlab/bballiu/FastGxE/"

# load gwas catalog data
if(1){
    GWAS_dir=paste0(bruna_dir, "results/eQTL_mapping/GWAS_enrichment/")
    gwas_catalog=read_tsv(paste0(GWAS_dir,"gwas_catalog_v1.0.2-associations_e100_r2020-06-17.tsv"), col_types = cols()) %>%
        separate_rows(SNPS,sep=";\\s+") %>% # split up rows that have multiple SNPs into their own rows 
        rename(DISEASE_TRAIT = "DISEASE/TRAIT", PVAL = "P-VALUE", OR_BETA = "OR or BETA",SNP=SNPS) %>%
        select(PARENT_TRAIT, DISEASE_TRAIT, MAPPED_TRAIT, SNP, CHR_ID, CHR_POS, CONTEXT, INTERGENIC, PVAL, OR_BETA) 
    cat("\n\n\n")
    ##### remember to only get distinct!!!
}

# cont table
#                       is GWAS SNP
#                        YES    NO
# is eQTL       YES       A     B
# SNP           NO        C     D
# GWAS: either for all dieases, or grouped by parent term
# eQTL sets: HOM_all, HET_all, FastGxE_all, TBT_all, 
# simple: all unique...HOM/HET/FastGxE(HOM+HET)/TBT
# or some combo: HOM+all HET SNPs found in only one tissue

#### question 1: are FastGxE SNPs (across all tissues) more enriched in GWAS hits (across all parent traits) than TBT? 
# - conclusion: FastGxE snps are more likely to be GWAS hits than TBT across all parent trait categories

# good code
if(1){
    
    # get tested SNPs
    tested_snps_all=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Feature_Enrichment/Tissue_Agnostic/Enrichment.Tissue_Agnostic.SNPs_tested.csv"), col_types = cols()) %>%
        select(set, SNP) # set: HET_TBT, HOM
    
    tested_snps_all=bind_rows(
        tested_snps_all %>% filter(set == "HET_TBT") %>% select(SNP) %>% distinct %>% mutate(set = "TBT"),
        #tested_snps_all %>% filter(set == "HOM") %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_HOM"),
        #tested_snps_all %>% filter(set == "HET_TBT") %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_HET"),
        tested_snps_all %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_all")
    )
    print(tested_snps_all %>% group_by(set) %>% summarize(n_snps = n()))
    
    fastgxe_eqtls=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.FastGxE.eSNPs.50Tissues.csv"), col_types = cols())

    results = tested_snps_all %>% group_by(set) %>% group_modify(function(tib, key){
        # tib: SNP
        s=key$set[1]
        print(s)
        
        if(s == "TBT"){
            eqtls=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.TbT.eSNPs.49Tissues.csv"), col_types = cols()) %>%
                select(SNP) %>% distinct %>% 
                mutate(is_eqtl = 1) %>% 
                select(SNP, is_eqtl)
        }
        else if(s == "FastGxE_all"){
            eqtls=fastgxe_eqtls %>%
                select(SNP) %>% distinct %>% 
                mutate(is_eqtl = 1) %>% 
                select(SNP, is_eqtl)
        }
        else if(s == "FastGxE_HET"){
            eqtls=fastgxe_eqtls %>% 
                mutate(is_eqtl = case_when(rowSums(.[2:50]) == 0 ~ 0,
                                          rowSums(.[2:50]) >= 1 ~ 1)) %>%
                filter(is_eqtl == 1) %>% 
                select(SNP, is_eqtl)
        }
        else if(s == "FastGxE_HOM"){
            eqtls=fastgxe_eqtls %>% 
                rename(is_eqtl = AverageTissue) %>% 
                filter(is_eqtl == 1) %>% 
                select(SNP, is_eqtl)
        }

        # all tested SNPs
        annotated = tib %>% 
            # add eqtls
            left_join(eqtls, by=c("SNP")) %>% select(SNP, is_eqtl) %>% mutate(is_eqtl = replace_na(is_eqtl,0)) %>% 
            # add gwas
            left_join((gwas_catalog[,c("SNP")] %>% distinct %>% mutate(is_GWAS = 1)), by = c("SNP")) %>% select(SNP,is_eqtl,is_GWAS) %>% mutate(is_GWAS = replace_na(is_GWAS, 0)) %>% 
            select(SNP,is_eqtl,is_GWAS)
            
        tab = table(annotated$is_eqtl,annotated$is_GWAS)
        print(tab)
        
        FET=fisher.test(x = tab)[c("estimate","conf.int","p.value")]
        print(FET)
        
        temp_ret = tribble(
            ~ OR,
            ~ conf_int1,
            ~ conf_int2,
            ~ pval,
            unname(FET$estimate),
            FET$conf.int[1],
            FET$conf.int[2],
            FET$p.value
        )
        print(temp_ret)
        return(temp_ret)

    })

    print(results)
    
    # results %>% 
    #     write_csv("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/GWAS_enrichment_results.across_all_tissues.across_all_traits.csv")
    
}

#### question 2: are FastGxE SNPs (across all tissues) more enriched in GWAS hits (in each parent/mapped trait) than TBT?
# - conclusion: FastGxE snps are more likely to be GWAS hits than TBT in (almost all?) (all?) parent trait categories 
if(0){
    
    # get tested SNPs
    tested_snps_all=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Feature_Enrichment/Tissue_Agnostic/Enrichment.Tissue_Agnostic.SNPs_tested.csv"), col_types = cols()) %>%
        select(set, SNP) # set: HET_TBT, HOM
    
    tested_snps_all=bind_rows(
        tested_snps_all %>% filter(set == "HET_TBT") %>% select(SNP) %>% distinct %>% mutate(set = "TBT"),
        #tested_snps_all %>% filter(set == "HOM") %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_HOM"),
        #tested_snps_all %>% filter(set == "HET_TBT") %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_HET"),
        tested_snps_all %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_all")
    )

    fastgxe_eqtls=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.FastGxE.eSNPs.50Tissues.csv"), col_types = cols())

    final_results = tested_snps_all %>% group_by(set) %>% group_modify(function(tib, key){
        # tib: SNP
        s=key$set[1]
        print(s)
        
        if(s == "TBT"){
            eqtls=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.TbT.eSNPs.49Tissues.csv"), col_types = cols()) %>%
                select(SNP) %>% distinct %>% 
                mutate(is_eqtl = 1) %>% 
                select(SNP, is_eqtl)
        }
        else if(s == "FastGxE_all"){
            eqtls=fastgxe_eqtls %>%
                select(SNP) %>% distinct %>% 
                mutate(is_eqtl = 1) %>% 
                select(SNP, is_eqtl)
        }
        else if(s == "FastGxE_HET"){
            eqtls=fastgxe_eqtls %>% 
                mutate(is_eqtl = case_when(rowSums(.[2:50]) == 0 ~ 0,
                                           rowSums(.[2:50]) >= 1 ~ 1)) %>%
                filter(is_eqtl == 1) %>% 
                select(SNP, is_eqtl)
        }
        else if(s == "FastGxE_HOM"){
            eqtls=fastgxe_eqtls %>% 
                rename(is_eqtl = AverageTissue) %>% 
                filter(is_eqtl == 1) %>% 
                select(SNP, is_eqtl)
        }

        results_by_trait = gwas_catalog %>% select(PARENT_TRAIT, SNP) %>% ###### parent trait or ????
            distinct %>% group_by(PARENT_TRAIT) %>% group_modify(function(tib2, key2){
            
                print(paste0("num SNPs for *",key2$PARENT_TRAIT[1],"* parent trait: ",nrow(tib2 %>% distinct)))
                
                #tib2:SNP
                annotated = tib %>% 
                    # add eqtls
                    left_join(eqtls, by=c("SNP")) %>% select(SNP, is_eqtl) %>% mutate(is_eqtl = replace_na(is_eqtl,0)) %>% 
                    # add gwas
                    left_join((tib2 %>% distinct %>% mutate(is_GWAS = 1)), by = c("SNP")) %>% select(SNP,is_eqtl,is_GWAS) %>% mutate(is_GWAS = replace_na(is_GWAS, 0)) %>% 
                    select(SNP,is_eqtl,is_GWAS)
                
                tab = table(annotated$is_eqtl,annotated$is_GWAS)
                FET=fisher.test(x = tab)[c("estimate","conf.int","p.value")]
                temp_ret = tribble(
                    ~OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4,
                    unname(FET$estimate), FET$conf.int[1], FET$conf.int[2], FET$p.value, c(tab)[1], c(tab)[2], c(tab)[3], c(tab)[4]
                )
                print(temp_ret)
                return(temp_ret)
            
        })
        
        print("results_by_trait")
        print(results_by_trait)
        return(results_by_trait)
        
    })
    
    print("final_results")
    print(final_results)
    
    final_results %>% 
        write_csv("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/GWAS_enrichment_results.across_all_tissues.in_each_parent_trait.WITH_CONT_TABLE.csv")
    
}

#### question 3: enrichment of tissue-specific SNPs in GWAS SNPs (by parent/mapped trait) [for, for]
####%%%%%%% no matching
if(0){
    args=commandArgs(TRUE)
    tissue=args[1]
    print(paste0("running tissue: ",tissue))
    
    eSNPs = list(FastGxE = read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.FastGxE.eSNPs.50Tissues.csv"), col_types = cols()), 
                 Tbt = read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.TbT.eSNPs.49Tissues.csv"), col_types = cols()))
    
    final_results = tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~ OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4)

    if(tissue=="AverageTissue"){
        tested_SNPs <- read_tsv(paste0(bruna_dir,"results/eQTL_mapping/MatrixEQTL/AverageTissue.v8.EUR.normalized_and_residualized_expression_homogeneous.all_pairs.txt"), col_types = cols()) %>% 
            select(SNP) %>% distinct
        methods=c("FastGxE") 
    } else {
        tested_SNPs <- read_tsv(paste0(bruna_dir,"results/eQTL_mapping/MatrixEQTL/",tissue,".v8.EUR.normalized_and_residualized_expression_heterogeneous.all_pairs.txt"), col_types = cols()) %>% 
            select(SNP) %>% distinct
        methods=c("FastGxE","Tbt")
    }
    
    for (method in methods){
        print(paste0("running method: ", method))
        
        for (found_in in c("single","each")){
            print(paste0("running found_in: ", found_in))
            
            if(found_in == "single"){
                df=eSNPs[[method]]
                tissue_method_eSNPs = df[(df[[tissue]]==1) & (rowSums(df[,!(names(df) %in% c("SNP",tissue))]) == 0),] %>% 
                    select(SNP) %>% distinct %>% 
                    mutate(is_eqtl = 1) %>% select(SNP, is_eqtl)
            }
            if(found_in == "each"){
                tissue_method_eSNPs = 
                    eSNPs[[method]] %>% 
                    select(SNP, all_of(tissue)) %>% 
                    filter(.[[tissue]] == 1) %>% 
                    select(SNP) %>% distinct %>% 
                    mutate(is_eqtl = 1) %>% select(SNP, is_eqtl)
            }
            for (trait_cat in c("MAPPED_TRAIT","PARENT_TRAIT")){
                print(paste0("running trait_cat: ", trait_cat))
                
                for (trait in unique(gwas_catalog[[trait_cat]])){
                    print(paste0("trait: ", trait))
                    
                    gwas_snps = gwas_catalog %>% filter(.[[trait_cat]] == trait) %>% select(SNP) %>% distinct %>% 
                        mutate(is_gwas = 1) %>% select(SNP, is_gwas)
                    
                    annotated = tested_SNPs %>% 
                        left_join(tissue_method_eSNPs, by = c("SNP")) %>% mutate(is_eqtl = replace_na(is_eqtl,0)) %>% 
                        left_join(gwas_snps, by = c("SNP")) %>% mutate(is_gwas = replace_na(is_gwas,0)) %>% 
                        select(SNP, is_eqtl, is_gwas)
                    
                    tab=table(annotated$is_eqtl,annotated$is_gwas)

                    tryCatch({
                        FET=fisher.test(x = tab)[c("estimate","conf.int","p.value")]
                        temp_results<<-tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4,
                                               tissue, method, found_in, trait_cat, trait, nrow(gwas_snps), "yes",unname(FET$estimate), FET$conf.int[1], FET$conf.int[2], FET$p.value, c(tab)[1], c(tab)[2], c(tab)[3], c(tab)[4])
                    }, error = function(e){
                        #print("no results")
                        temp_results<<-tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4,
                                               tissue, method, found_in, trait_cat, trait, nrow(gwas_snps), "no", 0, 0, 0, 0, 0, 0, 0, 0)
                    })
                    
                    #print(temp_results)
                    final_results = bind_rows(final_results,temp_results)
                }
            }
            cat("\n\n")
        }
        cat("\n\n\n")
    }
    ti_output=paste0("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/GWAS_enrichment_results.",
                     tissue, 
                     ".single_each.FastGxE_TBT.by_mapped_and_parent_gwas_traits.csv")
    cat("\n\n\n\n")
    print(paste0("final output will be saved to: ",ti_output))
    write_csv(final_results, ti_output)
}

####%%%%%%% with matching
if(0){
    args=commandArgs(TRUE)
    ti=args[1]
    print(paste0("running tissue: ",ti))
    
    # set,tissue,IS_SOI,SNP
    snps_matched=read_csv(paste0(bruna_dir,"results/eQTL_mapping/GWAS_enrichment/make_matched_snp_sets/final_merged.additional_SNP_sets.old_sets.with_matched_bg.all_tissues_merged.csv"), col_types = cols())
    snps_matched=snps_matched %>% mutate(set = case_when(set == "HOM" ~ "HOM.all", TRUE ~ set))
    
    # HET.each_tissue, HET.four_tissues, HET.single_tissue
    # TBT.each_tissue, TBT.four_tissues, TBT.single_tissue
    # HOM.all
    
    final_results = tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~ OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4)
    
    if(ti == "AverageTissue"){
        vec.methods = c("HOM") 
        vec.found_in = c("all")
        
    } else{
        vec.methods = c("HET","TBT")
        vec.found_in = c("each_tissue","single_tissue", "four_tissues")
    } 
    
    for (method in vec.methods){
        
        print(paste0("running method: ", method))
        
        for (found_in in vec.found_in){
            
            print(paste0("running found_in: ", found_in))
            
            tissue_method_eSNPs = snps_matched %>% 
                filter(set == paste0(method,".",found_in), tissue == ti) %>% 
                rename(is_eqtl = IS_SOI) %>% 
                select(SNP, is_eqtl)
            
            for (trait_cat in c("MAPPED_TRAIT","PARENT_TRAIT")){
                print(paste0("running trait_cat: ", trait_cat))
                
                for (trait in unique(gwas_catalog[[trait_cat]])){
                    print(paste0("trait: ", trait))
                    
                    gwas_snps = gwas_catalog %>% filter(.[[trait_cat]] == trait) %>% select(SNP) %>% distinct %>% 
                        mutate(is_gwas = 1) %>% select(SNP, is_gwas)
                    
                    annotated = tissue_method_eSNPs %>% 
                        left_join(gwas_snps, by = c("SNP")) %>% mutate(is_gwas = replace_na(is_gwas,0)) %>% 
                        select(SNP, is_eqtl, is_gwas)
                    
                    tab=table(annotated$is_eqtl,annotated$is_gwas)

                    tryCatch({
                        #### if num gwas snps with eqtls or num gwas snps with eqtls = 0, then add 1 so that OR wouldnt be inf
                        if(tab[1,2] == 0 | tab[2,2] == 0) tab = tab + 1
                        
                        FET=fisher.test(x = tab)[c("estimate","conf.int","p.value")]
                        temp_results<<-tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4,
                                               ti, method, found_in, trait_cat, trait, nrow(gwas_snps), "yes",unname(FET$estimate), FET$conf.int[1], FET$conf.int[2], FET$p.value, c(tab)[1], c(tab)[2], c(tab)[3], c(tab)[4])
                        #print(temp_results)
                    }, error = function(e){
                        temp_results<<-tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4,
                                               ti, method, found_in, trait_cat, trait, nrow(gwas_snps), "no", 0, 0, 0, 0, 0, 0, 0, 0)
                        #print("no results")
                    })
                    
                    final_results = bind_rows(final_results,temp_results)
                }
            }
            cat("\n\n")
        }
        cat("\n\n\n")
    }
    
    cat("\n\n\n\n")
    
    ti_output=paste0("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/GWAS_enrichment_results.",
                     ti, 
                     ".SNPs_MAF_matched.with_additional_SNP_sets.FastGxE_TBT.by_mapped_and_parent_gwas_traits.csv")
    
    print(paste0("final output will be saved to: ",ti_output))
    write_csv(final_results, ti_output)
}

# FINAL: combine results across all tissues after "with matching"
if(0){
    gwas_enr_results = "/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/matching_no_inf_withnewsets/"
    results_pat = ".SNPs_MAF_matched.with_additional_SNP_sets.FastGxE_TBT.by_mapped_and_parent_gwas_traits.csv"
    
    raw_gwas = tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~ OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4)
    
    for(f in list.files(path = gwas_enr_results, pattern = results_pat, full.names = T)){
        print(f)
        f_tib = read_csv(f,col_types = cols()) %>% filter(has_results=="yes") %>% select(-has_results)
        raw_gwas=bind_rows(raw_gwas, f_tib)
    }
    
    print("raw_gwas")
    print(raw_gwas)

    raw_gwas %>% 
        select(met, f_in, tis, cat, tra, n_gwas_snps, OR, conf_int1, conf_int2, pval, ct1, ct2, ct3, ct4) %>% 
        write_csv("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/matching_no_inf_withnewsets/Final_Merged.GWAS_enrichment_results.All_Tissues.SNPs_MAF_matched.with_additional_SNP_sets.FastGxE_TBT.by_mapped_and_parent_gwas_traits.csv")
}

#%% make additional SNP sets with matched BG SNPs - include HOM SNPs
if(0){
    
    # make new SNP sets
    if(0){
        bruna_project.dir <- "/u/project/zaitlenlab/bballiu/FastGxE/"
        eqtls.all <- read_csv(paste0(bruna_project.dir, "results/eQTL_mapping/Github_Data/eAssociations.v8.EUR.all_tissues.residualized_exp_types.txt")) %>% #exp_type,tissue,gene,snp
            rename(SNP = snp) %>% 
            select(exp_type, tissue, SNP)          
        
        het <- eqtls.all %>% filter(exp_type == "normalized_and_residualized_expression_heterogeneous") %>% select(tissue, SNP) %>% distinct
        tbt <- eqtls.all %>% filter(exp_type == "normalized_and_residualized_expression") %>% select(tissue, SNP) %>% distinct
        hom <- eqtls.all %>% filter(exp_type == "normalized_and_residualized_expression_homogeneous") %>% select(tissue, SNP) %>% distinct
        
        rm(eqtls.all)
        gc()
        
        writeLines("*** making snp sets... ***\n")
        
        bind_rows(
            # snps with het effects in <= 4 tissues
            het %>%
                select(SNP, tissue) %>% distinct %>%
                group_by(SNP) %>% filter(n() <= 4) %>% ungroup %>%
                mutate(set = "HET.four_tissues") %>%
                select(set, tissue, SNP),
            
            # snps with tbt effects in <= 4 tissues
            tbt %>% 
                select(SNP, tissue) %>% distinct %>%
                group_by(SNP) %>% filter(n() <= 4) %>% ungroup %>%
                mutate(set = "TBT.four_tissues") %>% 
                select(set, tissue, SNP),
            
            # snps with hom effects
            hom %>% 
                select(tissue, SNP) %>% distinct %>%
                mutate(set = "HOM") %>% 
                select(set, tissue, SNP)
            
        ) %>% 
            write_csv("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/make_matched_snp_sets/additional_SNP_sets.csv") #set, tissue, SNP
        
    }
    
    # add matched SNPs
    if(0){
        add_matched_snps_per_tissue <- function(tissue_run, snp_sets_dir, output_dir){
            print(paste0("***** ",tissue_run," ******"))
            # add_matched_snps_per_tissue(tissue = args[1],
            #                             snp_sets_dir = snp_sets.dir, 
            #                             output_dir = soi_bg.dir)
            
            
            library(MatchIt)
            
            print("reading in files...")
            maf <- read_csv("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/Feature_Enrichment/Tissue_Agnostic/MatchingInfo.SNPs_MAF.AverageTissue.csv") %>% 
                rename(MAF_avg = AverageTissue) %>% select(SNP, MAF_avg)
            #SNP	gene	beta	t-stat	p-value	FDR
            
            # tested <- read_tsv(paste0("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/MatrixEQTL/",
            #                           tissue_run, ".v8.EUR.normalized_and_residualized_expression_heterogeneous.all_pairs.txt"), col_types = cols()) %>% select(SNP) %>% distinct
            
            agnostic_tested = read_csv("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/Feature_Enrichment/Tissue_Agnostic/Enrichment.Tissue_Agnostic.SNPs_tested.csv") # set,SNP
         
            snp_sets <- read_csv(snp_sets_dir) %>% # tissue, set, SNP # sets = HET.four_tissues, TBT.four_tissues, HOM
                filter(tissue == tissue_run) %>% select(set, SNP)
            
            final <- snp_sets %>% group_by(set) %>% group_modify(function(tib, key){
                
                cat(paste0("\n\nrunning ",key$set[1],"...\n"))
                
                if(key$set[1] == "HET.four_tissues" | key$set[1] == "TBT.four_tissues"){
                    tested = agnostic_tested %>% filter(set == "HET_TBT") %>% select(SNP) %>% distinct
                } else if(key$set[1] == "HOM"){
                    tested = agnostic_tested %>% filter(set == "HOM") %>% select(SNP) %>% distinct
                } 
                
                soi_temp <- tib %>% mutate(IS_SOI = 1) %>% select(IS_SOI, SNP) %>% distinct
                joined <- right_join(soi_temp, tested, by = c("SNP")) %>% 
                    mutate(IS_SOI = replace_na(IS_SOI, 0)) %>% 
                    select(IS_SOI, SNP)
                
                print("adding random n...")
                # add new column (piece_n) that has random numbers for piece by piece matching
                #num_pieces <- 50
                print(joined %>% group_by(IS_SOI) %>% summarize(n_SNPs = n()))
                n_rows_soi <- nrow(joined %>% filter(IS_SOI == 1))
                n_rows_bg  <- nrow(joined %>% filter(IS_SOI == 0))
                # pick smaller of two number, and then how many times to split up to get 5000 SNPs per match
                if(min(n_rows_soi,n_rows_bg) > 5000) {
                    num_pieces = round(min(n_rows_soi,n_rows_bg) / 5000)
                }else {
                    num_pieces = 1
                }
                print(paste0("num pieces split = ",num_pieces))
                
                joined <- joined %>% group_by(IS_SOI) %>% group_modify(function(tt, kk){
                    tt$piece_n <- sample(x = 1:num_pieces, size = nrow(tt), replace = TRUE)
                    return(tt)
                }) %>% select(IS_SOI, SNP, piece_n)
                
                n_results <- tribble(~IS_SOI, ~SNP, ~MAF_avg)
                
                print("starting matching loop...")
                for(n in seq(num_pieces)){
                    cat(paste0(n,"/",num_pieces," pieces matched\n"))
                    
                    joined.n <- joined %>% filter(piece_n == n) %>% select(IS_SOI, SNP)
                    
                    annotated <- inner_join(joined.n, maf, by = c("SNP")) %>% select(IS_SOI, SNP, MAF_avg)
                    
                    print(annotated %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
                    
                    print("matching...")
                    match.obj <- matchit(IS_SOI ~ MAF_avg, 
                                         data = as.data.frame(annotated), 
                                         method="nearest", 
                                         ratio=1)
                    
                    matchdata <- match.data(match.obj) %>% as_tibble %>% select(IS_SOI, SNP, MAF_avg)
                    print(matchdata %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
                    
                    n_results <- bind_rows(n_results, matchdata) #IS_SOI, SNP, MAF_avg
                    cat("\n")
                }
                print("n_results")
                print(n_results)
                print(n_results %>% group_by(IS_SOI) %>% summarize(n_snps = n(), avg_maf = mean(MAF_avg), median_maf = median(MAF_avg), .groups = "drop"), n = 1000)
                return(n_results)
            }) #set, IS_SOI, SNP, MAF_avg
            
            print("final")
            print(final)
            final %>% write_csv(output_dir)
            
        }
        
        args=commandArgs(TRUE)
        ti=args[1]
        ti_output=paste0("/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/make_matched_snp_sets/additional_SNP_sets.with_matched_bg.",ti,".csv")
        print(paste0("final output will be saved to: ",ti_output))
        
        add_matched_snps_per_tissue(tissue_run = ti,
                                    snp_sets_dir = "/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/make_matched_snp_sets/additional_SNP_sets.csv", 
                                    output_dir = ti_output)

    }
    
    #### merge all the above back with sets I already made previously for feature enrichment
    if(0){
        enr_folder = "/u/project/zaitlenlab/bballiu/FastGxE/results/eQTL_mapping/GWAS_enrichment/make_matched_snp_sets/"
        new_sets <- bind_rows(lapply(list.files(path = enr_folder, pattern = "additional_SNP_sets.with_matched_bg.*", full.names = F), 
                                   function(f){
                                       print(f)
                                       print(strsplit(f, "[.]")[[1]][3])
                                       f.tib <- read_csv(paste0(enr_folder, f), col_type = cols()) %>% #set, IS_SOI, SNP, MAF_avg
                                           mutate(tissue = strsplit(f, "[.]")[[1]][3]) %>% 
                                           select(set, tissue, IS_SOI, SNP)
                                       #print(f.tib %>% group_by(set, tissue, IS_SOI) %>% summarize(n_snps = n(), .groups = "drop"))
                                       return(f.tib)
                                   }))
        print("done! here is new_sets:")
        print(new_sets)
        
        # set,tissue,IS_SOI,SNP
        
        old_sets = read_csv(paste0(bruna_dir,"results/eQTL_mapping/Feature_Enrichment/Tissue_Specific/Enrichment.Tissue_Specific.All_Tissues_Merged.SNP_sets_with_matched_BG.csv"), col_types = cols())
        print("here is old_sets:")
        print(old_sets)
        
        print("merging")
        combined_sets = bind_rows(new_sets, old_sets)
        
        print(combined_sets %>% group_by(set, tissue, IS_SOI) %>% summarize(n_snps = n(), .groups = "drop"), n = 100000)
        
        combined_sets %>% 
            write_csv(paste0(enr_folder, "final_merged.additional_SNP_sets.old_sets.with_matched_bg.all_tissues_merged.csv"))
    }
}

####%%%%%%% testing: what is fisher test error?
if(0){
    args=commandArgs(TRUE)
    ti=args[1]
    print(paste0("running tissue: ",ti))
    
    # set,tissue,IS_SOI,SNP
    snps_matched=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Feature_Enrichment/Tissue_Specific/Enrichment.Tissue_Specific.All_Tissues_Merged.SNP_sets_with_matched_BG.csv"), col_types = cols())
    snps_matched %>% select(set) %>% distinct %>% print(n=1000)
    
    final_results = tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~ OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4)
    
    for (method in c("HET","TBT")){
        
        print(paste0("running method: ", method))
        
        for (found_in in c("each_tissue","single_tissue")){
            
            print(paste0("running found_in: ", found_in))
            
            tissue_method_eSNPs = snps_matched %>% 
                filter(set == paste0(method,".",found_in), tissue == ti) %>% 
                rename(is_eqtl = IS_SOI) %>% 
                select(SNP, is_eqtl)
            
            for (trait_cat in c("MAPPED_TRAIT","PARENT_TRAIT")){
                print(paste0("running trait_cat: ", trait_cat))
                
                for (trait in unique(gwas_catalog[[trait_cat]])){
                    print(paste0("trait: ", trait))
                    
                    gwas_snps = gwas_catalog %>% filter(.[[trait_cat]] == trait) %>% select(SNP) %>% distinct %>% 
                        mutate(is_gwas = 1) %>% select(SNP, is_gwas)
                    
                    annotated = tissue_method_eSNPs %>% 
                        left_join(gwas_snps, by = c("SNP")) %>% mutate(is_gwas = replace_na(is_gwas,0)) %>% 
                        select(SNP, is_eqtl, is_gwas)
                    
                    tab=table(annotated$is_eqtl,annotated$is_gwas)
                    print(tab)
                    
                   
                    
                    tryCatch({
                        FET=fisher.test(x = tab)[c("estimate","conf.int","p.value")]
                        temp_results<<-tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4,
                                               ti, method, found_in, trait_cat, trait, nrow(gwas_snps), "yes",unname(FET$estimate), FET$conf.int[1], FET$conf.int[2], FET$p.value, c(tab)[1], c(tab)[2], c(tab)[3], c(tab)[4])
                        #print(temp_results)
                        
                    }, error = function(e){
                        print(tab)
                        print("ERROR!!!")
                        print(e)
                        temp_results<<-tribble(~tis, ~met, ~f_in, ~cat, ~tra, ~n_gwas_snps, ~has_results, ~OR, ~conf_int1, ~conf_int2, ~pval, ~ct1, ~ct2, ~ct3, ~ct4,
                                               ti, method, found_in, trait_cat, trait, nrow(gwas_snps), "no", 0, 0, 0, 0, 0, 0, 0, 0)
                        #print("no results")
                    })
                    
                    final_results = bind_rows(final_results,temp_results)
                }
            }
            cat("\n\n")
        }
        cat("\n\n\n")
    }
    
}



###################################################### old back up 
# hard code
if(0){
    
    results=tribble(~method, ~OR, ~conf_int1, ~conf_int2, ~pval)
    
    # get tested SNPs
    tested_snps_all=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Feature_Enrichment/Tissue_Agnostic/Enrichment.Tissue_Agnostic.SNPs_tested.csv"), col_types = cols()) %>%
        select(set, SNP) # set: HET_TBT, HOM
    
    # tested_snps_all=bind_rows(
    #     tested_snps_all %>% filter(set == "HET_TBT") %>% select(SNP) %>% distinct %>% mutate(set = "TBT"),
    #     tested_snps_all %>% filter(set == "HOM") %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_HOM"),
    #     tested_snps_all %>% filter(set == "HET_TBT") %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_HET"),
    #     tested_snps_all %>% select(SNP) %>% distinct %>% mutate(set = "FastGxE_all")
    # )
    # print(tested_snps_all %>% group_by(set) %>% summarize(n_snps = n()))
    
    fastgxe=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.FastGxE.eSNPs.50Tissues.csv"), col_types = cols()) %>% 
        mutate(is_het = case_when(rowSums(.[2:50]) == 0 ~ 0,
                                  rowSums(.[2:50]) >= 1 ~ 1)) %>%
        rename(is_hom = AverageTissue) %>% 
        mutate(is_fastgxe_all = case_when(is_het == 1 | is_hom == 1 ~ 1,
                                          TRUE ~ 0)) %>% 
        select(SNP, is_fastgxe_all, is_het, is_hom)
    
    #### FastGxE all
    fastgxe_all_annotated = tested_snps_all %>% select(SNP) %>% distinct %>%
        left_join(fastgxe[,c("SNP","is_fastgxe_all")], by = c("SNP")) %>% select(SNP,is_fastgxe_all) %>%
        mutate(is_fastgxe_all = replace_na(is_fastgxe_all, 0)) %>%
        left_join(gwas_catalog[,c("SNP")] %>% mutate(is_GWAS = 1), by = c("SNP")) %>% select(SNP,is_fastgxe_all,is_GWAS) %>% 
        mutate(is_GWAS = replace_na(is_GWAS, 0)) 
    print(fastgxe_all_annotated)
    
    fastgxe_all_tab=table(fastgxe_all_annotated$is_fastgxe_all,fastgxe_all_annotated$is_GWAS)
    print(fastgxe_all_tab)
    fastgxe_all_FET=fisher.test(x = fastgxe_all_tab)[c("estimate","conf.int","p.value")]
    print(fastgxe_all_FET)
    print(unlist(fastgxe_all_FET))
    
    results=bind_rows(results,tribble(~method, ~OR, ~conf_int1, ~conf_int2, ~pval,
                                      "FastGxE",
                                      unname(fastgxe_all_FET$estimate),
                                      fastgxe_all_FET$conf.int[1],
                                      fastgxe_all_FET$conf.int[2],
                                      fastgxe_all_FET$p.value
    ))
    print(results)
    
    #### TBT all
    tbt=read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.TbT.eSNPs.49Tissues.csv"), col_types = cols()) %>%
        mutate(is_tbt = case_when(rowSums(.[2:50]) == 0 ~ 0,
                                  rowSums(.[2:50]) >= 1 ~ 1)) %>%
        select(SNP, is_tbt)
    print(tbt)
    
    tbt_annotated = tested_snps_all %>% 
        filter(set == "HET_TBT") %>% 
        select(SNP) %>% distinct %>%
        left_join(tbt, by = c("SNP")) %>% select(SNP,is_tbt) %>%
        mutate(is_tbt = replace_na(is_tbt, 0)) %>%
        left_join(gwas_catalog[,c("SNP")] %>% mutate(is_GWAS = 1), by = c("SNP")) %>% select(SNP,is_tbt,is_GWAS) %>% 
        mutate(is_GWAS = replace_na(is_GWAS, 0)) 
    
    print(tbt_annotated)
    
    tbt_annotated_tab=table(tbt_annotated$is_tbt,tbt_annotated$is_GWAS)
    print(tbt_annotated_tab)
    tbt_FET=fisher.test(x = tbt_annotated_tab)[c("estimate","conf.int","p.value")]
    print(tbt_FET)
    print(unlist(tbt_FET))
    
    results=bind_rows(results,tribble(~method, ~OR, ~conf_int1, ~conf_int2, ~pval,
                                      "TBT",
                                      unname(tbt_FET$estimate),
                                      tbt_FET$conf.int[1],
                                      tbt_FET$conf.int[2],
                                      tbt_FET$p.value
    ))
    print(results)
    
    
    
    
    
}
if(0){
    ######## options
    min_snps_in_gwas_trait=50
    
    ######## read in eSNPs from FastGxE and TbT
    eSNPs = list(FastGxE = read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.FastGxE.eSNPs.50Tissues.csv"), col_types = cols()), 
                 Tbt = read_csv(paste0(bruna_dir,"results/eQTL_mapping/Github_Data/GTEx_v8.ResidualizedExp.TbT.eSNPs.49Tissues.csv"), col_types = cols()))
    
    ####### make empty tib to add results to
    final_results = tribble(~tis, ~met, ~cat, ~tra, ~ OR, ~conf_int1, ~conf_int2, ~pval, ~cont_table)
    
    #tissues=colnames(eSNPs$FastGxE)[-1]
    tissues=c("Whole_Blood") # for testing purposes; whole blood = smallest number of tested SNPs
    
    for (tissue in tissues){
        print(paste0("running tissue: ",tissue))
        
        if(tissue=="AverageTissue"){
            tested_SNPs <- read_tsv(paste0(bruna_dir,"results/eQTL_mapping/MatrixEQTL/AverageTissue.v8.EUR.normalized_and_residualized_expression_homogeneous.all_pairs.txt"), col_types = cols()) %>% 
                select(SNP) %>% distinct
            methods=c("FastGxE") 
        } else {
            tested_SNPs <- read_tsv(paste0(bruna_dir,"results/eQTL_mapping/MatrixEQTL/",tissue,".v8.EUR.normalized_and_residualized_expression_heterogeneous.all_pairs.txt"), col_types = cols()) %>% 
                select(SNP) %>% distinct
            methods=c("FastGxE","Tbt")
        }
        
        for (method in methods){
            print(paste0("running method: ", method))
            
            tissue_method_eSNPs = 
                eSNPs[[method]] %>% 
                select(SNP, all_of(tissue)) %>% 
                filter(get(tissue) == 1) %>% 
                select(SNP) %>% distinct %>% 
                mutate(is_eqtl = 1) %>% select(SNP, is_eqtl)
            
            for (trait_cat in c("PARENT_TRAIT","MAPPED_TRAIT")){
                print(paste0("running trait_cat: ", trait_cat))
                
                for (trait in unique(gwas_catalog[trait_cat])){ 
                    print(paste0("running trait: ", trait))
                    
                    gwas_snps = gwas_catalog %>% filter(get(trait_cat) == trait) %>% select(SNP) %>% distinct %>% 
                        mutate(is_gwas = 1) %>% select(SNP, is_gwas)
                    
                    if(nrow(gwas_snps)<=min_snps_in_gwas_trait) print("****not enough snps in this gwas trait category") else{
                        #print(paste0("number of snps in this gwas trait category: ",nrow(gwas_snps)))
                        
                        annotated = tested_SNPs %>% 
                            left_join(tissue_method_eSNPs, by = c("SNP")) %>% mutate(is_eqtl = replace_na(is_eqtl,0)) %>% 
                            left_join(gwas_snps, by = c("SNP")) %>% mutate(is_gwas = replace_na(is_gwas,0)) %>% 
                            select(SNP, is_eqtl, is_gwas)
                        #print(annotated %>% group_by(is_eqtl, is_gwas) %>% summarize(n_snps = n(), .groups = "drop"))
                        
                        tab=table(annotated$is_eqtl,annotated$is_gwas)
                        FET=fisher.test(x = tab)[c("estimate","conf.int","p.value")]
                        
                        final_results = bind_rows(final_results,
                                                  tribble(~tis, ~met, ~cat, ~tra, ~ OR, ~conf_int1, ~conf_int2, ~pval, ~cont_table,
                                                          tissue, method, trait_cat, trait, unname(FET$estimate), FET$conf.int[1], FET$conf.int[2], FET$p.value, c(tab)))
                        print(final_results, n = 1000)
                    }
                }
            }
        }
        cat("\n\n")
    }
    
    
}











