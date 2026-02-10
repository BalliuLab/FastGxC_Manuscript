#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Brunilda Balliu
#%%%%%%%%%%%%%%% Summarize simulation results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm(list=ls())


# Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(mvtnorm)
library(reshape2)
library(tidyr)
library(magrittr)
library(RColorBrewer)
library(mppa)

# work_dir="/u/home/b/bballiu/FastGxC"
# cluster=1
# I=1e03
# alpha = .05


args=commandArgs(TRUE)
work_dir=args[1]
cluster=as.numeric(args[2])
I=as.numeric(args[3])
alpha=as.numeric(args[4])

if(cluster){
  
  # Compute type I error rate and power
  if(TRUE){
    
    print("Started summarizing simulation results")
    
    setwd(work_dir)
    
    data_dir=paste0(work_dir,"/simulation_study/simulated_data/")
    res_dir=paste0(work_dir,"/simulation_study/simulation_results/")
    
    methods = c("FastGxC", "CxC", "CxC_Het", "Metasoft_FastGxC", "Metasoft_CxC", "METASOFT-mcmc", "MTFERE2", "METATISSUE-MCMCFERE2", "LM_GxC", "LMM_GxC") 
    nr_contexts=c(8,49)
    N_samples=c(100,698)
    missing=0:2
    
    all_sim_res = bind_rows(lapply(nr_contexts, function(nT){
      
      Scenarios = read.table(file = paste0(data_dir,'scenarios','_nC',nT,'.txt'), header = T,sep = '\t')
      
      bind_rows(lapply(N_samples, function(N){
        
        bind_rows(lapply(methods, function(method){
          
          bind_rows(lapply(missing, function(j){
            
            bind_rows(lapply(1:nrow(Scenarios), function(i){
              
              #nT=nr_contexts[2]
              #N=N_samples[1]
              #j=missing[0]
              #i=11
              #method=methods[2]
              
              print(c(method,N,nT,i,j))
              
              ParSpace=as.numeric(Scenarios[i,"ParSpace"]) # 0 : Null and 1: Alternative of heterogeneity 
              
              if(!ParSpace) {par_space="null"} else {par_space="alt"}
              
              w_corr = as.numeric(Scenarios[i,"w_corr"]) # correlation of contexts within an individual
              
              if(j==0){ prefix1=NULL; prefix2 = "Complete data"} 
              if(j==1) {prefix1='_with05prcNAs';prefix2 = "OneK1K missing"}
              if(j==2) {prefix1='_with50prcNAs';prefix2 = "GTEx missing"}
              
              if(file.exists(paste0(res_dir, method,'/',method, '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'))) {
                
                if(method %in% c("Metasoft_CxC", "METASOFT-mcmc")){
                  all_res_i=read.table(file = paste0(res_dir, method,'/',method, '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = F, skip = 1)
                  
                  colnames(all_res_i) = c(unlist(read.table(file = paste0(res_dir, method,'/',method, '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = F,nrows = 1,comment.char = ""))[1:16],
                                          paste0("PVALUES_","T",1:nT), paste0("M",1:nT))
                  
                }else if (method %in% c("Metasoft_FastGxC")) {
                   all_res_i=read.table(file = paste0(res_dir, method,'/',method, '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = F, skip = 1)
                  
                   colnames(all_res_i) = c(unlist(read.table(file = paste0(res_dir, method,'/',method, '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = F,nrows = 1,comment.char = ""))[1:16],
                                          paste0("PVALUES_","T",1:nT), paste0("PVALUES_", "SHARED"), paste0("M",1:nT), paste0("SHARED"))
                }else {
                  all_res_i=read.table(file = paste0(res_dir, method,'/',method, '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = T)
                }
                
                if(method=="CxC"){
                  
                  nr_converged=sum(!is.na(all_res_i$specific_p))
                  
                  # Power to identify eQTL
                  PD_global_eQTL=all_res_i %>%
                    summarise(method=method,
                              Tissues = "global_eQTL", 
                              PD = sum(specific_p<=alpha,na.rm = T)/nr_converged) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  # (Global) Power to identify context-specific eQTL (eQTL in a single context)
                  pval_cols=grep(pattern="p.value_T", x=colnames(all_res_i))
                  
                  PD_global=all_res_i %>%
                    summarise(method=method,
                              Tissues = "global", 
                              PD = sum(apply(t(apply(all_res_i[,pval_cols],1,p.adjust, method="BH"))<alpha,1,sum, na.rm=T, drop=F)==1)/nr_converged) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  PD_global_unadjasted=all_res_i %>%
                    summarise(method=paste0(method,"_unadjasted"),
                              Tissues = "global", 
                              PD = sum(apply(all_res_i[,pval_cols]<alpha,1,sum, na.rm=T, drop=F)==1)/nr_converged) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  final=rbind(
                    data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_global_eQTL, check.names = F, stringsAsFactors = F),
                    data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_global, check.names = F, stringsAsFactors = F),
                    data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_global_unadjasted, check.names = F, stringsAsFactors = F))
                }
                
                if(method=="FastGxC"){
                  nr_converged=sum(!is.na(all_res_i$specific_p))
                  
                  # Power to identify eQTL
                  PD_global_eQTL=data.frame(method=method,
                                            Tissues = "global_eQTL", 
                                            PD = sum(apply(all_res_i[,c("p.value_shared","specific_p")],1,simes.test)<=alpha)/nr_converged)%>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  PD_global_eQTL_unadjusted=all_res_i %>%
                    summarise(method=method,
                              Tissues = "global_eQTL_unadjusted", 
                              PD = sum(specific_p<=alpha | p.value_shared<=alpha,na.rm = T)/nr_converged) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  # (Global) Power to identify eQTL x context interaction effect               
                  PD_global=all_res_i %>%
                    summarise(method=method,
                              Tissues = "global", 
                              PD = sum(specific_p<=alpha,na.rm = T)/nr_converged) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  # (Marginal) Power to identify context driving eQTL x context interaction effect
                  pval_cols=grep(pattern="p.value_T", x=colnames(all_res_i))
                  PD_marginal_adjusted=data.table(
                    data.frame(method=method,
                               PD = apply(t(apply(all_res_i[,pval_cols], 1, p.adjust, method="BH"))<=alpha,2,sum, na.rm=T, drop=F)/I),keep.rownames = T) %>% 
                    rename(Tissues = rn) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/I))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/I)))
                  
                  PD_marginal=data.table(
                    data.frame(method=method,
                               PD = apply(all_res_i[,pval_cols]<=alpha,2,sum, na.rm=T, drop=F)/I),keep.rownames = T) %>% 
                    rename(Tissues = rn) %>%
                    mutate(Tissues=paste0(Tissues,"_nominal")) %>% 
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/I))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/I)))
                  
                  final=rbind(
                    data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_global_eQTL, check.names = F, stringsAsFactors = F),
                    data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_global_eQTL_unadjusted, check.names = F, stringsAsFactors = F),
                    data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_global, check.names = F, stringsAsFactors = F),
                    data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_marginal_adjusted, check.names = F, stringsAsFactors = F),
                    data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_marginal, check.names = F, stringsAsFactors = F))
                }
                
                if(method=="CxC_Het"){
                  nr_converged=sum(!is.na(all_res_i$pval.Q))
                  
                  PD_global=all_res_i %>%
                    summarise(method="CxC-Het",
                              Tissues = "global", 
                              PD = sum(all_res_i$pval.Q<=alpha)/nr_converged) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  
                  final=data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I=nr_converged, missing = prefix2, PD_global, check.names = F, stringsAsFactors = F)
                  
                }
                
                if(method %in% c("LM_GxC", "LMM_GxC")){
                  if(method == "LM_GxC"){
                    method = "LM-GxC"
                  }else if (method == "LMM_GxC") {
                    method = "LMM-GxC"
                  }
        
                  nr_converged=sum(!is.na(all_res_i$LRT_TxG))
                  
                  # Power to identify eQTL
                  PD_global_eQTL=all_res_i %>%
                    summarise(scenario_nr=i, par_space=par_space,  w_corr=w_corr, N=N, nC=nT, I = nr_converged, missing = prefix2, method=method, 
                              Tissues = "global_eQTL", 
                              PD = sum(LRT_G<=alpha,na.rm = T)/nr_converged) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  # Power to identify context-specific eQTL
                  PD_global=all_res_i %>%
                    summarise(scenario_nr=i, par_space=par_space,  w_corr=w_corr, N=N, nC=nT, I = nr_converged, missing = prefix2, method=method, 
                              Tissues = "global", 
                              PD = sum(LRT_TxG<=alpha,na.rm = T)/nr_converged) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged)))
                  
                  final=rbind(PD_global_eQTL,PD_global)
                }
                
                if(method %in% c("Metasoft_CxC", "METASOFT-mcmc", "MTFERE2") ){
                  
                  nr_converged_FE=sum(!is.na(all_res_i$PVALUE_FE))
                  PD_global_FE=all_res_i %>%
                    summarise(method=paste0(method,"_FE"),
                              Tissues = "global_eQTL", 
                              PD = sum(PVALUE_FE<alpha,na.rm = T)/nr_converged_FE) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_FE))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_FE)))

                  if(!method %in% "MTFERE2"){
                    nr_converged_RE=sum(!is.na(all_res_i$PVALUE_RE))
                    PD_global_RE=all_res_i %>%
                    summarise(method=paste0(method,"_RE"),
                              Tissues = "global_eQTL", 
                              PD = sum(PVALUE_RE<alpha,na.rm = T)/nr_converged_RE) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_RE))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_RE)))
                  }

                  nr_converged_RE2=sum(!is.na(all_res_i$PVALUE_RE2))
                  PD_global_RE2=all_res_i %>%
                    summarise(method=paste0(method,"_RE2"),
                              Tissues = "global_eQTL", 
                              PD = sum(PVALUE_RE2<alpha,na.rm = T)/nr_converged_RE2) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_RE2))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_RE2)))
                  
                  nr_converged_Q=sum(!is.na(all_res_i$PVALUE_Q))
                  PD_global_HetQ=all_res_i %>%
                    summarise(method=paste0(method,"_HetQ"),
                              Tissues = "global", 
                              PD = sum(PVALUE_Q<alpha,na.rm = T)/nr_converged_Q) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Q))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Q)))
                  
                  nr_converged_Q_sigRE2=sum((!is.na(all_res_i$PVALUE_Q))&(!is.na(all_res_i$PVALUE_RE2)))
                  PD_global_HetQ_sigRE2=all_res_i %>%
                    summarise(method=paste0(method,"_HetQ_sigRE2"),
                              Tissues = "global", 
                              PD = sum(PVALUE_Q<alpha & PVALUE_RE2<=alpha,na.rm = T)/nr_converged_Q_sigRE2) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Q_sigRE2))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Q_sigRE2)))
                  
                  
                  nr_converged_Mvals=sum(apply(!is.na(all_res_i[,colnames(all_res_i) %in% paste0("M", 1:nT)]),1,all))
                  PD_global_Mvalues=data.frame(method=paste0(method,"_Mvalues"), Tissues = "global", 
                                               PD = sum(apply(all_res_i[,colnames(all_res_i) %in% paste0("M", 1:nT)]>=.9,1,sum, na.rm=T)==1)/nr_converged_Mvals) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Mvals))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Mvals)))
                  
                  
                  nr_converged_Mvals_sigRE2=sum(apply(!is.na(all_res_i[,colnames(all_res_i) %in% paste0("M", 1:nT)]),1,all)&(!is.na(all_res_i$PVALUE_RE2)))
                  PD_global_Mvalues_sigRE2=data.frame(method=paste0(method,"_Mvalues_sigRE2"), Tissues = "global", 
                                                      PD = sum(apply(all_res_i[,colnames(all_res_i) %in% paste0("M", 1:nT)]>=.9,1,sum, na.rm=T)==1 & (all_res_i$PVALUE_RE2<=alpha), na.rm=T)/nr_converged_Mvals_sigRE2) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Mvals_sigRE2))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Mvals_sigRE2)))
                  
                  
                  if(!method %in% c("MTFERE2")){
                    final=rbind(data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Q, missing = prefix2, PD_global_HetQ, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_FE, missing = prefix2, PD_global_FE, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_RE, missing = prefix2, PD_global_RE, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_RE2, missing = prefix2, PD_global_RE2, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Mvals, missing = prefix2, PD_global_Mvalues, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Q_sigRE2, missing = prefix2, PD_global_HetQ_sigRE2, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Mvals_sigRE2, missing = prefix2, PD_global_Mvalues_sigRE2, check.names = F, stringsAsFactors = F)
                    )
                  }else{
                    final=rbind(data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Q, missing = prefix2, PD_global_HetQ, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_FE, missing = prefix2, PD_global_FE, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_RE2, missing = prefix2, PD_global_RE2, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Mvals, missing = prefix2, PD_global_Mvalues, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Q_sigRE2, missing = prefix2, PD_global_HetQ_sigRE2, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Mvals_sigRE2, missing = prefix2, PD_global_Mvalues_sigRE2, check.names = F, stringsAsFactors = F)
                    )
                  }
                  
                }

                if(method %in% c("Metasoft_FastGxC", "METASOFT-mcmc", "METATISSUE-MCMCFERE2") ){
                  
                  nr_converged_FE=sum(!is.na(all_res_i$PVALUE_FE))
                  PD_global_FE=all_res_i %>%
                    summarise(method=paste0(method,"_FE"),
                              Tissues = "global_eQTL", 
                              PD = sum(PVALUE_FE<alpha,na.rm = T)/nr_converged_FE) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_FE))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_FE)))

                  nr_converged_RE=sum(!is.na(all_res_i$PVALUE_RE))
                  PD_global_RE=all_res_i %>%
                    summarise(method=paste0(method,"_RE"),
                              Tissues = "global_eQTL", 
                              PD = sum(PVALUE_RE<alpha,na.rm = T)/nr_converged_RE) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_RE))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_RE)))
                  
                  nr_converged_RE2=sum(!is.na(all_res_i$PVALUE_RE2))
                  PD_global_RE2=all_res_i %>%
                    summarise(method=paste0(method,"_RE2"),
                              Tissues = "global_eQTL", 
                              PD = sum(PVALUE_RE2<alpha,na.rm = T)/nr_converged_RE2) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_RE2))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_RE2)))
                  
                  nr_converged_Q=sum(!is.na(all_res_i$PVALUE_Q))
                  PD_global_HetQ=all_res_i %>%
                    summarise(method=paste0(method,"_HetQ"),
                              Tissues = "global", 
                              PD = sum(PVALUE_Q<alpha,na.rm = T)/nr_converged_Q) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Q))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Q)))
                  
                  nr_converged_Q_sigRE2=sum((!is.na(all_res_i$PVALUE_Q))&(!is.na(all_res_i$PVALUE_RE2)))
                  PD_global_HetQ_sigRE2=all_res_i %>%
                    summarise(method=paste0(method,"_HetQ_sigRE2"),
                              Tissues = "global", 
                              PD = sum(PVALUE_Q<alpha & PVALUE_RE2<=alpha,na.rm = T)/nr_converged_Q_sigRE2) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Q_sigRE2))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Q_sigRE2)))
                  
                  
                  nr_converged_Mvals=sum(apply(!is.na(all_res_i[,colnames(all_res_i) %in% paste0("M", 1:nT)]),1,all))
                  PD_global_Mvalues=data.frame(method=paste0(method,"_Mvalues"), Tissues = "global", 
                                               PD = sum(apply(all_res_i[,colnames(all_res_i) %in% paste0("M", 1:nT)]>=.9,1,sum, na.rm=T)>=1)/nr_converged_Mvals) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Mvals))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Mvals)))
                  
                  
                  nr_converged_Mvals_sigRE2=sum(apply(!is.na(all_res_i[,colnames(all_res_i) %in% paste0("M", 1:nT)]),1,all)&(!is.na(all_res_i$PVALUE_RE2)))
                  PD_global_Mvalues_sigRE2=data.frame(method=paste0(method,"_Mvalues_sigRE2"), Tissues = "global", 
                                                      PD = sum(apply(all_res_i[,colnames(all_res_i) %in% paste0("M", 1:nT)]>=.9,1,sum, na.rm=T)>=1 & (all_res_i$PVALUE_RE2<=alpha), na.rm=T)/nr_converged_Mvals_sigRE2) %>%
                    mutate(LCI = PD-(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Mvals_sigRE2))) %>%
                    mutate(UCI = PD+(qnorm(1-(alpha/2))*sqrt(PD*(1-PD)/nr_converged_Mvals_sigRE2)))
                  
                  
                  
                  final=rbind(data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Q, missing = prefix2, PD_global_HetQ, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_FE, missing = prefix2, PD_global_FE, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_RE, missing = prefix2, PD_global_RE, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_RE2, missing = prefix2, PD_global_RE2, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Mvals, missing = prefix2, PD_global_Mvalues, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Q_sigRE2, missing = prefix2, PD_global_HetQ_sigRE2, check.names = F, stringsAsFactors = F),
                              data.frame(scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, nC=nT, I = nr_converged_Mvals_sigRE2, missing = prefix2, PD_global_Mvalues_sigRE2, check.names = F, stringsAsFactors = F)
                  )
                }
                final
              } 
            }))
          }))
        }))
      }))
    }))
    
    all_sim_res %<>% 
      mutate(LCI = ifelse(test = LCI>=0, yes = LCI, no = 0)) %>% 
      mutate(
        parspace = factor(x = case_when(
          scenario_nr %in% c(1:5) ~ "No Heterogeneity",
          scenario_nr %in% c(6:10) ~ "No Heterogeneity",
          scenario_nr %in% c(11:15) ~ "Single-context Heterogeneity",
          scenario_nr %in% c(16:20) ~ "Single-context Heterogeneity",
          scenario_nr %in% c(21:25) ~ "Two context Heterogeneity",
          scenario_nr %in% c(26:30) ~ "Two context Heterogeneity",
          scenario_nr %in% c(31:35) ~ "Weaker extensive heterogeneity",
          scenario_nr %in% c(36:40) ~ "Extensive heterogeneity")
        ),
        
        shared = factor(x = case_when(
          scenario_nr %in% c(1:5) ~ "No shared",
          scenario_nr %in% c(6:10) ~ "Shared",
          scenario_nr %in% c(11:15) ~ "No shared",
          scenario_nr %in% c(16:20) ~ "Shared",
          scenario_nr %in% c(21:25) ~ "No shared",
          scenario_nr %in% c(26:30) ~ "Shared",
          scenario_nr %in% c(31:35) ~ "Shared",
          scenario_nr %in% c(36:40) ~ "Shared")
        )
      ) 
    
    fwrite(x = all_sim_res, 
           file = paste0(work_dir, '/simulation_study/simulation_results/simulation_results_typeIErrorPower_reviews.txt'), 
           quote = FALSE, 
           sep = '\t', 
           row.names = FALSE, 
           col.names = TRUE)
    
    
  } 
  
  # Compute Bias
  if(TRUE){
    
    print("Started summarizing bias in simulation results")
    
    setwd(work_dir)
    
    data_dir=paste0(work_dir,"/simulation_study/simulated_data/")
    res_dir=paste0(work_dir,"/simulation_study/simulation_results/")
    
    method = c("FastGxC", "CxC", "LMM_GxC", "LM_GxC") 
    nr_contexts=c(8,49)
    N_samples=c(100,698)
    missing=0:2
    
    # nT=nr_contexts[1]
    # N=N_samples[1]
    # j=missing[1]
    # i=30
    
    
    all_biases = bind_rows(lapply(nr_contexts, function(nT){
      
      Scenarios = read.table(file = paste0(data_dir,'scenarios','_nC',nT,'.txt'), header = T,sep = '\t')
      
      bind_rows(lapply(N_samples, function(N){
        
        bind_rows(lapply(missing, function(j){
          
          #### use this line to run 2-context-het with shared to compute percent bias
          #bind_rows(lapply(26:30, function(i){
          bind_rows(lapply(1:nrow(Scenarios), function(i){
            
            print(c(N,nT,i,j))
            
            ParSpace=as.numeric(Scenarios[i,"ParSpace"]) # 0 : Null and 1: Alternative of heterogeneity 
            if(!ParSpace) {par_space="null"} else {par_space="alt"}
            
            w_corr = as.numeric(Scenarios[i,"w_corr"]) # correlation of contexts within an individual
            
            # Get true effect sizes
            maf = as.numeric(Scenarios[i,"maf"]) # minor allele frequency of genotype
            v_g = 2 * maf * (1-maf) # variance of genotype
            v_e = 1 # variance of expression error
            hsq = as.numeric(unlist(Scenarios[i,grepl(pattern = "hsq",x = colnames(Scenarios))]))  # heritability explained by genotype effect in each context
            betas=sqrt((hsq*v_e)/((1-hsq)*v_g))  # genotype effect in each context
            names(betas) = paste0("beta_T",gsub(pattern = "hsq", replacement = "", x = colnames(Scenarios)[grep(pattern = "hsq", x = colnames(Scenarios))]))
            
            if(j==0){ prefix1=NULL; prefix2 = "Complete data"} 
            if(j==1) {prefix1='_with05prcNAs';prefix2 = "OneK1K missing"}
            if(j==2) {prefix1='_with50prcNAs';prefix2 = "GTEx missing"}
            
            if(all(file.exists(paste0(res_dir, method,'/',method, '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt')))) {
              print(paste0(res_dir, method,'/',method, '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'))
              
              # CxC bias
              all_res_i_CxC=read.table(file = paste0(res_dir, "CxC",'/',"CxC", '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = T)
              #bias_CxC = matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(all_res_i_CxC)), nrow = nrow(all_res_i_CxC)) - all_res_i_CxC[,paste0("beta_T",1:nT)]
              CxC_effects = all_res_i_CxC[,paste0("beta_T",1:nT)]
              betas_df = melt(matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(all_res_i_CxC)), nrow = nrow(all_res_i_CxC)), id.vars = NULL) %>% rename(betas = value)
              final_CxC=data.frame(method= "CxC", scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, 
                                   nC=nT, missing = prefix2, melt(CxC_effects, id.vars = NULL), betas = betas_df$betas, check.names = F, stringsAsFactors = F) %>% 
                rename(effects=value, context=variable) %>%
                mutate(context = gsub(pattern= "beta_", replacement = "",x = context))
              
              # FastGxC bias
              all_res_i_FastGxC=read.table(file = paste0(res_dir, "FastGxC",'/',"FastGxC", '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = T)
              FastGxC_effects=matrix(rep(all_res_i_FastGxC[,"beta_shared"], nT), ncol = nT) + all_res_i_FastGxC[,paste0("beta_T",1:nT)]
              #bias_FastGxC = (matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(FastGxC_effects)), nrow = nrow(FastGxC_effects)) - FastGxC_effects)
              betas_df = melt(matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(all_res_i_FastGxC)), nrow = nrow(all_res_i_FastGxC)), id.vars = NULL) %>% rename(betas = value)
              #### two-context het case with shared to compute percent bias
              #bias_FastGxC = bias_FastGxC/(matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(FastGxC_effects)), nrow = nrow(FastGxC_effects)))
              #bias_FastGxC = all_res_i_CxC[,paste0("beta_T",1:nT)] - FastGxC_effects
              final_FastGxC=data.frame(method= "FastGxC", scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, 
                                       nC=nT, missing = prefix2, melt(FastGxC_effects, id.vars = NULL), betas = betas_df$betas, check.names = F, stringsAsFactors = F) %>% 
                rename(effects=value, context=variable) %>% 
                mutate(context = gsub(pattern= "beta_", replacement = "",x = context))
              
              beta_shared=mean(betas) 
              betas_specific=betas-beta_shared
              #bias_FastGxC_shared = beta_shared - all_res_i_FastGxC[,"beta_shared"]  
              #bias_FastGxC_specific = matrix(rep(betas_specific[paste0("beta_T",1:nT)], each = nrow(all_res_i_FastGxC)), nrow = nrow(all_res_i_FastGxC))  - all_res_i_FastGxC[,paste0("beta_T",1:nT)]  
              #bias_FastGxC_shared_specific = data.frame(shared=bias_FastGxC_shared, bias_FastGxC_specific)
              FastGxC_shared_specific_effects = data.frame(shared = all_res_i_FastGxC[,"beta_shared"], all_res_i_FastGxC[,paste0("beta_T",1:nT)])
              betas_df = melt(matrix(rep(c(beta_shared, betas_specific[paste0("beta_T",1:nT)]), each = nrow(all_res_i_FastGxC)), nrow = nrow(all_res_i_FastGxC)), id.vars = NULL) %>% rename(betas = value)
              final_FastGxC_shared_specific=data.frame(method= "FastGxC_shared_specific", scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, 
                                                       nC=nT, missing = prefix2, melt(FastGxC_shared_specific_effects, id.vars = NULL), betas = betas_df$betas, check.names = F, stringsAsFactors = F) %>% 
                rename(effects=value, context=variable) %>%
                mutate(context = gsub(pattern= "beta_", replacement = "",x = context))
              
              
              # LMM_GxC
              all_res_i_LMM=read.table(file = paste0(res_dir, "LMM_GxC",'/',"LMM_GxCbetas", '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = T)
              LMM_effects=cbind(all_res_i_LMM["G"], matrix(rep(all_res_i_LMM["G"], nT-1), ncol = nT-1)  + all_res_i_LMM[,paste0("T",2:nT,".G")])
              colnames(LMM_effects) = paste0("T",1:nT)
              bias_LMM = matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(all_res_i_CxC)), nrow = nrow(all_res_i_CxC)) - LMM_effects
              betas_df = melt(matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(all_res_i_LMM)), nrow = nrow(all_res_i_LMM)), id.vars = NULL) %>% rename(betas = value)
              final_LMM=data.frame(method= "LMM-GxC", scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, 
                                   nC=nT, missing = prefix2, melt(LMM_effects, id.vars = NULL),betas = betas_df$betas, check.names = F, stringsAsFactors = F) %>% 
                rename(effects=value, context=variable) %>%
                mutate(context = gsub(pattern= "beta_", replacement = "",x = context))
              
              
              # LM_GxC
              all_res_i_LM=read.table(file = paste0(res_dir, "LM_GxC",'/',"LM_GxCbetas", '_res_scenario_',i,'_N',N,"_nC",nT, "_nG", I, prefix1, '.txt'), header = T)
              LM_effects=cbind(all_res_i_LM["G"], matrix(rep(all_res_i_LM["G"], nT-1), ncol = nT-1)  + all_res_i_LM[,paste0("T",2:nT,".G")])
              colnames(LM_effects) = paste0("T",1:nT)
              bias_LM = matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(all_res_i_CxC)), nrow = nrow(all_res_i_CxC)) - LM_effects
              betas_df = melt(matrix(rep(betas[paste0("beta_T",1:nT)], each = nrow(all_res_i_LM)), nrow = nrow(all_res_i_LM)), id.vars = NULL) %>% rename(betas = value)
              final_LM=data.frame(method= "LM-GxC", scenario_nr=i, par_space=par_space, w_corr=w_corr, N=N, 
                                  nC=nT, missing = prefix2, melt(LM_effects, id.vars = NULL), betas = betas_df$betas, check.names = F, stringsAsFactors = F) %>% 
                rename(effects=value, context=variable) %>%
                mutate(context = gsub(pattern= "beta_", replacement = "",x = context))
              
              
              
              final = rbind(final_CxC,final_FastGxC,final_FastGxC_shared_specific,final_LMM,final_LM)
              
            } 
          }))
        }))
      }))
    }))
    
    
    all_biases %<>% 
      mutate(
        parspace = factor(x = case_when(
          scenario_nr %in% c(1:5) ~ "No Heterogeneity",
          scenario_nr %in% c(6:10) ~ "No Heterogeneity",
          scenario_nr %in% c(11:15) ~ "Single-context Heterogeneity",
          scenario_nr %in% c(16:20) ~ "Single-context Heterogeneity",
          scenario_nr %in% c(21:25) ~ "Two context Heterogeneity",
          scenario_nr %in% c(26:30) ~ "Two context Heterogeneity",
          scenario_nr %in% c(31:35) ~ "Weaker extensive heterogeneity",
          scenario_nr %in% c(36:40) ~ "Extensive heterogeneity")
        ),
        
        shared = factor(x = case_when(
          scenario_nr %in% c(1:5) ~ "No shared",
          scenario_nr %in% c(6:10) ~ "Shared",
          scenario_nr %in% c(11:15) ~ "No shared",
          scenario_nr %in% c(16:20) ~ "Shared",
          scenario_nr %in% c(21:25) ~ "No shared",
          scenario_nr %in% c(26:30) ~ "Shared",
          scenario_nr %in% c(31:35) ~ "Shared",
          scenario_nr %in% c(36:40) ~ "Shared")
        )
      ) 

    all_biases_summarized = all_biases %>% group_by(method, scenario_nr, w_corr, N, nC, missing, context, parspace, shared, betas) %>% summarise(mean_effect = mean(effects), n = n(), sd = sd(effects)) %>% 
        mutate(LCI = mean_effect-(qnorm(1-(alpha/2))*sd/sqrt(n))) %>%
        mutate(UCI = mean_effect+(qnorm(1-(alpha/2))*sd/sqrt(n)))   

    fwrite(x = all_biases_summarized, 
          file = paste0(work_dir, '/simulation_study/simulation_results/simulation_results_effects_summarized_2025.txt'), 
          quote = FALSE, 
          sep = '\t', 
          row.names = FALSE, 
          col.names = TRUE)
  
  } 
  
}

if(!cluster){
work_dir=getwd()
res_dir=paste0(work_dir,'/simulation_results/')
alpha = 0.05 

all_sim_res = read.table(file = paste0(res_dir,'simulation_results_summarized.txt'),sep = '\t', header = T) %>% 
  mutate(LCI = ifelse(test = LCI>=0, yes = LCI, no = 0)) %>% 
  # keep only methods that will be included in manuscript 
  filter(method %in% c("FastGxC", "CxC", "CxC_Het", "LM_GxC", "LMM_GxC") & 
           par_space2!=c("Extensive heterogeneity", "Extensive heterogeneity - Old")) %>% #, "METASOFT-mcmc_Mvalues","METATISSUE-MCMCFERE2_Mvalues"
  droplevels() %>% 
  # re-level methods
  mutate(method=factor(x = method,
                       levels = c("CxC", "CxC_Het", "LM_GxC", "LMM_GxC", "FastGxC"), #, "METASOFT-mcmc_Mvalues","METATISSUE-MCMCFERE2_Mvalues"
                       labels = c("CxC", "CxC-Het", "LM-GxC", "LMM-GxC", "FastGxC"))) %>% #, "METASOFT", "Meta-Tissue", 
  mutate(missing = factor(x = missing,levels = c("Complete data", "With GTEx NA scheme"), labels = c("Complete data", "Missing data"))) %>% 
  mutate(nC=factor(x = nC, levels=c(5,49), labels = paste0("#Contexts:", c(5,49))),
         N=factor(x = N, levels =  c(100,698), labels=paste0("#Individuals:", c(100,698)))) %>%
  droplevels() %>%
  mutate(par_space2=factor(x=par_space2, levels=c("No context specific - No shared effect", 
                                                  "No context specific - Shared effect",  
                                                  "Single context - No shared effect", 
                                                  "Single context - Shared effect",  
                                                  "Two-contexts  - No shared effect", 
                                                  "Two-context - Shared effect",  
                                                  "Stronger Extensive heterogeneity"))) %>% 
  mutate(parspace=factor(x = case_when(
    par_space2 == "No context specific - No shared effect" ~ "No Heterogeneity", 
    par_space2 == "No context specific - Shared effect" ~ "No Heterogeneity",  
    par_space2 == "Single context - No shared effect" ~ "Single-context Heterogeneity", 
    par_space2 == "Single context - Shared effect" ~ "Single-context Heterogeneity",  
    par_space2 == "Two-contexts  - No shared effect" ~ "Two context Heterogeneity", 
    par_space2 == "Two-context - Shared effect" ~ "Two context Heterogeneity",  
    par_space2 == "Stronger Extensive heterogeneity" ~ "Extensive Heterogeneity"
  ))) %>% 
  mutate( shared=factor(x = case_when(
    par_space2 == "No context specific - No shared effect" ~ "No shared", 
    par_space2 == "No context specific - Shared effect" ~ "Shared",  
    par_space2 == "Single context - No shared effect" ~ "No shared", 
    par_space2 == "Single context - Shared effect" ~ "Shared",  
    par_space2 == "Two-contexts  - No shared effect" ~ "No shared", 
    par_space2 == "Two-context - Shared effect" ~ "Shared",  
    par_space2 == "Stronger Extensive heterogeneity" ~ "Shared"
  )))

write.table(x = all_sim_res, file = paste0(work_dir,'/manuscript/Input_Files/Figure2_Simulation/simulation_results_summarized.txt'),quote = F,append = F,sep = '\t',row.names = F, col.names = T)

effect_sizes_diff = fread(file = paste0(res_dir,'simulation_results_effect_sizes_summarized.txt'), fill=TRUE, sep = '\t', header = T) %>% 
  filter(!par_space2 %in% c("Extensive heterogeneity - Old", "Extensive heterogeneity")) %>% 
  droplevels() %>% 
  # re-level methods
  mutate(missing = factor(x = missing,levels = c("Complete data", "With GTEx NA scheme"), labels = c("Complete data", "Missing data"))) %>% 
  mutate(nC=factor(x = nC, levels=c(5,49), labels = paste0("#Contexts:", c(5,49))),
         N=factor(x = N, levels =  c(100,698), labels=paste0("#Individuals:", c(100,698)))) %>%
  droplevels() %>%
  mutate(par_space2=factor(x=par_space2, levels=c("No context specific - No shared effect", 
                                                  "No context specific - Shared effect",  
                                                  "Single context - No shared effect", 
                                                  "Single context - Shared effect",  
                                                  "Two-contexts  - No shared effect", 
                                                  "Two-context - Shared effect",  
                                                  "Stronger Extensive heterogeneity"))) %>% 
  mutate(parspace=factor(
    levels=c("No Heterogeneity","Single-context Heterogeneity","Two context Heterogeneity","Extensive Heterogeneity"), 
    x = case_when(
      par_space2 == "No context specific - No shared effect" ~ "No Heterogeneity", 
      par_space2 == "No context specific - Shared effect" ~ "No Heterogeneity",  
      par_space2 == "Single context - No shared effect" ~ "Single-context Heterogeneity", 
      par_space2 == "Single context - Shared effect" ~ "Single-context Heterogeneity",  
      par_space2 == "Two-contexts  - No shared effect" ~ "Two context Heterogeneity", 
      par_space2 == "Two-context - Shared effect" ~ "Two context Heterogeneity",  
      par_space2 == "Stronger Extensive heterogeneity" ~ "Extensive Heterogeneity"
    ))) %>% 
  mutate(shared=factor(
    levels=c("No shared","Shared"), 
    x = case_when(
      par_space2 == "No context specific - No shared effect" ~ "No shared", 
      par_space2 == "No context specific - Shared effect" ~ "Shared",  
      par_space2 == "Single context - No shared effect" ~ "No shared", 
      par_space2 == "Single context - Shared effect" ~ "Shared",  
      par_space2 == "Two-contexts  - No shared effect" ~ "No shared", 
      par_space2 == "Two-context - Shared effect" ~ "Shared",  
      par_space2 == "Stronger Extensive heterogeneity" ~ "Shared"
    )))

write.table(x = effect_sizes_diff, file = paste0(work_dir,'/manuscript/Input_Files/Figure2_Simulation/simulation_results_bias_summarized.txt'),quote = F,append = F,sep = '\t',row.names = F, col.names = T)


manuscript_colors_vec = c("#FF00FF", "#00FFFF", "DeepSkyBlue", "#c87e7e","#000000","#009E73","#56A3E9")
names(manuscript_colors_vec) = c("CxC", "METASOFT", "Meta-Tissue", "CxC-Het", "LM-GxC", "LMM-GxC", "FastGxC")

manuscript_shapes_vec = c(16,16,16,15,15,15,15)
names(manuscript_shapes_vec) = c("CxC", "METASOFT", "Meta-Tissue", "CxC-Het", "LM-GxC", "LMM-GxC", "FastGxC")

methods_color_scale <- scale_colour_manual(values = manuscript_colors_vec, drop=T)
methods_fill_scale <- scale_fill_manual(values = manuscript_colors_vec, drop=T)
methods_shape_scale = scale_shape_manual(values=manuscript_shapes_vec, drop=T)

main_methods=c("CxC", "LM-GxC", "LMM-GxC", "FastGxC") #
# main_methods=c("CxC", "METASOFT", "Meta-Tissue", "CxC-Het", "LM-GxC", "LMM-GxC", "FastGxC")


d=0.05
strip_text = 12
legend_text=15
axis_title=20
axis_text=20

### Context-specific eQTL 
if(1){
  ####### Global Type I Error Rate
  # Main 
  if(1){
    # ggsave(filename="~/Desktop/speQTL_global_T1ErRate_1-10_main.png",
    p_null =  ggplot(all_sim_res %>% 
                       filter(Tissues == "global" & scenario_nr %in% c(1:10) &
                                (method %in% main_methods & missing!="Missing data" & 
                                   N=="#Individuals:698" & nC== "#Contexts:49" ) ) %>% 
                       droplevels(), 
                     aes(x=w_corr, y = PD, color = method)) + 
      geom_point(aes(shape=method),size=3, position=position_dodge(d)) + 
      geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
      geom_line(position=position_dodge(d)) +
      facet_grid(.~parspace+shared) +
      theme_bw() + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        strip.text = element_text(size = strip_text),
        legend.text = element_text(size=legend_text),
        axis.title = element_text(size=axis_title),
        axis.text = element_text(size=axis_text))  +   
      xlab("Intra-individual correlation") +
      ylab("Context-specific eQTL\nGlobal Type I Error Rate (%)")  + 
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,.1,.02), limits = c(0,0.1)) +
      scale_x_continuous(breaks = seq(0,.8,.2)) + 
      geom_hline(yintercept = alpha, linetype="dashed") + 
      methods_color_scale + methods_shape_scale  
    # ,width = 15, height = 10)
  }
  
  # Sup
  if(1){
    ggsave(filename="~/Desktop/speQTL_global_T1ErRate_1-10_sup.png",
           plot =  ggplot(all_sim_res %>% 
                            filter(Tissues == "global" & scenario_nr %in% c(1:10) &
                                     (method %in% main_methods) ) %>% #
                            droplevels(), 
                          aes(x=w_corr, y = PD, color = method)) + 
             geom_point(aes(shape=method),size=3, position=position_dodge(d)) + 
             geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
             geom_line(position=position_dodge(d)) +
             facet_grid(N+nC~par_space2+missing) +
             theme_bw() + 
             theme(
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               legend.position = "top",
               legend.title = element_blank(),
               strip.text = element_text(size = strip_text),
               legend.text = element_text(size=legend_text),
               axis.title = element_text(size=axis_title),
               axis.text = element_text(size=axis_text))  +   
             xlab("Within-individual correlation") +
             ylab("Type I Error Rate For sp-eQTL(%)")  + 
             scale_x_continuous(breaks = seq(0,.8,.2)) + 
             geom_hline(yintercept = alpha, linetype="dashed") + 
             methods_color_scale + methods_shape_scale  #+ ylim(c(0,.3)) 
           ,width = 15, height = 15)
  }
  
  ### Global Power
  ## Single context heterogeneity 
  # Main 
  if(1){
    ggsave(filename="~/Desktop/speQTL_global_Power_singleContext_main.png",
           plot =  ggplot(all_sim_res %>% 
                            mutate(par_space2=gsub(pattern="heterogeneity", replacement = "spec", x = par_space2)) %>% 
                            filter(Tissues == "global" & scenario_nr %in% c(11:20) &
                                     (method %in% main_methods & N=="#Individuals:698" & nC== "#Contexts:49" & missing!="Missing data" ) ) %>%
                            droplevels(), 
                          aes(x=w_corr, y = PD, color = method)) + 
             geom_point(aes(shape=method),size=3, position=position_dodge(d)) + 
             geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
             geom_line(position=position_dodge(d)) +
             facet_grid(N+nC~par_space2+missing) +
             theme_bw() + 
             theme(
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               legend.position = "top",
               legend.title = element_blank(),
               strip.text = element_text(size = 12),
               legend.text = element_text(size=15),
               axis.title = element_text(size=20),
               axis.text = element_text(size=20))  +   
             xlab("Within-individual correlation") +
             ylab("Power For sp-eQTL(%)")  + 
             scale_x_continuous(breaks = seq(0,.8,.2)) + 
             geom_hline(yintercept = .8, linetype="dashed") + 
             methods_color_scale + methods_shape_scale  #+ ylim(c(0,.3))
           , width = 15, height = 10)
  }
  
  # Sup
  if(1){
    ggsave(filename="~/Desktop/speQTL_global_Power_singleContext_sup.png",
           plot =  ggplot(all_sim_res %>% 
                            mutate(par_space2=gsub(pattern="heterogeneity", replacement = "spec", x = par_space2)) %>% 
                            filter(Tissues == "global" & scenario_nr %in% c(11:20) &
                                     (method %in% main_methods) ) %>%
                            droplevels(), 
                          aes(x=w_corr, y = PD, color = method)) + 
             geom_point(aes(shape=method),size=3, position=position_dodge(d)) + 
             geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
             geom_line(position=position_dodge(d)) +
             facet_grid(N+nC~par_space2+missing) +
             theme_bw() + 
             theme(
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               legend.position = "top",
               legend.title = element_blank(),
               strip.text = element_text(size = 12),
               legend.text = element_text(size=15),
               axis.title = element_text(size=20),
               axis.text = element_text(size=20))  +   
             xlab("Within-individual correlation") +
             ylab("Power For sp-eQTL(%)")  + 
             scale_x_continuous(breaks = seq(0,.8,.2)) + 
             geom_hline(yintercept = .8, linetype="dashed") + 
             methods_color_scale + methods_shape_scale  #+ ylim(c(0,.3))
           ,width = 10, height = 10)
  }
  
  ## Two context heterogeneity 
  # Sup
  if(1){
    ggsave(filename="~/Desktop/speQTL_global_Power_TwoContexts_sup.png",
           plot =  ggplot(all_sim_res %>% 
                            mutate(par_space2=gsub(pattern="heterogeneity", replacement = "spec", x = par_space2)) %>% 
                            filter(Tissues == "global" & scenario_nr %in% c(21:30) &
                                     (method %in% main_methods) ) %>%
                            droplevels(), 
                          aes(x=w_corr, y = PD, color = method)) + 
             geom_point(aes(shape=method),size=3, position=position_dodge(d)) + 
             geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
             geom_line(position=position_dodge(d)) +
             facet_grid(N+nC~par_space2+missing) +
             theme_bw() + 
             theme(
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               legend.position = "top",
               legend.title = element_blank(),
               strip.text = element_text(size = 12),
               legend.text = element_text(size=15),
               axis.title = element_text(size=20),
               axis.text = element_text(size=20))  +   
             xlab("Within-individual correlation") +
             ylab("Power For sp-eQTL(%)")  + 
             scale_x_continuous(breaks = seq(0,.8,.2)) + 
             geom_hline(yintercept = .8, linetype="dashed") + 
             methods_color_scale + methods_shape_scale  #+ ylim(c(0,.3))
           ,width = 10, height = 10)
  }
  
  ## Extensive heterogeneity 
  # Sup
  if(1){
    ggsave(filename="~/Desktop/speQTL_global_Power_Extensive_sup.png",
           plot =  ggplot(all_sim_res %>% 
                            filter(Tissues == "global" & scenario_nr %in% c(36:40) &
                                     method %in% main_methods  & N!="#Individuals:698" ) %>%
                            droplevels(), 
                          aes(x=w_corr, y = PD, color = method)) + 
             geom_point(aes(shape=method),size=3, position=position_dodge(d)) + 
             geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
             geom_line(position=position_dodge(d)) +
             facet_grid(N+nC~parspace+missing) +
             theme_bw() + 
             theme(
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               legend.position = "top",
               legend.title = element_blank(),
               strip.text = element_text(size = 12),
               legend.text = element_text(size=15),
               axis.title = element_text(size=20),
               axis.text = element_text(size=20))  +   
             xlab("Within-individual correlation") +
             ylab("Power For sp-eQTL(%)")  + 
             scale_x_continuous(breaks = seq(0,.8,.2)) + 
             geom_hline(yintercept = .8, linetype="dashed") + 
             methods_color_scale + methods_shape_scale  #+ ylim(c(0,.3))
           ,width = 7, height = 7)
  }
  
  ## MARGINAL
  if(1){
    ggplot(all_sim_res %>% 
             filter(Tissues != "global") %>% # & missing != "Complete data"
             filter(Tissues %in% c("T1", "T2", "T15", "T20", "T30", "T48", "T49")) %>%
             mutate(Tissues = factor(x = Tissues, levels = paste0("T",1:49))) , 
           aes(x=w_corr, y = PD, col=Tissues)) + 
      geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
      geom_line(position=position_dodge(d)) +
      geom_point(size=2, position=position_dodge(d)) + 
      theme_bw() + 
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size=15),
            axis.title = element_text(size=20),
            axis.text = element_text(size=20))  +   
      xlab("Within-individual correlation") +
      ylab("Marginal Type I Error Rate / Power (%)")  + 
      scale_y_continuous(breaks = seq(0,1,.1)) +
      scale_x_continuous(breaks = unique(all_sim_res$w_corr)) + 
      facet_grid(missing~par_space2) +
      geom_hline(yintercept = c(.05,.8), linetype="dashed") + 
      guides(shape = 'none')
  }
}

### Identify eQTL
if(1){
  ####### Global Type I Error Rate 
  ggsave(filename="~/Desktop/T1ErRate_eQTL_1-5.png",
         plot =  ggplot(all_sim_res %>% 
                          filter(Tissues == "global_eQTL" & scenario_nr %in% c(1:5) & (method %in% main_methods) ) %>% # & N=="#Individuals:100" & nC== "#Contexts:5" & missing=="Missing data" 
                          droplevels(), 
                        aes(x=w_corr, y = PD, color = method)) + 
           geom_point(aes(shape=method),size=3, position=position_dodge(d)) + 
           geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
           geom_line(position=position_dodge(d)) +
           facet_grid(N+nC~par_space2+missing) +
           theme_bw() + 
           theme(
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             legend.position = "top",
             legend.title = element_blank(),
             strip.text = element_text(size = 12),
             legend.text = element_text(size=15),
             axis.title = element_text(size=20),
             axis.text = element_text(size=20))  +   
           xlab("Within-individual correlation") +
           ylab("Type I Error Rate For eQTL(%)")  + 
           scale_x_continuous(breaks = seq(0,.8,.2)) + 
           geom_hline(yintercept = alpha, linetype="dashed") + 
           methods_color_scale + methods_shape_scale  #+ ylim(c(0,.3))
         ,width = 10, height = 15)
  
  
  ### Global Power -  Identify eQTL
  ggsave(filename="~/Desktop/gPower_eQTL_6-20.png",
         plot =  ggplot(all_sim_res %>%
                          filter(Tissues == "global_eQTL" & scenario_nr %in% c(6:40) & missing!="Missing data"  & 
                                   (method %in% main_methods & method!="LM-GxC") ) %>% # & N=="#Individuals:100" & nC== "#Contexts:5" 
                          droplevels(), 
                        aes(x=w_corr, y = PD, color = method)) + 
           geom_point(aes(shape=method),size=3, position=position_dodge(d)) + 
           geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, position=position_dodge(d)) +
           geom_line(position=position_dodge(d)) +
           facet_grid(N+nC~par_space2+missing) +
           theme_bw() + 
           theme(
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             legend.position = "top",
             legend.title = element_blank(),
             strip.text = element_text(size = 12),
             legend.text = element_text(size=15),
             axis.title = element_text(size=20),
             axis.text = element_text(size=20))  +   
           xlab("Within-individual correlation") +
           ylab("Power For eQTL(%)")  + 
           scale_x_continuous(breaks = seq(0,.8,.2)) + 
           geom_hline(yintercept = .8, linetype="dashed") + 
           methods_color_scale + methods_shape_scale,
         width = 20, height = 15)
}  


### Effect sizes
if(1){
  
  
  # Without missing data: no bias
  # effect_sizes_diff  
  sim_res_bias%>% 
    filter(missing == "Complete data" & nC=="#Contexts:49" & N=="#Individuals:698" & w_corr %in% c(0,.8)) %>% 
    ggplot(mapping = aes(fill=variable,y = value, x = as.character(w_corr))) + 
    geom_boxplot() + 
    facet_grid(parspace+shared+missing~N+nC) +
    geom_hline(yintercept = 0, col="red")  + theme(legend.position = "none")
  
  # With missing data but no shared effect: no bias
  effect_sizes_diff  %>% 
    filter(!missing == "Complete data" & (scenario_nr %in% c(1:5,11:15,21:25)) & nC=="#Contexts:49" & N=="#Individuals:698" & w_corr %in% c(0,.8)) %>% 
    ggplot(mapping = aes(fill=variable,y = value, x = as.character(w_corr))) + 
    geom_boxplot() + 
    facet_grid(parspace+shared+missing~N+nC) +
    geom_hline(yintercept = 0, col="red") + theme(legend.position = "none")
  
  # With missing data and shared effect: bias
  effect_sizes_diff  %>% 
    filter(!missing == "Complete data" & (!scenario_nr %in% c(1:5,11:15,21:25)) & nC=="#Contexts:49" & N=="#Individuals:698" & w_corr %in% c(0,.8)) %>% 
    ggplot(mapping = aes(fill=variable,y = value, x = as.character(w_corr))) + 
    geom_boxplot() + 
    facet_grid(parspace+shared+missing~N+nC) +
    geom_hline(yintercept = 0, col="red") + theme(legend.position = "none")
  
}


}



