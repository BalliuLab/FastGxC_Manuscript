shared_exp_file_name=paste0(res_dir,'scenario_',scenario, prefix, '_shared_expression.txt')
spec_exp_t_file_name=paste0(res_dir,'scenario_',scenario, prefix, "_",tissues[j],'_specific_expression.txt')




expression = exp_all

shared_exp_file_name= paste0(data_dir, "AverageContext_", cohort, ".", psuedobulk,"_norm_res_exp.shared.txt")
spec_exp_file_name= paste0(data_dir, contexts,"_", cohort, ".", psuedobulk,"_norm_res_exp.specific.txt")


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

