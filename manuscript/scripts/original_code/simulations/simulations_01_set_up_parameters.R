#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%% Create file with parameters for each scenario
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Parameters
args=commandArgs(TRUE)
work_dir=args[1]
nT=as.numeric(args[2])


data_dir=paste0(work_dir,"/simulation_study/simulated_data/")
dir.create(data_dir)
setwd(data_dir)

print(paste("number of contexts",nT))

Scenarios=data.frame(rbind(

  # 1-5 Under the null - no shared - no context specific
  c(ParSpace=0, maf = 0.2, hsq = rep(0.00, nT), w_corr = 0.0, alpha = 1),
  c(ParSpace=0, maf = 0.2, hsq = rep(0.00, nT), w_corr = 0.2, alpha = 1),
  c(ParSpace=0, maf = 0.2, hsq = rep(0.00, nT), w_corr = 0.4, alpha = 1),
  c(ParSpace=0, maf = 0.2, hsq = rep(0.00, nT), w_corr = 0.6, alpha = 1),
  c(ParSpace=0, maf = 0.2, hsq = rep(0.00, nT), w_corr = 0.8, alpha = 1),

  # 6-10 Under the null - shared but no context specific
  c(ParSpace=0, maf = 0.2, hsq = rep(0.05, nT), w_corr = 0.0, alpha = 1),
  c(ParSpace=0, maf = 0.2, hsq = rep(0.05, nT), w_corr = 0.2, alpha = 1),
  c(ParSpace=0, maf = 0.2, hsq = rep(0.05, nT), w_corr = 0.4, alpha = 1),
  c(ParSpace=0, maf = 0.2, hsq = rep(0.05, nT), w_corr = 0.6, alpha = 1),
  c(ParSpace=0, maf = 0.2, hsq = rep(0.05, nT), w_corr = 0.8, alpha = 1),

  # 11-15 Single context heterogeneity - no shared effect
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-1),.05), w_corr = 0.0, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-1),.05), w_corr = 0.2, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-1),.05), w_corr = 0.4, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-1),.05), w_corr = 0.6, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-1),.05), w_corr = 0.8, alpha = 1),

  # 16-20 Single context heterogeneity - shared and context specific
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-1),.10), w_corr = 0.0, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-1),.10), w_corr = 0.2, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-1),.10), w_corr = 0.4, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-1),.10), w_corr = 0.6, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-1),.10), w_corr = 0.8, alpha = 1),

  # 21-25 Two-context heterogeneity  - no shared effects
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-2),0.05,.05), w_corr = 0.0, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-2),0.05,.05), w_corr = 0.2, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-2),0.05,.05), w_corr = 0.4, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-2),0.05,.05), w_corr = 0.6, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.0, nT-2),0.05,.05), w_corr = 0.8, alpha = 1),

  # 26-30 Two-context heterogeneity - shared and context specific
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-2),0.10,.10), w_corr = 0.0, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-2),0.10,.10), w_corr = 0.2, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-2),0.10,.10), w_corr = 0.4, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-2),0.10,.10), w_corr = 0.6, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = c(rep(0.05, nT-2),0.10,.10), w_corr = 0.8, alpha = 1),

  # 31-35 Extensive heterogeneity - shared and context specific
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.05, length.out = nT), w_corr = 0.0, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.05, length.out = nT), w_corr = 0.2, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.05, length.out = nT), w_corr = 0.4, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.05, length.out = nT), w_corr = 0.6, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.05, length.out = nT), w_corr = 0.8, alpha = 1),

  # 36-40 (Stronger) extensive heterogeneity - shared and context specific
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.1, length.out = nT), w_corr = 0.0, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.1, length.out = nT), w_corr = 0.2, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.1, length.out = nT), w_corr = 0.4, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.1, length.out = nT), w_corr = 0.6, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(0.0,0.1, length.out = nT), w_corr = 0.8, alpha = 1),

  # 41 - 45 Old extensive heterogeneity - shared and context specific
  c(ParSpace=1, maf = 0.2, hsq = seq(.000,.074, length.out = nT), w_corr = 0.0, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(.000,.074, length.out = nT), w_corr = 0.2, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(.000,.074, length.out = nT), w_corr = 0.4, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(.000,.074, length.out = nT), w_corr = 0.6, alpha = 1),
  c(ParSpace=1, maf = 0.2, hsq = seq(.000,.074, length.out = nT), w_corr = 0.8, alpha = 1)),

  check.names = F, stringsAsFactors = F)

write.table(x = Scenarios, file = paste0('scenarios','_nC',nT,'.txt'),quote = F,sep = '\t',row.names = F, col.names = T)

