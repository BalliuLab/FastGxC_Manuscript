#!/bin/bash

work_dir="/u/project/bballiu/bballiu/FastGxC/"

I=1000 # number of iterations (genes, SNPs, and gene-SNP pairs)

JobType=4
          # 1: Simulation study: Set up simulation parameters
          # 2: Simulation study: Simulate genotypes
          # 3: Simulation study: Simulate gene expression
          # 4: Simulation study: Run different methods
          # 5: Simulation study: Summarize results

######################################################
# 1: Simulation study: Set up simulation parameters
# scp simulations_01_set_up_parameters.R simulations_submit_jobs.sh simulations_submit_job.sh bballiu@hoffman2.idre.ucla.edu:/u/home/b/bballiu/FastGxC/scripts/simulations
######################################################
if ([ $JobType -eq 1 ]); then
      qsub -N sim.pars -o ${work_dir}/logfiles/sim.pars.o -e ${work_dir}/logfiles/sim.pars.e $work_dir/scripts/simulations/simulations_submit_job.sh $JobType $work_dir
fi

######################################################
# 2: Simulation study: Simulate genotypes
# scp simulations/simulations_02_simulate_genotypes.R simulations/simulations_submit_jobs.sh simulations/simulations_submit_job.sh bballiu@hoffman2.idre.ucla.edu:/u/home/b/bballiu/FastGxC/scripts/simulations
######################################################
if ([ $JobType -eq 2 ]); then
  for N in 900 ; do #100 698 
    qsub -N sim.gen.${N} -o ${work_dir}/logfiles/sim.gen.${N}.o -e ${work_dir}/logfiles/sim.gen.${N}.e $work_dir/scripts/simulations/simulations_submit_job.sh $JobType $work_dir $I $N 
  done
fi

######################################################
# 3: Simulation study: Simulate gene expression
# scp simulations/simulations_03_simulate_expression.R simulations/simulations_submit_jobs.sh simulations/simulations_submit_job.sh bballiu@hoffman2.idre.ucla.edu:/u/home/b/bballiu/FastGxC/scripts/simulations
######################################################
if ([ $JobType -eq 3 ]); then
  for N in 100 698; do 
    for i in $(seq 1 40); do 
      qsub -N sim.exp.${i}.${N} -o ${work_dir}/logfiles/sim.exp.${i}.${N}.o -e ${work_dir}/logfiles/sim.exp.${i}.${N}.e -l h_data=32G,h_rt=12:00:00 $work_dir/scripts/simulations/simulations_submit_job.sh $JobType $work_dir $I $N $i 
    done
  done
fi

######################################################
# 4: Simulation study: Run different methods
# scp simulations/simulations_submit_jobs.sh simulations/simulations_submit_job.sh simulations/simulations_04_run_MatrixEQTL_by_context.R simulations/simulations_04_runCxC_Het.R simulations/simulations_04_runLMMGxC.R  bballiu@hoffman2.idre.ucla.edu:/u/home/b/bballiu/FastGxC/scripts/simulations
######################################################
if ([ $JobType -eq 4 ]); then
  for method in FastGxC CxC; do #CxC FastGxC LM_GxC LMM_GxC ; do # MetaTissue CxC_Het 
    for N in 698; do #100 698
      for nT in 8; do #8 49
        for i in $(seq 11 20) $(seq 36 40); do #40
          for missing_data in 0 1 2; do #1 2
            qsub -N sim.$method.$i.${N}.${nT}.${missing_data} -o ${work_dir}/logfiles/sim.$method.${i}.${N}.${nT}.${missing_data}.o -e ${work_dir}/logfiles/sim.$method.${i}.${N}.${nT}.${missing_data}.e -l h_data=64G,h_rt=12:00:00 $work_dir/scripts/simulations/simulations_submit_job.sh $JobType $work_dir $I $N $nT $i $method $missing_data
          done
        done
      done
    done
  done  
fi


################################################################
# 5: Simulation study: Summarize results from different methods
# scp simulations/simulations_submit_jobs.sh simulations/simulations_submit_job.sh simulations/simulations_04_run_MatrixEQTL_by_context.R simulations/simulations_04_runCxC_Het.R simulations/simulations_04_runLMMGxC.R  bballiu@hoffman2.idre.ucla.edu:/u/home/b/bballiu/FastGxC/scripts/simulations
################################################################
if ([ $JobType -eq 5 ]); then
  alpha=0.05
  cluster=1
    qsub -N sum.sim -o ${work_dir}/logfiles/sum.sim.o -e ${work_dir}/logfiles/sum.sim.e -l h_data=16G,h_rt=12:00:00 $work_dir/scripts/simulations/simulations_submit_job.sh $JobType $work_dir $cluster $I $alpha
fi


