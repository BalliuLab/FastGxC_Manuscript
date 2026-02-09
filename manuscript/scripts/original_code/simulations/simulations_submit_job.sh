#!/bin/bash

# qrsh -l h_data=32G,h_rt=12:00:00

echo "Loading dependencies"
. /u/local/Modules/default/init/modules.sh
module load R/4.2.2-BIO
 source /u/home/l/lkrocken/.bashrc


JobType=$1
echo $JobType


# 1: Simulation study: Set up simulation parameters
if ([ $JobType -eq 1 ]); then
  work_dir=$2
  
  for nT in 14; do #8 49
    echo Set up simulation parameters $nT contexts
    my_script=$work_dir/scripts/simulations/simulations_01_set_up_parameters.R
    R --vanilla --slave -f $my_script --args $work_dir $nT
  done
  
fi


# 2: Simulation study: Simulate genotypes
if ([ $JobType -eq 2 ]); then
work_dir=$2
I=$3
N=$4

echo Simulating genotypes for $N individuals and $I iterations 
my_script=$work_dir/scripts/simulations/simulations_02_simulate_genotypes.R
R --vanilla --slave -f $my_script --args $work_dir $I $N
fi



# 3: Simulation study: Simulate gene expression
if ([ $JobType -eq 3 ]); then
work_dir=$2
I=$3
N=$4
i=$5

for nT in 8 49; do
echo Simulating expression for $I iterations in $N individuals and $nT contexts 
my_script=$work_dir/scripts/simulations/simulations_03_simulate_expression.R
R --vanilla --slave -f $my_script --args $work_dir $I $N $nT $i
done
fi



# 4: Simulation study: Run different methods
if ([ $JobType -eq 4 ]); then
work_dir=$2
I=$3
N=$4 
nT=$5 
i=$6
method=$7
missing_data=$8

echo Analyzing data for scenario $i with $I genes $N individuals and $nT contexts with $method

if ([ "$method" = 'FastGxC' ] || [ "$method" = 'CxC' ] ); then
echo Running $method

#my_script=$work_dir/scripts/simulations/simulations_04_run_MatrixEQTL_by_context.R
my_script=$work_dir/scripts/simulations/simulations_04_run_MatrixEQTL_by_context_no_remove_NA.R 
R --vanilla --slave -f $my_script --args $i $work_dir $missing_data $N $nT $I $method $work_dir/simulation_study/
fi


if ([ "$method" = 'CxC_Het' ]); then 
echo Running $method

cxc_het_script=$work_dir/scripts/simulations/simulations_04_runCxC_Het.R
R --vanilla --slave -f $cxc_het_script --args $i $work_dir $missing_data $N $nT $I $method 

fi

if ([ "$method" = 'LMM_GxC' ] || [ "$method" = 'LM_GxC' ] ); then
echo Running $method

my_script=$work_dir/scripts/simulations/simulations_04_runLMMGxC.R
R --vanilla --slave -f $my_script --args $i $work_dir $missing_data  $N $nT $I $method 

fi


if ([ "$method" = 'MetaTissue' ] ); then
echo Running $method

export work_dir I N nT i method missing_data 
metatissue_script=$work_dir/scripts/simulations/simulations_04_run_MetaTissue.sh
sh $metatissue_script

fi



if  ([ "$method" = 'Metasoft_FastGxC' ] || [ "$method" = 'Metasoft_CxC' ] ); then
echo Running $method

metasoft_dir=$work_dir/simulation_study/external_software/METASOFT
metasoft_script=$work_dir/scripts/simulations/simulations_04_runMetasoft.R
R --vanilla --slave -f $metasoft_script --args $i $work_dir $missing_data $N $nT $I $method $metasoft_dir

fi



fi


# 5: Simulation study: Summarize results from different methods
if ([ $JobType -eq 5 ]); then
work_dir=$2
cluster=$3
I=$4
alpha=$5
my_script=$work_dir/scripts/simulations/simulations_05_summarize_results.R
R --vanilla --slave -f $my_script --args $work_dir $cluster $I $alpha
fi
