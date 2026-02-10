#!/bin/bash

# work_dir=/u/project/zaitlenlab/bballiu/FastGxE/
work_dir="/u/project/bballiu/bballiu/FastGxC/"
scratch_dir="/u/scratch/l/lkrocken/qc_outputs/MatrixEQTL/"
pseudobulk="mean"
JobType=$1

### All JopType options
# 1: Residualized expression files for covariates
# 2: Merge expression files and Decompose expression files and compute PCA
####### 4: Preprocess the vcf files
# 3: Filter duplicated SNPs from genotype file
# 4: Compute MAF per tissue 
# 5: Run MatrixEQTL per tissue 
# 8: Get number of SNPs tested per gene and genes tested per SNP in each tissue in GTEx
# 9: Filter MatrixEQTL results in for p-value <= threshold
#10: Run TreeQTL 
#11: prepare metasoft files
# 12: Run metasoft 
# 13: Format metasoft files for treeQTl 
# 14 Run treeQTL on metasoft output 


# Create genes_by_context matrix per cohort

# Residualized expression files for covariates
if ([ $JobType -eq 1 ]); then

    rm ${work_dir}/logfiles/*_resexp.*

    file="${work_dir}/scripts/eQTL_mapping_PBMCs/01_residualize_expression_for_covariates.R"
    for cohort in "CLUES_ASN" "CLUES_EUR" "OneK1K"; do
        n_contexts=$(ls ${scratch_dir}/${cohort}/expression_matrices/*expression.txt | wc -l)
        input_dir="${scratch_dir}/${cohort}/expression_matrices/"
        data_dir="${work_dir}data/${cohort}/MatrixEQTL_input/"
        cov_dir="${scratch_dir}/${cohort}/expression_covariates/"
        for i in $(seq 1 $n_contexts); do
            qsub -N residualize_cov.$i -o ${work_dir}/logfiles/${cohort}_resexp.${i}.o -e ${work_dir}/logfiles/${cohort}_resexp.${i}.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $i $cohort $work_dir $pseudobulk $input_dir $data_dir $cov_dir
        done
    done
fi


# Merge expression files
#if ([ $JobType -eq 2 ]); then

#    rm ${work_dir}/logfiles/*_merge.*

#    for cohort in "CLUES_ASN" "CLUES_EUR" "OneK1K"; do
#        file="${work_dir}/scripts/eQTL_mapping_PBMCs/02_merge_expression_files.R"
#        qsub -N ${cohort}_merge -o ${work_dir}/logfiles/${cohort}_merge.o -e ${work_dir}/logfiles/${cohort}_merge.e -l h_data=64G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $file $work_dir $cohort
#    done
#fi

# Decompose expression files and compute PCA
if ([ $JobType -eq 2 ]); then

    rm ${work_dir}/logfiles/*_decomp.*
    file="${work_dir}/scripts/eQTL_mapping_PBMCs/03_decompose_expression.R"
    functions_file="${work_dir}scripts/eQTL_mapping_PBMCs/00_functions.R"
    for cohort in "CLUES_ASN" "CLUES_EUR" "OneK1K"; do
        data_dir="${work_dir}data/${cohort}/MatrixEQTL_input/"
        SNP_file_name="${work_dir}data/${cohort}/MatrixEQTL_input/${cohort}_5prcMAF_genotypes.txt"
        qsub -N ${cohort}_decomp -o ${work_dir}/logfiles/${cohort}_decomp.o -e ${work_dir}/logfiles/${cohort}_decomp.e -l h_data=80G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $work_dir $cohort $pseudobulk $data_dir $SNP_file_name $functions_file
    done
fi


# Andrew can you add here the scripts for processing the vcf files? Maybe merge with the next script that filters the duplicated SNPs. 
# if ([ $JobType -eq 4 ]); then
# echo "Under construction"
# fi

# Filter duplicated SNPs from genotype file
if ([ $JobType -eq 3 ]); then

    rm ${work_dir}/logfiles/*_remove_dup_snps.*  
    file="${work_dir}/scripts/eQTL_mapping_PBMCs/05_remove_duplicated_snps.R"

    for cohort in "CLUES_ASN" "CLUES_EUR" "OneK1K"; do
        SNP_file_name="${work_dir}data/${cohort}/MatrixEQTL_input/${cohort}_5prcMAF_genotypes.txt"
        snps_location_file_name="${work_dir}data/${cohort}/MatrixEQTL_input/${cohort}_5prcMAF_snpsloc.txt"
        outdir="${work_dir}data/${cohort}/MatrixEQTL_input/"
        qsub -N ${cohort}_remove_dup_snps -o ${work_dir}/logfiles/${cohort}_remove_dup_snps.o -e ${work_dir}/logfiles/${cohort}_remove_dup_snps.e -l h_data=164G,h_rt=4:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $work_dir $cohort $SNP_file_name $snps_location_file_name $outdir
    done
fi

# Compute MAF per tissue
if ([ $JobType -eq 4 ]); then
    
    rm ${work_dir}/logfiles/*_MAF.*  
    file="${work_dir}/scripts/eQTL_mapping_PBMCs/06_compute_MAF_per_tissue.R"

    for cohort in "CLUES_ASN" "CLUES_EUR" "OneK1K"; do
        exp_suffix=".${cohort}.${pseudobulk}_norm_res_exp"
        exp_file_path="${work_dir}/data/${cohort}/MatrixEQTL_input/"
        SNP_file_name="${work_dir}/data/${cohort}/MatrixEQTL_input/${cohort}_5prcMAF_genotypes.txt"
        qsub -N ${cohort}_MAF -o ${work_dir}/logfiles/${cohort}_MAF.o -e ${work_dir}/logfiles/${cohort}_MAF.e -l h_data=80G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $work_dir $cohort $exp_suffix $exp_file_path $SNP_file_name
    done
fi

# Run MatrixEQTL by context
if ([ $JobType -eq 5 ]); then

    rm ${work_dir}/logfiles/*_eQTL_sp.*
    rm ${work_dir}/logfiles/*_eQTL_sh.*
    outdir="${work_dir}/results/eQTL_mapping_PBMCs/sc_MatrixEQTL/"

    file="${work_dir}/scripts/eQTL_mapping_PBMCs/07_run_MatrixEQTL_by_context.R"
    for cohort in "CLUES_ASN" "CLUES_EUR" "OneK1K"; do
        n_contexts=$(ls ${scratch_dir}/${cohort}/expression_matrices/*expression.txt | wc -l)
        data_dir="${work_dir}data/${cohort}/MatrixEQTL_input/"
        SNP_file_name="${data_dir}${cohort}_5prcMAF_genotypes.txt"
        snps_location_file_name="${data_dir}${cohort}_5prcMAF_snpsloc.txt"
        gene_location_file_name="${data_dir}${cohort}_geneloc.txt"
        MAF_file="${work_dir}/data/${cohort}/misc/${cohort}_SNPs_by_Context_MAF.txt"
        for exp_scale in $(seq 1 2); do
            # 1: normalized_and_residualized_expression
            # 2: normalized_and_residualized_expression_heterogeneous
            for i in $(seq 1 $n_contexts); do
                qsub -N ${cohort}_eQTL_sp.${exp_scale}.${i} -o ${work_dir}/logfiles/${cohort}_eQTL_sp.${exp_scale}.${i}.o -e ${work_dir}/logfiles/${cohort}_eQTL_sp.${exp_scale}.${i}.e -l h_data=64G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $work_dir $cohort $exp_scale $i $n_contexts $outdir $data_dir $SNP_file_name $snps_location_file_name $gene_location_file_name $MAF_file
            done
        done

        exp_scale=3
        # 3: normalized_and_residualized_expression_homogeneous
        i=1
        qsub -N ${cohort}_eQTL_sh.${exp_scale}.${i} -o ${work_dir}/logfiles/${cohort}_eQTL_sh.${exp_scale}.${i}.o -e ${work_dir}/logfiles/${cohort}_eQTL_sh.${exp_scale}.${i}.e -l h_data=64G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $work_dir $cohort $exp_scale $i $n_contexts $outdir $data_dir $SNP_file_name $snps_location_file_name $gene_location_file_name $MAF_file
    done
fi

# Get number of SNPs tested per gene and number of genes tested per SNP in each tissue
if ([ $JobType -eq 8 ]); then

    input_dir="/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping_PBMCs/sc_MatrixEQTL/"
    output_dir="/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping_PBMCs/sc_TreeQTL/"
    for cohort in "OneK1K" "CLUES_ASN" "CLUES_EUR"; do
        # Number of SNPs tested per gene for full and het
        nSNPs=1
        cd ${work_dir}/results/eQTL_mapping_PBMCs/sc_MatrixEQTL/

        ls -l *.${cohort}.mean_norm_res_exp.all_pairs.txt | awk '{print $9}' > tmp_MatrixEQTL_files

        cat tmp_MatrixEQTL_files | while read MatrixEQTL_file ; do
            context_name=$(echo $MatrixEQTL_file | tr "." " " | awk '{print $1}')
            qsub -N nrTests.${cohort}_${context_name}.SNPs -o ${work_dir}/logfiles/${cohort}_nrTests.${context_name}.SNPs.o -e ${work_dir}/logfiles/${cohort}_nrTests.${context_name}.SNPs.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/08_get_ntests.sh $context_name $nSNPs $cohort $input_dir $output_dir
        done

        rm tmp_MatrixEQTL_files

        # Number of genes tested per SNPs for full and het
        #nSNPs=2
        #tissue_name="Whole_Blood"
        #qsub -N nrTests.Genes -o ${work_dir}/logfiles/nrTests.Genes.o -e ${work_dir}/logfiles/nrTests.Genes.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs_GTEx/GTEx_submit_job.sh $JobType $tissue_name $nSNPs

        # Number of SNPs tested per gene and genes tested per SNPs for hom
        context_name="AverageContext"
            for nSNPs in 3; do
            qsub -N nrTests.${cohort}_${context_name}.${nSNPs} -o ${work_dir}/logfiles/nrTests.${cohort}_${context_name}.${nSNPs}.o -e ${work_dir}/logfiles/nrTests.${cohort}_${context_name}.${nSNPs}.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/08_get_ntests.sh $context_name $nSNPs $cohort $input_dir $output_dir 
        done
    done

fi

#### changed this but didn't run it
# Filter MatrixEQTL results for p-value <= threshold
if ([ $JobType -eq 9 ]); then
    cohort="OneK1K"
    input_dir="/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping_PBMCs/sc_MatrixEQTL/"
    output_dir=""
    cd ${work_dir}/results/eQTL_mapping_PBMCs/sc_MatrixEQTL/
    ls -l *.${cohort}.*.all_pairs.txt | awk '{print $9}' > tmp_MatrixEQTL_files

    cat tmp_MatrixEQTL_files | while read file ; do
    qsub -N FilterQTLs.${cohort}_${file} -o ${work_dir}/logfiles/FilterQTLs.${cohort}_${file}.o -e ${work_dir}/logfiles/FilterQTLs.${file}.e -l h_data=16G,h_rt=12:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/09_filter_matrixEQTL_pvalue.sh ${file} $cohort $input_dir $output_dir
    done

    rm tmp_MatrixEQTL_files

fi

# Run TreeQTL for all cohorts (no meta analysis)
if ([ $JobType -eq 10 ]); then

rm ${work_dir}/logfiles/treeQTL.*

file="${work_dir}/scripts/eQTL_mapping_PBMCs/10_run_TreeQTL.R"
functions_file="${work_dir}/scripts/eQTL_mapping_PBMCs/00_functions.R"
num_contexts=8
metasoft=0
m_eqtl_out_dir="${work_dir}/results/eQTL_mapping_PBMCs/sc_MatrixEQTL/"
treeQTL_dir="${work_dir}/results/eQTL_mapping_PBMCs/sc_TreeQTL/"
for cohort in "OneK1K" "CLUES_ASN" "CLUES_EUR"; do
    for exp_scale in $(seq 1 3); do
    qsub -N treeQTL.${cohort}_${exp_scale} -o ${work_dir}/logfiles/treeQTL.${cohort}_${exp_scale}.o -e ${work_dir}/logfiles/treeQTL.${cohort}_${exp_scale}.e -l h_data=80G,h_rt=20:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $exp_scale $cohort $num_contexts $work_dir $functions_file $m_eqtl_out_dir $treeQTL_dir $metasoft
    done
done
fi

# prepare metasoft files
if ([ $JobType -eq 11 ]); then

rm ${work_dir}/logfiles/*Metasoft_prep*

file="${work_dir}/scripts/eQTL_mapping_PBMCs/11_format_for_metasoft.R"
input_dir="${work_dir}/results/eQTL_mapping_PBMCs/sc_MatrixEQTL/"
functions_file="${work_dir}/scripts/eQTL_mapping_PBMCs/00_functions.R"

    for cohorts in "CLUES_ASN.CLUES_EUR.OneK1K" "CLUES_ASN.CLUES_EUR";
    do
        outdir="${work_dir}/results/eQTL_mapping_PBMCs/${cohorts}_Metasoft/"
        mkdir -p $outdir
        for exp_scale in $(seq 1 2); do #"CLUES_ASN" "CLUES_EUR"; do
            if ([ $exp_scale -eq 1 ]); then
                n_contexts=$(ls ${input_dir}*CLUES_ASN*exp.all_pairs.txt | wc -l)
            fi
            if ([ $exp_scale -eq 2 ]); then
                n_contexts=$(($(ls ${input_dir}*CLUES_ASN*exp.all_pairs.txt | wc -l)+1))
            fi
            for i in $(seq 1 $n_contexts); do
                qsub -N Metasoft_prep_${exp_scale}.${i} -o ${work_dir}/logfiles/Metasoft_prep_${exp_scale}.${i}.o -e ${work_dir}/logfiles/Metasoft_prep_${exp_scale}.${i}.e -l h_data=80G,h_rt=20:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $input_dir $exp_scale $cohorts $outdir $functions_file $i
            done
        done
    done
fi

# run metasoft 
if ([ $JobType -eq 12 ]); then

rm ${work_dir}/logfiles/*run_Metasoft*

metasoft_tool_dir="/u/project/bballiu/lkrocken/FastGxC/pipeline/Metasoft/"
metasoft_file=${metasoft_tool_dir}Metasoft.jar
pvalue_table=${metasoft_tool_dir}HanEskinPvalueTable.txt

for cohorts in "CLUES_ASN.CLUES_EUR.OneK1K" "CLUES_ASN.CLUES_EUR";
do
    metasoft_files_dir="${work_dir}/results/eQTL_mapping_PBMCs/${cohorts}_Metasoft/"
    cd $metasoft_files_dir
    rm $metasoft_files_dir/output.*.tsv

        for formatted_metasoft_file in *.tsv
        do
                output="/u/project/bballiu/bballiu/FastGxC/results/eQTL_mapping_PBMCs/${cohorts}_Metasoft/output.${formatted_metasoft_file}"
                qsub -N run_Metasoft -o ${work_dir}/logfiles/run_Metasoft.o -e ${work_dir}/logfiles/run_Metasoft.e -l h_data=80G,h_rt=20:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType ${metasoft_file} ${metasoft_files_dir}$formatted_metasoft_file $pvalue_table $output
        done
done

fi


## format metasoft files for treeQTL
if ([ $JobType -eq 13 ]); then

rm ${work_dir}/logfiles/*format_Metasoft*
file="${work_dir}/scripts/eQTL_mapping_PBMCs/13_format_metasoft_treeQTL.R"

for cohorts in "CLUES_ASN.CLUES_EUR.OneK1K" "CLUES_ASN.CLUES_EUR";
do
    workdir="${work_dir}/results/eQTL_mapping_PBMCs/${cohorts}_Metasoft/"
    num_files=$(ls $workdir | grep "output" | wc -l)
    for i in $(seq 1 $num_files); do
        qsub -N format_Metasoft -o ${work_dir}/logfiles/format_Metasoft_${cohorts}.$i.o -e ${work_dir}/logfiles/format_Metasoft_${cohorts}.$i.e -l h_data=64G,h_rt=5:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $workdir $i
    done
done

fi

# Run TreeQTL for meta analyzed data
if ([ $JobType -eq 14 ]); then

rm ${work_dir}/logfiles/*treeQTL.*_*_*

file="${work_dir}/scripts/eQTL_mapping_PBMCs/10_run_TreeQTL.R"
functions_file="${work_dir}/scripts/eQTL_mapping_PBMCs/00_functions.R"
num_contexts=8
metasoft=1

for dir in "CLUES_ASN.CLUES_EUR.OneK1K_Metasoft" "CLUES_ASN.CLUES_EUR_Metasoft";
do
    for subdir in "random_effect2" "fixed_effect" 
    do
        m_eqtl_out_dir="${work_dir}/results/eQTL_mapping_PBMCs/${dir}/${subdir}/"
        treeQTL_dir="${work_dir}/results/eQTL_mapping_PBMCs/${dir}/${subdir}/"
        for exp_scale in $(seq 1 3); do
            cohort=""
            qsub -N treeQTL.${dir}_${subdir}_${exp_scale} -o ${work_dir}/logfiles/treeQTL.${dir}_${subdir}_${exp_scale}.o -e ${work_dir}/logfiles/treeQTL.${dir}_${subdir}_${exp_scale}.e -l h_data=80G,h_rt=10:00:00,highp $work_dir/scripts/eQTL_mapping_PBMCs/submit_job_lena.sh $JobType $file $exp_scale "" $num_contexts $work_dir $functions_file $m_eqtl_out_dir $treeQTL_dir $metasoft
        done
    done
done
fi

