
# # sample parameters for troubleshooting
# work_dir='/u/project/bballiu/bballiu/FastGxC/'
# I=1000
# N=100
# nT=8
# i=30
# method='MetaTissue'
# missing_data=2

# print parameters
prefix4NA=("" "_with05prcNAs"  "_with50prcNAs")
prefix4NA=${prefix4NA[$missing_data]}
simnameout=scenario_${i}_N${N}_nC${nT}_nG${I}${prefix4NA}

echo ${simnameout}



echo "running MetaTissue step 1 (prep sim data into intermediate expr & geno format)"
    helper_r_script="${work_dir}scripts/simulations/simulations_04_run_MetaTissue_helperstep1.R"
    R --vanilla --slave -f $helper_r_script --args $i $work_dir $missing_data $N $nT $I $method



echo "running MetaTissue step 2 (convert data into final, Meta-Tissue ready format)"
    dir_metatissuebin="${work_dir}/simulation_study/external_software/Meta-Tissue.v.0.5/" # <--
    filepath_mtinputgen="${dir_metatissuebin}/MetaTissueInputGenerator.jar"  # <--
    dir_mtdattmp="${work_dir}/simulation_study/simulation_results/metatissue_tmp/${simnameout}"

    java -jar ${filepath_mtinputgen} \
        -i ${dir_mtdattmp}/tissueinfo.txt \
        -l ${dir_mtdattmp}/genelist.txt \
        -m ${dir_mtdattmp}/probeinfo.txt \
        -a ${dir_mtdattmp}/geno.eigenstrat \
        -b ${dir_mtdattmp}/ind.txt \
        -c ${dir_mtdattmp}/snp.txt \
        -p ${dir_mtdattmp}/finalgene.txt \
        -q ${dir_mtdattmp}/finalsnp.txt \
        -r ${dir_mtdattmp}/finalmatrix.txt



echo "running MetaTissue step 3 (running mixed model)"
    filepath_metatissue="${dir_metatissuebin}/MetaTissueMM_alt" # <-- 'MetaTissueMM_2019', intel 18.0.3 compiler
    mtprefix="MT" # <-- * consider changing if alt MetaTissue params used  (e.g., 'MTheuristic')
    ${filepath_metatissue} \
         --expr ${dir_mtdattmp}/finalgene.txt \
         --geno ${dir_mtdattmp}/finalsnp.txt \
         --matrix ${dir_mtdattmp}/finalmatrix.txt \
         --cisonly 10 \
         --output ${dir_mtdattmp}/${mtprefix} \
         --metatissue_bin_path ${dir_metatissuebin}
    
    
echo "running MetaTissue step 4 (mixed-model --> METASOFT)"
    filepath_metasoft="${dir_metatissuebin}/Metasoft/Metasoft.jar" # <-- 
    filepath_metasoft_pvals="${dir_metatissuebin}/Metasoft/HanEskinPvalueTable.txt"  # <--
    msoftprefix="FERE2" # <-- change if e.g., want to check exact vs mcmc, binary effects 
        java -jar ${filepath_metasoft} \
            -pvalue_table ${filepath_metasoft_pvals} \
            -input ${dir_mtdattmp}/${mtprefix}.SNP.0.mm.beta.std.txt.gz \
            -correlation ${dir_mtdattmp}/${mtprefix}.SNP.0.mm.corr.txt.gz \
            -output ${dir_mtdattmp}/${mtprefix}${msoftprefix}.METASOFT.output.txt.gz \
            -log ${dir_mtdattmp}/${mtprefix}${msoftprefix}.metasoft.log.txt \
            -mvalue -mvalue_prior_sigma 0.4 -mvalue_p_thres 1 \
            -mvalue_method mcmc
    # - notes: the non-default arguments supplied to METASOFT,
    #  (namely -mvalue_prior_sigma 0.4 -mvalue_p_thres 1)
    #  come via the '${mtprefix}.SNP.0.metasoft.sh' script output by MetaTissue in step 3
    # - this METASOFT version is bundled with MetaTissue
    #   thus some arguments (-correlation) will give errors if other METASOFT vers used



echo "MetaTissue complete. moving files to final result folder."
    diroutfinal=${work_dir}/simulation_study/simulation_results/MetaTissue/
    fpfinal_beta=${diroutfinal}/${mtprefix}${msoftprefix}_betas_res_${simnameout}.txt
    fpfinal_se=${diroutfinal}/${mtprefix}${msoftprefix}_se_res_${simnameout}.txt
    fpfinal_mtmetasoft=${diroutfinal}/${mtprefix}${msoftprefix}_res_${simnameout}.txt

    
    # consider adding a check to avoid overwritting 
    if [[ -s $fpfinal_mtmetasoft || -s $fpfinal_beta  || -s $fpfinal_se ]]
    then
        echo "WARNING: final file(s) already may exist for these sim parameters."
    fi
    
    echo "final filepaths of output files:"
    echo $fpfinal_mtmetasoft
    echo $fpfinal_beta
    echo $fpfinal_se
    
    mkdir -p $diroutfinal

    # copy METASOFT output as-is
    zcat ${dir_mtdattmp}/${mtprefix}${msoftprefix}.METASOFT.output.txt.gz \
        > ${fpfinal_mtmetasoft}

    # effect sizes (beta)
    awk -v nT=$nT 'BEGIN { printf "QTL"; for (i=1; i<=nT; i++) printf "\tT%s", i; printf "\n" }' \
        > ${fpfinal_beta}
    zcat ${dir_mtdattmp}/${mtprefix}.SNP.0.mm.beta.std.txt.gz \
        | awk '{printf $1; for (i=2; i<=NF; i+=2) printf "\t%s", $i; printf "\n" }' - \
        >> ${fpfinal_beta}
        
    # standard error
    awk -v nT=$nT 'BEGIN { printf "QTL"; for (i=1; i<=nT; i++) printf "\tT%s", i; printf "\n" }' \
        > ${fpfinal_se}
    zcat ${dir_mtdattmp}/${mtprefix}.SNP.0.mm.beta.std.txt.gz \
        | awk '{printf $1; for (i=3; i<=NF; i+=2) printf "\t%s", $i; printf "\n" }' - \
        >> ${fpfinal_se}

    # optionally remove intermediate MetaTissue files to save space 
    delete_tmp="FALSE" # <--
    if [[ "${delete_tmp}" == "TRUE" ]]
    then
        echo "delete_tmp == TRUE. clearing metatissue_tmp folder."
        rm -rf ${dir_mtdattmp}
    fi

