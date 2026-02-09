
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

mtprefix="MT" # <-- * consider changing if alt MetaTissue params used  (e.g., 'MTheuristic')
msoftprefix="FERE2" # <-- change if e.g., want to check exact vs mcmc, binary effects 


echo "MetaTissue complete. moving files to final result folder."
    diroutfinal=${work_dir}/simulation_study/simulation_results/MTFERE2/
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

    dir_mtdattmp="${work_dir}/simulation_study/simulation_results/metatissue_tmp/${simnameout}"
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



