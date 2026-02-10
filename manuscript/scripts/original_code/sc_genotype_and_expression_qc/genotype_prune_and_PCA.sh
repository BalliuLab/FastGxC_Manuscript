    #!/bin/bash

    geno_file_plink=$1 ## plink file prefix
    cell=$2 ## current celltype 
    individual_file=$3 ## list of individuals in a file who pass expression QC
    
    ## first prune snps based on LD
    ### LD parameters
    window_size=200
    step_size=50
    r2=0.25

    ## thin SNPs and create new plink files
    plink --bfile $geno_file_plink --keep $individual_file --indep-pairwise $window_size $step_size $r2 --make-bed --out ${geno_file_plink}_$cell

    ## run PCA
    plink2 --bfile ${geno_file_plink}_$cell --pca --out ${geno_file_plink}_${cell}_pca

    ## clean up uneeded files 
    rm ${geno_file_plink}_$cell.*




