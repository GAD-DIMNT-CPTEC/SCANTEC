#! /bin/bash

bpath=/lustre_xc50/carlos_bastarz/SCANPLOT/SCANPLOT_T11212

Figs=(lines scorecard dTaylor lines_tStudent)

cat << EOF > ${bpath}/test_cmd-plot_functions.qsb
#!/bin/bash -x
#PBS -o ${bpath}/test_cmd-plot_functions.out
#PBS -e ${bpath}/test_cmd-plot_functions.err
#PBS -l walltime=00:05:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N SCANPLOT
#PBS -q pesq
#PBS -J 0-$((${#Figs[@]}-1))

$(for fig in ${!Figs[@]}; do echo "if [ \${PBS_ARRAY_INDEX} -eq ${fig} ]; then nfig=${Figs[$fig]}; fi"; done)

source /lustre_xc50/carlos_bastarz/.python/anaconda3/envs/SCANPLOT-XC50/bin/activate

cd ${bpath}

python ${bpath}/test_cmd-plot_\${nfig}.py
EOF

qsub ${bpath}/test_cmd-plot_functions.qsb

exit 0
