#! /bin/bash

bpath=/lustre_xc50/carlos_bastarz/SCANPLOT/SCANPLOT_T11212

Figs=(lines scorecard dTaylor lines_tStudent)

if [ ${#Figs[@]} == 1 ]
then 
  PBSDIRECT=""
  CMD="nfig=$Figs"
else
  PBSDIRECT="#PBS -J 0-$((${#Figs[@]}-1))"
  CMD="$(for fig in ${!Figs[@]}; do echo "if [ \${PBS_ARRAY_INDEX} -eq ${fig} ]; then nfig=${Figs[$fig]}; fi"; done)"
fi

cat << EOF > ${bpath}/test_cmd-get_dataframe.qsb
#!/bin/bash -x
#PBS -o ${bpath}/test_cmd-get_dataframe.out
#PBS -e ${bpath}/test_cmd-get_dataframe.err
#PBS -l walltime=00:05:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N SCANPLOT_GDF
#PBS -q pesq

source /lustre_xc50/carlos_bastarz/.python/anaconda3/envs/SCANPLOT-XC50/bin/activate

cd ${bpath}

aprun -n 1 -N 1 -d 1 /lustre_xc50/carlos_bastarz/.python/anaconda3/envs/SCANPLOT-XC50/bin/python3 ${bpath}/test_cmd-get_dataframe.py
EOF

cat << EOF > ${bpath}/test_cmd-get_dataset.qsb
#!/bin/bash -x
#PBS -o ${bpath}/test_cmd-get_dataset.out
#PBS -e ${bpath}/test_cmd-get_dataset.err
#PBS -l walltime=00:05:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N SCANPLOT_GDS
#PBS -q pesq

source /lustre_xc50/carlos_bastarz/.python/anaconda3/envs/SCANPLOT-XC50/bin/activate

cd ${bpath}

aprun -n 1 -N 1 -d 1 /lustre_xc50/carlos_bastarz/.python/anaconda3/envs/SCANPLOT-XC50/bin/python3 ${bpath}/test_cmd-get_dataset.py
EOF

cat << EOF > ${bpath}/test_cmd-plot_functions.qsb
#!/bin/bash -x
#PBS -o ${bpath}/test_cmd-plot_functions.out
#PBS -e ${bpath}/test_cmd-plot_functions.err
#PBS -l walltime=00:05:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N SCANPLOT_FGS
#PBS -q pesq
$PBSDIRECT

${CMD}

source /lustre_xc50/carlos_bastarz/.python/anaconda3/envs/SCANPLOT-XC50/bin/activate

cd ${bpath}

aprun -n 1 -N 1 -d 1 /lustre_xc50/carlos_bastarz/.python/anaconda3/envs/SCANPLOT-XC50/bin/python3 ${bpath}/test_cmd-plot_\${nfig}.py
EOF

get_dataframe=$(qsub ${bpath}/test_cmd-get_dataframe.qsb)
jobid1=$(echo $get_dataframe | awk 'match($0,/[0-9]+/){print substr($0, RSTART, RLENGTH)}')
get_dataset=$(qsub -W depend=afterok:${jobid1} ${bpath}/test_cmd-get_dataset.qsb)
jobid2=$(echo $get_dataset | awk 'match($0,/[0-9]+/){print substr($0, RSTART, RLENGTH)}')
qsub -W depend=afterok:${jobid2} ${bpath}/test_cmd-plot_functions.qsb

exit 0
