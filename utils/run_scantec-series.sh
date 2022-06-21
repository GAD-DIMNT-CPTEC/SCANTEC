#! /bin/bash -x

bpath=/lustre_xc50/carlos_bastarz/SCANTEC.2.0.0b2/bin/serie

Exps=(X666 X126ALEX XENMALEX T126OPER TENMOPER)
Regs=(gl hn tr hs as)

inctime=${HOME}/bin/inctime

data_i=2020060100
data_f=2020081500

data_a=${data_i}

while [ ${data_a} -le ${data_f} ]
do

  DATAI=${data_a}
  DATAF=${data_a}
  
  for exp in ${Exps[@]}
  do
  
    mkdir -p ${bpath}/${exp}
  
    EXP="${exp}"
  
    if [ ${exp} == "GFS" ]
    then
      MODELTABLE="GFS_0p25_5levs"
      FILENAME="/lustre_xc50/carlos_bastarz/GFS_subset/%iy4%im2%id2%ih2/gfs.t00z.pgrb2.0p25.f%h3.%iy4%im2%id2%ih2.ctl"
      ICNFILENAME="/lustre_xc50/carlos_bastarz/GFS_subset/%iy4%im2%id2%ih2/gfs.t00z.pgrb2.0p25.f000.%iy4%im2%id2%ih2.ctl"
      FCTOTAL="168"
    fi
  
    if [ ${exp} == "X666" ]
    then
      MODELTABLE="BAM_TQ0666L064_33levs"
      FILENAME="/lustre_xc50/ioper/data/BAM/TQ0666L064/%iy4%im2%id2%ih2/dataout/pos/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0666L064.ctl"
      ICNFILENAME="/lustre_xc50/ioper/data/BAM/TQ0666L064/%iy4%im2%id2%ih2/dataout/pos/GPOSNMC%iy4%im2%id2%ih2%iy4%im2%id2%ih2P.icn.TQ0666L064.ctl"
      FCTOTAL="264"
    fi
  
    if [ ${exp} == "X126ALEX" ]
    then
      MODELTABLE="BAM_TQ0126L028_9levs"
      FILENAME="/lustre_xc50/carlos_bastarz/oensMB09.svn/pos/dataout.exp_anlalex_choppsmt_018_100/TQ0126L028/%iy4%im2%id2%ih2/NMC/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0126L028.ctl"
      ICNFILENAME="/lustre_xc50/carlos_bastarz/oensMB09.svn/pos/dataout.exp_anlalex_choppsmt_018_100/TQ0126L028/%iy4%im2%id2%ih2/NMC/GPOSNMC%iy4%im2%id2%ih2%iy4%im2%id2%ih2P.icn.TQ0126L028.ctl"
      FCTOTAL="360"
    fi
  
    if [ ${exp} == "XENMALEX" ]
    then
      MODELTABLE="BAM_TQ0126L028_9levs"
      FILENAME="/lustre_xc50/carlos_bastarz/oensMB09.svn/ensmed/dataout.exp_anlalex_choppsmt_018_100/TQ0126L028/%iy4%im2%id2%ih2/GPOSENM%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0126L028.ctl"
      ICNFILENAME="/lustre_xc50/carlos_bastarz/oensMB09.svn/ensmed/dataout.exp_anlalex_choppsmt_018_100/TQ0126L028/%iy4%im2%id2%ih2/GPOSENM%iy4%im2%id2%ih2%iy4%im2%id2%ih2P.icn.TQ0126L028.ctl"
      FCTOTAL="360"
    fi
  
    if [ ${exp} == "X126" ]
    then
      MODELTABLE="BAM_TQ0126L028_9levs"
      FILENAME="/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/pos/dataout/TQ0126L028/%iy4%im2%id2%ih2/NMC/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0126L028.ctl"
      ICNFILENAME="/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/pos/dataout/TQ0126L028/%iy4%im2%id2%ih2/NMC/GPOSNMC%iy4%im2%id2%ih2%iy4%im2%id2%ih2P.icn.TQ0126L028.ctl"
      FCTOTAL="360"
    fi
  
    if [ ${exp} == "XENM" ]
    then
      MODELTABLE="BAM_TQ0126L028_9levs"
      FILENAME="/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/ensmed/dataout/TQ0126L028/%iy4%im2%id2%ih2/GPOSENM%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0126L028.ctl"
      ICNFILENAME="/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/ensmed/dataout/TQ0126L028/%iy4%im2%id2%ih2/GPOSENM%iy4%im2%id2%ih2%iy4%im2%id2%ih2P.icn.TQ0126L028.ctl"
      FCTOTAL="360"
    fi
  
    if [ ${exp} == "T126" ]
    then
      MODELTABLE="BAM_TQ0126L028_9levs"
      FILENAME="/lustre_xc50/carlos_bastarz/from_tupa/dados/ensemble/dsk001/oens_MB09_tupa/pos/dataout/TQ0126L028/%iy4%im2%id2%ih2/NMC/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0126L028.ctl"
      ICNFILENAME="/lustre_xc50/carlos_bastarz/from_tupa/dados/ensemble/dsk001/oens_MB09_tupa/pos/dataout/TQ0126L028/%iy4%im2%id2%ih2/NMC/GPOSNMC%iy4%im2%id2%ih2%iy4%im2%id2%ih2P.icn.TQ0126L028.ctl"
      FCTOTAL="360"
    fi
  
    if [ ${exp} == "TENM" ]
    then
      MODELTABLE="BAM_TQ0126L028_9levs"
      FILENAME="/lustre_xc50/carlos_bastarz/from_tupa/dados/ensemble/dsk001/oens_MB09_tupa/ensmed/dataout/TQ0126L028/%iy4%im2%id2%ih2/GPOSENM%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0126L028.ctl"
      ICNFILENAME="/lustre_xc50/carlos_bastarz/from_tupa/dados/ensemble/dsk001/oens_MB09_tupa/ensmed/dataout/TQ0126L028/%iy4%im2%id2%ih2/GPOSENM%iy4%im2%id2%ih2%iy4%im2%id2%ih2P.icn.TQ0126L028.ctl"
      FCTOTAL="360"
    fi
  
    SCANNAME="SCANTEC_${EXP}"
    PATHBIN=${bpath}/${EXP}
  
    cat ${bpath}/scantec.qsb.template | sed "s,#SCANNAME#,${SCANNAME},g" > ${bpath}/${exp}/scantec.qsb
    sed -i "s,#PATHBIN#,${PATHBIN},g" ${bpath}/${exp}/scantec.qsb
    sed -i "s,#EXP#,${exp},g" ${bpath}/${exp}/scantec.qsb
  
    for reg in ${Regs[@]}
    do
  
      if [ ${reg} == "gl" ]; then lowletflat="-80    "; lowleftlon="  0    "; uprightlat=" 80    "; uprightlon="360    "; fi
      if [ ${reg} == "hn" ]; then lowletflat=" 20    "; lowleftlon="  0    "; uprightlat=" 80    "; uprightlon="360    "; fi
      if [ ${reg} == "tr" ]; then lowletflat="-20    "; lowleftlon="  0    "; uprightlat=" 20    "; uprightlon="360    "; fi
      if [ ${reg} == "hs" ]; then lowletflat="-80    "; lowleftlon="  0    "; uprightlat="-20    "; uprightlon="360    "; fi
      if [ ${reg} == "as" ]; then lowletflat="-49.875"; lowleftlon="-82.625"; uprightlat=" 11.375"; uprightlon="-35.375"; fi
  
      mkdir -p ${bpath}/${exp}/${reg}
  
      cat ${bpath}/scantec.conf.template | sed "s,#DATAI#,${DATAI},g" > ${bpath}/${exp}/${reg}/scantec.conf
      sed -i "s,#DATAI#,${DATAI},g" ${bpath}/${exp}/${reg}/scantec.conf
      sed -i "s,#DATAF#,${DATAF},g" ${bpath}/${exp}/${reg}/scantec.conf  
  
      sed -i "s,#lowletflat#,${lowletflat},g" ${bpath}/${exp}/${reg}/scantec.conf
      sed -i "s,#lowleftlon#,${lowleftlon},g" ${bpath}/${exp}/${reg}/scantec.conf
      sed -i "s,#uprightlat#,${uprightlat},g" ${bpath}/${exp}/${reg}/scantec.conf
      sed -i "s,#uprightlon#,${uprightlon},g" ${bpath}/${exp}/${reg}/scantec.conf
  
      sed -i "s,#MODELTABLE#,${MODELTABLE},g" ${bpath}/${exp}/${reg}/scantec.conf
      sed -i "s,#EXP#,${EXP},g" ${bpath}/${exp}/${reg}/scantec.conf
      sed -i "s,#ICNFILENAME#,${ICNFILENAME},g" ${bpath}/${exp}/${reg}/scantec.conf
      sed -i "s,#FILENAME#,${FILENAME},g" ${bpath}/${exp}/${reg}/scantec.conf
    
      sed -i "s,#FCTOTAL#,${FCTOTAL},g" ${bpath}/${exp}/${reg}/scantec.conf
  
      PATHOUT=/lustre_xc50/carlos_bastarz/SCANTEC.2.0.0b2/dataout/serie/${reg}
  
      sed -i "s,#PATHOUT#,${PATHOUT},g" ${bpath}/${exp}/${reg}/scantec.conf  
   
      mkdir -p ${PATHOUT}
  
      cp ${bpath}/scantec.x ${bpath}/${exp}/${reg}/
  
    done
  
    rm ${bpath}/${exp}/monitor.t
    qsub -W block=true ${bpath}/${exp}/scantec.qsb
    until [ -e ${bpath}/${exp}/monitor.t ]; do sleep 1s; done
  
  done

  data_a=$(${inctime} ${data_a} +1dy %y4%m2%d2%h2)

done
