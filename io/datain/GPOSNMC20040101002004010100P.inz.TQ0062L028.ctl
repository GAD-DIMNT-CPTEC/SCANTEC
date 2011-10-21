dset ^GPOSNMC20040101002004010100P.inz.TQ0062L028.grb
*
index ^GPOSNMC20040101002004010100P.inz.TQ0062L028.idx
*
undef -2.56E+33
*
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T062L28  COLD
*
dtype grib   255
*
options yrev
*
xdef   144 linear    0.000   2.5000000000
ydef    73 linear  -90.000   2.5000000000
tdef     1 linear 00Z01JAN2004 6hr
*
zdef    18 levels  1000  925  850  775  700  500  400  300  250  200
                  150  100   70   50   30   20   10    3
vars    26
topo  0 132,1,0 ** surface TOPOGRAPHY [m]
lsmk  0  81,1,0 ** surface LAND SEA MASK [0,1]
PSLC    0  135,    1,    0  ** sfc   SURFACE PRESSURE                        (HPA             )
UVES    0  192,    1,    0  ** sfc   SURFACE ZONAL WIND (U)                  (M/S             )
UVEL   18   33,  100,    0  **       ZONAL WIND (U)                          (M/S             )
VVES    0  194,    1,    0  ** sfc   SURFACE MERIDIONAL WIND (V)             (M/S             )
VVEL   18   34,  100,    0  **       MERIDIONAL WIND (V)                     (M/S             )
OMEG   18   39,  100,    0  **       OMEGA                                   (PA/S            )
FCOR   18   35,  100,    0  **       STREAM FUNCTION                         (M2/S            )
ZGEO   18    7,  100,    0  **       GEOPOTENTIAL HEIGHT                     (GPM             )
PSNM    0    2,  102,    0  ** msl   SEA LEVEL PRESSURE                      (HPA             )
TEMS    0  188,    1,    0  ** sfc   SURFACE ABSOLUTE TEMPERATURE            (K               )
TEMP   18   11,  100,    0  **       ABSOLUTE TEMPERATURE                    (K               )
UMRS    0  226,    1,    0  ** sfc   SURFACE RELATIVE HUMIDITY               (NO DIM          )
UMRL   18   52,  100,    0  **       RELATIVE HUMIDITY                       (NO DIM          )
UMES   18   51,  100,    0  **       SPECIFIC HUMIDITY                       (KG/KG           )
AGPL    0   54,  200,    0  ** atm   INST. PRECIPITABLE WATER                (KG/M2           )
ZORL    0   83,    1,    0  ** sfc   ROUGHNESS LENGTH                        (M               )
TSFC    0  187,    1,    0  ** sfc   SURFACE TEMPERATURE                     (K               )
USSL    0  182,    1,    0  ** sfc   SOIL WETNESS OF SURFACE                 (0-1             )
UZRS    0  183,  112,    0  ** landt SOIL WETNESS OF ROOT ZONE               (0-1             )
UZDS    0  184,  112,    0  ** landt SOIL WETNESS OF DRAINAGE ZONE           (0-1             )
T02M    0  128,  105,    2  ** sfc2m TEMPERATURE AT 2-M FROM SURFACE         (K               )
Q02M    0  199,    1,    0  ** sfc   SPECIFIC HUMIDITY AT 2-M FROM SURFACE   (KG/KG           )
U10M    0  130,  105,   10  ** sfc10m10 METRE U-WIND COMPONENT               (M/S             )
V10M    0  131,  105,   10  ** sfc10m10 METRE V-WIND COMPONENT               (M/S             )
endvars
