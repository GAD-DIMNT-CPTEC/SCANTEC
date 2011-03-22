dset ^Noah/%y4/%y4%m2%d2/%y4%m2%d2%h200.d01.grb
options template
index ^teste1.idx
undef 9.999E+20
title LISMERRA - Noah
*  produced by grib2ctl v0.9.12.5p45
dtype grib 255
ydef 607 linear -48.750000 0.1
xdef 490 linear -82.850000 0.100000
tdef 81984 linear 06Z01Jan1979 3hr
zdef 4 levels 0.1 0.3 0.6 1.0
vars 31
SNFRALBEDO  0  84,  1,0  ** surface Albedo [%]
QSB         0 234,  1,0  ** surface Baseflow-groundwater runoff [kg/m^2]
CANOPINT    0 223,  1,0  ** surface Plant canopy surface water [kg/m^2]
SNOWFFORC   0 161,  1,0  ** surface Clear sky downward solar flux [W/m^2]
RAINFFORC   0 162,  1,0  ** surface Clear sky upward long wave flux [W/m^2]
LWDOWNFORC  0 205,  1,0  ** surface Downward long wave flux [W/m^2]
SWDOWNFORC  0 204,  1,0  ** surface Downward short wave flux [W/m^2]
WINDFORC    0 177,  1,0  ** surface East longitude (0-360) [deg]
ESOIL       0 199,  1,0  ** surface Direct evaporation from bare soil [W/m^2]
ECANOP      0 200,  1,0  ** surface Canopy water evaporation [W/m^2]
EVAP        0  57,  1,0  ** surface Evaporation [kg/m^2]
QG          0 155,  1,0  ** surface Ground heat flux [W/m^2]
QLE         0 121,  1,0  ** surface Latent heat flux [W/m^2]
SOILWET     0 207,  1,0  ** surface Moisture availability [%]
LWNET       0 112,  1,0  ** surface Net long wave (surface) [W/m^2]
SWNET       0 111,  1,0  ** surface Net short wave (surface) [W/m^2]
PSURFFORC   0   1,  1,0  ** surface Pressure [Pa]
DELSWE      0 149,  1,0  ** surface Potential vorticity [m^2/s/kg]
QH          0 122,  1,0  ** surface Sensible heat flux [W/m^2]
SNOWDPTH    0  66,  1,0  ** surface Snow depth [m]
QSM         0  99,  1,0  ** surface Snow melt [kg/m^2]
SNOWCVR     0 238,  1,0  ** surface Snow cover [%]
SOILM       4  86,112  ** 0-1 cm underground Soil moisture content [kg/m^2]
QAIRFORC    0  51,  1,0  ** surface Specific humidity [kg/kg]
QS          0 235,  1,0  ** surface Storm surface runoff [kg/m^2]
TAIRFORC    0  11,  1,0  ** surface Temp. [K]
TVEG        0 210,  1,0  ** surface Transpiration [W/m^2]
TSOIL       4  85,112,1  ** 0-1 cm underground Soil temp. [K]
AVGSURFT    0 148,  1,0  ** surface Meridional gravity wave stress [N/m^2]
SWE         0  65,  1,0  ** surface Accum. snow [kg/m^2]
POTEVAP     0 145,  1,0  ** Potential Evapotranspiration []
ENDVARS
