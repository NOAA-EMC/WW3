# About the bin directory 

This is the WW3 bin directory which mostly serves to create the make file
and set up the environment.  Below we have three sections, build environment
varibles, quick start instructions and information from various centers that
use WW3.

## CMake Build

Requires CMake 3.19 or greated.

To do an out-of-source build:

Create a build directory in the top-level directory:

```
mkdir build && cd build
cmake .. -DSWITCH=<switch_file> -DCMAKE_INSTALL_PREFIX=<dir>
make <-jn>
make install
```

The CMake build builds all available executables with the given switches.

# Build Enivronment Variables 

This is the WW3 bin directory which mostly serves to create the make file 
and set up the environment 

WW3 build can have the following env variables set: 

## Optional variable to use with any configuration: 

WW3_PARCOMPN = <how many parallel make tasks to use, default to 4 if not set >


## To build NetCDF executables you need:

The requirements for NetCDF are: 
* NetCDF version 4.1.1 or higher (Use "nc-config --version" to check) 
* NetCDF-4 API enabled (Use "nc-config --has-nc4" to check) 
* Define NETCDF_CONFIG env variable

NETCDF_CONFIG = < path to NetCDF-4 nc-config utility >

## To use the PDLIB switch:

METIS_PATH = < path before lib/libparmetis.a and lib/libmetis.a >

## To compile ww3_grib 

The following paths/libraries are expected to be set: 

G2_LIB4, W3EMC_LIB4, BACIO_LIB4, JASPER_LIB, PNG_LIB, and Z_LIB

If using NCEP hpc-stack, you just need to set the following: 

    JASPER_LIB=$JASPER_ROOT/lib64/libjasper.a
    PNG_LIB=$PNG_ROOT/lib64/libpng.a
    Z_LIB=$ZLIB_ROOT/lib/libz.a 
    
the other vairables should already be appropriately set. 

# Quick Start for building WW3 

After cloning and changing into the WW3 directory: 

Run the w3_setup script: 

    ./model/bin/w3_setup model -c <comp> -s <switch> 

Build a WW3 exe after setting up any appropraite environment variables described above. 

    cd model/bin/ 
    ./w3_make ww3_grid 


# Below is information about various centers use of WW3: 

## Information from Ifremer: 

At Ifremer we run several wave models routinely using the WAVEWATCH codes, 
these are part of the MARC system and are reported under "LOPS" 
in the monthly JCOMM verification reports. 
The switch files and namelist values (other than default settings) for the 
different models are given below. Please note that we use different parameter
settings for hindcasts and forecasts to adjust for the wind biases. 

globmulti : A routine run global wave model driven by ECMWF winds

    switch file : switch_Ifremer1 
    Namelist values (for forecasts, as of 2014/03/18 ):
      &MISC CICE0 = 0.25, CICEN = 0.75, FLAGTR = 4 /  
      &SIN4 BETAMAX = 1.45 /
    Namelist values for hindcasts with CFSR winds (1994 to 2006) : 
      &MISC CICE0 = 0.25, CICEN = 0.75, FLAGTR = 4 /
      &SIN4 BETAMAX = 1.33

medmulti :  A routine run driven by ECMWF winds

    switch file : switch_Ifremer1
    Namelist values : 
      &SIN4 BETAMAX = 1.50, ZALP=0.006, ZWND = 5.,
            Z0MAX = 0.0020, TAUWSHELTER = 0.0 /
      &SDS4 SDSBR = 0.00085,
            SDSC2 = -2.2E-5, SDSCUM = 0.000, FXFM3= 2.5 /
      &SNL1 NLPROP = 2.7E7 /

norgasug : a 110 k node mesh of the French Atlantic + Channel + North sea

    switch file : switch_Ifremer1
    Namelist values : 
      &UG   UGOBCAUTO = T, UGOBCDEPTH = -20.0,  EXPFSN = T/
      &SBT4 SEDMAPD50 = T, BOTROUGHMIN =  0.0400, BOTROUGHFAC = 1.0 /
      &REF1 REFCOAST=0.1, REFSLOPE=0.20, REFMAP = 0, REFFREQPOW = 3,
            REFCOSP_STRAIGHT=4, REFFREQ=1., REFSUBGRID = 0.00, REFRMAX = 0.8  /


## Information from NCEP


At the NCEP operational center we run several wave models using the WAVEWATCH III codes. The switch files 
and namelist values (other than default settings) for the different models are given below

multi_1 : An operational global wave model driven by GFS winds (retired system)

    switch file : switch_NCEP_st4sbs 
    Namelist values :
      &SIN4 BETAMAX = 1.33 /
      &MISC CICE0 = 0.25, CICEN = 0.75, FLAGTR = 4 /

multi_2 : An operational global hurricane wave model driven by a blend of hurricane / atmospheric winds (retired system)

    switch file : switch_NCEP_st2 
    Namelist values : 
      &SBT1 GAMMA = -0.019 /
      &PRO3 WDTHCG = 4.00, WDTHTH = 4.00 /
      &MISC FLAGTR = 4, CICE0 = 0.25, CICEN = 0.75 /

Great Lakes: the GLW/GLWN system caters for the five great lakes between USA and Canada. Settings for ST4 differ to account 
for specific wind forcing and dissipation designs

    switch file : switch_NCEP_st4
    Namelist values :
      &SIN4 BETAMAX = 1.55, Z0MAX = 1.002 /
      &SDS4 SDSC1 = 1.0, SDSC2 = 0.0, SDSBCK = 0.185, SDSHCK = 1.5, SDSDTH = 0 /
      &SNL1 NLPROP = 2.5E7 /
      &MISC FLAGTR = 0, CICE0 = 0.25, CICEN = 0.75 /


## Information from NRL

The Oceanography Division of the Naval Research Laboratory (NRL) is an R&D organization that, in addition 
to a number of other missions, supports model development and testing for Fleet Numerical Meteorology 
and Oceanography Center (FNMOC), a U.S. Navy operational center.

 NRL circa 2012 & NAVO circa 2013/2014
 
    switch_file : switch_NRL1
    Namelist values : 
      &MISC FLAGTR = 4, CICE0 = 0.25, CICEN = 0.75 /

 NRL circa 2014
 
    switch_file : switch_NRL2
    Namelist values : 
      &MISC FLAGTR = 4, CICE0 = 0.25, CICEN = 0.75 /
      &SBT1 GAMMA = -0.038 /

 NRL experimental model circa 2014
 
    switch_file : switch_NRL3
    Namelist values : 
      &MISC FLAGTR = 4, CICE0 = 0.25, CICEN = 0.75 /
      &SBT1 GAMMA = -0.038 /
      &FLX4 CDFAC = 1.230E-4 /

 NRL pre-operational model circa 2018
 
    switch_file : switch_NRL4
    Namelist values :
      &SIC4 IC4METHOD = 6,
       IC4FC =   0.045   , 0.055   ,  0.10   , 0.15    , 0.20    ,
                 0.25    , 0.30    , 0.35    , 0.40    , 99.0
       IC4KI =   1.0e-6  , 2.0e-6  , 2.94e-06, 4.27e-06, 7.95e-06,
                 2.95e-05, 1.12e-04, 2.74e-04, 4.95e-04, 8.94e-04
                    /
    &MISC FLAGTR = 2 , TRCKCMPR = F /
    &SIN4 BETAMAX = 1.2000 /
    &SBT1 GAMMA = -0.038 /


## Information from UKMO

At writing (October 2018) the Met Office run operational models based on WAVEWATCH III v4.18. Models using version 6 will 
be used for R&D and operational implementation at a future date. Version 6 switch settings and namelist values for Global 
and UK operational configurations are provided as follows:

Global wave model (25-12-6-3km SMC grid)

    switch file : switch_UKMO_gbl
    Namelist settings : 
    &PSMC DTIME = 57600.0, LATMIN=85.0, RFMAXD = 36.0, AVERG=.TRUE., 
        LvSMC=4, JEQT=2816  /
    &SIN4 BETAMAX = 1.36 /
    &SBT1 GAMMA = -0.038 /
    &SDB1 BJALFA = 0.2 /
    &MISC CICE0 = 0.5, CICEN = 0.5, FLAGTR = 2, PTM = 2 /
    &FLX3 CDMAX = 3.5E-3 , CTYPE = 0 /

UK regional model (7km regular grid)

    switch file : switch_UKMO_reg
    Namelist settings :
    &SIN4 BETAMAX = 1.45 /
    &SBT1 GAMMA = -0.038 /
    &SDB1 BJALFA = 0.2 /
    &PRO3 WDTHCG = 3.0, WDTHTH = 3.0 /
    &MISC CICE0 = 0.25, CICEN = 0.75, FLAGTR = 4, PTM = 2 /
    &FLX3 CDMAX = 3.5E-3 , CTYPE = 0 /

UK wave model (3-1.5km SMC grid)

    switch file : switch_UKMO_uk
    Namelist settings : 
    &PSMC DTIME = 10800.0, LATMIN=85.0, RFMAXD = 36.0, AVERG=.TRUE.,
                LvSMC=2, NBISMC=1711,   /
    &ROTD PLAT=37.50, PLON=177.50 /
    &SIN4 BETAMAX = 1.45 /
    &SBT1 GAMMA = -0.038 /
    &SDB1 BJALFA = 0.2 /
    &MISC CICE0 = 0.25, CICEN = 0.25, FLAGTR = 1, PTM = 2 /
    &FLX3 CDMAX = 3.5E-3 , CTYPE = 0 /


## Information from University of Melbourne


The Ocean Engineering Center at the University of Melbourne, closely
collaborating with the U.S. Naval Research Laboratory, has put
extensive research effort into the development of source terms for
spectral wave models, such as the ST6 source term package (Sin, Sds,
Sswl), IC5 (Sice). A new nonlinear term based on the Generalized Kenitic
Equation is also under preparation.

Since version 6.05, we run ST6 in two different ways (By default,
    UPROXY = 32 * UST):

1) [switch_UoM_nl1] for ST6+DIA (**the default setup of ST6**)
   Namelist values:

        &FLX4 CDFAC = 1.0 /
        &SIN6 SINA0 = 0.090, SINFC = 6.00 /
        &SDS6 SDSA1 = 4.75E-06, SDSA2 = 7E-05 /
        &SWL6 SWLB1 = 0.41E-02 /

2) [switch_UoM_nl3] for ST6+GMD
   Namelist values:

        $
        $ ST6 parameters
        $
          &FLX4 CDFAC = 1.0 /
          &SIN6 SINA0 = 0.05, SINFC = 6.00 /
          &SDS6 SDSA1 = 4.75E-06, SDSA2 = 7E-05 /
          &SWL6 SWLB1 = 6E-3 /
        $
        $ G35 for ST6
        $
           &SNL3 NQDEF =  5, MSC =  0.00,  NSC = -3.50 /
           &ANL3 QPARMS = 0.127, 0.000,   3.0, 0.488E+08, 0.000E+00 ,
                          0.127, 0.097,  21.0, 0.126E+09, 0.000E+00 ,
                          0.233, 0.098,  26.5, 0.620E+08, 0.000E+00 ,
                          0.283, 0.237,  24.7, 0.283E+08, 0.000E+00 ,
                          0.355, 0.183,   0.0, 0.117E+08, 0.000E+00 /

3) [switch_UoM_nl3s] for ST6+GMD+Nonlinear filter

   Same as 2) but **NLS** is included in the switch file, which is
   helpful to suppress the spurious high-frequency noise in the wave
   spectrum when a high-resolution spectral grid is used (say,
   Δθ <= 5 deg).

