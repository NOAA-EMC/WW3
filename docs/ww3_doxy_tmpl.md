# WAVEWATCH III - Doxygen Reference

This document contains the needed information to mark-up WW3 source files with 
doxygen tags.  The main task for this is adding [headers](#header-templates) for 
each Fortran90 component (`FILE`,`MODULE`,`SUBROUTINE`,`FUNCTION`,`PROGRAM`). [Examples](#header-examples) 
are provided with input from existing code, and the html output shown as it will look from the browser. Additionally,
[inline](#inlining-example) documentation for `MODULE` variables is required (these variables are listed after the
`MODULE` declaration statement, but before the `CONTAINS` statement). Lastly, the `shell` [commands](#commands) for
building the doxygen documentation and displaying it in a browser are also given.
<br>
<br>
<br>


## Contents
* [Header Templates](#header-templates)
  * [FILE](#file), [MODULE](#module), [SUBROUTINE](#subroutine),
[FUNCTION](#function), [PROGRAM](#program)
* [Header Examples](#header-examples)
  * [FILE](#file-example), [MODULE](#module-example), [SUBROUTINE](#subroutine-example),
[FUNCTION](#function-example), [PROGRAM](#program-example)
* [Inlining Example](#inlining-example)
  * [MODULE variables](#module-variables) 
* [Commands](#commands)
<br>
<br>



## Header Templates
These header should be placed directly *above* the Fortran90 component declaration.

#### FILE
```
!> @file 
!> @brief 
!> 
!> @author 
!> @date 
!> 
```


#### MODULE
```
!>
!> @brief 
!> 
!> @details 
!>
!> @author 
!> @date 
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
```


#### SUBROUTINE
```
!>
!> @brief 
!>
!> @details 
!>
!> @param[inout] 
!>
!> @author 
!> @date 
!>
```


#### FUNCTION
```
!>
!> @brief 
!>
!> @details 
!>
!> @param 
!> @returns 
!>
!> @author 
!> @date 
!>
```


#### PROGRAM
```
!>
!> @brief 
!>
!> @details 
!> 
!> @author 
!> @date 
!> 
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
```
---
[\[Return to Contents\]](#contents)

<br>
<br>



## Header Examples
#### FILE Example
```F90
!> @file                                                                                         
!> @brief Contains driver program for multi-grid, W3MLTI.                                        
!>                                                                                               
!> @author H. L. Tolman @date 29-May-2009                                                        
                                                                                                 
#include "w3macros.h"                                                                            
!/ ------------------------------------------------------------------- /                         
!
```
---
[\[Return to Contents\]](#contents)
<br>
<br>


#### MODULE Example
```F90
!>                                                                                               
!> @brief Mean wave parameter computation for case without input and                             
!>  dissipation.                                                                                 
!>                                                                                               
!> @author H. L. Tolman  @date 29-May-2009                                                       
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>                                                                                               
!/ ------------------------------------------------------------------- /                         
      MODULE W3SRC0MD                                                                            
!/                                                                                               
!/                  +-----------------------------------+                                        
!/                  | WAVEWATCH III           NOAA/NCEP |                                        
!/                  |           H. L. Tolman            |                                        
!/                  |                        FORTRAN 90 |                                        
!/                  | Last update :         29-May-2009 |                                        
!/                  +-----------------------------------+                                        
!/                                                                                               
!/    05-Jul-2006 : Origination.                        ( version 3.09 )                         
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )                         
!/                                                                                               
!/    Copyright 2009 National Weather Service (NWS),                                             
!/       National Oceanic and Atmospheric Administration.  All rights                            
!/       reserved.  WAVEWATCH III is a trademark of the NWS.                                     
!/       No unauthorized use without permission.                                                 
!/                                                                                               
!  1. Purpose :                                        
!                                                                                                
!     Mean wave parameter computation for case without input and                                 
!     dissipation.                                                                               
!                                                                                                
!  2. Variables and types :                                                                      
!                                                                                                
!  3. Subroutines and functions :                                                                
!                                                                                                
!      Name      Type  Scope    Description                                                      
!     ----------------------------------------------------------------                           
!      W3SPR0    Subr. Public   Mean parameters from spectrum.                                   
!     ----------------------------------------------------------------                           
!                                                                                                
!  4. Subroutines and functions used :                                                           
!                                                                                                
!      Name      Type  Module   Description                                                      
!     ---------------------------------------------------------------- 
!      W3SPR0    Subr. Public   Mean parameters from spectrum.                                   
!     ----------------------------------------------------------------                           
!                                                                                                
!  4. Subroutines and functions used :                                                           
!                                                                                                
!      Name      Type  Module   Description                                                      
!     ----------------------------------------------------------------                           
!      STRACE    Subr. W3SERVMD Subroutine tracing.            ( !/S )                           
!     ----------------------------------------------------------------                           
!                                                                                                
!  5. Remarks :                                                                                  
!                                                                                                
!  6. Switches :                                                                                 
!                                                                                                
!       !/S      Enable subroutine tracing.                                                      
!       !/T      Test output, see subroutines.                                                   
!                                                                                                
!  7. Source code :                                                                              
!                                                                                                
!/ ------------------------------------------------------------------- /                         
!/                                                                                               
      PUBLIC
```
---
[\[Return to Contents\]](#contents)
<br>
<br>


#### SUBROUTINE Example
```F90
!> @brief Interface to watershed partitioning routines.                                          
!>                                                                                               
!> @details Watershed Algorithm of Vincent and Soille, 1991,                                     
!>  implemented by Barbara Tracy (USACE/ERDC) for NOAA/NCEP.                                     
!>                                                                                               
!>  This version of W3PART contains alternate Met Office partitioning                            
!>  methods, selected at runtime using the \c PTMETH namlist variable:                           
!>    -# Standard WW3 partitioning, as per original method described                             
!>       by Barbary Tracy.                                                                       
!>    -# Met Office extended partitioning using split-partitions                                 
!>       (removes the wind sea part of any swell partiton and combines                           
!>       with total wind sea partition).                                                         
!>    -# Met Office "wave systems" - no classification or combining of                           
!>       wind sea partitions. All partitions output and ordered simply                           
!>       by wave height.                                                                         
!>    -# Classic, simple wave age based partitioning generating                                  
!>       a single wind sea and swell partition.                                                  
!>    -# 2-band partitioning; produces hi and low freqency band partitions                       
!>       using a user-defined cutoff frequency (\c PTFCUT). 
!>                                                                                               
!> @remarks                                                                                      
!>    - \c DIMXP will always be of size 2 when using \c PTMETH 4 or 5.                           
!>                                                                                               
!>    - To achieve minimum storage but guaranteed storage of all                                 
!>      partitions <tt>DIMXP = ((NK+1)/2) * ((NTH-1)/2)</tt>                                     
!>      unless specified otherwise below.                                                        
!>                                                                                               
!> @param[in]    SPEC    2-D spectrum E(f,theta)                                                 
!> @param[in]    UABS    Wind speed                                                              
!> @param[in]    UDIR    Wind direction                                                          
!> @param[in]    DEPTH   Water depth                                                             
!> @param[in]    WN      Wavenumebers for each frequency                                         
!> @param[out]   NP      Number of partitions found                                              
!>                       (-1=Spectrum without minumum energy;                                    
!>                       0=Spectrum with minumum energy but no partitions)                       
!> @param[out]   XP      Parameters describing partitions.                                       
!>                       Entry '0' contains entire spectrum                                      
!> @param[in]    DIMXP   Second dimension of XP                                                  
!>                                                                                               
!> @author Barbara Tracey, H. L. Tolman, M. Szyszka, Chris Bunney                                
!> @date 23 Jul 2018                                                                             
!>                                                                                               
      SUBROUTINE W3PART ( SPEC, UABS, UDIR, DEPTH, WN, NP, XP, DIMXP )                           
!/                                                                                               
!/                  +-----------------------------------+                                        
!/                  | WAVEWATCH III          USACE/NOAA |                                        
!/                  |          Barbara  Tracy           |                                        
!/                  |           H. L. Tolman            |                                        
!/                  |                        FORTRAN 90 |                                        
!/                  | Last update :         02-Dec-2010 |                                        
!/                  +-----------------------------------+                                    
!/                                                                                               
!/    28-Oct-2006 : Origination.                       ( version 3.10 )                          
!/    02-Dec-2010 : Adding a mapping PMAP between      ( version 3.14 )                          
!/                  original and combined partitions                                             
!/                  ( M. Szyszka )                                                               
!/                                                                                               
!  1. Purpose :                                                                                  
!                                                                                                
!     Interface to watershed partitioning routines.                                              
!                                                                                                
!  2. Method :                                                                                   
!                                                                                                
!     Watershed Algorithm of Vincent and Soille, 1991, implemented by                            
!     Barbara Tracy (USACE/ERDC) for NOAA/NCEP.                                                  
!                                                                                                
!  3. Parameters :                                                                               
!                                                                                                
!     Parameter list                                                                             
!     ----------------------------------------------------------------                           
!       SPEC    R.A.   I   2-D spectrum E(f,theta).                                              
!       UABS    Real   I   Wind speed.                                                           
!       UDIR    Real   I   Wind direction.                                                       
!       DEPTH   Real   I   Water depth.                                                          
!       WN      R.A.   I   Wavenumebers for each frequency.                                      
!       NP      Int.   O   Number of partitions.                                                 
!                           -1 : Spectrum without minumum energy.                                
!                            0 : Spectrum with minumum energy.                                   
!                                but no partitions.                                              
!       XP      R.A.   O   Parameters describing partitions.                                     
!                          Entry '0' contains entire spectrum.                                   
!       DIMXP   Int.   I   Second dimension of XP.                                               
!     ----------------------------------------------------------------
!                                                                                                
!  4. Subroutines used :                                                                         
!                                                                                                
!      Name      Type  Module   Description                                                      
!     ----------------------------------------------------------------                           
!      STRACE    Sur.  W3SERVMD Subroutine tracing.                                              
!     ----------------------------------------------------------------                           
!                                                                                                
!  5. Called by :                                                                                
!                                                                                                
!  6. Error messages :                                                                           
!                                                                                                
!  7. Remarks :                                                                                  
!                                                                                                
!     - To achieve minimum storage but guaranteed storage of all                                 
!       partitions DIMXP = ((NK+1)/2) * ((NTH-1)/2) unless specified                             
!       otherwise below.                                                                         
!                                                                                                
!     This version of W3PART contains alternate Met Office partitioning                          
!     methods, selected at runtime using the PTMETH namlist variable:                            
!         1) Standard WW3 partitioning                                                           
!         2) Met Office extended partitioning using split-partitions                             
!            (removes the wind sea part of any swell partiton and combines                       
!            with total wind sea partition).                      
!         3) Met Office "wave systems" - no classification or combining of                       
!            wind sea partitions. All partitions output and ordered simply                       
!            by wave height.                                                                     
!         4) Classic, simple wave age based partitioning generating                              
!            a single wind sea and swell partition. [DIMXP = 2]                                  
!         5) 2-band partitioning; produces hi and low freqency band partitions                   
!            using a user-defined cutoff frequency (PTFCUT). [DIMXP = 2]                         
!                                                                                                
!     (Chris Bunney, UK Met Office, Jul 2018)                                                    
!                                                                                                
!  8. Structure :                                                                                
!                                                                                                
!  9. Switches :                                                                                 
!                                                                                                
!     !/S    Enable subroutine tracing.                                                          
!     !/T    Enable test output                                                                  
!                                         
!                                                                                                
! 10. Source code :                                                                              
!                                                                                                
!/ ------------------------------------------------------------------- /                         
!/
```
---
[\[Return to Contents\]](#contents)
<br>
<br>


#### FUNCTION Example
```F90
!>                                                                                               
!> @brief Perform interpolation from regular to curvilinear grid                                 
!>  for a scalar field.                                                                          
!>                                                                                               
!> @details This function uses bilinear interpolation to                                         
!>  estimate the value of a function f at point (x,y). f is assumed                              
!>  to be on a regular grid, with the grid x values specified by                                 
!>  xarray with dimension x_len and the grid y values specified by                               
!>  yarray with dimension y_len.                                                                 
!>                                                                                               
!> @param X_LEN Dimension in X                                                                   
!> @param XARRAY 1D array for Longitudes                                                         
!> @param Y_LEN Dimension in Y                                                                   
!> @param YARRAY 1D array for Latitudes                                                          
!> @param FUNC 1D Field                                                                          
!> @param X Long for point in the curv grid                                                      
!> @param Y Lat for point in the curv grid                                                       
!> @param DELTA Threshold to determine if two values are equal                                   
!>                                                                                               
!> @returns INTERPOLATE                                                                          
!>                                                                                               
!> @author H. L. Tolman @date 25-Jul-2019                                                        
!>                                                                                               
      REAL FUNCTION INTERPOLATE(X_LEN,XARRAY,Y_LEN,YARRAY,FUNC, &                                
                                X,Y,DELTA)
                                X,Y,DELTA)                                                       
!/                                                                                               
!/                  +-----------------------------------+                                        
!/                  | WAVEWATCH III           NOAA/NCEP |                                        
!/                  |           H. L. Tolman            |                                        
!/                  |                        FORTRAN 90 |                                        
!/                  | Last update :        25-July-2019 |                                        
!/                  +-----------------------------------+                                        
!/                                                                                               
!/                     (R. Padilla-Hernandez, EMC/NOAA)                                          
!/                                                                                               
!/    29-July-2019 :                        ( version 7.13 )                                     
!/                                                                                               
!  1. Purpose :                                                                                  
!                                                                                                
!     Perform interpolation from regular to curvilinear grid for a                               
!     scalar field. THIS FUNCTION USES BILINEAR INTERPOLATION TO                                 
!     ESTIMATE THE VALUE OF A FUNCTION F AT POINT (X,Y) F IS ASSUMED                             
!     TO BE ON A REGULAR GRID, WITH THE GRID X VALUES SPECIFIED BY                               
!     XARRAY WITH DIMENSION X_LEN AND THE GRID Y VALUES SPECIFIED BY                             
!     YARRAY WITH DIMENSION Y_LEN                                                                
!                                                                                                
!  2. Parameters :                                                                               
!                                                                                                
!     Parameter list                                                                             
!     ----------------------------------------------------------------                           
!       X_LEN    Int.  Dimension in X                                                            
!       XARRAY   Int.  1D array for Longitudes                                                   
!       Y_LEN    Int.  Dimension in Y                                                            
!       YARRAY   Int.  1D array for Latitudes
!       FUNC     Int.  1D Field                                                                  
!       X,Y      Real  Long-Lat for point in the curv grid                                       
!       DELTA    Real  Threshold to determine if two values are equal                            
!     ----------------------------------------------------------------                           
!                                                                                                
!     Internal parameters                                                                        
!     ----------------------------------------------------------------                           
!       INX     Int.  Index in X on the rectiliniear grid that is                                
!                     closest to, but less than, the given value for a                           
!                     point in the curvilinear grid.                                             
!       JNX     Int.  Idem INX for for Y.                                                        
!       X1,Y1   Real  (Long, Lat) left-bottom corner for the square in                           
!                     regular grid, where the given value for the point                          
!                     in the curvilinear grid lies                                               
!       X2,Y2   Real  (Long, Lat) right-upper corner for the square in                           
!                     regular grid, where the given value for the point                          
!                     in the curvilinear grid lies                                               
!     ----------------------------------------------------------------                           
!                                                                                                
!  3. Subroutines used :                                                                         
!                                                                                                
!      Name          Type   Module   Description                                                 
!     ----------------------------------------------------------------                           
!     XYCURVISEARCH   Func. wmupdtmd Look for indexes in 1D array.                               
!     ----------------------------------------------------------------                           
!                                                                                                
!  4. Called by :                                                                                
!                                                                                                
!     Main program in which it is contained. 
                                                                                                
!     Main program in which it is contained.                                                     
!                                                                                                
!  5. Error messages :                                                                           
!                                                                                                
!       None.                                                                                    
!                                                                                                
!  6. Remarks :                                                                                  
!                                                                                                
!     -                                                                                          
!                                                                                                
!  7. Structure :                                                                                
!                                                                                                
!     See source code.                                                                           
!                                                                                                
!  8. Switches :                                                                                 
!                                                                                                
!       -                                                                                        
!                           
```
---
[\[Return to Contents\]](#contents)
<br>
<br>


#### PROGRAM Example
```F90
!> @brief Preprocessing of input data.                                                           
!>                                                                                               
!> @details Pre-processing of the input water level, current, wind, ice                          
!>  fields, momentum and air density, as well as assimilation data                               
!>  for the generic shell W3SHEL (ww3_shel.ftn).                                                 
!>                                                                                               
!> @author H. L. Tolman 
!> @date 22-Mar-2021                                                                             
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!                                                                                                
      PROGRAM W3PREP                                                                             
!/                                                                                               
!/                  +-----------------------------------+                                        
!/                  | WAVEWATCH III           NOAA/NCEP |                                        
!/                  |           H. L. Tolman            |                                        
!/                  |             A. Chawla             |                                        
!/                  |                        FORTRAN 90 |                                        
!/                  | Last update :         22-Mar-2021 |                                        
!/                  +-----------------------------------+                                        
!/                                                                                               
!/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )                         
!/    18-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )                         
!/    11-Jan-2001 : Flat grid option added              ( version 2.06 )                         
!/    17-Jul-2001 : Clean-up                            ( version 2.11 )                         
!/    24-Jan-2002 : Add data for data assimilation.     ( version 2.17 )                         
!/    30-Apr-2002 : Fix 'AI' bug for 1-D fields.        ( version 2.20 ) 
!/    24-Apr-2003 : Fix bug for NDAT = 0 in data.       ( version 3.03 )                         
!/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )                         
!/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )                         
!/    25-Sep-2007 : Switch header of file on or off,    ( version 3.13 )                         
!/                  Times to file (!/O15) (A. Chawla)                                            
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )                         
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )                         
!/                  (W. E. Rogers & T. J. Campbell, NRL)                                         
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )                         
!/                  (W. E. Rogers & T. J. Campbell, NRL)                                         
!/    15-May-2010 : Add ISI (icebergs and sea ice).     ( version 3.14.4 )                       
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )                       
!/                  (A. Roland and F. Ardhuin)                                                   
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to                          
!/                  specify index closure for a grid.   ( version 3.14 )                         
!/                  (T. J. Campbell, NRL)                                                        
!/     1-Apr-2011 : Fix bug GLOBX forcing with unst.    ( version 3.14.4 )                       
!/    19-Sep-2011 : Fix bug prep forcing with unst.     ( version 4.04 )                         
!/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.OF )                         
!/     3-Mar-2013 : Allows for longer input file name   ( version 4.09 )                         
!/    11-Nov-2013 : Allows for input binary files to be of WAVEWATCH                             
!/                  type (i.e. accounts for the header) ( version 4.13 )                         
!/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )                         
!/    22-Mar-2021 : Add momentum and air density        ( version 7.13 )                         
!/                                                                                               
!/    Copyright 2009-2012 National Weather Service (NWS),                                        
!/       National Oceanic and Atmospheric Administration.  All rights                            
!/       reserved.  WAVEWATCH III is a trademark of the NWS.                                     
!/       No unauthorized use without permission.
!/                                                                                               
!  1. Purpose :                                                                                  
!                                                                                                
!     Pre-processing of the input water level, current, wind, ice                                
!     fields, momentum and air density, as well as assimilation data                             
!     for the generic shell W3SHEL (ww3_shel.ftn).                                               
!                                                                                                
!  2. Method :                                                                                   
!                                                                                                
!     See documented input file.                                                                 
!                                                                                                
!  3. Parameters :                                                                               
!                                                                                                
!     Local parameters.                                                                          
!     ----------------------------------------------------------------                           
!       NDSI    Int.  Input unit number ("ww3_prep.inp").                                        
!       NDSLL   Int.  Unit number(s) of long-lat file(s)                                         
!       NDSF    I.A.  Unit number(s) of input file(s).                                           
!       NDSDAT  Int.  Unit number for output data file.                                          
!       IFLD    Int.  Integer input type.                                                        
!       ITYPE   Int.  Integer input 'format' type.                                               
!       NFCOMP  Int.  Number of partial input to be processed.                                   
!       FLTIME  Log.  Time flag for input fields, if false, single                               
!                     field, time read from NDSI.                                                
!       IDLALL  Int.  Layout indicator used by INA2R. +                                          
!       IDFMLL  Int.  Id. FORMAT indicator.           |                                          
!       FORMLL  C*16  Id. FORMAT.                     | Long-lat                                 
!       FROMLL  C*4   'UNIT' / 'NAME' indicator       |    file(s)                               
!       NAMELL  C*65  Name of long-lat file(s)        +                                          
!       IDLAF   I.A.   +                                                                         
!       IDFMF   I.A.   |    
!       FORMLL  C*16  Id. FORMAT.                     | Long-lat                                 
!       FROMLL  C*4   'UNIT' / 'NAME' indicator       |    file(s)                               
!       NAMELL  C*65  Name of long-lat file(s)        +                                          
!       IDLAF   I.A.   +                                                                         
!       IDFMF   I.A.   |                                                                         
!       FORMF   C.A.   | Idem. fields file(s)                                                    
!       FROMF   C*4    |                                                                         
!       NAMEF   C*65   +                                                                         
!       FORMT   C.A.  Format or time in field.                                                   
!       XC      R.A.  Components of input vector field or first                                  
!                     input scalar field                                                         
!       YC      R.A.  Components of input vector field or second                                 
!                     input scalar field                                                         
!       FX,FY   R.A.  Output fields.                                                             
!       ACC     Real  Required interpolation accuracy.                                           
!     ----------------------------------------------------------------                           
!                                                                                                
!  4. Subroutines used :                                                                         
!                                                                                                
!      Name      Type  Module   Description                                                      
!     ----------------------------------------------------------------                           
!      W3NMOD    Subr. W3GDATMD Set number of model.                                             
!      W3SETG    Subr.   Id.    Point to selected model.                                         
!      W3NDAT    Subr. W3WDATMD Set number of model for wave data.                               
!      W3SETW    Subr.   Id.    Point to selected model for wave data.                           
!      W3NOUT    Subr. W3ODATMD Set number of model for output.                                  
!      W3SETO    Subr.   Id.    Point to selected model for output.                              
!      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.                               
!      STRACE    Subr.   Id.    Subroutine tracing.                                              
!      NEXTLN    Subr.   Id.    Get next line from input filw
!      EXTCDE    Subr.   Id.    Abort program as graceful as possible.                           
!      STME21    Subr. W3TIMEMD Convert time to string.                                          
!      INAR2R    Subr. W3ARRYMD Read in an REAL array.                                           
!      INAR2I    Subr.   Id.    Read in an INTEGER array.                                        
!      PRTBLK    Subr.   Id.    Print plot of array.                                             
!      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.                           
!      W3FLDO    Subr. W3FLDSMD Opening of WAVEWATCH III generic shell                           
!                               data file.                                                       
!      W3FLDP    Subr.   Id.    Prepare interp. from arbitrary grid.                             
!      W3FLDG    Subr.   Id.    Reading/writing shell input data.                                
!      W3FLDD    Subr.   Id.    Reading/writing shell assim. data.                               
!      W3GSUC    Func. W3GSRUMD Create grid-search-utility object                                
!      W3GSUD    Subr. W3GSRUMD Destroy grid-search-utility object                               
!      W3GRMP    Func. W3GSRUMD Compute interpolation weights                                    
!     ----------------------------------------------------------------                           
!                                                                                                
!  5. Called by :                                                                                
!                                                                                                
!     None, stand-alone program.                                                                 
!                                                                                                
!  6. Error messages :                                                                           
!                                                                                                
!     - Checks on files and reading from file.                                                   
!     - Checks on validity of input parameters.                                                  
!                                                                                                
!  7. Remarks :                                                                                  
!                                                                                                
!     - Input fields need to be continuous in longitude and latitude.                            
!     - Longitude - latitude grid (Section 4.a) : program attempts to
!       detect closure type (ICLO) using longitudes of the grid. Thus,                           
!       it does not allow the user to specify the closure type, and so                           
!       tripole closure is not supported.                                                        
!     - Grid(s) from file (Section 4.a) : program reads logical variable                         
!       CLO(J) from .inp file. Thus, it does not allow the user to                               
!       specify more than two closure type (SMPL or NONE), and so                                
!       tripole closure is not supported.                                                        
                                                                                                 
!  8. Structure :                                                                                
!                                                                                                
!     ----------------------------------------------------                                       
!        1.a  Number of models.                                                                  
!                   ( W3NMOD , W3NOUT , W3SETG , W3SETO )                                        
!          b  I-O setup.                                                                         
!          c  Print heading(s).                                                                  
!        2.   Read model definition file.      ( W3IOGR )                                        
!        3.a  Read major types from input file.                                                  
!          b  Check major types.                                                                 
!          c  Additional input format types and time.                                            
!        4.   Prepare interpolation.                                                             
!          a  Longitude - latitude grid                                                          
!          b  Grid(s) from file.               ( W3FLDP )                                        
!          c  Initialize fields.                                                                 
!          d  Input location and format.                                                         
!        5    Prepare input and output files.                                                    
!          a  Open input file                                                                    
!          b  Open and prepare output file     ( W3FLDO )                                        
!        6    Until end of file                                                                  
!          a  Read new time and fields                                                           
!          b  Interpolate fields                                                                 
!          c  Write fields                     ( W3FLDG )                                        
!     ---------------------------------------------------- 
!                                                                                                
!  9. Switches :                                                                                 
!                                                                                                
!     !/WNT0  = !/WNT1                                                                           
!     !/WNT1  Correct wind speeds to (approximately) conserve the wind                           
!             speed over the interpolation box.                                                  
!     !/WNT2  Id. energy (USE ONLY ONE !)                                                        
!     !/CRT1  Like !/WNT1 for currents.                                                          
!     !/CRT2  Like !/WNT2 for currents.                                                          
!                                                                                                
!     !/O3    Additional output in fields processing loop.                                       
!     !/O15   Generate file with the times of the processed fields.                              
!                                                                                                
!     !/S     Enable subroutine tracing.                                                         
!     !/T     Enable test output,                                                                
!     !/T1    Full interpolation data.                                                           
!     !/T1a   Echo of lat-long data in type Fn                                                   
!     !/T2    Full input data.                                                                   
!     !/T3    Print-plot of output data.                                                         
!                                                                                                
!     !/NCO   NCEP NCO modifications for operational implementation.                             
!                                                                                                
! 10. Source code :                                                                              
!                                                                                                
!/ ------------------------------------------------------------------- / 
```
---
[\[Return to Contents\]](#contents)
<br>
<br>
<br>


## Inlining Example
Module variables, which occur between the keywords `MODULE` and `CONTAINS`, must be declared individually and have an inline
doxygen comment for documentation.  Local variables (in SUBROUTINE, FUNCTION, PROGRAM) are **not** documented with a doxygen comment.

#### MODULE variables
Unlike the headers, no doxygen tags are needed for inline documentation.  Instead you include `!<` followed by the documentation directly
after the variable declaration.  If more than one line is needed, you can continue to put additional `!<` on blank lines that follow with 
the rest of the documentation.

```F90
                                                                       
module m_constants                                                              
!------------------------------------------------------------------------------ 
!                                                                               
! physical constants                                                            
!                                                                               
real grav    !< gravitational acceleration                                      
real sqrtg   !< square root of grav                                             
real gsq     !< square of grav                                                  
real nu      !< kinematic viscosity of water                                    
!                                                                               
real d_water !< density of water                                                
real d_air   !< density of air                                                  
!                                                                               
! mathematical constants                                                        
!                                                                               
real pi     !< circular constant, 3.1415...                                     
real pi2    !< 2*pi                                                             
real pih    !< pi/2                                                             
real dera   !< conversion from degrees to radians                               
real rade   !< conversion from radians to degrees                               
real expmin !< min argument for exp. function to avoid underflow                
real expmax !< max argument for exp. function to avoid overflow                 
real sqrt2  !< square root of 2 ~ 1.41                                          
!                                                                               
contains                                                                        
!                                                                               
                                                                                
!------------------------------------------------------------------------------ 
```
---
[\[Return to Contents\]](#contents)
<br>
<br>
<br>


## Commands
#### Build doxygen documentation
From the commandline, issue the following command from the WW3 root directory to build the doxygen documentation
```bash
doxygen ./docs/Doxyfile.in
```
_Note: use doxygen version 1.8.17_.

<br>

#### Display documentation in browser
The html output can be displayed in a browser by issuing this command from the WW3 root directory
```bash
firefox ./docs/html/index.html
```
Firefox is used here as an example, but can be the browser of your choice.

---
[\[Return to Contents\]](#contents)
<br>
<br>


