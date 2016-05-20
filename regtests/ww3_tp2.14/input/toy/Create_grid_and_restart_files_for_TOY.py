#!/usr/bin/python
# -*- coding: utf-8 -*-
#
################################################################################################
#==============================================================================================#
#      Creating grid and restart file for toy model
#      Author : J. Pianezze
#===============================================================================================#
#################################################################################################

import netCDF4
import numpy as np
import scipy
import matplotlib.pyplot as plt
import math
from pylab import *
import os

OUTPUT_PATH=os.path.abspath(os.curdir)+'/'

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# To be defined by the user
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
TYPE_DATA='INPUT_FOR_COUPLING'
#
LAT_DOMAIN=[47.0, 49.5]
LON_DOMAIN=[-6.2, -4.0]
#
CTYPE_FCT='SINUS' 
# CNSTE or SINUS
VALUE_CNSTE=10.0
COEF=50.0
LENGTH=1000.
#
INPUT_PATH_TOPO=os.path.abspath(os.curdir)+'/'
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

print '###############################################'
print '##                                           ##'
print '##  WRITING NETCDF FILES FOR OASIS COUPLING  ##'
print '##                                           ##'
print '###############################################'

#------ Create directory
try :
  os.mkdir(OUTPUT_PATH+TYPE_DATA+'/')
except OSError:
  print 'Directory already created'
else:
  print 'Making directory'

#------ Create links
try:
  os.symlink(INPUT_PATH_TOPO+'etopo2.nc', 'topo.nc')
except OSError:
  print('Link already exist....!!!')

file_TOPO = netCDF4.Dataset('topo.nc')

#------ Read variables
LON_FULL=file_TOPO.variables['lon'][:]
LAT_FULL=file_TOPO.variables['lat'][:]
TOPO_FULL=file_TOPO.variables['topo'][:,:]

IND_MIN_LON=find(LON_FULL[:]>LON_DOMAIN[0])[0] ; print IND_MIN_LON
IND_MAX_LON=find(LON_FULL[:]>LON_DOMAIN[1])[0] ; print IND_MAX_LON
IND_MIN_LAT=find(LAT_FULL[:]>LAT_DOMAIN[0])[0] ; print IND_MIN_LAT
IND_MAX_LAT=find(LAT_FULL[:]>LAT_DOMAIN[1])[0] ; print IND_MAX_LAT

LON=LON_FULL[IND_MIN_LON:IND_MAX_LON]
LAT=LAT_FULL[IND_MIN_LAT:IND_MAX_LAT]
TOPO=TOPO_FULL[IND_MIN_LAT:IND_MAX_LAT,IND_MIN_LON:IND_MAX_LON]

#------- Suppress links
os.unlink('topo.nc')

#
plt.contourf(TOPO)
plt.show()

NLON=np.size(LON) ;  print 'NLON=', NLON
NLAT=np.size(LAT) ;  print 'NLAT=', NLAT
NCORN=4           ;  print 'NCORN=', NCORN

print '---- longitude/latitude'
LON2D=np.zeros((NLAT,NLON))
LAT2D=np.zeros((NLAT,NLON))

for ind_lon in xrange(NLON):
  LAT2D[:,ind_lon]=LAT[:]
for ind_lat in xrange(NLAT):
  LON2D[ind_lat,:]=LON[:]

print '---- corners longitude/latitude'
CLO=np.zeros((NCORN,NLAT,NLON))
CLA=np.zeros((NCORN,NLAT,NLON))

DELTAX=LON[1]-LON[0] ; print 'DELTAX=', DELTAX
CLO[0,:,:]=LON2D[:,:]+DELTAX/2.0
CLO[1,:,:]=LON2D[:,:]-DELTAX/2.0
CLO[2,:,:]=LON2D[:,:]-DELTAX/2.0
CLO[3,:,:]=LON2D[:,:]+DELTAX/2.0

DELTAY=LAT[1]-LAT[0] ; print 'DELTAY=', DELTAY
CLA[0,:,:]=LAT2D[:,:]+DELTAY/2.0
CLA[1,:,:]=LAT2D[:,:]+DELTAY/2.0
CLA[2,:,:]=LAT2D[:,:]-DELTAY/2.0
CLA[3,:,:]=LAT2D[:,:]-DELTAY/2.0

print '---- surface'
SURFACE=np.zeros((NLAT,NLON))
SURFACE[:,:]=100.0


print '---- mask and VARSND01'
MASK=np.zeros((NLAT,NLON))
TOYVAROU=np.zeros((NLAT,NLON))

for ind_lon in xrange(NLON):
  for ind_lat in xrange(NLAT):
    if TOPO[ind_lat,ind_lon] > 0.0 :
      MASK[ind_lat,ind_lon]=0
      if CTYPE_FCT=='CNSTE':
        TOYVAROU[ind_lat,ind_lon]=VALUE_CNSTE
      elif CTYPE_FCT=='SINUS':
        TOYVAROU[ind_lat,ind_lon]= COEF*math.sin(LAT[ind_lat]*math.pi/180.0*LENGTH)
    else:
      MASK[ind_lat,ind_lon]=1
      if CTYPE_FCT=='CNSTE':
        TOYVAROU[ind_lat,ind_lon]=VALUE_CNSTE
      elif CTYPE_FCT=='SINUS':
        TOYVAROU[ind_lat,ind_lon]= COEF*math.sin(LAT[ind_lat]*math.pi/180.0*LENGTH)


############################################################################################################
print '------------------------------------------'
print ' Opening netcdf file : grid_toy_model.nc'

GRID_FILE=netCDF4.Dataset(OUTPUT_PATH+TYPE_DATA+'/'+'grid_toy_model.nc','w',format='NETCDF3_64BIT')
GRID_FILE.Description='Grid file for OASIS coupling'

# ----------------------------------
# Create the dimensions of the files
# ----------------------------------
GRID_FILE.createDimension ('longitude', NLON)
GRID_FILE.createDimension ('latitude', NLAT)
GRID_FILE.createDimension ('ncorner', 4 )

# ----------------------------------
# Create the variables of the files
# ----------------------------------

# Longitude
VAROUT=GRID_FILE.createVariable('longitude','d',('latitude','longitude'))
VAROUT.long_name = 'longitude' ;
VAROUT.units = 'degree' ;
VAROUT.actual_range = np.min(LON2D), np.max(LON2D) ;

# Latitude
VAROUT=GRID_FILE.createVariable('latitude','d',('latitude','longitude'))
VAROUT.long_name = 'latitude' ;
VAROUT.units = 'degree' ;
VAROUT.actual_range = np.min(LAT2D), np.max(LAT2D) ;

# Coins
VAROUT=GRID_FILE.createVariable('clo','d',('ncorner','latitude','longitude'))
VAROUT.long_name = 'longitude of the corners' ;
VAROUT.units = 'degree' ;
VAROUT.actual_range = np.min(CLO), np.max(CLO) ;

VAROUT=GRID_FILE.createVariable('cla','d',('ncorner','latitude','longitude'))
VAROUT.long_name = 'latitude of the corners' ;
VAROUT.units = 'degree' ;
VAROUT.actual_range = np.min(CLA), np.max(CLA) ;

# Surface
VAROUT=GRID_FILE.createVariable('srf','d',('latitude','longitude'))
VAROUT.long_name = 'surface' ;
VAROUT.units = '-' ;
VAROUT.actual_range = np.min(SURFACE), np.max(SURFACE) ;

# Mask
VAROUT=GRID_FILE.createVariable('imask','d',('latitude','longitude'))
VAROUT.long_name = 'imask' ;
VAROUT.units = '0 for land, 1 for sea' ;
VAROUT.actual_range = np.min(MASK), np.max(MASK) ;

# ---------------------------------------
# Write out the data arrays into the file
# ---------------------------------------
GRID_FILE.variables['longitude'][:,:] = LON2D[:,:]
GRID_FILE.variables['latitude'][:,:] = LAT2D[:,:]
GRID_FILE.variables['clo'][:,:] = CLO[:,:,:]
GRID_FILE.variables['cla'][:,:] = CLA[:,:,:]
GRID_FILE.variables['srf'][:,:] = SURFACE[:,:]
GRID_FILE.variables['imask'][:,:] = MASK[:,:]

# ---------------------------------------
# Close the file
# ---------------------------------------
GRID_FILE.close()

print ' Close netcdf file : grid_toy_model.nc'
print '------------------------------------------'
############################################################################################################

############################################################################################################
print '------------------------------------------'
print ' Opening netcdf file : rstrt_SAVE.nc'

RSTRT_FILE=netCDF4.Dataset(OUTPUT_PATH+TYPE_DATA+'/'+'rstrt_SAVE.nc','w',format='NETCDF3_64BIT')
RSTRT_FILE.Description='Restart file for OASIS coupling'

# ----------------------------------
# Create the dimensions of the files
# ----------------------------------
RSTRT_FILE.createDimension ('longitude', NLON)
RSTRT_FILE.createDimension ('latitude', NLAT)

# ----------------------------------
# Create the variables of the files
# ----------------------------------
VAROUT=RSTRT_FILE.createVariable('VARSND01','d',('latitude','longitude'))
VAROUT.long_name = 'Output variable for toy model' ;
VAROUT.units = '-' ;
VAROUT.actual_range = np.min(TOYVAROU), np.max(TOYVAROU) ;

VAROUT=RSTRT_FILE.createVariable('VARSND02','d',('latitude','longitude'))
VAROUT.long_name = 'Output variable for toy model' ;
VAROUT.units = '-' ;
VAROUT.actual_range = np.min(TOYVAROU), np.max(TOYVAROU) ;


# ---------------------------------------
# Write out the data arrays into the file
# ---------------------------------------
RSTRT_FILE.variables['VARSND01'][:,:] = TOYVAROU[:,:]
RSTRT_FILE.variables['VARSND02'][:,:] = TOYVAROU[:,:]


# ---------------------------------------
# Close the file
# ---------------------------------------
RSTRT_FILE.close()

print ' Close netcdf file : rstrt_SAVE.nc'
print '-----------------------------------------'
############################################################################################################

print '##############################################'
print '##                                          ##'
print '##   CLOSE NETCDF FILES FOR OASIS COUPLING  ##'
print '##                                          ##'
print '##############################################'
