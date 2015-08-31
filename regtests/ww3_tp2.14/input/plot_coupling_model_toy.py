##!/usr/bin/python
# -*- coding: utf-8 -*-
#
################################################################################################
#==============================================================================================#
#==============================================================================================#
#      Visualization of the coupling outputs
#      Author : J. Pianezze
#===============================================================================================#
#################################################################################################

import netCDF4
import numpy as np
import scipy
import matplotlib.pyplot as plt
from pylab import *
import os
import sys

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# To be defined by the user
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
OUTPUT_PATH=os.path.abspath(os.curdir)+'/'
print '=============================================================================='
INPUT_PATH=os.path.abspath(os.curdir)+'/' ; print INPUT_PATH
#
VAR1='U10'
liste_VAR1_TOY=['VARSND01_toyexe_02.nc']
liste_VAR1_MOD=['WW3__U10_wwatch_02.nc']
#
VAR2='CHA'
liste_VAR2_TOY=['VARRCV01_toyexe_01.nc']
liste_VAR2_MOD=['WW3__CHA_wwatch_01.nc']
#
liste_GRIDS=['grids.nc']
liste_MOD=['ww3.200803.nc']
#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

print '=============================================================================='
try :
  os.mkdir(OUTPUT_PATH+VAR1+'_'+VAR2+'/')
except OSError:
  print 'Directory already created'
else:
  print 'Making directory'

print '=============================================================================='
print '~~~~ Plot these variables :', VAR1, VAR2

file_VAR1_TOY = netCDF4.Dataset(INPUT_PATH+liste_VAR1_TOY[0])
file_VAR1_MOD = netCDF4.Dataset(INPUT_PATH+liste_VAR1_MOD[0])

file_VAR2_TOY = netCDF4.Dataset(INPUT_PATH+liste_VAR2_TOY[0])
file_VAR2_MOD = netCDF4.Dataset(INPUT_PATH+liste_VAR2_MOD[0])
 
file_MOD = netCDF4.Dataset(INPUT_PATH+liste_MOD[0])
file_GRIDS = netCDF4.Dataset(INPUT_PATH+liste_GRIDS[0])

#~~~~~ TOY
LAT_TOY=file_GRIDS.variables['toyt.lat']
LON_TOY=file_GRIDS.variables['toyt.lon']
DIM_LAT_TOY=shape(LAT_TOY)[0]
DIM_LON_TOY=shape(LON_TOY)[1]
print 'DIM_LAT_TOY, DIM_LON_TOY', DIM_LAT_TOY, DIM_LON_TOY

#~~~~~ MOD
LAT_MOD=file_GRIDS.variables['ww3t.lat']
LON_MOD=file_GRIDS.variables['ww3t.lon']
DIM_LAT_MOD=np.shape(file_MOD.variables['latitude'][:])[0]-1
DIM_LON_MOD=np.shape(file_MOD.variables['longitude'][:])[0]-1
print 'DIM_LAT_MOD, DIM_LON_MOD', DIM_LAT_MOD, DIM_LON_MOD
LAT_MOD=reshape(LAT_MOD,(DIM_LAT_MOD,DIM_LON_MOD))
LON_MOD=reshape(LON_MOD,(DIM_LAT_MOD,DIM_LON_MOD))

#~~~~~ VAR1/VAR2
VAR1_TOY=file_VAR1_TOY.variables[liste_VAR1_TOY[0][0:8]] ; print np.shape(VAR1_TOY)
TIME_TOY=shape(VAR1_TOY)[0] ; print TIME_TOY
VAR1_TOY=reshape(VAR1_TOY,(TIME_TOY,DIM_LAT_TOY, DIM_LON_TOY))
VAR1_MOD=file_VAR1_MOD.variables[liste_VAR1_MOD[0][0:8]]
TIME_MOD=shape(VAR1_MOD)[0]
VAR1_MOD=reshape(VAR1_MOD,(TIME_MOD,DIM_LAT_MOD, DIM_LON_MOD))

VAR2_TOY=file_VAR2_TOY.variables[liste_VAR2_TOY[0][0:8]] ; print np.shape(VAR2_TOY) 
VAR2_TOY=reshape(VAR2_TOY,(TIME_TOY-1,DIM_LAT_TOY, DIM_LON_TOY))
VAR2_MOD=file_VAR2_MOD.variables[liste_VAR2_MOD[0][0:8]]
VAR2_MOD=reshape(VAR2_MOD,(TIME_MOD,DIM_LAT_MOD, DIM_LON_MOD))

MASK_VAR2_MOD = (VAR2_MOD[:,:] == 0.0)
VAR2_MOD = np.ma.MaskedArray(VAR2_MOD, mask=MASK_VAR2_MOD)

OPVAR1=''
OPVAR2=''

#~~~~~ CONVERT VARIABLES
if VAR2=='CHA':
  print 'Multiply Charnock coefficient by 1000'
  VAR2_TOY[:,:,:]=VAR2_TOY[:,:,:]*1000.0
  VAR2_MOD[:,:,:]=VAR2_MOD[:,:,:]*1000.0
  OPVAR2='*1000.0'

#==================================================================================================
#==================================================================================================

levels_VAR1 = MaxNLocator(nbins=30).tick_values(np.min(VAR1_TOY), np.max(VAR1_TOY))
levels_VAR2 = MaxNLocator(nbins=30).tick_values(np.min(VAR2_MOD), np.max(VAR2_MOD))

#--------------------------------------------------------------------------------------------------
for i in xrange(TIME_TOY-1):

  #----------------------
  ax = subplot(221)
  plt.title(liste_VAR1_TOY[0][0:8]+OPVAR1,fontsize=18)
  CS=plt.contourf(LON_TOY[:,:],LAT_TOY[:,:],VAR1_TOY[i,:,:],cmap=plt.cm.RdBu_r,levels=levels_VAR1)
  cbar = plt.colorbar(CS,orientation='vertical',format='%.3f')
  plt.ylabel(r'latitude [-]',fontsize=18)
  plt.tick_params(\
      axis='x',          # changes apply to the x-axis
      which='both',      # both major and minor ticks are affected
      bottom='on',      # ticks along the bottom edge are off
      top='on',         # ticks along the top edge are off
      labelbottom='off') # labels along the bottom edge are off
  xlim(( max(np.min(LON_MOD[:,:]),np.min(LON_TOY[:,:])), min(np.max(LON_MOD[:,:]),np.max(LON_TOY[:,:])) ))
  ylim(( max(np.min(LAT_MOD[:,:]),np.min(LAT_TOY[:,:])), min(np.max(LAT_MOD[:,:]),np.max(LAT_TOY[:,:])) ))

  #----------------------
  ax = subplot(222)
  plt.title(liste_VAR1_MOD[0][0:8]+OPVAR1,fontsize=18)
  CS=plt.contourf(LON_MOD[:,:],LAT_MOD[:,:],VAR1_MOD[i,:,:],cmap=plt.cm.RdBu_r,levels=levels_VAR1)
  cbar = plt.colorbar(CS,orientation='vertical',format='%.3f')
  plt.tick_params(\
      axis='x',          # changes apply to the x-axis
      which='both',      # both major and minor ticks are affected
      bottom='on',      # ticks along the bottom edge are off
      top='on',         # ticks along the top edge are off
      labelbottom='off') # labels along the bottom edge are off

  plt.tick_params(\
      axis='y',          # changes apply to the x-axis
      which='both',      # both major and minor ticks are affected
      bottom='on',      # ticks along the bottom edge are off
      top='on',         # ticks along the top edge are off
      labelleft='off') # labels along the bottom edge are off
  xlim(( max(np.min(LON_MOD[:,:]),np.min(LON_TOY[:,:])), min(np.max(LON_MOD[:,:]),np.max(LON_TOY[:,:])) ))
  ylim(( max(np.min(LAT_MOD[:,:]),np.min(LAT_TOY[:,:])), min(np.max(LAT_MOD[:,:]),np.max(LAT_TOY[:,:])) ))

  #----------------------
  ax = subplot(223)
  plt.title(liste_VAR2_TOY[0][0:8]+OPVAR2,fontsize=18)
  CS=plt.contourf(LON_TOY[:,:],LAT_TOY[:,:],VAR2_TOY[i,:,:],cmap=plt.cm.RdBu_r,levels=levels_VAR2)
  cbar = plt.colorbar(CS,orientation='vertical',format='%.3f')
  plt.ylabel(r'latitude [-]',fontsize=18)
  plt.xlabel(r'longitude [-]',fontsize=18)
  xlim(( max(np.min(LON_MOD[:,:]),np.min(LON_TOY[:,:])), min(np.max(LON_MOD[:,:]),np.max(LON_TOY[:,:])) ))
  ylim(( max(np.min(LAT_MOD[:,:]),np.min(LAT_TOY[:,:])), min(np.max(LAT_MOD[:,:]),np.max(LAT_TOY[:,:])) ))
   
  #----------------------
  ax = subplot(224)
  plt.title(liste_VAR2_MOD[0][0:8]+OPVAR2,fontsize=18)
  CS=plt.contourf(LON_MOD[:,:],LAT_MOD[:,:],VAR2_MOD[i,:,:],cmap=plt.cm.RdBu_r,levels=levels_VAR2)
  cbar = plt.colorbar(CS,orientation='vertical',format='%.3f')
  plt.xlabel(r'longitude [-]',fontsize=18)
  plt.tick_params(\
      axis='y',          # changes apply to the x-axis
      which='both',      # both major and minor ticks are affected
      bottom='on',      # ticks along the bottom edge are off
      top='on',         # ticks along the top edge are off
      labelleft='off') # labels along the bottom edge are off
  xlim(( max(np.min(LON_MOD[:,:]),np.min(LON_TOY[:,:])), min(np.max(LON_MOD[:,:]),np.max(LON_TOY[:,:])) ))
  ylim(( max(np.min(LAT_MOD[:,:]),np.min(LAT_TOY[:,:])), min(np.max(LAT_MOD[:,:]),np.max(LAT_TOY[:,:])) ))

  #------------------------
  plt.savefig(OUTPUT_PATH+VAR1+"_"+VAR2+"/"+VAR1+"_"+VAR2+"_MOD_TOY_T"+str(i)+".pdf")
  plt.savefig(OUTPUT_PATH+VAR1+"_"+VAR2+"/"+VAR1+"_"+VAR2+"_MOD_TOY_T"+str(i)+".png")

  plt.close()
print '=============================================================================='
