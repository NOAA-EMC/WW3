from netCDF4 import Dataset
from matplotlib import pyplot as plt
import numpy as np


def plotMapAtTime(ncfilepath, timeIndex):
  ds = Dataset(ncfilepath)
  hs = ds.variables['hs'][timeIndex, :]
 #hs = ds.variables['vwnd'][timeIndex, :]
  xs = ds.variables['longitude'][:]
  ys = ds.variables['latitude'][:]
  hs[hs.mask] = 0
  
  levels = np.arange(0., np.max(hs.flatten())*1.05, .02)
  cf = plt.tricontourf(xs, ys, hs, levels)
 #cf = bm.contourf(xs, ys, hs, tri=True)
  cf.cmap.set_over([.5,0,0])
 #cf.set_clim(0, 1.6)
  plt.colorbar()
 #plt.savefig('plt_t=' + str(timeIndex) + '.png')
  plt.show()


if __name__ == '__main__':
  ncfilepath = '../work_ug/ww3.200001.nc'
  timeIndex = 10
 #timeIndex = 0
 #import pdb; pdb.set_trace()
  plotMapAtTime(ncfilepath, timeIndex)

