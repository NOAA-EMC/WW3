"""
#Ursa environment
module purge 
module use /scratch4/NCEPDEV/marine/Ali.Salimi/Hera_Data/HR4-OPT/FromJessica/Keston/ICunstructuredRuns15km-implicit-450s/global-workflow/sorc/ufs_model.fd/modulefiles 
module load ufs_ursa.intel 
module load py-scipy/1.14.1 
module load py-netcdf4/1.7.1.post2 
pip list 
"""


import numpy as np
import netCDF4 as nc
import subprocess
# Example 1: Basic command execution
# This will run 'ls -l' and print the output to the console.

#[0->7] variables(dimensions): |S1 crs(), float64 lat(lat), float64 lon(lon), float32 z(lat, lon)
#[8->9]variables(dimensions): float64 x(x), float64 y(y), float32 z(y, x)
#[10]variables(dimensions): float64 lon(num_lon), float64 lat(num_lat), int16 bed_elevation(num_row, num_col)

flin="../input/level.nc"
flout="ice.nc"

data0 = nc.Dataset(flin,"r")
x=np.asarray(data0["longitude"][:])
y=np.asarray(data0["latitude"][:])
wlv=np.asarray(data0["wlv"][:])
#tri=np.asarray(data0["tri"][:])

ice=wlv
dy=np.max(y)-np.min(y)
tpy=np.mean(y)+.25*dy
print(tpy)
mlat=40.85
mlon=-72.1

f= (x - mlon)/(np.max(x)-mlon-.1)
f[np.where(f<0.)]=0.
f[np.where(f>1.)]=1.


subprocess.run(['cp', flin, flout], check=True)

ds = nc.Dataset(flout, 'a')
ds.renameVariable("wlv","ice")
ds.close

ds = nc.Dataset(flout, 'a')
ice=ds.variables["ice"]
print(ice.shape)
nt=ice.shape[0]
mlon=-72.4

for k in range(nt):
    f=(x - mlon-.2*k/nt)/(np.max(x)-mlon-.1)
    f[np.where(f<0.)]=0.
    f[np.where(f>1.)]=1.
    ice[k,:]=f[:]
ds.close
