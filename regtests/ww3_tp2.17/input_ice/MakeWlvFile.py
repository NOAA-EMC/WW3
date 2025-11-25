import numpy as np
import netCDF4 as nc
import subprocess
# remove nan's from origonal water level file

flin="../input/level.nc"
flout="levelNoNaN.nc"

data0 = nc.Dataset(flin,"r")
x=np.asarray(data0["longitude"][:])
y=np.asarray(data0["latitude"][:])
wlv0=np.asarray(data0["wlv"][:])
#tri=np.asarray(data0["tri"][:])

subprocess.run(['cp', flin, flout], check=True)

#ds = nc.Dataset(flout, 'a')
#ds.renameVariable("wlv","ice")
#ds.close

ds = nc.Dataset(flout, 'a')
wlv=ds.variables["wlv"]
print(wlv.shape)
nt=wlv.shape[0]

mlon=-72.4
for k in range(nt):
#    f=.5+x*0.+k/nt/10.
#    wlv[k,:]=f[:]
    f=wlv[k,0]+0.*x
    wlv[k,:]=f[:]
ds.close
