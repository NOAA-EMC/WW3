meshTools limon_ll.2dm --multiply 1,1,0 -o meshzero.2dm
meshTools meshzero.2dm --inpolygon polywavebnd.dat --translate 0,0,2 -o meshwave.2dm
meshTools meshwave.2dm --inpolygon polylateral.dat --translate 0,0,3 -o meshbnd.2dm

