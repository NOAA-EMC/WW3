tar -cvf rt_visumain.tar  visu.pro initmain.pro rt_visumain.pro read_timeseries.pro julday.pro mapping.pro gridbuild.pro  bathytool.pro editspecials.pro maketransect.pro raytracer.pro parametros.pro   palette.pro xloadpalette.pro  
tar -uvf rt_visumain.tar palettes/*
tar -uvf rt_visumain.tar rayp.f90  raypfc.f90  raypf.f90  raypg.f90 
tar -uvf rt_visumain.tar TWIST_init.txt
gzip rt_visumain.tar

