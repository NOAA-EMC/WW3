.FULL_RESET_SESSION
.compile initmain.pro
.compile rt_visumain.pro
.compile read_timeseries.pro
.compile mapping.pro
.compile gridbuild.pro
.compile bathytool.pro
.compile editspecials.pro
.compile maketransect.pro
.compile raytracer.pro
.compile parametros.pro
.compile julday.pro
RESOLVE_ALL
SAVE,/ROUTINES,FILENAME='rt_visumain.sav'
