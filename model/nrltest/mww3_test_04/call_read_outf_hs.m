
clear

path(path,'./../nrltest/matlab')

hmax=5;
dt=1/24;
units='m';

zpwd=pwd;
i=strfind(zpwd,'/');
i=i(length(i));

runid=zpwd((i+1):length(zpwd));
time_filename=datenum(1968,06,06,0,0,0);
read_outf_hs_generic(hmax,dt,units,runid,time_filename)
