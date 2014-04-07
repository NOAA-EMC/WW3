
function [tri,lat,lon,time,mat1,var1,unit1]=read_WWNCUG_var(filename,varname,date1,lon1,lat1)
%
% Reads all or a subsed of a NetCDF file. 
% If date1 is specified: takes only the closest dates in the file
% if lon1 is specificied: takes only the closest longitude ... 

%
% 1. Opens file and gets dimensions of arrays
%
  fid=netcdf.open(filename,'NC_NOWRITE');
  nodeid=netcdf.inqDimID(fid,'node');
  elemid=netcdf.inqDimID(fid,'element');
  timeid=netcdf.inqDimID(fid,'time');
  
  [d0,nx]=netcdf.inqDim(fid,nodeid);
  [d1,ntri]=netcdf.inqDim(fid,elemid);
  [d3,nt]=netcdf.inqDim(fid,timeid);
  
  varlon = netcdf.inqVarID(fid,'longitude');
  varlat = netcdf.inqVarID(fid,'latitude');
  vartri = netcdf.inqVarID(fid,'tri');
  
  lon=netcdf.getVar(fid,varlon);
  lat=netcdf.getVar(fid,varlat);
  tri=netcdf.getVar(fid,vartri);


 
%
% We assume that the date reference is 1 Jan 1990. 
% This is normally written in the time attributes        
%
  time0=datenum(1990,1,1);
  vartime = netcdf.inqVarID(fid,'time');
  time=netcdf.getVar(fid,vartime)+time0;
  varids=[];
myvar= netcdf.inqVarID(fid,varname);

%
% Gets all the indices for the variables
%
      [var1 type vardims]=netcdf.inqVar(fid,myvar);
%
% 2.  defines the indices for the data subset 
%
if exist('date1') 
    [timedist,kk]=min(abs(time-date1));
    time=time(kk);
    KK=kk;
    nk=1;
else
    KK=1;
    nk=nt;
end
if exist('lon1') & exist('lat1') 
    [xdist,ii]=min(abs(lon-lon1)+abs(lat-lat1));
    lon=lon(ii);
    lat=lat(ii);
    II=ii;
    ni=1;
else
    II=1;
    ni=nx;
end

%
% 3. Extracts data
%
    j=1;
    j1=j;
    varid=myvar;
    eval(['unit' num2str(j) '=netcdf.getAtt(fid,varid,''units'');']);
    eval(['scale=netcdf.getAtt(fid,varid,''scale_factor'');']);
    eval(['fillv=netcdf.getAtt(fid,varid,''_FillValue'');']);
    eval(['vali=netcdf.getVar(fid,varid,[II-1 KK-1],[ni nk]);']);
    I=find(vali== fillv);
    val=double(vali).*scale;
    val(I)=NaN;
    eval(['mat'  num2str(j) '=val;']);

netcdf.close(fid);    
    

