
function [lat,lon,time,mat1,var1,unit1]=read_WWNC_var(filename,varname,date1,lon1,lat1)
%
% Reads all or a subsed of a NetCDF file. 
% If date1 is specified: takes only the closest dates in the file
% if lon1 is specificied: takes only the closest longitude ... 

%
% 1. Opens file and gets dimensions of arrays
%
  fid=netcdf.open(filename,'NC_NOWRITE');
  [ndims,nvars,ngatts,unlimdimid]=netcdf.inq(fid);
  [d0,nx]=netcdf.inqDim(fid,ndims-3);
  [d1,ny]=netcdf.inqDim(fid,ndims-2);
  [d2,nt]=netcdf.inqDim(fid,ndims-1);
  v0=netcdf.inqVar(fid,0);
  if (v0=='x') 
      varlon=0;
      varlat=1;
  else
    varlon = netcdf.inqVarID(fid,'longitude');
    varlat = netcdf.inqVarID(fid,'latitude');
  end
  lon=netcdf.getVar(fid,varlon);
  lat=netcdf.getVar(fid,varlat);
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
if exist('lon1') 
    [xdist,ii]=min(abs(lon-lon1));
    lon=lon(ii);
    II=ii;
    ni=1;
else
    II=1;
    ni=nx;
end
if exist('lat1') 
    [ydist,jj]=min(abs(lat-lat1));
    lat=lat(jj);
    JJ=jj;
    nj=1;
else
    JJ=1;
    nj=ny;
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
    eval(['vali=netcdf.getVar(fid,varid,[II-1 JJ-1 KK-1],[ni nj nk]);']);
    %eval(['var' num2str(j) '=netcdf.getAtt(fid,var' num2str(j) 'id,''long_name'');']);
    I=find(vali== fillv);
    val=double(vali).*scale;
    val(I)=NaN;
    eval(['mat'  num2str(j) '=val;']);

netcdf.close(fid);    
%    pcolor(lon1,lat1,squeeze(val1(:,:,1))');shading flat;
    

