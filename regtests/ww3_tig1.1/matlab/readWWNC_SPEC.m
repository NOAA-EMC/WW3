
function [lat,lon,freq,dir,df,time,spectra1,depth,curr,currdir,unit1]=readWWNC_SPEC(filename,varname,date1)
%
% Reads all or a subset of a NetCDF file, with a frequency dimension.
% If date1 is specified: takes only the closest dates in the file
% if lon1 is specificied: takes only the closest longitude ... 

%
% 1. Opens file and gets dimensions of arrays
%
  fid=netcdf.open(filename,'NC_NOWRITE');
  [ndims,nvars,ngatts,unlimdimid]=netcdf.inq(fid);

  dimstat = netcdf.inqDimID(fid,'station');
  dimtime = netcdf.inqDimID(fid,'time');
  dimf = netcdf.inqDimID(fid,'frequency');
  dimd = netcdf.inqDimID(fid,'direction');
  [d0,ns]=netcdf.inqDim(fid,dimstat);
  [d1,nd]=netcdf.inqDim(fid,dimd);
  [d3,nt]=netcdf.inqDim(fid,dimtime);
  [d4,nf]=netcdf.inqDim(fid,dimf);

  vard = netcdf.inqVarID(fid,'direction');
  varf = netcdf.inqVarID(fid,'frequency');
  varf1 = netcdf.inqVarID(fid,'frequency1');
  varf2 = netcdf.inqVarID(fid,'frequency2');
  varlon = netcdf.inqVarID(fid,'longitude');
  varlat = netcdf.inqVarID(fid,'latitude');
  freq=netcdf.getVar(fid,varf);
  dir=netcdf.getVar(fid,vard);
  freq1=netcdf.getVar(fid,varf1);
  freq2=netcdf.getVar(fid,varf2);
  df=freq2-freq1;
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
II=1;
ni=nd;
JJ=1;
nj=nf;

LL=1;
nl=ns;

%
% 3. Extracts data
%
    j=1;
    j1=j;
    varid=myvar;
    eval(['unit' num2str(j) '=netcdf.getAtt(fid,varid,''units'');']);
    eval(['scale=netcdf.getAtt(fid,varid,''scale_factor'');']);
    eval(['fillv=netcdf.getAtt(fid,varid,''_FillValue'');']);
    eval(['vali=netcdf.getVar(fid,varid,[II-1 JJ-1 LL-1 KK-1],[ni nj nl nk]);']);
    %eval(['var' num2str(j) '=netcdf.getAtt(fid,var' num2str(j) 'id,''long_name'');']);

      I=find(vali== fillv);
      val=double(vali).*scale;
      val(I)=NaN;
      eval(['spectra'  num2str(j) '=val;']);
      
    varc1 = netcdf.inqVarID(fid,'cur');
    curr=  netcdf.getVar(fid,varc1,[LL-1 KK-1],[nl nk]);
    varc1 = netcdf.inqVarID(fid,'curdir');
    currdir=  netcdf.getVar(fid,varc1,[LL-1 KK-1],[nl nk]);
    varc1 = netcdf.inqVarID(fid,'dpt');
    depth=  netcdf.getVar(fid,varc1,[LL-1 KK-1],[nl nk]);
 netcdf.close(fid);    
%    pcolor(lon1,lat1,squeeze(val1(:,:,1))');shading flat;
    
