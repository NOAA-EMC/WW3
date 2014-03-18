

function [lat,lon,freq,time,mat1,var1,unit,MAPSTA]=read_WWNCf_var(filename,varname,date1,lon1,lat1)

%

% Reads all or a subset of a NetCDF file, with a frequency dimension.

% If date1 is specified: takes only the closest dates in the file

% if lon1 is specificied: takes only the closest longitude ... 



%

% 1. Opens file and gets dimensions of arrays

%

  fid=netcdf.open(filename,'NC_NOWRITE');

  [ndims,nvars,ngatts,unlimdimid]=netcdf.inq(fid);



  MAPSTAid=netcdf.inqVarID(fid,'MAPSTA');



  dimlon = netcdf.inqDimID(fid,'longitude');

  dimlat = netcdf.inqDimID(fid,'latitude');

  dimtime = netcdf.inqDimID(fid,'time');

  dimf = netcdf.inqDimID(fid,'f');

  [d0,nx]=netcdf.inqDim(fid,dimlon);

  [d1,ny]=netcdf.inqDim(fid,dimlat);

  [d3,nt]=netcdf.inqDim(fid,dimtime);

  [d4,nf]=netcdf.inqDim(fid,dimf);

  v0=netcdf.inqVar(fid,0);

  if (v0=='x') 

      varlon=0;

      varlat=1;

  else

    varlon = netcdf.inqVarID(fid,'longitude');

    varlat = netcdf.inqVarID(fid,'latitude');

  end

  varf = netcdf.inqVarID(fid,'f');

  varM = netcdf.inqVarID(fid,'MAPSTA');

  freq=netcdf.getVar(fid,varf);

  MAPSTA=netcdf.getVar(fid,varM);

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



LL=1;

nl=nf;



%

% 3. Extracts data

%

    unit=netcdf.getAtt(fid,myvar,'units');

    scale=netcdf.getAtt(fid,myvar,'scale_factor');

    fillv=netcdf.getAtt(fid,myvar,'_FillValue');

    vali=netcdf.getVar(fid,myvar,[II-1 JJ-1 LL-1 KK-1],[ni nj nl nk]);

 

%

% Converts to normal units in case of log scales ... 

%

    lg10=log(10);

    tabOK=0;

    imin=-2^15+1;

    imax=2^15;

    tabmat=linspace(imin,imax,imax-imin+1);

lenu=length(unit);

    if unit(1:15) == 'log10(m4s+1E-12'

      tabmat=exp(lg10.*tabmat.*scale)-(1E-12-1E-16);

      tabOK=1;

    end

    if unit(1:15) == 'log10(m4s+0.01)'

       tabmat=exp(lg10.*tabmat.*scale)-0.009999;

       tabOK=2;

    end



if (lenu >= 17) 

  if unit(1:17) == 'log10(Pa2s+1E-12)'

    tabmat=exp(lg10.*tabmat.*scale)-(1E-12-1E-16);

    tabOK=2;

  end

  if unit(1:17) == 'log10(m2 s+1E-12)'

    tabmat=exp(lg10.*tabmat.*scale)-(1E-12-1E-16);

    tabOK=2;

  end

end



if (lenu >= 18) 

if unit(1:18) == 'log10(Pa2 s+1E-12)'

    tabmat=exp(lg10.*tabmat.*scale)-(1E-12-1E-16);

    tabOK=2;

end

end



    

if tabOK > 0

        mat1=tabmat(imax+int32(squeeze(vali)));

        I=find(vali== fillv);

        mat1(I)=NaN;



else

      I=find(vali== fillv);

      val=double(vali).*scale;

      val(I)=NaN;

      eval(['mat'  num2str(j) '=val;']);

end

netcdf.close(fid);    

%    pcolor(lon1,lat1,squeeze(val1(:,:,1))');shading flat;

    
