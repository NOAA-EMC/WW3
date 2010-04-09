function [lon,lat] = read_ww3meta(fname)

% -------------------------------------------------------------------------------------
%|                                                                                     |
%|                          +----------------------------+                             |
%|                          | GRIDGEN          NOAA/NCEP |                             |
%|                          |      Arun Chawla           |                             |
%|                          |                            |                             |
%|                          | Last Update :  31-Jul-2007 |                             |
%|                          +----------------------------+                             |
%|                                    Arun.Chawla@noaa.gov                             |
%|                          Distributed with WAVEWATCH III                             |
%|                                                                                     |
%|                     Copyright 2009 National Weather Service (NWS),                  |
%|       National Oceanic and Atmospheric Administration.  All rights reserved.        |
%|                                                                                     |
%| DESCRIPTION                                                                         |
%| Read the meta data file to obtain the lon and lat for a set of grids                |
%|                                                                                     |
%| [lon,lat] = read_ww3meta(fname)                                                     |
%|                                                                                     |
%| INPUT                                                                               |
%|  fname       : Input meta data file name                                            |
%|                                                                                     |
%| OUTPUT                                                                              |
%|  lon,lat     : Longitude array (x) and lattitude array (y) of the final grids       |
% -------------------------------------------------------------------------------------

fid = fopen(fname,'r');

[messg,errno] = ferror(fid);

if (errno == 0)
   for i = 1:11
       tmp = fgetl(fid);
   end;
   Nx = fscanf(fid,'%d',1);
   Ny = fscanf(fid,'%d',1);
   dx = fscanf(fid,'%f',1);
   dy = fscanf(fid,'%f',1);
   scale = fscanf(fid,'%f',1);
   dx = dx/scale;
   dy = dy/scale;
   lons = fscanf(fid,'%f',1);
   lats = fscanf(fid,'%f',1);
   scale = fscanf(fid,'%f',1);

   lon = lons/scale + [0:(Nx-1)]*dx;
   lat = lats/scale + [0:(Ny-1)]*dy;

else
   fprintf(1,'!!ERROR!!: %s \n',messg);
end;

fclose(fid);

return;
