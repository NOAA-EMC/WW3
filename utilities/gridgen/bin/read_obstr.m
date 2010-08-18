function [m1,m2] = read_obstr(fname,Nx,Ny)

% -------------------------------------------------------------------------------------
%|                                                                                     |
%|                          +----------------------------+                             |
%|                          | GRIDGEN          NOAA/NCEP |                             |
%|                          |      Arun Chawla           |                             |
%|                          |                            |                             |
%|                          | Last Update :  24-Feb-2009 |                             |
%|                          +----------------------------+                             |
%|                                    Arun.Chawla@noaa.gov                             |
%|                          Distributed with WAVEWATCH III                             |
%|                                                                                     |
%|                     Copyright 2009 National Weather Service (NWS),                  |
%|       National Oceanic and Atmospheric Administration.  All rights reserved.        |
%|                                                                                     |
%| DESCRIPTION                                                                         |
%| Read obstruction data from file                                                     |
%|                                                                                     |
%| [m1,m2] = read_mask(fname,Nx,Ny)                                                    |
%|                                                                                     |
%| INPUT                                                                               |
%|  fname       : Input file name containing data                                      |
%|  Nx,Ny       : Array dimensions in x and y                                          |
%|                                                                                     |
%| OUTPUT                                                                              |
%|  m1           : 2D array with x obstruction data                                    |
%|  m2           : 2D array with y obstruction data                                    |
% -------------------------------------------------------------------------------------
  
fid = fopen(fname,'r');

[messg,errno] = ferror(fid);

if (errno == 0)
   for i = 1:Ny
       a = fscanf(fid,'%d',Nx);
       m1(i,:) = a;
   end;
   for i = 1:Ny
       a = fscanf(fid,'%d',Nx);
       m2(i,:) = a;
   end;
else
   fprintf(1,'!!ERROR!!: %s \n',messg);
end;

fclose(fid);
return;
