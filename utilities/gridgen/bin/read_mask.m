function m = read_mask(fname,Nx,Ny)

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
%| Read data from file                                                                 |
%|                                                                                     |
%| m = read_mask(fname,Nx,Ny)                                                          |
%|                                                                                     |
%| INPUT                                                                               |
%|  fname       : Input file name containing data                                      |
%|  Nx,Ny       : Array dimensions in x and y                                          |
%|                                                                                     |
%| OUTPUT                                                                              |
%|  m           : 2D array with data                                                   |
% -------------------------------------------------------------------------------------
  
fid = fopen(fname,'r');

[messg,errno] = ferror(fid);

if (errno == 0)
   for i = 1:Ny
       a = fscanf(fid,'%d',Nx);
       m(i,:) = a;
   end;
else
   fprintf(1,'!!ERROR!!: %s \n',messg);
end;

fclose(fid);
return;
