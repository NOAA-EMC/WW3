function [messg,errno] = write_wwIIIobstr(fname,d1,d2);

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
%| Write the output arrays into ascii file                                             |
%|                                                                                     |
%| write_wwIIIfile(fname,d1,d2)                                                        |
%|                                                                                     |
%| INPUT                                                                               |
%|  fname       : Output file name                                                     |
%|  d1,d2       : Output 2D obstruction arrays in x (d1) and y (d2)                    |
%|                                                                                     |
%| OUTPUT                                                                              |
%|  messg       : Error message. Is blank if no error occurs                           |
%|  errno       : Error number. Is zero for succesful write                            |
% -------------------------------------------------------------------------------------

[Ny,Nx] = size(d1);

fid = fopen(fname,'w');

[messg,errno] = ferror(fid);

if (errno == 0)
   for i = 1:Ny
       a = d1(i,:);
       fprintf(fid,' %d ',a);
       fprintf(fid,'\n');
   end;
   fprintf(fid,'\n');
   for i = 1:Ny
       a = d2(i,:);
       fprintf(fid,' %d ',a);
       fprintf(fid,'\n');
   end;
else
   fprintf(1,'!!ERROR!!: %s \n',messg);
end;

fclose(fid);

return;
