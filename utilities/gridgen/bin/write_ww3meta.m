function [messg,errno] = write_ww3meta(fname,lon,lat,N1,N2)

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
%| Write the meta data associated with the grids generated in this software. This      |
%| data needs to be provided as input to ww3_grid.inp when generating the mod_def      |
%| files for WAVEWATCH III. Note that the paths for the actual file locations          |
%| as well as the file names may be changed from what is written out in the meta file  |
%| and the user must make the appropriate changes as well                              |
%|                                                                                     |
%| [messg,errno] = write_ww3meta(fname,lon,lat,N1,N2)                                  |
%|                                                                                     |
%| INPUT                                                                               |
%|  fname       : Output file name prefix (the same prefix is used for creating the    |
%|                grids)                                                               |
%|  lon,lat     : Longitude array (x) and lattitude array (y) of the final grids       |
%|  N1          : Scaling applied to bottom bathymetry data                            |
%|  N2          : Scaling applied to obstruction grids                                 |  
%|                                                                                     |
%| OUTPUT                                                                              |
%|  messg       : Error message. Is blank if no error occurs                           |
%|  errno       : Error number. Is zero for succesful write                            |
% -------------------------------------------------------------------------------------

fid = fopen([fname,'.meta'],'w');

[messg,errno] = ferror(fid);

if (errno == 0)
   str1 = '$ Define grid -------------------------------------------------------- $';
   str2 = '$ Four records containing :';
   str3 = '$  1 NX, NY. As the outer grid lines are always defined as land';
   str4 = '$    points, the minimum size is 3x3.';
   str5 = '$  2 Grid increments SX, SY (degr.or m) and scaling (division) factor.';
   str6 = '$    If NX*SX = 360., latitudinal closure is applied.';
   str7 = '$  3 Coordinates of (1,1) (degr.) and scaling (division) factor.';
   str8 = '$  4 Limiting bottom depth (m) to discriminate between land and sea';
   str9 = '$    points, minimum water depth (m) as allowed in model, unit number';
   str10 = '$    of file with bottom depths, scale factor for bottom depths (mult.),';
   str11 = '$    IDLA, IDFM, format for formatted read, FROM and filename.';

   fprintf(fid,'%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n',str1,str2,str3,str4,str5,...
       str6,str7,str8,str9,str10,str11);
   fprintf(fid,'%d \t %d \n',length(lon),length(lat));
   fprintf(fid,'%5.2f \t %5.2f \t %5.2f \n',(lon(2)-lon(1))*60,(lat(2)-lat(1))*60,60);
   fprintf(fid,'%8.4f \t %8.4f \t %5.2f\n',lon(1),lat(1),1);
   fprintf(fid,'%5.2f  %5.2f  %d  %f  %d  %d %s  %s  %s \n',-0.1,2.5,20,N1,1,1,'''(....)''','NAME',['''',fname,'.depth_ascii','''']);
   fprintf(fid,'$ Sub-grid information \n');
   fprintf(fid,'%d  %f  %d  %d  %s  %s  %s  \n',30,N2,1,1,'''(....)''','NAME',['''',fname,'.obstr_lev1','''']);

else
   fprintf(1,'!!ERROR!!: %s \n',messg);
end;

fclose(fid);

return;
