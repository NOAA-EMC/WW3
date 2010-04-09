function [lon_sub,lat_sub,depth_sub] = generate_grid(ref_dir,bathy_source,coord,dx,dy,coords,limit,cut_off,dry);

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
%| This function creates a 2D bathymetry data set from high resolution "ETOPO2" or     | 
%| "DBDB2" global bathymetry sets. Global bathymetry data sets are assumed to be       |
%| stored in Netcdf formats                                                            |
%|                                                                                     |
%| [lon,lat,depth] = generate_grid(ref_dir,bathy_source,coord,dx,dy,coords,limit,...   |
%|                                                                     cut_off,dry)    |
%|                                                                                     |
%|  INPUT                                                                              |
%|     ref_dir      : PATH string to where the global reference bathymetry data sets   |
%|                    are stored                                                       |
%|     bathy_source : String file to indicate which type of bathymetry is being used.  |
%|                    (User needs to make sure that the bathymetry data files          |
%|                    corresponding to the options are available in ref_dir            |
%|                    Options are --                                                   |
%|                        'etopo2' -- ETOPO2 bathymetry (corresponding file ETOPO2.nc) |
%|                        'dbdb2'  -- DBDB2 bathymetry  (corresponding file DBDB2.nc)  |
%|     coord        : An array defining the corner points of the grid                  |
%|                    coord(1) = Lattitude (y) of lower left hand corner               |
%|                    coord(2) = Longitude (x) of lower left hand corner               |
%|                    coord(3) = Lattitude (y) of upper right hand corner              |
%|                    coord(2) = Longitude (x) of upper right hand corner              |
%|     dx,dy        : Grid resolution in x and y                                       |
%|     coords       : Coordinate system representation for longitude data              |
%|                    Options are --                                                   |
%|                       0 --> Longitudes range from -180 to 180                       |
%|                       1 --> Longitudes range from 0 to 360                          |
%|    limit         : Value ranging between 0 and 1 indicating what fraction of a grid |
%|                    cell needs to be covered by wet cells (from the base grid) for   | 
%|                    the cell to be marked wet.                                       |
%|    cut_off       : Cut_off depth to distinguish between dry and wet cells, all      |
%|                    depths below the cut_off depth are marked as wet cells           | 
%|    dry           : Depth value assigned to the dry cells                            |
%|                                                                                     |
%|  OUTPUT                                                                             |
%|    lon           : An array of length Nx consisting of the longitude (x)            |
%|                    coordinates of the grid                                          |
%|    lat           : An array of length Ny consisting of the lattitude (y)            |
%|                    coordinates of the grid                                          |
%|    depth         : A 2D array of dimensions (Nx,Ny) consisting of the grid depths   |
%|                                                                                     |
% -------------------------------------------------------------------------------------

%@@@ Initialize the corners of the grid domain

lats = coord(1);
lons = coord(2);
late = coord(3);
lone = coord(4);

latss = lats;
lonss = lons;
lates = late;
lones = lone;

%@@@ Determine the file name for source bathymetry

if (strcmp(bathy_source,'etopo2'))
    fname_base = [ref_dir,'/Etopo2.nc'];
elseif (strcmp(bathy_source,'dbdb2'))
    fname_base = [ref_dir,'/Dbdb2.nc'];
else
    fprintf(1,'Unrecognized source bathymetry option \n');
    return;
end;

dx_base = 1/30.0;
dy_base = 1/30.0;
Nx_base = 10801;
Ny_base = 5401;

%@@@ Check the range of lattitudes and longitudes

if (coords ~= 0 && coords ~=1)
    fprintf(1,'incorrect value for coords\n');
    return;
end;

lats_base = -90;
late_base = 90; 
lons_base = -180+coords*180;
lone_base = lons_base+360;

if (lats < lats_base || lats > late_base || late < lats_base || late > late_base)
    fprintf(1,'ERROR : Lattitudes (%d,%d) beyond range (%d,%d) \n',lats,late,lats_base,late_base);
    return;
end;
  
if (lons < lons_base || lons > lone_base || lone < lons_base || lone > lone_base)
    fprintf(1,'ERROR : Longitudes (%d,%d) beyond range (%d,%d) \n',lons,lone,lons_base,lone_base);
    return;
end;

%@@@ Determine the starting and end points for extracting lattitude data from NETCDF

lat_start = floor(( (lats-2*dy) - lats_base)/dy_base);

if (lat_start < 1)
    lat_start = 1;
end;
 
lat_end = ceil(((late+2*dy) - lats_base)/dy_base) +1;

if (lat_end > Ny_base)
    lat_end = Ny_base;
end;

%@@@ Determine the starting and end points for extracting longitude data from NETCDF

%@@@ Code assumes that the longitude data in source file is stored in -180 to 180 range

if (coords == 1)
    if (lons > 180)
        lons = lons - 360;
    end;
    if (lone > 180)
        lone = lone - 360;
    end;
end;

lon_start = floor(((lons-2*dx)+180)/dx_base);
lon_end = ceil(((lone+2*dx)+180)/dx_base) +1;

if (lon_start < 1)
    lon_start = 1;
end;

if (lon_start > Nx_base)
    lon_start = Nx_base;
end;

if (lon_end < 1)
    lon_end = 1;
end;

if (lon_end >Nx_base)
    lon_end = Nx_base;
end;    

%@@@ Extract data from Netcdf files

f = netcdf(fname_base,'nowrite');                                 %!!!! NETCDF DEPENDENCY !!!!!!!

lat_base = f{'Latitude'}(lat_start:lat_end);                      %!!!! NETCDF DEPENDENCY !!!!!!!

if (coords == 1 & lone <= lons)
     lon1 = f{'Longitude'}(lon_start:Nx_base);                    %!!!! NETCDF DEPENDENCY !!!!!!!
     lon2 = f{'Longitude'}(2:lon_end);                            %!!!! NETCDF DEPENDENCY !!!!!!!
    lon_base = [lon1;lon2];
    dep1 = f{'Depth'}(lat_start:lat_end,lon_start:Nx_base);       %!!!! NETCDF DEPENDENCY !!!!!!!
    dep2 = f{'Depth'}(lat_start:lat_end,2:lon_end);               %!!!! NETCDF DEPENDENCY !!!!!!!
    depth_base = [dep1 dep2];
else
    lon_base = f{'Longitude'}(lon_start:lon_end);                 %!!!! NETCDF DEPENDENCY !!!!!!!
    depth_base = f{'Depth'}(lat_start:lat_end,lon_start:lon_end); %!!!! NETCDF DEPENDENCY !!!!!!!
end;

fprintf(1,'read in the base bathymetry \n');

if (coords == 1)
    loc = find(lon_base < 0);
    lon_base(loc) = lon_base(loc)+360;
    if (lon_base(end) == 0)
        lon_base(end) = lon_base(end)+360;
    end;
end;

Nx0 = length(lon_base);
Ny0 = length(lat_base);

%@@@ Ratio of desired resolution to base resolution

ndx = round(dx/dx_base);
ndy = round(dy/dy_base);

%@@@ 2D averaging of bathymetry (only done if the desired grid is coarser than the base grid)
%@@@ Checks if grid cells wrap around in Longitudes. Does not do so for Lattitudes

if (ndx <= 1 & ndy <= 1)
    fprintf(1,'Target grid is too fine, returning base bathymetry \n');
    [tmp,lon_start] = min(abs(lon_base-lonss));
    [tmp,lon_end] = min(abs(lon_base-lones));
    [tmp,lat_start] = min(abs(lat_base-latss));
    [tmp,lat_end] = min(abs(lat_base-lates));
    lon_sub = lon_base(lon_start:lon_end);
    lat_sub = lat_base(lat_start:lat_end);
    depth_sub = depth_base(lat_start:lat_end,lon_start:lon_end);
    loc = find(depth_sub > cut_off);
    depth_sub(loc) = dry;
    clear loc;
    return;
end;


lon_sub = [lonss:dx:lones];
lat_sub = [latss:dy:lates];

Nx = length(lon_sub);
Ny = length(lat_sub);

fprintf(1,'Starting grid averaging ....\n');

itmp = 0;
Nb = Nx*Ny;

%@@@ 2D grid averaging over base bathymetry

for i = 1:Nx
    for j = 1:Ny

        %@@@ Determine the edges of each cell

        lon_start = lon_sub(i)-dx/2.0;
        lon_end = lon_sub(i)+dx/2.0;
        lat_start = lat_sub(j)-dy/2.0;
        lat_end = lat_sub(j)+dy/2.0;
        if (coords == 1)
            if (lon_start < 0)
                lon_start = lon_start + 360;
            end;
            if (lon_end > 360)
                lon_end = lon_end-360;
            end;
        else
            if (lon_start < -180)
                lon_start = lon_start + 360;
            end;
            if (lon_end > 180)
                lon_end = lon_end - 360;
            end;
        end;

        %@@@ Determine all the source points within this cell

        [tmp,lat_start_pos] = min(abs(lat_base-lat_start));
        [tmp,lat_end_pos] = min(abs(lat_base-lat_end));        
        [tmp,lon_start_pos] = min(abs(lon_base-lon_start));
        [tmp,lon_end_pos] = min(abs(lon_base-lon_end));

        %@@@ Average the depth over all the wet cells in source that lie within the cell
        %@@@ Cell is marked dry if the proportion of wet cells in source is less than specified
        %@@@ limit

        if (lon_start_pos < lon_end_pos)      %@@@ grid cells do not wrap around
            
            depth_tmp = depth_base(lat_start_pos:lat_end_pos,lon_start_pos:lon_end_pos);
            loc = find(depth_tmp <= cut_off);
            Nt = numel(depth_tmp);
            if (~isempty(loc))
                Ntt = length(loc);
                if (Ntt/Nt > limit)
                    depth_sub(j,i) = mean(depth_tmp(loc));
                else
                    depth_sub(j,i) = dry;
                end;
            else
                depth_sub(j,i) = dry;
            end;
            clear depth_tmp;
            clear loc;

	 else                                 %@@@ grid cell wraps around
 
            depth_tmp1 = depth_base(lat_start_pos:lat_end_pos,lon_start_pos:end);
            depth_tmp2 = depth_base(lat_start_pos:lat_end_pos,2:lon_end_pos);
            loc1 = find(depth_tmp1 <= cut_off);
            loc2 = find(depth_tmp2 <= cut_off);
            Nt = numel(depth_tmp1) + numel(depth_tmp2);
            if (~isempty(loc1) || ~isempty(loc2))
                Ntt = length(loc1) + length(loc2);
                if (Ntt/Nt > limit)
                    depth_sub(j,i) = mean(mean(depth_tmp1(loc1)) + mean(depth_tmp2(loc2)));
                else
                    depth_sub(j,i) = dry;
                end;
            else
                depth_sub(j,i) = dry;
            end;
            clear depth_tmp1;
            clear depth_tmp2;
            clear loc1;
            clear loc2;

        end;           %@@@ end of check to see if cell wraps around

        %@@@ Counter to check proportion of cells completed

        Nl = (i-1)*Ny+j;
        itmp_prev = itmp;
        itmp = floor(Nl/Nb*100);
        if (mod(itmp,5) == 0 & itmp_prev ~= itmp)
           fprintf(1,'Completed %d per cent of the cells \n',itmp);
        end;

    end;    %@@@ end of for loop through all the rows (lattitudes)

end;    %@@@ end of for loop through all the columns (longitudes)

clear lon_base;
clear lat_base;
clear depth_base;

return;

