function [lon_sub,lat_sub,depth_sub] = generate_grid(ref_dir,bathy_source,coord,dx,dy,coords,limit,cut_off,dry);

% -------------------------------------------------------------------------------------
%|                                                                                     |
%|                          +----------------------------+                             |
%|                          | GRIDGEN          NOAA/NCEP |                             |
%|                          |      Arun Chawla           |                             |
%|                          |    Andre VanderWesthuysen  |                             |
%|                          |                            |                             |
%|                          | Last Update :  20-Aug-2010 |                             |
%|                          +----------------------------+                             |
%|                                    Arun.Chawla@noaa.gov                             |
%|                         Andre.VanderWesthuysen@noaa.gov                             |
%|                          Distributed with WAVEWATCH III                             |
%|                                                                                     |
%|                     Copyright 2009 National Weather Service (NWS),                  |
%|       National Oceanic and Atmospheric Administration.  All rights reserved.        |
%|                                                                                     |
%| DESCRIPTION                                                                         |
%| This function creates a 2D bathymetry data set from high resolution "ETOPO1" or     | 
%| "ETOPO2" global bathymetry sets. Global bathymetry data sets are assumed to be       |
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
%|                        'etopo1' -- ETOPO1 bathymetry (corresponding file etopo1.nc) |
%|                        'etopo2' -- ETOPO2 bathymetry (corresponding file etopo2.nc) |
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
%|   NOTES                                                                             |
%|    a. This version uses the in-house matlab NETCDF functions which is available     |
%|       with Matlab 2008a or higher. If you are using an older version of MATLAB      |
%|       then you will have to install a NETCDF package and change portions of this    |
%|       script that handle netcdf files. Keep in mind that the NETCDF functions       |
%|       used in this package start their index from 0, this may not be the case in    |
%|       other NETCDF packages                                                         |
%|    b. The default bathymetric sets are etopo1 and etopo2, and while this package    |
%|       will allow you to create grids finer than these base grids, make sure that    |
%|       features are defined in the base bathymetries before using them.              |
%|    c. While the code allows for wrapping around in the base grid it assumes that    |
%|       the target grid will be monotonically increasing.                             |
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
    fname_base = [ref_dir,'/etopo2.nc'];
    var_x = 'x';
    var_y = 'y';
    dx_base = 1/30.0;
    dy_base = 1/30.0;
    Nx_base = 10801;
    Ny_base = 5401;
elseif (strcmp(bathy_source,'etopo1'))
    fname_base = [ref_dir,'/etopo1.nc'];
    var_x = 'lon';
    var_y = 'lat';
    dx_base = 1/60.0;
    dy_base = 1/60.0;
    Nx_base = 21601;
    Ny_base = 10801;
else
    fprintf(1,'Unrecognized source bathymetry option \n');
    return;
end;


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
%@@@ The next few lines assume that your Matlab
%@@@ version has ability to read NETCDF files

f = netcdf.open(fname_base,'nowrite');                            %!!!! NETCDF DEPENDENCY !!!!!!!
count_lat = (lat_end - lat_start) + 1;
lat_base = netcdf.getVar(f,0,lat_start-1,count_lat);              %!!!! NETCDF DEPENDENCY !!!!!!!
                                                                  %!! NOTE: Indexing from 0 !!
if (coords == 1 & lone <= lons)
    count_lon1 = (Nx_base - lon_start) + 1;                       %!!!! NETCDF DEPENDENCY !!!!!!!
    count_lon2 = (lon_end - 2) + 1;                               %!!!! NETCDF DEPENDENCY !!!!!!!
    lon1 = netcdf.getVar(f,1,lon_start-1,count_lon1);             %!!!! NETCDF DEPENDENCY !!!!!!!
    lon2 = netcdf.getVar(f,1,1,count_lon2);                       %!!!! NETCDF DEPENDENCY !!!!!!!
    lon_base = [lon1;lon2];
    depth_base_all = netcdf.getVar(f,2);                          %!!!! NETCDF DEPENDENCY !!!!!!!
    dep1 = depth_base_all([lon_start:Nx_base],[lat_start:lat_end]);
    dep2 = depth_base_all([2:lon_start_end],[lat_start:lat_end]);
    depth_base = [dep1 dep2];
else
    count_lon = (lon_end - lon_start) + 1;                        %!!!! NETCDF DEPENDENCY !!!!!!!
    lon_base = netcdf.getVar(f,1,lon_start-1,count_lon);          %!!!! NETCDF DEPENDENCY !!!!!!!
    depth_base_all = netcdf.getVar(f,2);                          %!!!! NETCDF DEPENDENCY !!!!!!!
    depth_base = depth_base_all([lon_start:lon_end],...
                                [lat_start:lat_end]);
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

lon_sub = [lonss:dx:lones];
lat_sub = [latss:dy:lates];

Nx = length(lon_sub);
Ny = length(lat_sub);

%@@@ Obtaining data from base bathymetry. If desired grid is coarser than base grid then 2D averaging of bathymetry 
%@@@ else grid is interpolated from base grid 
%@@@ Checks if grid cells wrap around in Longitudes. Does not do so for Lattitudes

itmp = 0;
Nb = Nx*Ny;

if (ndx <= 1 & ndy <= 1)
    
    %@@@ Interpolating from base grid
    
    fprintf(1,'Target grid is too fine, interpolating from base bathymetry ... \n');
    den = dx_base*dy_base;
    
    %@@@ Initializing starting points
    
    [lon_prev,tmp] = min(abs(lon_base-lon_sub(1)));
    if (lon_base(lon_prev) > lon_sub(1))
        lon_prev = lon_prev - 1;
    end;
    lon_next = lon_prev + 1;
    if (lon_prev == 0 ) 
        lon_prev = Nx_base - 1;
    end;
    if (lon_next > Nx_base)
        lon_next = 2;
    end;
    [lat_prev,tmp] = min(abs(lat_base-lat_sub(1)));
    if (lat_base(lat_prev) > lat_sub(1))
        lat_prev = lat_prev - 1;
    end;
    lat_next = lat_prev + 1;
    
    for i = 1:Nx
        
        %@@@ Find prev and next lon points
        
        while (lon_base(lon_next) < lon_sub(i))
            lon_prev = lon_next;
            lon_next = lon_next+1;
            if lon_next > Nx_base
                lon_next = 2;
            end; 
        end;
        dx1 = min(abs(lon_sub(i) - lon_base(lon_prev)),abs(lon_sub(i) - lon_base(lon_prev)- ...
                      360*sign(lon_sub(i) - lon_base(lon_prev))));
        dx2 = dx_base - dx1;
        
        for j = 1:Ny
            
            %@@@ Find prev and next lat points
            
            while (lat_base(lat_next) < lat_sub(j))
                lat_prev = lat_next;
                lat_next = lat_next+1;
            end;
            dy1 = lat_sub(j) - lat_base(lat_prev);
            dy2 = dy_base - dy1;
            
            %@@@ Four point interpolation
            
            a11 = depth_base(lat_prev,lon_prev);
            a12 = depth_base(lat_prev,lon_next);
            a21 = depth_base(lat_next,lon_prev);
            a22 = depth_base(lat_next,lon_next);
            depth_sub(j,i) = (a11*dy2*dx2 + a12*dy2*dx1 + a21*dy1*dx2 + a22*dx1*dy1)/den;
            
            %@@@ Counter to check proportion of cells completed

            Nl = (i-1)*Ny+j;
            itmp_prev = itmp;
            itmp = floor(Nl/Nb*100);
            if (mod(itmp,5) == 0 & itmp_prev ~= itmp)
                fprintf(1,'Completed %d per cent of the cells \n',itmp);
            end;
            
        end;  %@@@ end loop through lattitudes
        
    end;   %@@@ end loop through longitudes
    
    depth_sub(loc) = dry;
    clear loc;
    return;  %@@@ End of interpolating part of routine
    
else
    
    %@@@ Averaging from base grid
    
    fprintf(1,'Starting grid averaging ....\n');

    for i = 1:Nx
        
        %@@@ Determine the edges of cell along longitude 
        
        lon_start = lon_sub(i)-dx/2.0;
        lon_end = lon_sub(i)+dx/2.0;
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
        [tmp,lon_start_pos] = min(abs(lon_base-lon_start));
        [tmp,lon_end_pos] = min(abs(lon_base-lon_end));
        
        for j = 1:Ny
            
            %@@@ Determine the edges of cell along lattitude
            
            lat_start = lat_sub(j)-dy/2.0;
            lat_end = lat_sub(j)+dy/2.0;
            [tmp,lat_start_pos] = min(abs(lat_base-lat_start));
            [tmp,lat_end_pos] = min(abs(lat_base-lat_end));        
        
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
                clear depth_tmp loc;
            else                                 %@@@ grid cell wraps around 
                depth_tmp1 = depth_base(lat_start_pos:lat_end_pos,lon_start_pos:end);
                depth_tmp2 = depth_base(lat_start_pos:lat_end_pos,2:lon_end_pos);
                loc1 = find(depth_tmp1 <= cut_off);
                loc2 = find(depth_tmp2 <= cut_off);
                Nt = numel(depth_tmp1) + numel(depth_tmp2);
                if (~isempty(loc1) || ~isempty(loc2))
                    Ntt = length(loc1) + length(loc2);
                    if (Ntt/Nt > limit)
                        depth_sub(j,i) = 0.5*(mean(depth_tmp1(loc1)) + mean(depth_tmp2(loc2)));
                    else
                        depth_sub(j,i) = dry;
                    end;
                else
                    depth_sub(j,i) = dry;
                end;
                clear depth_tmp1 depth_tmp2 loc1 loc2;
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

    return;  %@@@ end of averaging part of routine

end;

