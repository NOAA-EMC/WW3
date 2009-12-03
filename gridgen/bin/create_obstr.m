function [sx,sy] = create_obstr(lon,lat,bound,mask,coords,offset_left,offset_right)

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
%| This routine generates the 2D obstruction grid in x and y given a 2D mask and set   |
%| of boundary polygons. Obstructions are only generated for wet cells and obstructions|
%| for cells on either side of a dry cell are also set to 0 (to prevent spurious       |
%| suppression of swell near the coast)                                                | 
%|                                                                                     |
%| [sx,sy] = create_obstr(lon,lat,bound,mask,coords,offset_left,offset_right)          |
%|                                                                                     |
%| INPUT                                                                               |
%|  lon          : An array of length Nx consisting of the longitude (x) coordinates   |
%|                 of the grid                                                         | 
%|  lat          : An array of length Ny consisting of the lattitude (y) coordinates   |
%|                 of the grid                                                         | 
%|  bound        : Data structure array of boundary polygons inside the grid           |
%|  mask         : 2D array of size (Nx,Ny) that determines land/sea mask              |
%|  coords       : Coordinate system representation for longitude data                 |
%|                 Options are --                                                      |
%|                    0 --> Longitudes range from -180 to 180                          |
%|                    1 --> Longitudes range from 0 to 360                             |
%|  offset_left  : Flag to determine if neighbor to the left/down in x/y should be     |
%|                 considered. Value of 0 indicates should not be considered while 1   |
%|                 indicates it should be                                              |
%|  offset_right : Similar to offset_left for neighbor to the right/up in x/y          |
%|                                                                                     |
%| OUTPUT                                                                              |
%|  sx,sy        : 2D obstruction grids of size (Nx,Ny) for obstructions in x and y.   |
%|                 Values range from 0 for no obstruction to 1 for full obstruction    | 
%|                                                                                     |
% -------------------------------------------------------------------------------------

%@@@ Initialize variables

dx = lon(2)-lon(1);
dy = lat(2)-lat(1);

Ny = length(lat);
Nx = length(lon);

sx = zeros(Ny,Nx);
sy = sx;
obfx = sy;
obfy = sx;

loc = find(mask == 0);
sx(loc) = 0;
sy(loc) = 0;
clear loc;

coord_start = -180+coords*180;
coord_end = coord_start+360;
itmp = 0;

N = length(bound);

Nb = Nx*Ny;

%@@@ Initialize boundary information for the cells of the grid

for j = 1:Nx
    for k = 1:Ny
        lon_start = lon(j)-dx/2;
        lon_end = lon(j)+dx/2;
        lat_start = lat(k)-dy/2;
        lat_end = lat(k)+dy/2;
        if (lon_start < coord_start)
            lon_start = lon_start+360;
        end;
        if (lon_end > coord_end)
            lon_end = lon_end-360;
        end;
        if (lon_start < lon_end)
            cell(k,j).nc = 1;
            cell(k,j).px(1,:) = [lon_start lon_end lon_end lon_start lon_start];
            cell(k,j).py(1,:) = [lat_start lat_start lat_end lat_end lat_start];
        else
            cell(k,j).nc = 2;
            cell(k,j).px(1,:) = [lon_start coord_end coord_end lon_start lon_start];
            cell(k,j).py(1,:) = [lat_start lat_start lat_end lat_end lat_start];
            cell(k,j).px(2,:) = [coord_start lon_end lon_end coord_start coord_start];
            cell(k,j).py(2,:) = [lat_start lat_start lat_end lat_end lat_start];
        end;        
        cell(k,j).south_lim = [];
        cell(k,j).north_lim = [];
        cell(k,j).x_count = 0;
        cell(k,j).y_count = 0;
        cell(k,j).west_lim = [];
        cell(k,j).east_lim = [];
        Nl = (j-1)*Ny+k;
    end;
end;

fprintf(1,'completed setting up the cells\n');

%@@@ Loop through the boundaries and determine the segments in each cell

for  i = 1:N    

    %@@@ Determine the cells that the boundary covers

    west = bound(i).west-dx;
    east = bound(i).east+dx;
    north = bound(i).north+dy;
    south = bound(i).south-dy;
    lat_loc = find(lat >= south & lat <= north);
    if (west < east)
        lon_loc = find(lon >= west & lon <= east);
    else        
        loc1 = find(lon >= west & lon <= coord_end);
        loc2 = find(lon >= coord_start & lon <= east);
        lon_loc = [loc1 loc2];
    end;
    
        
    Nxc = length(lon_loc);
    Nyc = length(lat_loc);
    Nb = Nxc*Nyc;

    %@@@ Loop through the cells covered by the boundary
    
    for j = 1:Nxc
        for k = 1:Nyc
            xp = lon_loc(j);
            yp = lat_loc(k);

            %@@@ Compute boundary segment only for wet cells

            if (mask(yp,xp) == 1)

                for m = 1:cell(yp,xp).nc

                    px = cell(yp,xp).px(m,:);
                    py = cell(yp,xp).py(m,:);

                    %@@@ Compute part of boundary inside the cell domain

                    in_box = inpolygon(bound(i).x,bound(i).y,px,py);
                    in_box_coords = find(in_box > 0);

                    if (~isempty(in_box_coords))

                        %@@@ Boundary segment along the N-S direction
                              
                        south_limit = min(bound(i).y(in_box_coords)); 
                        north_limit = max(bound(i).y(in_box_coords)); 
                   
                        if (south_limit <= py(4) && north_limit >= py(1))
                            south_limit = max([south_limit py(1)]);
                            north_limit = min([north_limit py(4)]);
                      
                            if (obfx(yp,xp) == 0)
                                obfx(yp,xp) = 1;
                                cell(yp,xp).x_count = 1;
                                cell(yp,xp).south_lim(cell(yp,xp).x_count) = south_limit;
                                cell(yp,xp).north_lim(cell(yp,xp).x_count) = north_limit;
                                cell(yp,xp).boundx(cell(yp,xp).x_count) = i;
                            else
                                cell(yp,xp).x_count = cell(yp,xp).x_count+1;
                                cell(yp,xp).south_lim(cell(yp,xp).x_count) = south_limit;
                                cell(yp,xp).north_lim(cell(yp,xp).x_count) = north_limit;
                                cell(yp,xp).boundx(cell(yp,xp).x_count) = i;
                            end;
                        end;

                        %@@@ Boundary segment along the E-W direction

                        west_limit = min(bound(i).x(in_box_coords)); 
                        east_limit = max(bound(i).x(in_box_coords)); 

                        if (west_limit <= px(2) && east_limit >= px(1))
                            west_limit = max([west_limit px(1)]);
                            east_limit = min([east_limit px(2)]);
                            if (obfy(yp,xp) == 0)
                                obfy(yp,xp) = 1;
                                cell(yp,xp).y_count = 1;
                                cell(yp,xp).west_lim(cell(yp,xp).y_count) = west_limit;
                                cell(yp,xp).east_lim(cell(yp,xp).y_count) = east_limit;
                                cell(yp,xp).boundy(cell(yp,xp).y_count) = i;
                            else
                                cell(yp,xp).y_count = cell(yp,xp).y_count+1;
                                cell(yp,xp).west_lim(cell(yp,xp).y_count) = west_limit;
                                cell(yp,xp).east_lim(cell(yp,xp).y_count) = east_limit;
                                cell(yp,xp).boundy(cell(yp,xp).y_count) = i;
                            end;
                        end;

                        clear in_box;
                        clear in_box_coords;

                    end;   %@@@ End of if condition to check if boundary lies inside cell

	        end;       %@@@ For loop for all segments of the cell (to account for cells wrapping around)            
            end;           %@@@ End of if condition to check if cell is wet

        end;               %@@@ For loop for cell rows

    end;                   %@@@ For loop for cell columns

    %@@@ Record of boundaries accounted for

    itmp_prev = itmp;
    itmp = floor(i/N*100);
    if (mod(itmp,5) == 0 & itmp_prev ~= itmp)
        fprintf(1,'Checked %d per cent of boundaries \n',itmp);
    end;

end;    %@@@ For loop for all the boundaries

%@@@ Loop through all the cells and move boundary segments that are part of the same boundary
%@@@ and cross neighboring cells. This is done to prevent double counting in building the obstruction
%@@@ grids. Only the immediate neighbors are checked when moving the segments

for j = 1:Nx
    for k = 1:Ny

        %@@@ Check only if cell is wet

        if (mask(k,j)==1)
            
            %@@@ First check the neighbors in x direction
            
            if (j < Nx)
                jj = j+1;

                %@@@ Check to see that boundary segments are present in both cells

                if (obfx(k,j)~=0 & obfx(k,jj)~=0)

                    %@@@ Save information to temporary variables

                    set1 = cell(k,j);
                    set2 = cell(k,jj);
                    found_common = 0;

                    %@@@ Loop through boundary segments and move segments of common boundaries 
                    %@@@ to the cell with the larger segment

                    for l = 1:set1.x_count
                        for m = 1:set2.x_count
                            if (set1.boundx(l) == set2.boundx(m))
                                if ((set1.north_lim(l)-set1.south_lim(l)) >= ...
                                        (set2.north_lim(m)-set2.south_lim(m)))
                                    set1.north_lim(l) = max([set1.north_lim(l) set2.north_lim(m)]);
                                    set1.south_lim(l) = min([set1.south_lim(l) set2.south_lim(m)]);
                                    for n = (m+1):set2.x_count
                                        set2.boundx(n-1) = set2.boundx(n);
                                        set2.north_lim(n-1) = set2.north_lim(n);
                                        set2.south_lim(n-1) = set2.south_lim(n);
                                    end;
                                    set2.x_count = set2.x_count-1;
                                else
                                    set2.north_lim(m) = max([set1.north_lim(l) set2.north_lim(m)]);
                                    set2.south_lim(m) = min([set1.south_lim(l) set2.south_lim(m)]);
                                    for n = (l+1):set1.x_count
                                        set1.boundx(n-1) = set1.boundx(n);
                                        set1.north_lim(n-1) = set1.north_lim(n);
                                        set1.south_lim(n-1) = set1.south_lim(n);
                                    end;
                                    set1.x_count = set1.x_count-1;
                                end;
                                found_common = 1;
                                break;
                            end;
                        end;
                    end;

                    %@@@ Write cell information back from temporary variables if common boundaries
                    %@@@ were found

                    if (found_common == 1)
                        cell(k,j).boundx = [];
                        cell(k,j).north_lim = [];
                        cell(k,j).south_lim = [];
                        cell(k,jj).boundx = [];
                        cell(k,jj).north_lim = [];
                        cell(k,jj).south_lim = [];
                        
                        cell(k,j).x_count = set1.x_count;
                        cell(k,j).boundx = set1.boundx(1:set1.x_count);
                        cell(k,j).north_lim = set1.north_lim(1:set1.x_count);
                        cell(k,j).south_lim = set1.south_lim(1:set1.x_count);
                        
                        cell(k,jj).x_count = set2.x_count;
                        cell(k,jj).boundx = set2.boundx(1:set2.x_count);
                        cell(k,jj).north_lim = set2.north_lim(1:set2.x_count);
                        cell(k,jj).south_lim = set2.south_lim(1:set2.x_count);

                        %@@@ Re-set the obstruction flags if number of boundary segments fall
                        %@@@ to zero because of the move(s)
                        
                        if (cell(k,j).x_count == 0)
                            obfx(k,j) = 0;
                        end;
                        if (cell(k,jj).x_count == 0)
                            obfx(k,jj) = 0;
                        end;

                    end;

                end;

            end;
            
	    %@@@ Now check the neighboris in y direction (comments are omitted
	    %@@@ because the operations are same as for x)
            
            if (k < Ny)
                kk = k+1;

                if (obfy(k,j)~=0 & obfy(kk,j)~=0)
                    set1 = cell(k,j);
                    set2 = cell(kk,j);
                    found_common = 0;

                    for l = 1:set1.y_count
                        for m = 1:set2.y_count

                            if (set1.boundy(l) == set2.boundy(m))
                                if ((set1.east_lim(l)-set1.west_lim(l)) >= ...
                                        (set2.east_lim(m)-set2.west_lim(m)))
                                    set1.east_lim(l) = max([set1.east_lim(l) set2.east_lim(m)]);
                                    set1.west_lim(l) = min([set1.west_lim(l) set2.west_lim(m)]);
                                    for n = (m+1):set2.y_count
                                        set2.boundy(n-1) = set2.boundy(n);
                                        set2.east_lim(n-1) = set2.east_lim(n);
                                        set2.west_lim(n-1) = set2.west_lim(n);
                                    end;
                                    set2.y_count = set2.y_count-1;
                                else
                                    set2.east_lim(m) = max([set1.east_lim(l) set2.east_lim(m)]);
                                    set2.west_lim(m) = min([set1.west_lim(l) set2.west_lim(m)]);
                                    for n = (l+1):set1.y_count
                                        set1.boundy(n-1) = set1.boundy(n);
                                        set1.east_lim(n-1) = set1.east_lim(n);
                                        set1.west_lim(n-1) = set1.west_lim(n);
                                    end;
                                    set1.y_count = set1.y_count-1;
                                end;
                                found_common = 1;
                                break;
                            end;

                        end;

                    end;

                    if (found_common == 1)
                        cell(k,j).boundy = [];
                        cell(k,j).east_lim = [];
                        cell(k,j).west_lim = [];
                        cell(kk,j).boundy = [];
                        cell(kk,j).east_lim = [];
                        cell(kk,j).west_lim = [];
                        
                        cell(k,j).y_count = set1.y_count;
                        cell(k,j).boundy = set1.boundy(1:set1.y_count);
                        cell(k,j).east_lim = set1.east_lim(1:set1.y_count);
                        cell(k,j).west_lim = set1.west_lim(1:set1.y_count);
                        
                        cell(kk,j).y_count = set2.y_count;
                        cell(kk,j).boundy = set2.boundy(1:set2.y_count);
                        cell(kk,j).east_lim = set2.east_lim(1:set2.y_count);
                        cell(kk,j).west_lim = set2.west_lim(1:set2.y_count);
                        
                        if (cell(k,j).y_count == 0)
                            obfy(k,j) = 0;
                        end;
                        if (cell(kk,j).y_count == 0)
                            obfy(kk,j) = 0;

                        end;

                    end;

                end;

            end; %@@@ End of if condition to check neighbor in y direction

        end; %@@@ End for if condition to check cell map

    end; %@@@ For loop along all the cell rows

end;  %@@@ For loop along all the cell columns

%@@@ Loop through the cells a second time to reduce overlapping segments into 
%@@@ individual non-overlapping segments

for j = 1:Nx
    for k = 1:Ny

        if (mask(k,j)==1)

            %@@@ First for x obstruction

            if (obfx(k,j) ~= 0)

                %@@@ Reduction of overlapping segments only done if there are
                %@@@ more than 1 segment

                if (cell(k,j).x_count > 1)
                    n_segs = cell(k,j).x_count;
                    baseseg_n = cell(k,j).north_lim;
                    baseseg_s = cell(k,j).south_lim;
                    cell(k,j).north_lim = [];
                    cell(k,j).south_lim = [];
                    ind_segs=0;
                    indseg_n = [];
                    indseg_s = [];

                    %@@@ Loop till all the segments in the cell accounted for

                    while (n_segs > 0)
                        overlap_found = 0;

                        %@@@ Check the first segment against all the other segments
                        %@@@ If no overlap is found store the segment and reduce the list
                        %@@@ by one. If overlap is found then replace the first segment by 
                        %@@@ the combined segment and still reduce the list but not store this
                        %@@@ segment as it is still not unique, and repeat the comparison. 
                        %@@@ By the time all the segments are accounted for (n_segs == 0),
                        %@@@ we are left with a list of non-overlapping segments

                        if (n_segs > 1)
                            for l = 2:n_segs
                                if (baseseg_n(1) >= baseseg_s(l) && baseseg_s(1) <= baseseg_n(l))
                                    baseseg_n(1) = max([baseseg_n(1) baseseg_n(l)]);
                                    baseseg_s(1) = min([baseseg_s(1) baseseg_s(l)]);
                                    overlap_found = 1;
                                    if (l == n_segs)
                                        n_segs = n_segs-1;
                                    else
                                        for m = l+1:n_segs
                                            baseseg_n(m-1) = baseseg_n(m);
                                            baseseg_s(m-1) = baseseg_s(m);
                                        end;
                                        n_segs = n_segs-1;
                                    end;
                                    break;
                                end;
                            end;
                        end;
                        
                        if (n_segs == 1)
                            ind_segs = ind_segs+1;
                            indseg_n(ind_segs) = baseseg_n(1);
                            indseg_s(ind_segs) = baseseg_s(1);
                            n_segs = n_segs-1;
                        else
                            if (overlap_found == 0)
                                ind_segs = ind_segs+1;
                                indseg_n(ind_segs) = baseseg_n(1);
                                indseg_s(ind_segs) = baseseg_s(1);
                                for l = 2:n_segs                                    
                                    baseseg_n(l-1) = baseseg_n(l);
                                    baseseg_s(l-1) = baseseg_s(l);
                                end;
                                n_segs = n_segs-1;
                            end;
                        end;
                    end;

                    %@@@ Store the unique segments back in the cell variables

                    cell(k,j).x_count = ind_segs;
                    cell(k,j).north_lim = indseg_n;
                    cell(k,j).south_lim = indseg_s;

                end; %@@@ Corresponds to check for multiple segments in x

           end; %@@@ Corresponds to check for obstruction in x

           %@@@ Now check for overlapping segments in y using the same algorithm as x

           if (obfy(k,j) ~= 0)

                if (cell(k,j).y_count > 1)

                    n_segs = cell(k,j).y_count;
                    baseseg_n = cell(k,j).east_lim;
                    baseseg_s = cell(k,j).west_lim;
                    cell(k,j).east_lim = [];
                    cell(k,j).west_lim = [];
                    ind_segs=0;
                    indseg_n = [];
                    indseg_s = [];

                    while (n_segs > 0)
                        overlap_found = 0;
                        if (n_segs > 1)
                            for l = 2:n_segs
                                if (baseseg_n(1) >= baseseg_s(l) && baseseg_s(1) <= baseseg_n(l))
                                    baseseg_n(1) = max([baseseg_n(1) baseseg_n(l)]);
                                    baseseg_s(1) = min([baseseg_s(1) baseseg_s(l)]);
                                    overlap_found = 1;
                                    if (l == n_segs)
                                        n_segs = n_segs-1;
                                    else
                                        for m = l+1:n_segs
                                            baseseg_n(m-1) = baseseg_n(m);
                                            baseseg_s(m-1) = baseseg_s(m);
                                        end;
                                        n_segs = n_segs-1;
                                    end;
                                    break;
                                end;
                            end;
                        end;
                        
                        if (n_segs == 1)
                            ind_segs = ind_segs+1;
                            indseg_n(ind_segs) = baseseg_n(1);
                            indseg_s(ind_segs) = baseseg_s(1);
                            n_segs = n_segs-1;
                        else
                            if (overlap_found == 0)
                                ind_segs = ind_segs+1;
                                indseg_n(ind_segs) = baseseg_n(1);
                                indseg_s(ind_segs) = baseseg_s(1);
                                for l = 2:n_segs                                    
                                    baseseg_n(l-1) = baseseg_n(l);
                                    baseseg_s(l-1) = baseseg_s(l);
                                end;
                                n_segs = n_segs-1;
                            end;
                        end;
                    end;

                    cell(k,j).y_count = ind_segs;
                    cell(k,j).east_lim = indseg_n;
                    cell(k,j).west_lim = indseg_s;

                end;

           end; %@@@ End of overlapping segment computation in y

        end;  %@@@ Check for land/sea mask of cell

    end;  %@@@ End of for loop along cell rows

end; %@@@ End of for loop along cell columns


%@@@ Final loop through the cell rows and columns to construct the obstruction grid
%@@@ accounting for neighboring cell information (if applicable)

fprintf(1,'Computing subgrid obstruction masks \n');

for j = 1:Nx
    for k = 1:Ny

        if (mask(k,j)==1)

            %@@@ Computing x obstruction

            if (obfx(k,j)~=0)

                n_segs = cell(k,j).x_count;
                baseseg_n = cell(k,j).north_lim;
                baseseg_s = cell(k,j).south_lim;
                
                no_boundary = 0;    

                %@@@ Compare boundary segments in cell with that of cell to left
                %@@@ (if applicable)

                for off = 1:offset_left

                    jj = j-off;
                    if (jj >= 1)
                        if (obfx(k,jj) == 1)
                            set1 = cell(k,jj); 

                            %@@@ First determine all the segments that are shadows in 
                            %@@@ segments of previous cell and remove them as they
		            %@@@ do not influence the obstruction process

                            shadow_flags = zeros(size(baseseg_n));

                            for m = 1:n_segs
                                for l = 1:set1.x_count
                                    if (set1.north_lim(l) >= baseseg_n(m) & set1.south_lim(l) <= baseseg_s(m))
                                        shadow_flags(m) = 1;
                                        break;
                                    end;
                                end;                                
                            end;

                            %@@@ Identify all the segments in the cell that are not in the shadow.
                            %@@@ If none exist then no_boundary flag is set to 1 and the obstruction
                            %@@@ is set to 0 as the cell does not influence the obstruction process
                            %@@@ Otherwise just remove the segments that are in the shadow

                            loc = find(shadow_flags == 0);
                            if (isempty(loc))
                                no_boundary = 1;
                                n_segs = 0;
                                baseseg_n = [];
                                baseseg_s = [];
                            elseif (length(loc) < n_segs)
                                tmp_n = baseseg_n;
                                tmp_s = baseseg_s;
                                baseseg_n = [];
                                baseseg_s = [];
                                baseseg_n = tmp_n(loc);
                                baseseg_s = tmp_s(loc);
                                n_segs = length(baseseg_n);
                                clear tmp_n;
                                clear tmp_s;
                            end;
                            clear loc;
			    clear shadow_flags;

                            %@@@ Now if there is/are still boundary/ies determine all the segments
                            %@@@ in previous cell that are shadows of present cell, remove them
                            %@@@ and add the remaining segments to the present segments list

                            if (~no_boundary)

                                shadow_flags = zeros(size(set1.north_lim));

                                for m = 1:set1.x_count
                                    for l = 1:n_segs
                                        if (set1.north_lim(m) <= baseseg_n(l) & set1.south_lim(m) >= baseseg_s(l))
                                            shadow_flags(m) = 1;
                                            break;
                                        end;
                                    end;                                
                                end;

                                loc = find(shadow_flags == 0);
                                if (~isempty(loc))
                                    if (length(loc) < set1.x_count)
                                        tmp_n = set1.north_lim;
                                        tmp_s = set1.south_lim;
                                        set1.north_lim = [];
                                        set1.south_lim = [];
                                        set1.north_lim = tmp_n(loc);
                                        set1.south_lim = tmp_s(loc);
                                        set1.x_count = length(set1.north_lim);
                                        clear tmp_n;
                                        clear tmp_s;
                                    end;
                                    n_segs = n_segs+set1.x_count;
                                    baseseg_n = [baseseg_n set1.north_lim];
                                    baseseg_s = [baseseg_s set1.south_lim];
                                end;
                                clear loc;
                                clear shadow_flags;
                            end;                           
                        end;
                    end;
                end; %@@@ End of search of segments in the left cell

		%@@@ If boundary segment(s) are still present from search of cell to the left
		%@@@ repeat the operations for cell to the right (if applicable) in a manner
                %@@@ similar to the cell on the left

                if (~no_boundary)

                    for off = 1:offset_right

                        jj = j+off;

                        if (jj <= Nx)
                            if (obfx(k,jj) == 1)
                                set1 = cell(k,jj); 

                                %@@@ See if segments lie in shadow zone to segments in right cell

                                shadow_flags = zeros(size(baseseg_n));
                                for m = 1:n_segs
                                    for l = 1:set1.x_count
                                        if (set1.north_lim(l) >= baseseg_n(m) & ...
                                                 set1.south_lim(l) <= baseseg_s(m))
                                            shadow_flags(m) = 1;
                                            break;
                                        end;
                                    end;                                
                                end;

                                %@@@ Set no_boundary flag if all segments removed

                                loc = find(shadow_flags == 0);
                                if (isempty(loc))
                                    no_boundary = 1;
                                    n_segs = 0;
                                    baseseg_n = [];
                                    baseseg_s = [];
                                elseif (length(loc) < n_segs)
                                    tmp_n = baseseg_n;
                                    tmp_s = baseseg_s;
                                    baseseg_n = [];
                                    baseseg_s = [];
                                    baseseg_n = tmp_n(loc);
                                    baseseg_s = tmp_s(loc);
                                    n_segs = length(baseseg_n);
                                    clear tmp_n;
                                    clear tmp_s;
                                end;

                                clear loc;
				clear shadow_flags;

                                %@@@ Remove segments from right cell that are in the shadow zone
                                %@@@ and add to the total list of segments 
                            
                                if (~no_boundary)
                                    shadow_flags = zeros(size(set1.north_lim));
                                    for m = 1:set1.x_count
                                        for l = 1:n_segs
                                            if (set1.north_lim(m) <= baseseg_n(l) & ...
                                                   set1.south_lim(m) >= baseseg_s(l))
                                                shadow_flags(m) = 1;
                                                break;
                                            end;
                                        end;                                
                                    end;

                                    loc = find(shadow_flags == 0);
                                    if (~isempty(loc))
                                        if (length(loc) < set1.x_count)
                                            tmp_n = set1.north_lim;
                                            tmp_s = set1.south_lim;
                                            set1.north_lim = [];
                                            set1.south_lim = [];
                                            set1.north_lim = tmp_n(loc);
                                            set1.south_lim = tmp_s(loc);
                                            set1.x_count = length(set1.north_lim);
                                            clear tmp_n;
                                            clear tmp_s;
                                        end;
                                        n_segs = n_segs+set1.x_count;
                                        baseseg_n = [baseseg_n set1.north_lim];
                                        baseseg_s = [baseseg_s set1.south_lim];
                                    end;
                                    clear loc;
                                    clear shadow_flags;
                                end;                           
                            end;
                        end;
                    end;
                end; %@@@ End of check of cells to the right
                
                %@@@ Now ready to build obstruction grid from the total set of segments

                if (~no_boundary)

                    %@@@ Obstruction grid straightforward if n_segs == 1

                    if (n_segs==1)

                        sx(k,j) = (baseseg_n(1)-baseseg_s(1))/dy;

                    else    

		        %@@@ Remove overlapping segments (from neighboring cell(s) info)

                        ind_segs=0;
                        indseg_n = [];
                        indseg_s = [];
                        while (n_segs > 0)
                            overlap_found = 0;
                            if (n_segs > 1)
                                for l = 2:n_segs
                                    if (baseseg_n(1) >= baseseg_s(l) && baseseg_s(1) <= baseseg_n(l))
                                        baseseg_n(1) = max([baseseg_n(1) baseseg_n(l)]);
                                        baseseg_s(1) = min([baseseg_s(1) baseseg_s(l)]);
                                        overlap_found = 1;
                                        if (l == n_segs)
                                            n_segs = n_segs-1;
                                        else
                                            for m = l+1:n_segs
                                                baseseg_n(m-1) = baseseg_n(m);
                                                baseseg_s(m-1) = baseseg_s(m);
                                            end;
                                            n_segs = n_segs-1;
                                        end;
                                        break;
                                    end;
                                end;
                            end;
                        
                            if (n_segs == 1)
                                ind_segs = ind_segs+1;
                                indseg_n(ind_segs) = baseseg_n(1);
                                indseg_s(ind_segs) = baseseg_s(1);
                                n_segs = n_segs-1;
                            else
                                if (overlap_found == 0)
                                    ind_segs = ind_segs+1;
                                    indseg_n(ind_segs) = baseseg_n(1);
                                    indseg_s(ind_segs) = baseseg_s(1);
                                    for l = 2:n_segs                                    
                                        baseseg_n(l-1) = baseseg_n(l);
                                        baseseg_s(l-1) = baseseg_s(l);
                                    end;
                                    n_segs = n_segs-1;
                                end;
                            end;
                        end;
      
                        %@@@ Compute the obstruction values from the independant segments

                        for l = 1:ind_segs
                            sx(k,j) = sx(k,j) + (indseg_n(l)-indseg_s(l))/dy;
                        end;

                        cell(k,j).indsegs_north = indseg_n;
                        cell(k,j).indsegs_south = indseg_s;
                        clear baseseg_n;
                        clear baseseg_s;
                        clear baseseg_bound;

                    end; %@@@ End of if statement to check number of segments

                end; %@@@ End of if statement to check wether any boundaries are still left in the cell
 
            end; %@@@ End of if statement to check if cell had obstructions along x to begin with
            
            %@@@ Now computing obstruction in y in a similar fashion to x

            if (obfy(k,j)~=0)

                n_segs = cell(k,j).y_count;
                baseseg_n = cell(k,j).east_lim;
                baseseg_s = cell(k,j).west_lim;
                
                no_boundary = 0;    

                %@@@ First check with boundaries in cell below (if applicable)

                for off = 1:offset_left

                    kk = k-off;

                    if (kk >= 1)
                        if (obfy(kk,j) == 1)
                            set1 = cell(kk,j); 
                            
                            %@@@ Like in x obstruction, remove boundaries in shadow zone
                            %@@@ and combine the two sets of segments
                           
                            shadow_flags = zeros(size(baseseg_n));
                            for m = 1:n_segs
                                for l = 1:set1.y_count
                                    if (set1.east_lim(l) >= baseseg_n(m) & set1.west_lim(l) <= baseseg_s(m))
                                        shadow_flags(m) = 1;
                                        break;
                                    end;
                                end;                                
                            end;

                            loc = find(shadow_flags == 0);

                            if (isempty(loc))
                                no_boundary = 1;
                                n_segs = 0;
                                baseseg_n = [];
                                baseseg_s = [];
                            elseif (length(loc) < n_segs)
                                tmp_n = baseseg_n;
                                tmp_s = baseseg_s;
                                baseseg_n = [];
                                baseseg_s = [];
                                baseseg_n = tmp_n(loc);
                                baseseg_s = tmp_s(loc);
                                n_segs = length(baseseg_n);
                                clear tmp_n;
                                clear tmp_s;
                            end;

                            clear loc;
                            clear shadow_flags
                            
                            if (~no_boundary)
                                shadow_flags = zeros(size(set1.east_lim));
                                for m = 1:set1.y_count
                                    for l = 1:n_segs
                                        if (set1.east_lim(m) <= baseseg_n(l) & set1.west_lim(m) >= baseseg_s(l))
                                            shadow_flags(m) = 1;
                                            break;
                                        end;
                                    end;                                
                                end;
                                loc = find(shadow_flags == 0);
                                if (~isempty(loc))
                                    if (length(loc) < set1.y_count)
                                        tmp_n = set1.east_lim;
                                        tmp_s = set1.west_lim;
                                        set1.east_lim = [];
                                        set1.west_lim = [];
                                        set1.east_lim = tmp_n(loc);
                                        set1.west_lim = tmp_s(loc);
                                        set1.y_count = length(set1.east_lim);
                                        clear tmp_n;
                                        clear tmp_s;
                                    end;
                                    n_segs = n_segs+set1.y_count;
                                    baseseg_n = [baseseg_n set1.east_lim];
                                    baseseg_s = [baseseg_s set1.west_lim];
                                end;
                                clear loc;
                                clear shadow_flags;
                            end;                          
                        end;
                    end;
                end;  %@@@ End of accounting for boundaries in the cell below

	        %@@@ Now moving to boundaries in the cell above (if applicable)
                
                if (~no_boundary)
                    for off = 1:offset_right
                        kk = k+off;
                        if (kk <= Ny)
                            if (obfy(kk,j) == 1)
                                set1 = cell(kk,j); 

                                %@@@ Again follow the same procedure to merge boundaries 
                                
                                shadow_flags = zeros(size(baseseg_n));
                                for m = 1:n_segs
                                    for l = 1:set1.y_count
                                        if (set1.east_lim(l) >= baseseg_n(m) & set1.west_lim(l) <= baseseg_s(m))
                                            shadow_flags(m) = 1;
                                            break;
                                        end;
                                    end;                                
                                end;

                                loc = find(shadow_flags == 0);
                                if (isempty(loc))
                                    no_boundary = 1;
                                    n_segs = 0;
                                    baseseg_n = [];
                                    baseseg_s = [];
                                elseif (length(loc) < n_segs)
                                    tmp_n = baseseg_n;
                                    tmp_s = baseseg_s;
                                    baseseg_n = [];
                                    baseseg_s = [];
                                    baseseg_n = tmp_n(loc);
                                    baseseg_s = tmp_s(loc);
                                    n_segs = length(baseseg_n);
                                    clear tmp_n;
                                    clear tmp_s;
                                end;
                                clear loc;
                                clear shadow_flags
                                
                                if (~no_boundary)
                                    shadow_flags = zeros(size(set1.east_lim));
                                    for m = 1:set1.y_count
                                        for l = 1:n_segs
                                            if (set1.east_lim(m) <= baseseg_n(l) & set1.west_lim(m) >= baseseg_s(l))
                                                shadow_flags(m) = 1;
                                                break;
                                            end;
                                        end;                                
                                    end;

                                    loc = find(shadow_flags == 0);
                                    if (~isempty(loc))
                                        if (length(loc) < set1.y_count)
                                            tmp_n = set1.east_lim;
                                            tmp_s = set1.west_lim;
                                            set1.east_lim = [];
                                            set1.west_lim = [];
                                            set1.east_lim = tmp_n(loc);
                                            set1.west_lim = tmp_s(loc);
                                            set1.y_count = length(set1.east_lim);
                                            clear tmp_n;
                                            clear tmp_s;
                                        end;
                                        n_segs = n_segs+set1.y_count;
                                        baseseg_n = [baseseg_n set1.east_lim];
                                        baseseg_s = [baseseg_s set1.west_lim];
                                    end;
                                    clear loc;
                                    clear shadow_flags;

                                end;                           
                            end;
                        end;
                    end;
                end;  %@@@ End of accounting for cell above
                
                %@@@ Buuilding obstruction grid in y from the different segments 
                
                if (~no_boundary)

                    if (n_segs==1)
                        sy(k,j) = (baseseg_n(1)-baseseg_s(1))/dx;
                    else    

                        ind_segs=0;
                        indseg_n = [];
                        indseg_s = [];

                        while (n_segs > 0)
                            overlap_found = 0;
                            if (n_segs > 1)
                                for l = 2:n_segs
                                    if (baseseg_n(1) >= baseseg_s(l) && baseseg_s(1) <= baseseg_n(l))
                                        baseseg_n(1) = max([baseseg_n(1) baseseg_n(l)]);
                                        baseseg_s(1) = min([baseseg_s(1) baseseg_s(l)]);
                                        overlap_found = 1;
                                        if (l == n_segs)
                                            n_segs = n_segs-1;
                                        else
                                            for m = l+1:n_segs
                                                baseseg_n(m-1) = baseseg_n(m);
                                                baseseg_s(m-1) = baseseg_s(m);
                                            end;
                                            n_segs = n_segs-1;
                                        end;
                                        break;
                                    end;
                                end;
                            end;
                        
                            if (n_segs == 1)
                                ind_segs = ind_segs+1;
                                indseg_n(ind_segs) = baseseg_n(1);
                                indseg_s(ind_segs) = baseseg_s(1);
                                n_segs = n_segs-1;
                            else
                                if (overlap_found == 0)
                                    ind_segs = ind_segs+1;
                                    indseg_n(ind_segs) = baseseg_n(1);
                                    indseg_s(ind_segs) = baseseg_s(1);
                                    for l = 2:n_segs                                    
                                        baseseg_n(l-1) = baseseg_n(l);
                                        baseseg_s(l-1) = baseseg_s(l);
                                    end;
                                    n_segs = n_segs-1;
                                end;
                            end;
                        end;
      
                        for l = 1:ind_segs
                            sy(k,j) = sy(k,j) + (indseg_n(l)-indseg_s(l))/dx;
                        end;
                        cell(k,j).indsegs_east = indseg_n;
                        cell(k,j).indsegs_west = indseg_s;
                        clear baseseg_n;
                        clear baseseg_s;
                        clear baseseg_bound;
                    end;
                end;
            end; %@@@ End of obstruction grid in y                                 

        end; %@@@ End of check of land/sea mask

        %@@@ Finally, setting the obstruction grid to zero if neighboring cells are
	%@@@ dry cells to prevent spurious swell attenuation near the coast 

        if (j < Nx & mask(k,j+1) == 0)
            sx(k,j) = 0;
        end;
        if (j > 1 & mask(k,j-1) == 0)
            sx(k,j) = 0;
        end;
        if (k < Ny & mask(k+1,j) == 0)
            sy(k,j) = 0;
        end;
        if (k > 1 & mask(k-1,j) == 0)
            sy(k,j) = 0;
        end;

    end; %@@@ End of cell row loop
end;  %@@@ End of cell column loop

return;
