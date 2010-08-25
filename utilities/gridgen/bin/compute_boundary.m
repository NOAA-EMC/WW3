function [bound_ingrid,Nb] = compute_boundary(coord,bound,icoords)

% -------------------------------------------------------------------------------------
%|                                                                                     |
%|                          +----------------------------+                             |
%|                          | GRIDGEN          NOAA/NCEP |                             |
%|                          |      Arun Chawla           |                             |
%|                          |  Andre van der Westhuysen  |                             |
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
%| Computes the shoreline polygons from the GSHHS database that lie within the         |
%| grid domain, properly accounting for polygons that cross the domain. The routine    |
%| has been designed to work with coastal polygons only but that can be changed by     | 
%| changing the flag bound(i).flag in the code. See GSHHS documentation for the        |
%| meaning of the different flags                                                      |
%|                                                                                     |
%| [bound_ingrid,Nb] = compute_boundary(coord,bound)                                   |
%|                                                                                     |
%| INPUT                                                                               |
%|   coord : An array defining the corner points of the grid                           |
%|           coord(1) = Lattitude (y) of lower left hand corner                        |
%|           coord(2) = Longitude (x) of lower left hand corner                        |
%|           coord(3) = Lattitude (y) of upper right hand corner                       |
%|           coord(4) = Longitude (x) of upper right hand corner                       |
%|   bound : A data structure array of the basic polygons (The GSHHS polygons are      |
%|           stored as mat files with several different resolutions and the user       | 
%|           should ensure that the files have been loaded before using this routine). |
%|                                                                                     |
%|           The different available files are --                                      |
%|              coastal_bound_ful.mat    -- Full resolution         (188606 polygons)  |
%|		coastal_bound_high.mat   -- High resolution (0.2 km; 153539 polygons)  |
%|		coastal_bound_inter.mat  -- Intermediate resolution                    |
%|                                                             (1 km; 41523 polygons)  |
%|		coastal_bound_low.mat    -- Low resolution     (5 km; 10769 polygons)  |
%|		coastal_bound_coarse.mat -- Coarse resolution  (25 km; 1866 polygons)  |
%|                                                                                     |
%|	    Loading any of these files would create a data structure array of the      |
%|          GSHHS polygons called "bound" and the user can use any of the above        |
%|	    Alternatively, a separate list of user defined polygons can also be        |
%|          generated having the same fields as bound. One such list is                |
%|	    "optional_coastal_polygons.mat" which is also distributed with the         |
%|          reference data. This is an ever growing list of water bodies that see      |
%|          very little wave action and for most practical purposes can be masked out  |
%|          as land.                                                                   |
%|                                                                                     | 
%|          See also optional_bound.m which shows how the optional coastal polygons    |
%|          are used                                                                   |
%|   icoords : Coordinate system representation for longitude data                     |
%|               Options are --                                                        |
%|                  0 --> Longitudes range from -180 to 180                            |
%|                  1 --> Longitudes range from 0 to 360                               |
%|                                                                                     | 
%| OUTPUT                                                                              |
%|  bound_ingrid : Subset data structure array of polygons that lie inside the grid    |
%|  Nb           : Total number of polygons found that lie inside the grid             |
%|                                                                                     |
% -------------------------------------------------------------------------------------

lat_start = coord(1);
lon_start = coord(2);
lat_end = coord(3);
lon_end = coord(4);

%@@@ Definitions

%@@@ Minimum distance between points (to avoid round off errors from points too close to each other)

eps = 1e-5;

%@@@ Polygon defining the bounding grid. Bounding grid is defined in the counter clockwise direction

px = [lon_start lon_end lon_end lon_start lon_start];
py = [lat_start lat_start lat_end lat_end lat_start];

%@@@ Slope and intercepts for each of the 4 lines of the bounding box
 
for i = 1:4
    if (px(i+1)==px(i))
        m_grid(i)=inf;
        c_grid(i)=0;
    else
        p = polyfit(px(i:i+1),py(i:i+1),1);
        m_grid(i) = p(1);
        c_grid(i) = p(2);
    end;
    box_length(i) = sqrt((px(i+1)-px(i))^2+(py(i+1)-py(i))^2);
    norm(i,1) = (py(i+1)-py(i))./box_length(i);
    norm(i,2) = -(px(i+1)-px(i))./box_length(i);
end;
norm(end+1,1) = norm(1,1);
norm(end+1,2) = norm(1,2);

%@@@ Initializing variables

N = length(bound);
in_coord = 1;
itmp = 0;

%@@@ Convert base data to specified longitude range

      if (icoords == 0)
          for i = 1:N
              loc = find(bound(i).x > 180);
              bound(i).x(loc) = bound(i).x(loc) - 360;
              bound(i).west = min(bound(i).x);
              bound(i).east = max(bound(i).x);
              clear loc;
          end;
      elseif (icoords == 1)
          for i = 1:N
              loc = find(bound(i).x < 0);
              bound(i).x(loc) = bound(i).x(loc) + 360;
              bound(i).west = min(bound(i).x);
              bound(i).east = max(bound(i).x);
              clear loc;
          end;
      end;

%@@@ Loop through all the boundaries in the database

for i = 1:N
    
    %@@@ Limit boundaries to coastal type only. This flag needs to be changed if 
    %@@@ interested in other boundaries. See GSHHS documentation for boundary type flags
   
    if (bound(i).level == 1)
                                                     
        %@@@ Determine if boundary lies completely outside the domain
        
        if (bound(i).west > lon_end || bound(i).east < lon_start ...     
              || bound(i).south > lat_end || bound(i).north < lat_start) 
            in_grid = 0;                                                 
        else
            in_grid = 1;
        end;
        
        %@@@ Determine if boundary lies completely inside the domain

        if (bound(i).west >= lon_start & bound(i).east <= lon_end & ...
               bound(i).south >= lat_start & bound(i).north <= lat_end)
            inside_grid = 1;
        else
            inside_grid = 0;
        end;

        %@@@ Ignore boundaries outside the domain

        if (in_grid)

            %@@@ Modify boundaries that are not completely inside domain
 
            if (~inside_grid)

                %@@@ Determine the points of the boundary that are inside/on/outside the bounding box
 
                [in_points,on_points] = inpolygon(bound(i).x,bound(i).y,px,py);
                
                loc1 = find(in_points == 1);
                loc2 = find(on_points == 1);

                %@@@ Ignore points that lie on the domain but neighboring points do not
                
                for j = 1:length(loc2)
                    if (loc2(j) == 1)
                        p1 = bound(i).n;
                        p2 = loc2(j)+1;
                    elseif (loc2(j) == bound(i).n)
                        p1 = loc2(j)-1;
                        p2 = 1;
                    else
                        p1 = loc2(j)-1;
                        p2 = loc2(j)+1;
                    end;
                    if (in_points(p1) == 0 & in_points(p2) == 0)
                        in_points(loc2(j)) = 0;
                    end;
                end;
                
                %@@@ Loop through only if there are points inside the domain
                
                if (~isempty(loc1)) 
                
                    n = bound(i).n;

                    %@@@ Flag the points where the boundary moves from in to out of the domain
                    %@@@ as well as out to in

                    in2out_count = 1;
                    out2in_count = 1;
                
                    out2in = [];
                    in2out = [];

                    for j = 1:bound(i).n-1
                        if (in_points(j) > 0 & in_points(j+1) == 0)
                            in2out(in2out_count) = j;
                            in2out_count = in2out_count+1;
                        end;
                        if (in_points(j) == 0 & in_points(j+1) > 0)
                            out2in(out2in_count) = j;
                            out2in_count = out2in_count+1;
                        end;
                    end;

                    in2out_count = in2out_count-1;
                    out2in_count = out2in_count-1;
                    if (in2out_count ~= out2in_count)
                        fprintf(1,'Error: mismatch in grid crossings, check boundary %d !! \n',i);
                        return;
                    end;                
               
                    %@@@ Crossing points are oriented to make sure we start from out to in

                    if (in_points(1) > 0)
                        in2out_tmp = in2out;
                        for j = 1:in2out_count-1
                            in2out(j) = in2out_tmp(j+1);
                        end;
                        in2out(in2out_count) = in2out_tmp(1);
                    end;
                    
                    clear in2out_tmp;
                                  
                    %@@@ For each in2out and out2in find a grid intersecting point

                    in2out_gridbox = [];
                    out2in_gridbox = [];
                    in2out_gridboxdist = [];
                    out2in_gridboxdist = [];
                    in2out_xcross = [];
                    out2in_xcross = [];
                    in2out_ycross = [];
                    out2in_ycross = [];
                    
                    in2out_gridbox = zeros(in2out_count,1);
                    out2in_gridbox = zeros(out2in_count,1);
                    in2out_gridboxdist = zeros(in2out_count,1);
                    out2in_gridboxdist = zeros(out2in_count,1);
                
                    for j = 1:in2out_count

                        if(on_points(in2out(j)) == 1)

                            x1 = bound(i).x(in2out(j));
                            y1 = bound(i).y(in2out(j));

                            for k = 1:4

                                if (isinf(m_grid(k)))
                                    g = abs(x1-px(k));
				else
                                    g = abs(m_grid(k)*x1+c_grid(k)-y1);
                                end;

                                if (g <= eps)
                                   in2out_gridbox(j) = k;
                                   in2out_gridboxdist(j) = k-1 + sqrt((px(k)-x1)^2+(py(k)-y1)^2)/box_length(k);
                                   break;
                                end;

                            end;

                            in2out_xcross(j) = NaN;
                            in2out_ycross(j) = NaN;

			else

                            x1 = bound(i).x(in2out(j));
                            x2 = bound(i).x(in2out(j)+1);
                            y1 = bound(i).y(in2out(j));
                            y2 = bound(i).y(in2out(j)+1);
                        
                            if (x2==x1)
                                m = inf;
                                c = 0;
                            else
                                if (abs(x2-x1) > 90)
                                    x2 = x2+360;
                                    if (abs(x2-x1) > 90)
                                        x2 = x2-720;
                                    end;
                                end;
                                p = polyfit([x1 x2],[y1 y2],1);
                                m = p(1);
                                c = p(2);
                            end;
                            d = sqrt((x1-x2)^2+(y1-y2)^2);
           
                            for k = 1:4
                                if (m ~= m_grid(k));
                                    if (~isinf(m) && ~isinf(m_grid(k)))
                                        x = (c_grid(k)-c)/(m-m_grid(k));
                                        y = m*x+c;
                                    elseif (isinf(m))
                                        x = x1;
                                        y = m_grid(k)*x+c_grid(k);
                                    else
                                        x = px(k);
                                        y = m*x+c;
                                    end;
                                
                                    d1 = sqrt((x1-x)^2+(y1-y)^2);
                                    d2 = sqrt((x-x2)^2+(y-y2)^2);
                                    if (abs(1-(d1+d2)/d) < 0.001)
                                        in2out_gridbox(j) = k;
                                        break;
                                    end;
                                end;
                            end;
                        
                            in2out_xcross(j) = x;
                            in2out_ycross(j) = y;
                            in2out_gridboxdist(j) = k-1 + sqrt((px(k)-x)^2+(py(k)-y)^2)/box_length(k);

                        end; %@@@ corresponds to if(on_points(in2out(j)) == 1)
                        
                        if(on_points(out2in(j)+1) == 1)

                            x1 = bound(i).x(out2in(j)+1);
                            y1 = bound(i).y(out2in(j)+1);

                            for k = 1:4
                                if (isinf(m_grid(k)))
                                    g = abs(x1-px(k));
                                else
                                    g = abs(m_grid(k)*x1+c_grid(k)-y1);
                                end;
                                if (g <= eps)
                                   out2in_gridbox(j) = k;
                                   out2in_gridboxdist(j) = k-1 + sqrt((px(k)-x1)^2+(py(k)-y1)^2)/box_length(k);
                                   break;
                                end;
                            end;

                            out2in_xcross(j) = NaN;
                            out2in_ycross(j) = NaN;

			else

                            x1 = bound(i).x(out2in(j));
                            x2 = bound(i).x(out2in(j)+1);
                            y1 = bound(i).y(out2in(j));
                            y2 = bound(i).y(out2in(j)+1);
                        
                            if (x2==x1)
                                m = inf;
                                c = 0;
                            else
                                if (abs(x2-x1) > 90)
                                    x1 = x1+360;
                                    if (abs(x2-x1) > 90)
                                        x1 = x1-720;
                                    end;
                                end;
                                p = polyfit([x1 x2],[y1 y2],1);
                                m = p(1);
                                c = p(2);
                            end;
                            d = sqrt((x1-x2)^2+(y1-y2)^2);
           
                            for k = 1:4
                                if (m ~= m_grid(k))
                                    if (~isinf(m) && ~isinf(m_grid(k)))
                                        x = (c_grid(k)-c)/(m-m_grid(k));
                                        y = m*x+c;
                                    elseif (isinf(m))
                                        x = x1;
                                        y = m_grid(k)*x+c_grid(k);
                                    else
                                        x = px(k);
                                        y = m*x+c;
                                    end;
                                    d1 = sqrt((x1-x)^2+(y1-y)^2);
                                    d2 = sqrt((x-x2)^2+(y-y2)^2);
                                    if (abs(1-(d1+d2)/d) < 0.001)
                                        out2in_gridbox(j) = k;
                                        break;
                                    end;
                                end;
                            end;
                            out2in_xcross(j) = x;
                            out2in_ycross(j) = y;
                            out2in_gridboxdist(j) = k-1 + sqrt((px(k)-x)^2+(py(k)-y)^2)/box_length(k);

                        end; %@@@ corresponds to if(on_points(out2in(j)) == 1)

                    end;     %@@@ end of j loop for all the intersection points
                
                    %@@@ Loop through the intersection points 

                    if (in2out_count > 0)

                        subseg_acc = zeros(in2out_count,1);

                        %@@@ Keep looping till all intersection points accounted for

                        while(~isempty(find(subseg_acc == 0)))

                            j=1;
                            while(subseg_acc(j) == 1)
                                j=j+1;
                                if (j > in2out_count)
                                    j=1;
                                end;
                                if (isempty(find(subseg_acc == 0)))
                                    break;
                                end;
                            end;
                            
                            %@@@ Starting a new boundary from the first unaccounted for intersection
                            %@@@ point. Boundary polygon is constructed by starting from the first time
                            %@@@ the original polygon intersects the domain coming in and including all
                            %@@@ the internal points and the intersection point from where it goes out again.

                            if (subseg_acc(j) == 0)
                        
                                bound_ingrid(in_coord).x = [];
                                bound_ingrid(in_coord).y = [];
                                bound_ingrid(in_coord).n = 0;
                                bound_ingrid(in_coord).east = 0;
                                bound_ingrid(in_coord).west = 0;
                                bound_ingrid(in_coord).north = 0;
                                bound_ingrid(in_coord).south = 0;
                                bound_ingrid(in_coord).height = 0;
                                bound_ingrid(in_coord).width = 0;

                                if (~isnan(out2in_xcross(j)))
                                    bound_ingrid(in_coord).x = out2in_xcross(j);
                                    bound_ingrid(in_coord).y = out2in_ycross(j);
                                end;

                                if ((out2in(j)+1) <= in2out(j))
                                    bound_ingrid(in_coord).x = [bound_ingrid(in_coord).x;...
                                                          bound(i).x((out2in(j)+1):in2out(j))];
                                    bound_ingrid(in_coord).y = [bound_ingrid(in_coord).y;...
                                                          bound(i).y((out2in(j)+1):in2out(j))];                       
                                else                               
                                    bound_ingrid(in_coord).x = [bound_ingrid(in_coord).x;...
                                                          bound(i).x((out2in(j)+1):n);...
                                                          bound(i).x(2:in2out(j))];
                                    bound_ingrid(in_coord).y = [bound_ingrid(in_coord).y;...
                                                          bound(i).y((out2in(j)+1):n);...
                                                          bound(i).y(2:in2out(j))];                              
                                end;

                                if (~isnan(in2out_xcross(j)))
                                    bound_ingrid(in_coord).x = [bound_ingrid(in_coord).x;in2out_xcross(j)];
                                    bound_ingrid(in_coord).y = [bound_ingrid(in_coord).y;in2out_ycross(j)];
                                end;

                                new_bound=0; %@@@ Flag initializing a new boundary

                                subseg_acc(j) = 1;
                                seg_start = j;
                                seg_index = j;
                            
                                %@@@ Loop through intersection points till boundary is closed

                                while (new_bound == 0)
 
                                    %@@@ Find the next closest point where the original polygon crosses the
                                    %@@@ domain in again
 
                                    kstart = in2out_gridbox(seg_index);
                                    start_dist = in2out_gridboxdist(seg_index);
                                    min_pos = 0;
                                    min_val = 4.0;

                                    for k1 = 1:out2in_count                                        
                                        if ((out2in_gridboxdist(k1)-start_dist) > eps ... 
                                            && (out2in_gridboxdist(k1)-start_dist) < min_val)
                                            min_pos = k1;
                                            min_val = out2in_gridboxdist(k1)-start_dist;
                                        end;                                        
                                    end;

                                    if (min_pos == 0)              %@@@ did not find any crossings between in2out 
                                                                   %@@@ and the end of the box 
                                        for k1 = 1:out2in_count
                                            if (out2in_gridboxdist(k1) < min_val)
                                                min_pos = k1;
                                                min_val = out2in_gridboxdist(k1);
                                            end;
                                        end;                                                                                    
                                    end;

                                    kend = out2in_gridbox(min_pos);
                                    x_mid = [];
                                    y_mid = [];

                                    %@@@ If the boundary polygon crosses the grid domain along different
                                    %@@@ domain edges then include the common grid domain corner points

                                    if (kstart ~= kend)
                                        if (kstart < kend)
                                            for k1 = kstart:(kend-1)
                                                x_mid = [x_mid;px(k1+1)];
                                                y_mid = [y_mid;py(k1+1)];
                                            end;
                                        else
                                            for k1 = kstart:4
                                                x_mid = [x_mid;px(k1+1)];
                                                y_mid = [y_mid;py(k1+1)];
                                            end;
                                            for k1 = 1:(kend-1)
                                                x_mid = [x_mid;px(k1+1)];
                                                y_mid = [y_mid;py(k1+1)];
                                            end;
                                        end;
                                    end;

                                    if (~isempty(x_mid))
                                        bound_ingrid(in_coord).x = [bound_ingrid(in_coord).x;x_mid];
                                        bound_ingrid(in_coord).y = [bound_ingrid(in_coord).y;y_mid];
                                    end;

                                    %@@@ Check if next crossing from out to in matches starting point of 
                                    %@@@ new boundary segment. If yes, then close the boundary

                                    if (min_pos == seg_start)             %@@@ need to close the grid

                                        bound_ingrid(in_coord).x(end+1) = bound_ingrid(in_coord).x(1);
                                        bound_ingrid(in_coord).y(end+1) = bound_ingrid(in_coord).y(1);
                                        bound_count = length(bound_ingrid(in_coord).x);
                                        bound_ingrid(in_coord).n = bound_count;                                 
                                        bound_ingrid(in_coord).east = max(bound_ingrid(in_coord).x);
                                        bound_ingrid(in_coord).west = min(bound_ingrid(in_coord).x);
                                        bound_ingrid(in_coord).north = max(bound_ingrid(in_coord).y);
                                        bound_ingrid(in_coord).south = min(bound_ingrid(in_coord).y);
                                        bound_ingrid(in_coord).height = bound_ingrid(in_coord).north ...
                                                            - bound_ingrid(in_coord).south;
                                        bound_ingrid(in_coord).width = bound_ingrid(in_coord).east ...
                                                            - bound_ingrid(in_coord).west;
                                        bound_ingrid(in_coord).level = 1;
                                        in_coord=in_coord+1;            %@@@ increment boundary counter
                                        new_bound=1;                    %@@@ reset flag to exit the boundary loop
                                                                        %@@@ and start new boundary
                    
				    else                                %@@@ add segment to the boundary 

                                        if (~isnan(out2in_xcross(min_pos)))
                                            bound_ingrid(in_coord).x = [bound_ingrid(in_coord).x;...
                                                               out2in_xcross(min_pos)];
                                            bound_ingrid(in_coord).y = [bound_ingrid(in_coord).y;...
                                                               out2in_ycross(min_pos)];
                                        end;

                                        if ((out2in(min_pos)+1) <= in2out(min_pos))                                        
                                            bound_ingrid(in_coord).x = [bound_ingrid(in_coord).x;...
                                                               bound(i).x((out2in(min_pos)+1):...
                                                                                in2out(min_pos))];
                                            bound_ingrid(in_coord).y = [bound_ingrid(in_coord).y;...
                                                               bound(i).y((out2in(min_pos)+1):...
                                                                                in2out(min_pos))];                                        
                                        else
                                            bound_ingrid(in_coord).x = [bound_ingrid(in_coord).x;...
                                                               bound(i).x((out2in(min_pos)+1):n);...
                                                               bound(i).x(2:in2out(min_pos))];
                                            bound_ingrid(in_coord).y = [bound_ingrid(in_coord).y;...
                                                               bound(i).y((out2in(min_pos)+1):n);...
                                                               bound(i).y(2:in2out(min_pos))];      
                                        end;

                                        if (~isnan(in2out_xcross(min_pos)))
                                            bound_ingrid(in_coord).x = [bound_ingrid(in_coord).x;...
                                                                          in2out_xcross(min_pos)];
                                            bound_ingrid(in_coord).y = [bound_ingrid(in_coord).y;...
                                                                          in2out_ycross(min_pos)];
                                        end;

                                        seg_index = min_pos;
                                        subseg_acc(min_pos) = 1;     %@@@ activate flag that segment has been 
                                                                     %@@@ accounted for

                                    end; %@@@ end corresponding to check if min_pos == seg_start

                                end; %@@@ end of while loop for new_bound. This loop exits when a
                                     %@@@ a boundary is closed and a new one has to be started

			    end;     %@@@ corresponds to if subseg_acc(j) == 0

			end;         %@@@ corresponds to while loop that checks if all sections (subseg_acc)
                                     %@@@ have been accounted for. Loop exits when all accounted for
                   
		    end;             %@@@ corresponds to if in2out_count > 0 (i.e. there are finite domain crossings)

                end;                 %@@@ corresponds to if statement checking if there are boundary points inside 
                                     %@@@ the domain

            else                     %@@@ boundary lies completely inside the grid

                %@@@ initializing and adding the boundary to the list

                bound_ingrid(in_coord).x = [];
                bound_ingrid(in_coord).y = [];
                bound_ingrid(in_coord).n = 0;
                bound_ingrid(in_coord).east = 0;
                bound_ingrid(in_coord).west = 0;
                bound_ingrid(in_coord).north = 0;     
                bound_ingrid(in_coord).south = 0;
                bound_ingrid(in_coord).height = 0;
                bound_ingrid(in_coord).width = 0;
                bound_ingrid(in_coord).n = bound(i).n;
                bound_ingrid(in_coord).x = bound(i).x;
                bound_ingrid(in_coord).y = bound(i).y;
                bound_ingrid(in_coord).east = max(bound_ingrid(in_coord).x);
                bound_ingrid(in_coord).west = min(bound_ingrid(in_coord).x);
                bound_ingrid(in_coord).north = max(bound_ingrid(in_coord).y);
                bound_ingrid(in_coord).south = min(bound_ingrid(in_coord).y);
                bound_ingrid(in_coord).height = bound_ingrid(in_coord).north - bound_ingrid(in_coord).south;
                bound_ingrid(in_coord).width = bound_ingrid(in_coord).east-bound_ingrid(in_coord).west;
                bound_ingrid(in_coord).level = 1;
                in_coord = in_coord+1;

            end;     %@@@ corresponds to if statement that determines if boundary lies partially/completely in domain

        end;         %@@@ corresponds to if statement that determines if boundary  lies outside the domain

    end;             %@@@ corresponds to if statement that determines boundary type

    %@@@ counter to keep tab on the level of processing

    itmp_prev = itmp;
    itmp = floor(i/N*100);
    if (mod(itmp,5)==0 & itmp_prev ~= itmp & N > 100)
        fprintf(1,'Completed %d per cent of %d boundaries and found %d internal boundaries \n',itmp,N,in_coord-1);
    end;

end;     %@@@ end of for loop that loops through all the GSHHS boundaries

Nb = in_coord-1;

if (Nb == 0)
    bound_ingrid(1) = -1;
end;

return;
