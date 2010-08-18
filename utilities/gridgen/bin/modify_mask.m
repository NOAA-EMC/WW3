function m_new = modify_mask2(m,lon,lat,px,py,mb,lonb,latb,igl);

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
%| This routine was developed for WAVEWATCH III v3.10 or higher (multi-grid version)   | 
%| where the traditional mask with values of 0 and 1 for land and water are            | 
%| modified to give values of 0,1,2 and 3 for cells that are on land, water,           | 
%| boundary or to be ignored, respectively. These masks are needed for grids           |
%| that are nested with a larger grid from which they get their boundary information   |
%|                                                                                     |
%| m_new = modify_mask(m,lon,lat,px,py,mb,lonb,latb,igl)                               |
%|                                                                                     |
%| INPUT                                                                               |
%|   m     : 2D land sea mask for grid                                                 |
%|   lon   : longitude (x) coordinates of grid                                         |
%|   lat   : lattitude (y) coordinates of grid                                         |
%|   mb    : 2D mask for base grid (grid with which boundary data is exchanged)        |
%|   lonb  : longitude (x) coordinates of base grid                                    |
%|   latb  : lattitude (y) coordinates of base grid                                    |
%|   px,py : x,y coordinates of polygon defining the region of active computation in   |
%|           the grid                                                                  |
%|   igl   : flag indicating if base grid is global (1) or not (0)                     |
%|                                                                                     |
%| OUTPUT                                                                              |
%|   m_new : New 2D mask file with values ranging from 0-3.                            |
%|              0 -> Active (within the computational region) dry cells                |
%|              1 -> Active wet cells                                                  |
%|              2 -> Boundary cells (for data exchange)                                |
%|              3 -> Inactive cells                                                    |
%|                                                                                     |
% -------------------------------------------------------------------------------------

m_new = m;

% Use the center of the grid as reference point

%lon0 = mean(lon);
%lat0 = mean(lat);
%R = 6370000;

% Convert to Cartesian coordinates

%lon = R.*cos(lat0*pi/180.0).*(lon-lon0).*pi/180.0;
%lat = R.*(lat-lat0).*pi/180.0;
%lonb = R.*cos(lat0*pi/180.0).*(lonb-lon0).*pi/180.0;
%latb = R.*(latb-lat0).*pi/180.0;
%px = R.*cos(lat0*pi/180.0).*(px-lon0).*pi/180.0;
%py = R.*(py-lat0).*pi/180.0;

%
 
[lon2,lat2] = meshgrid(lon,lat);
[in_points,on_points] = inpolygon(lon2,lat2,px,py);

clear lon2 lat2;

Nx = length(lon);
Ny = length(lat);

loc = find(in_points == 0);
length(loc)
m_new(loc) = 3;
clear loc;
loc = find(on_points == 1 & m == 1);
length(loc)
m_new(loc) = 2;
clear loc;

dx = lon(2)-lon(1)
dy = lat(2)-lat(1)

N = length(px);

% Find the cells to specify as boundary points

fprintf(1,'Finished initial allocation ....\n');                
% moving down the columns

fprintf(1,'Moving down the columns \n');

Nb = Nx*(Ny-1);
itmp = 0;

for j = 1:Nx
    for i = 1:Ny-1
        if (((m_new(i,j) == 1 & m_new(i+1,j) == 3)) || (m_new(i,j) == 3 & m_new(i+1,j) == 1))
            trans_lth = [];
            for k = i:i+1
                lons = lon(j)-0.5*dx;
                lone = lon(j)+0.5*dx;
                lats = lat(k)-0.5*dy;
                late = lat(k)+0.5*dy;
                px1 = [lons lone lone lons lons];
                py1 = [lats lats late late lats];
                for r = 1:4
                    if (px1(r+1)==px1(r))
                        m_grid(r)=inf;
                        c_grid(r)=0;
                    else
                        p = polyfit(px1(r:r+1),py1(r:r+1),1);
                        m_grid(r) = p(1);
                        if (abs(m_grid(r)) < 1e-10)
                            m_grid(r) = 0;
                        end;
                        c_grid(r) = p(2);
                    end;
                end;
                trans_lth(k-i+1) = 0;
                [poly_bound,on_pts] = inpolygon(px,py,px1,py1);                
                for l = 1:N-1
                    if (poly_bound(l) == 1 & poly_bound(l+1) == 1)
                        trans_lth(k-i+1) = trans_lth(k-i+1) + sqrt((px(l+1)-px(l))^2+(py(l+1)-py(l))^2);
                    else
                        x1 = px(l);
                        x2 = px(l+1);
                        y1 = py(l);
                        y2 = py(l+1);
                        if (x2==x1)
                            mt = inf;
                            ct = 0;
                        else
                            if (abs(x2-x1) > 90)                                
                                x2 = x2+360;
                                if (abs(x2-x1) > 90)
                                    x2 = x2-720;
                                end;
                            end;
                            p = polyfit([x1 x2],[y1 y2],1);                            
                            mt = p(1);
                            ct = p(2);
                            if (abs(mt) < 1e-10)
                                mt = 0;
                            end;
                        end;                        
                        count = 0;
                        tx = [];
                        ty = [];
                        d0 = sqrt((x1-x2)^2+(y1-y2)^2);
                        for r = 1:4                            
                            if (mt == m_grid(r))
                                if (mt == 0)
                                    if (y1>= min(py1(r:r+1)) & y1<= max(py1(r:r+1)))
                                        y = y1;
                                        if (poly_bound(l) == 1 & poly_bound(l+1) == 0)
                                            count = count+1;
                                            tx(count) = x2;
                                            ty(count) = y;
                                        elseif (poly_bound(l) == 0 & poly_bound(l+1) == 1)
                                            count = count+1;
                                            tx(count) = x1;
                                            ty(count) = y;
                                        elseif (poly_bound(l) == 0 & poly_bound(l+1) == 0)
                                            if (px1(r) >= min([x1 x2]) & px1(r) <= max([x1 x2]))
                                                count = count+1;
                                                tx(count) = px1(r);
                                                ty(count) = y;
                                            end;
                                            if (px1(r+1) >= min([x1 x2]) & px1(r+1) <= max([x1 x2]))
                                                count = count+1;
                                                tx(count) = px1(r+1);
                                                ty(count) = y;
                                            end;
                                        end;
                                    end;
                                elseif (isinf(mt))
                                    if (x1>= min(px1(r:r+1)) & x1<= max(px1(r:r+1)))
                                        x = x1;
                                        if (poly_bound(l) == 1 & poly_bound(l+1) == 0)
                                            count = count+1;
                                            ty(count) = y2;
                                            tx(count) = x;
                                        elseif (poly_bound(l) == 0 & poly_bound(l+1) == 1)
                                            count = count+1;
                                            ty(count) = y1;
                                            tx(count) = x;
                                        elseif (poly_bound(l) == 0 & poly_bound(l+1) == 0)
                                            if (py1(r) >= min([y1 y2]) & py1(r) <= max([y1 y2]))
                                                count = count+1;
                                                ty(count) = py1(r);
                                                tx(count) = x;
                                            end;
                                            if (py1(r+1) >= min([y1 y2]) & py1(r+1) <= max([y1 y2]))
                                                count = count+1;
                                                ty(count) = py1(r+1);
                                                tx(count) = x;
                                            end;
                                        end;
                                    end;
                                end;                                                                                       
                            elseif (mt ~= m_grid(r));
                                if (~isinf(mt) && ~isinf(m_grid(r)))
                                    x = (c_grid(r)-ct)/(mt-m_grid(r));
                                    y = mt*x+ct;
                                elseif (isinf(mt))
                                    x = x1;
                                    y = m_grid(r)*x+c_grid(r);
                                else
                                    x = px1(r);
                                    y = mt*x+ct;
                                end;
                                d = sqrt((px1(r)-px1(r+1))^2+(py1(r)-py1(r+1))^2);
                                d1 = sqrt((px1(r)-x)^2+(py1(r)-y)^2);
                                d2 = sqrt((x-px1(r+1))^2+(y-py1(r+1))^2);                               
                                if (abs(1-(d1+d2)/d) < 0.00001)
                                    d1 = sqrt((x1-x)^2+(y1-y)^2);
                                    d2 = sqrt((x2-x)^2+(y2-y)^2);
                                    if (abs(1-(d1+d2)/d0) < 0.00001)
                                        count = count+1;
                                        tx(count) = x;
                                        ty(count) = y;
                                    end;
                                end;
                                
                            end;
                        end;
                        if (count > 0)
                            if (count > 1)                                
                                txp = tx;
                                typ = ty;
                                clear tx ty;                                
                                tx(1) = txp(end);
                                ty(1) = typ(end);
                                txp = txp(1:end-1);
                                typ = typ(1:end-1);
                                Ntt = length(txp);
                                for Nt = Ntt:-1:1 
                                    x = txp(Nt);
                                    y = typ(Nt);
                                    isc = 0;                                    
                                    for ct = 1:length(tx)
                                        if (abs(x-tx(ct)) < 1e-10 & abs(y-ty(ct)) < 1e-10)
                                            isc = 1;
                                            break;
                                        end;
                                    end;
                                    
                                    if (isc == 0)
                                        tx(end+1) = x;
                                        ty(end+1) = y;
                                    end;
                                end;                               
                                count = length(tx);
                            end;
                            if (poly_bound(l) == 0 & poly_bound(l+1) == 0 & length(tx) > 1 & length(ty) > 1)
                                trans_lth(k-i+1) = trans_lth(k-i+1) + sqrt((tx(2)-tx(1))^2+(ty(2)-ty(1))^2);
                            else       
                                if (on_pts(l) == 1)
                                    for ct = 1:count
                                        if (abs(tx(ct)-x1) < 1e-10 & abs(ty(ct)-y1) < 1e-10)
                                            for ct2 = (ct+1):count
                                                tx(ct2-1) = tx(ct2);
                                                ty(ct2-1) = ty(ct2);
                                            end;
                                            count = count-1;
                                        end;
                                    end;
                                end;
                                if (on_pts(l+1) == 1)
                                    for ct = 1:count
                                        if (abs(tx(ct)-x2) < 1e-10 & abs(ty(ct)-y2) < 1e-10)
                                            for ct2 = (ct+1):count
                                                tx(ct2-1) = tx(ct2);
                                                ty(ct2-1) = ty(ct2);
                                            end;
                                            count = count-1;
                                        end;
                                    end;
                                end;
                                if (count ~= 0)                                    
                                    if (poly_bound(l) == 0)
                                        trans_lth(k-i+1) = trans_lth(k-i+1) + sqrt((tx(1)-x2)^2+(ty(1)-y2)^2);
                                    else
                                        trans_lth(k-i+1) = trans_lth(k-i+1) + sqrt((tx(1)-x1)^2+(ty(1)-y1)^2);
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
            if (trans_lth(2) > trans_lth(1))
                m_new(i+1,j) = 2;
            elseif (trans_lth(1) > trans_lth(2))
                m_new(i,j) = 2;
            else
                m_new(i,j) = 2;
                m_new(i+1,j) = 2;
            end;
        end;
        Nl = (j-1)*(Ny-1) + i;
        itmp_prev = itmp;
        itmp = floor(Nl/Nb*100);
        if (mod(itmp,5)==0 & itmp_prev ~= itmp)
           fprintf(1,'Completed %d per cent of the cells\n',itmp);
        end;
    end;
end;


%moving down the rows

fprintf(1,'Moving down the rows \n');

Nb = Ny*(Nx-1);
itmp = 0;

for i = 1:Ny
    for j = 1:Nx-1
        if (((m_new(i,j) == 1 & m_new(i,j+1) == 3)) || (m_new(i,j) == 3 & m_new(i,j+1) == 1))
            trans_lth = [];
            for k = j:j+1
                lons = lon(k)-0.5*dx;
                lone = lon(k)+0.5*dx;
                lats = lat(i)-0.5*dy;
                late = lat(i)+0.5*dy;
                px1 = [lons lone lone lons lons];
                py1 = [lats lats late late lats];
                for r = 1:4
                    if (px1(r+1)==px1(r))
                        m_grid(r)=inf;
                        c_grid(r)=0;
                    else
                        p = polyfit(px1(r:r+1),py1(r:r+1),1);
                        m_grid(r) = p(1);
                        if (abs(m_grid(r)) < 1e-10)
                            m_grid(r) = 0;
                        end;
                        c_grid(r) = p(2);
                    end;
                end;
                trans_lth(k-j+1) = 0;
                [poly_bound,on_pts] = inpolygon(px,py,px1,py1);
                for l = 1:N-1
                    if (poly_bound(l) == 1 & poly_bound(l+1) == 1)
                        trans_lth(k-j+1) = trans_lth(k-j+1) + sqrt((px(l+1)-px(l))^2+(py(l+1)-py(l))^2);
                    else
                        x1 = px(l);
                        x2 = px(l+1);
                        y1 = py(l);
                        y2 = py(l+1);
                        if (x2==x1)
                            mt = inf;
                            ct = 0;
                        else
                            if (abs(x2-x1) > 90)                                
                                x2 = x2+360;
                                if (abs(x2-x1) > 90)
                                    x2 = x2-720;
                                end;
                            end;
                            p = polyfit([x1 x2],[y1 y2],1);                            
                            mt = p(1);
                            if (abs(mt) < 1e-10)
                                mt = 0;
                            end;
                            ct = p(2);
                        end;
                        
                        count = 0;
                        tx = [];
                        ty = [];
                        d0 = sqrt((x1-x2)^2+(y1-y2)^2);
                        for r = 1:4
                            if (mt == m_grid(r))
                                if (mt == 0)
                                    if (y1>= min(py1(r:r+1)) & y1<= max(py1(r:r+1)))
                                        y = y1;
                                        if (poly_bound(l) == 1 & poly_bound(l+1) == 0)
                                            count = count+1;
                                            tx(count) = x2;
                                            ty(count) = y;
                                        elseif (poly_bound(l) == 0 & poly_bound(l+1) == 1)
                                            count = count+1;
                                            tx(count) = x1;
                                            ty(count) = y;
                                        elseif (poly_bound(l) == 0 & poly_bound(l+1) == 0)
                                            if (px1(r) >= min([x1 x2]) & px1(r) <= max([x1 x2]))
                                                count = count+1;
                                                tx(count) = px1(r);
                                                ty(count) = y;
                                            end;
                                            if (px1(r+1) >= min([x1 x2]) & px1(r+1) <= max([x1 x2]))
                                                count = count+1;
                                                tx(count) = px1(r+1);
                                                ty(count) = y;
                                            end;
                                        end;
                                    end;
                                elseif (isinf(mt))
                                    if (x1>= min(px1(r:r+1)) & x1<= max(px1(r:r+1)))
                                        x = x1;
                                        if (poly_bound(l) == 1 & poly_bound(l+1) == 0)
                                            count = count+1;
                                            ty(count) = y2;
                                            tx(count) = x;
                                        elseif (poly_bound(l) == 0 & poly_bound(l+1) == 1)
                                            count = count+1;
                                            ty(count) = y1;
                                            tx(count) = x;
                                        elseif (poly_bound(l) == 0 & poly_bound(l+1) == 0)
                                            if (py1(r) >= min([y1 y2]) & py1(r) <= max([y1 y2]))
                                                count = count+1;
                                                ty(count) = py1(r);
                                                tx(count) = x;
                                            end;
                                            if (py1(r+1) >= min([y1 y2]) & py1(r+1) <= max([y1 y2]))
                                                count = count+1;
                                                ty(count) = py1(r+1);
                                                tx(count) = x;
                                            end;
                                        end;
                                    end;
                                end; 
                            elseif (mt ~= m_grid(r));
                                if (~isinf(mt) && ~isinf(m_grid(r)))
                                    x = (c_grid(r)-ct)/(mt-m_grid(r));
                                    y = mt*x+ct;
                                elseif (isinf(mt))
                                    x = x1;
                                    y = m_grid(r)*x+c_grid(r);
                                else
                                    x = px1(r);
                                    y = mt*x+ct;
                                end;                                
                                d = sqrt((px1(r)-px1(r+1))^2+(py1(r)-py1(r+1))^2);
                                d1 = sqrt((px1(r)-x)^2+(py1(r)-y)^2);
                                d2 = sqrt((x-px1(r+1))^2+(y-py1(r+1))^2);                                
                                if (abs(1-(d1+d2)/d) < 0.00001 )
                                    d1 = sqrt((x1-x)^2+(y1-y)^2);
                                    d2 = sqrt((x2-x)^2+(y2-y)^2);
                                    if (abs(1-(d1+d2)/d0) < 0.00001 )
                                        count = count+1;
                                        tx(count) = x;
                                        ty(count) = y;
                                    end;
                                end;                                
                            end;                         
                        end;
                        
                        
                        if (count > 0)
                            if (count > 1)                                
                                txp = tx;
                                typ = ty;
                                clear tx ty;                                
                                tx(1) = txp(end);
                                ty(1) = typ(end);
                                txp = txp(1:end-1);
                                typ = typ(1:end-1);
                                Ntt = length(txp);
                                for Nt = Ntt:-1:1 
                                    x = txp(Nt);
                                    y = typ(Nt);
                                    isc = 0;                                    
                                    for ct = 1:length(tx)
                                        if (abs(x-tx(ct)) < 1e-10 & abs(y-ty(ct)) < 1e-10)
                                            isc = 1;
                                            break;
                                        end;
                                    end;
                                    
                                    if (isc == 0)
                                        tx(end+1) = x;
                                        ty(end+1) = y;
                                    end;
                                end;                               
                                count = length(tx);
                            end;
                            if (poly_bound(l) == 0 & poly_bound(l+1) == 0 & length(tx) > 1 & length(ty) > 1)
                                trans_lth(k-j+1) = trans_lth(k-j+1) + sqrt((tx(2)-tx(1))^2+(ty(2)-ty(1))^2);
                            else
                                if (on_pts(l) == 1)
                                    for ct = 1:count
                                        if (abs(tx(ct)-x1) < 1e-10 & abs(ty(ct)-y1) < 1e-10)
                                            for ct2 = (ct+1):count
                                                tx(ct2-1) = tx(ct2);
                                                ty(ct2-1) = ty(ct2);
                                            end;
                                            count = count-1;
                                        end;
                                    end;
                                end;
                                if (on_pts(l+1) == 1)
                                    for ct = 1:count
                                        if (abs(tx(ct)-x2) < 1e-10 & abs(ty(ct)-y2) < 1e-10)
                                            for ct2 = (ct+1):count
                                                tx(ct2-1) = tx(ct2);
                                                ty(ct2-1) = ty(ct2);
                                            end;
                                            count = count-1;
                                        end;
                                    end;
                                end;
                                if (count ~= 0)
                                    if (poly_bound(l) == 0)
                                        trans_lth(k-j+1) = trans_lth(k-j+1) + sqrt((tx(1)-x2)^2+(ty(1)-y2)^2);
                                    else
                                        trans_lth(k-j+1) = trans_lth(k-j+1) + sqrt((tx(1)-x1)^2+(ty(1)-y1)^2);
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
            if (trans_lth(2) > trans_lth(1))
                m_new(i,j+1) = 2;
            elseif (trans_lth(1) > trans_lth(2))
                m_new(i,j) = 2;
            else
                m_new(i,j) = 2;
                m_new(i,j+1) = 2;
            end;
        end;
        Nl = (i-1)*(Nx-1) + j;
        itmp_prev = itmp;
        itmp = floor(Nl/Nb*100);
        if (mod(itmp,5)==0 & itmp_prev ~= itmp)
           fprintf(1,'Completed %d per cent of the cells\n',itmp);
        end;
    end;
end;


for i = 1:Ny
    if (m(i,1) == 1 & in_points(i,1) == 1)
        m_new(i,1) = 2;
    end;
    if (m(i,Nx) == 1 & in_points(i,Nx) == 1)
        m_new(i,Nx) = 2;
    end;
end;

for i = 1:Nx
    if (m(1,i) == 1 & in_points(1,i) == 1)
        m_new(1,i) = 2;
    end;
    if (m(Ny,i) == 1 & in_points(Ny,i) == 1)
        m_new(Ny,i) = 2;
    end;
end;

% check to see if you can find cells from the base grid to interpolate from 
% for the boundary points

[rows,cols] = find(m_new == 2);

Nr = length(rows);

dxb = lonb(2)-lonb(1);
dyb = latb(2)-latb(1);
Nxb = length(lonb);
Nyb = length(latb);

for i = 1:Nr
    y = lat(rows(i));
    x = lon(cols(i));
    ry = (y-latb(1))/dyb;
    jy = 1 + floor(ry);
    ry = ry - (jy-1);
    if (ry < 0)
        ry = 1 - ry;
        jy = jy-1;
    end;
    if (jy == 0 & abs(ry-1) < 0.05) 
        jy = 1;
        ry = 0;
    end;
    if (jy == Nyb & abs(ry) < 0.05)
        jy = jy-1;
        ry = 1;
    end;
    if (jy < 1 || jy >= Nyb)
        m_new(rows(i),cols(i)) = 3;
        continue;
    end;

    rx = (x-lonb(1))/dxb;
    jx = 1 + floor(rx);
    rx = rx - (jx-1);
 
    if (igl ~= 1) 
        if (jx == 0 & abs(rx-1) < 0.05)
            jx = 1;
            rx = 0.;
        end;
        if (jx == Nxb & abs(rx) < 0.05)
            jx = jx-1;
            rx = 1;
        end;
        if (jx < 1 || jx >= Nxb)
            m_new(rows(i),cols(i)) = 3;
            continue;
        end;
    else
        jx = 1 + mod(jx-1,Nxb);
    end;

    jx1 = jx;
    if (igl == 1) 
        jx2 = 1 + mod(jx,Nxb);
    else
        jx2 = jx + 1;
    end;

    jy1 = jy;
    jy2 = jy+1;

    flagok = (abs(mb(jy1,jx1)) == 1 || abs(mb(jy1,jx1)) == 2 || (1-rx)*(1-ry) < 0.05 ) & ...
                 (abs(mb(jy1,jx2)) == 1 || abs(mb(jy1,jx2)) == 2 || rx*(1-ry) < 0.05 ) & ...
                 (abs(mb(jy2,jx1)) == 1 || abs(mb(jy2,jx1)) == 2 || (1-rx)*ry < 0.05 ) & ...
                     (abs(mb(jy2,jx2)) == 1 || abs(mb(jy2,jx2)) == 2 || rx*ry < 0.05 ); 
               
    if (flagok == 0)
        m_new(rows(i),cols(i)) = 3;
        continue;
    end;
end;

loc = find(in_points == 0 & m == 0);
m_new(loc) = 3;

return;

