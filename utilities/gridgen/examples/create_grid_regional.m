% THIS IS AN EXAMPLE SCRIPT FOR GENERATING A REGIONAL GRID AND CAN BE USED AS  A TEMPLATE FOR
% DESIGNING GRIDS

% 0. Initialization

% 0.a Path to directories 

  bin_dir = '/export/lnx375/wd20ac/matlab/gridgen1.0/bin';             % matlab scripts location
  ref_dir = '/export/lnx375/wd20ac/matlab/gridgen1.0/reference_data';  % reference data location
  out_dir = '/export/lnx375/wd20ac/matlab/gridgen1.0/examples/data';   % output directory (for grid files)

% 0.b Design grid parameters

  fname_poly = 'user_polygons.flag'; % A file with switches for using user defined polygons. An
                                     % example file has been provided with the reference data
                                     % for an existing user defined polygon database. A switch of
                                     % 0 ignores the polygon while 1 accounts for the polygon.

  fname = 'Alaska_reg';              % file name prefix (used to save grid info)
  icoords = 1;                       % longitude range  (0 -> -180 - 180
                                     %                   1 -> 0 - 360 )
  grid_box = [44 140 75 240];        % starting and ending lat,lon for grid domain
  dx = 15.0/60.0;                    % grid resolution in x (degrees)
  dy = 10.0/60.0;                    % grid resolution in y (degrees)
  ref_grid = 'etopo2';               % reference grid source (etopo2 = Etopo2 grid
                                     %                        dbdb2 = DBDB2 v3.0)
  boundary = 'full';                 % Option to determine which GSHHS .mat file to load
                                     %         full = full resolution
                                     %         high = 0.2 km
                                     %         inter = 1 lm
                                     %         low   = 5 km
                                     %         coarse = 25 km 

% 0.c Setting the paths for subroutines

  addpath(bin_dir,'-END');

% 0.d Reading Input data

  read_boundary = 1;              % flag to determine if input boundary information needs to be read
                                  % boundary data files can be significantly large and needs to be
                                  % read only the first time. So when making multiple grids the flag
                                  % can be set to 0 for subsequent grids. (Note : If the workspace is
                                  % cleared the boundary data will have to be read again)

  opt_poly = 1;                   % flag for reading the optional user defined polygons. Set to 0 if 
                                  % you do not wish to use this option

  if (read_boundary == 1)

      fprintf(1,'.........Reading Boundaries..................\n');

      load([ref_dir,'/coastal_bound_',boundary,'.mat']); % loading the appropriate boundary file
                                                         % datastructure array is stored in bound

      N = length(bound);

      if (opt_poly == 1)
          [bound_user,Nu] = optional_bound(ref_dir,...
                             fname_poly,icoords);       % reading optional user defined polygons
      end;                                              % flags for setting up which polygons to 
                                                        % use is defined in a file in the same
                                                        % location where the code is run. See
                                                        % documentation of 'optional_bound' for 
                                                        % more details

      if (Nu == 0)
          opt_poly = 0;
      end;

  end;

% 1. Generate the grid

  fprintf(1,'.........Creating Bathymetry..................\n');

  [lon,lat,depth] = generate_grid(ref_dir,ref_grid,grid_box,...
                                  dx,dy,icoords,0.1,0,999); % this grid has been generated using a 
                                                            % limit of 0.1 (cell will be marked wet
                                                            % if 10% of area has wet reference cells)
                                                            % using depth=0 to distinguish wet and dry
                                                            % cells and setting dry cells to 999

% 2. Computing boundaries within the domain

% 2.a Set the domain big enough to include the cells along the edges of the grid

  lon_start = lon(1)-dx;
  lon_end = lon(end)+dx;
  lat_start = lat(1)-dy;
  lat_end = lat(end)+dy;

  coord_start = -180+icoords*180;
  coord_end = coord_start + 360;

  if (lon_start < coord_start)
      lon_start = lon_start + 360;
  end;
  if (lon_end > coord_end)
      lon_end = lon_end - 360;
  end;

% 2.b Extract the boundaries from the GSHHS and the optional databases
%     the subset of polygons within the grid domain are stored in b and b_opt
%     for GSHHS and user defined polygons respectively

  fprintf(1,'.........Computing Boundaries..................\n');

  if (lon_start < lon_end)                                  % Domain does not wrap around

      coord = [lat_start lon_start lat_end lon_end];        % Define the Domain and  extract polygons
      [b,N1] = compute_boundary(coord,bound,icoords);               
      if (opt_poly == 1)
          [b_opt,N2] = compute_boundary(coord,bound_user,icoords);            
      end;
  else                                                      % Domain wraps around the end

      coord = [lat_start lon_start lat_end coord_end];      % Extract from domain start to end of 
      [ba,N1a] = compute_boundary(coord,bound,icoords);     % longitude range
      N1 = 0;
      if (N1a>0)
          b = ba;
          N1 = N1a;
      end;
      if (opt_poly == 1)
          [ba_opt,N2a] = compute_boundary(coord,bound_user,icoords);
          N2 = 0;
          if (N2a > 0)
              b_opt = ba_opt;
              N2 = N2a;
          end;
      end;
      coord = [lat_start coord_start lat_end lon_end];      % Extract from start of longitude range
      [bb,N1b] = compute_boundary(coord,bound,icoords);     % to end of grid domain
      if (N1b > 0)
          if (N1 == 0)
              b = bb;
              N1 = N1b;
          else
              b = [ba bb];
              N1 = N1+N1b;
          end;
      end;
      if (opt_poly == 1)
          [bb_opt,N2b] = compute_boundary(coord,bound_user,icoords);
          if (N2b > 0)
              if (N2 == 0)
                  b_opt = bb_opt;
                  N2 = N2b;
              else
                  b_opt = [ba_opt bb_opt];
                  N2 = N2+N2b;
              end;
          end;
      end;

      clear ba bb ba_opt bb_opt;

  end;

% 3. Set up Land - Sea Mask

% 3.a Set up initial land sea mask. The cells can either all be set to wet or to make the code
%     more efficient the cells marked as dry in 'generate_grid' can be marked as dry cells

  m = ones(size(depth)); 
  loc = find(depth == 999); 
  m(loc) = 0;

% 3.b Split the larger GSHHS polygons for efficient computation of the land sea mask. This is an 
%     optional step but recomended as it significantly speeds up the computational time. Rule of
%     thumb is to set the limit for splitting the polygons at least 4-5 times dx,dy

  fprintf(1,'.........Splitting Boundaries..................\n');

  b_split = split_boundary(b,2);

% 3.c Get a better estimate of the land sea mask using the polygon data sets.
%     (NOTE : This part will have to be commented out if cells above the MSL are being
%      marked as wet, like in inundation studies)

  fprintf(1,'.........Cleaning Mask..................\n');

  m2 = clean_mask(lon,lat,icoords,m,b_split,0.5);          % GSHHS Polygons. If 'split_boundary'
                                                           % routine is not used then replace
                                                           % b_split with b
  if (opt_poly == 1 & N2 ~= 0)
      m3 = clean_mask(lon,lat,icoords,m2,b_opt,0.5);       % Masking out regions defined by
  else                                                     % optional polygons
      m3 = m2;
  end;

% 3.d Remove lakes and other minor water bodies

  fprintf(1,'.........Separating Water Bodies..................\n');

  [m4,mask_map] = remove_lake(m3,-1,0);                    % Routine set up to mask out all but the
                                                           % largest water body. See documentation
                                                           % for other options

% 4. Generate sub - grid obstruction sets in x and y direction, based on the final land/sea 
%    mask and the coastal boundaries

  fprintf(1,'.........Creating Obstructions..................\n');

  [sx1,sy1] = create_obstr(lon,lat,b,m4,icoords,1,1);      % Routine set up to generate obstructions
                                                           % using neighboring cell information on
                                                           % both sides

% 5. Output to ascii files for WAVEWATCH III

  depth_scale = 1000;
  obstr_scale = 100;

  d = round((depth)*depth_scale);
  write_ww3file([out_dir,'/',fname,'.depth_ascii'],d);                 % Writing depth info out to file

  write_ww3file([out_dir,'/',fname,'.maskorig_ascii'],m4);             % Writing mask info out to file

  d1 = round((sx1)*obstr_scale);
  d2 = round((sy1)*obstr_scale);
  write_ww3obstr([out_dir,'/',fname,'.obstr_lev1'],d1,d2);             % Writing obstruction data to file

  write_ww3meta([out_dir,'/',fname],lon,lat,1/depth_scale,...
                                            1/obstr_scale);            % Writing meta data out to file

% 6. Vizualization (this part can be commented out if resources are limited)

  figure(1);
  clf;
  loc = find(m4 == 0);
  d2 = depth;
  d2(loc) = NaN;

  pcolor(lon,lat,d2);
  shading interp;
  colorbar;
  title(['Bathymetry for ',fname],'fontsize',14);
  set(gca,'fontsize',14);
  clear d2;

  figure(2);
  clf;
  d2 = mask_map;
  loc2 = find(mask_map == -1);
  d2(loc2) = NaN;

  pcolor(lon,lat,d2);
  shading flat;
  colorbar;
  title(['Different water bodies for ',fname],'fontsize',14);
  set(gca,'fontsize',14);
  clear d2;

  figure(3);
  clf;

  pcolor(lon,lat,m4);
  shading flat;
  colorbar;
  title(['Final Land-Sea Mask ',fname],'fontsize',14);
  set(gca,'fontsize',14);

  figure(4);
  clf;
  d2 = sx1;
  d2(loc) = NaN;

  pcolor(lon,lat,d2);
  shading flat;
  colorbar;
  title(['Sx obstruction for ',fname],'fontsize',14);
  set(gca,'fontsize',14);
  clear d2;

  figure(5);
  clf;
  d2 = sy1;
  d2(loc) = NaN;

  pcolor(lon,lat,d2);
  shading flat;
  colorbar;
  title(['Sy obstruction for ',fname],'fontsize',14);
  set(gca,'fontsize',14);
  clear d2;

% END OF SCRIPT
