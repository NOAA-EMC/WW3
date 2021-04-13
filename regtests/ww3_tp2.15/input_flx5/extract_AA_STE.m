function extract_AA_STE
clc
clear
close all

%% ########################################################################
%
%                             extract_AA_STE.m
%
% Matlab function to extract STE outputs of WW3 regtest ww3_tp2.15 at the 
% Acqua Alta (AA) tower location.
% Results simulate stereo observations during experiment on
% March 10th 2014 09:40UTC-10:10UTC, as described in References below.
% 
% DISCLAIMER: Model results are presented here for the purpose of testing
% the formulations of space-time extreme parameters. This regression test
% oversimplifies real-case bathymetry and wind fields, thus comparison with
% measurements are merely qualitative.
%
% -------------------------------------------------------------------------
% References:
% Barbariol et al. (2016), Numerical Modeling of Space-Time Wave Extremes 
%   using WAVEWATCH III, Ocean Dynamics, under review
% Benetazzo et al. (2015), Observation of extreme sea waves in a space-time
%   ensemble. Journal of Physical Oceanography 45(9), 2261-2275 
%
% #########################################################################

%% INPUTs

% Insert WW3 nc file (name and directory)
dir = input(' Enter path of directory with gridded netcdf output: ','s');
fnc = input(' Enter file name with gridded netcdf data: ','s');

%% AA data

% AA event time
AA.t_event = datenum(2014,03,10,09,40,00);

% AA coordinates
AA.lon = 12.5088; % E
AA.lat = 45.3138; % N


% AA reference results (Hs, STE) from 5km run and 15km run (second column)

AA.Hs = [1.4108 1.5155]; % m
AA.STMAXE = [1.8430 1.9672]; % m
AA.STMAXD = [0.1461 0.1585]; % m
AA.HCMAXE = [2.6217 2.8083]; % m
AA.HCMAXD = [0.1850 0.2013]; % m
AA.HMAXE = [2.8966 3.0959]; % m
AA.HMAXD = [0.2044 0.2219]; % m

%% extract results from WW3 nc

cd(dir)

WW3.time = double(ncread(fnc,'time') + datenum(1990,01,01,00,00,00));
evt = find(WW3.time >= AA.t_event-1/24 & WW3.time <= AA.t_event+1/24); % Indices within +-1h of measurements

WW3.lon = double(ncread(fnc,'longitude'));
WW3.lat = double(ncread(fnc,'latitude'));

HS = double(read_interp(fnc,'hs',WW3.lon,WW3.lat,AA.lon,AA.lat,2,2)); 
WW3.Hs = mean(HS(evt));

% STE (Compute mean parameters from event times)
STMAXE = double(read_interp(fnc,'stmaxe',WW3.lon,WW3.lat,AA.lon,AA.lat,2,2)); 
WW3.STMAXE = mean(STMAXE(evt)); % m
STMAXD = double(read_interp(fnc,'stmaxd',WW3.lon,WW3.lat,AA.lon,AA.lat,2,2)); 
WW3.STMAXD = mean(STMAXD(evt)); % m
HMAXE = double(read_interp(fnc,'hmaxe',WW3.lon,WW3.lat,AA.lon,AA.lat,2,2)); 
WW3.HMAXE = mean(HMAXE(evt)); % m
HMAXD = double(read_interp(fnc,'hmaxd',WW3.lon,WW3.lat,AA.lon,AA.lat,2,2)); 
WW3.HMAXD = mean(HMAXD(evt)); % m
HCMAXE = double(read_interp(fnc,'hcmaxe',WW3.lon,WW3.lat,AA.lon,AA.lat,2,2)); 
WW3.HCMAXE = mean(HCMAXE(evt)); % m
HCMAXD = double(read_interp(fnc,'hcmaxd',WW3.lon,WW3.lat,AA.lon,AA.lat,2,2)); 
WW3.HCMAXD = mean(HCMAXD(evt)); % m

disp(' ')
disp('            This Run   | 5km Reference  | 15km Reference')
disp(['Hs            ',num2str(WW3.Hs,'%6.2f'),'     |      ',num2str(AA.Hs(1),'%6.2f'),'      |     ',num2str(AA.Hs(2),'%6.2f')])
disp(['C_max/Hs      ',num2str(WW3.STMAXE./WW3.Hs,'%6.2f'),'     |      ',num2str(AA.STMAXE(1)./AA.Hs(1),'%6.2f'),'      |     ',num2str(AA.STMAXE(2)./AA.Hs(2),'%6.2f')])
disp(['C_std/Hs      ',num2str(WW3.STMAXD./WW3.Hs,'%6.2f'),'     |      ',num2str(AA.STMAXD(1)./AA.Hs(1),'%6.2f'),'      |     ',num2str(AA.STMAXD(2)./AA.Hs(2),'%6.2f')])
disp(['HC_max/Hs     ',num2str(WW3.HCMAXE./WW3.Hs,'%6.2f'),'     |      ',num2str(AA.HCMAXE(1)./AA.Hs(1),'%6.2f'),'      |     ',num2str(AA.HCMAXE(2)./AA.Hs(2),'%6.2f')])
disp(['HC_std/Hs     ',num2str(WW3.HCMAXD./WW3.Hs,'%6.2f'),'     |      ',num2str(AA.HCMAXD(1)./AA.Hs(1),'%6.2f'),'      |     ',num2str(AA.HCMAXD(2)./AA.Hs(2),'%6.2f')])
disp(['H_max/Hs      ',num2str(WW3.HMAXE./WW3.Hs,'%6.2f'),'     |      ',num2str(AA.HMAXE(1)./AA.Hs(1),'%6.2f'),'      |     ',num2str(AA.HMAXE(2)./AA.Hs(2),'%6.2f')])
disp(['C_std/Hs      ',num2str(WW3.HMAXD./WW3.Hs,'%6.2f'),'     |      ',num2str(AA.HMAXD(1)./AA.Hs(1),'%6.2f'),'      |     ',num2str(AA.HMAXD(2)./AA.Hs(2),'%6.2f')])
disp(' ')
disp(' Model resolution, domain extent and wind model skill ')
disp('   affect wave model results.')
disp(' ')
disp(' Reference values obtained in a Cray Compute Cluster using ')
disp('   both high (5km) and low (15km) resolution options.')
disp(' ')
disp(' For actual observations and field experiment description ')
disp('   see references in comment section of this script.')


function int_var = read_interp(fnc,var_name,lonM,latM,lon_obs,lat_obs,dx,dy)

dd = double(sqrt((lon_obs-lonM).^2+(lat_obs-latM).^2));
[yy,xx] = find(dd == min(dd(:)));

var = double(ncread(fnc,var_name,[yy-dy/2,xx-dx/2,1],[dy+1 dx+1 Inf]));

lon_mod = lonM(yy-dy/2:yy+dy/2,xx-dx/2:xx+dx/2);
lat_mod = latM(yy-dy/2:yy+dy/2,xx-dx/2:xx+dx/2);

% Interpolate over space
for i = 1 : size(var,3)
    var_i = squeeze(var(:,:,i));
    I = TriScatteredInterp(lon_mod(:), lat_mod(:), var_i(:),'nearest' );
    int_var(i) = I(lon_obs,lat_obs);
end
