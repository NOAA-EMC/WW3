i% example_plotf_from_netcdf_rect
%
% example of using read_WWNCf_var and plotting some results: 
% this reads frequency-dependent parameters. In the example here 
% we read the energy spectrum E(f). 
%
% Here is how you can obtain the sample data: 
% Warning this is a VERY BIG file (1.6 Gb). 
% Hopefully it will be soon accessible via OpenDAP. 
%
%  p1='http://tinyurl.com/iowagaftp/HINDCAST'
%  p2='2011_ECMWF/ef'
%  wget ${p1}/GLOBAL/${p2}/ww3.201102_ef.nc -O ww3.201102_ef.nc


% 
clear all;
close all;

% Example for seismic noise sources 
%filename='ww3.200803_p2l.nc';     
%date1=datenum(2008,03,20,12,0,0);
%varname='Fp3D';

% Example for wave spectra 
filename='ww3.201102_ef.nc';     
date1=datenum(2011,02,15,18,0,0);
varname='ef';

[lat,lon,freq,time,mat1,var1,unit1,MAPSTA]=read_WWNCf_var(filename,varname,date1);

rundate =  date1;
%
% choice of frequency 
%
%jf=10;
jj=find(freq <= 0.05);
df=freq.*(0.5*(1.1-1/1.1)); % Only good if XFR =1.1 in WW3 
nx=length(lon);
ny=length(lat);
matplot=zeros(nx,ny);
for i=1:length(jj)
    efmap=double(squeeze(mat1(:,:,jj(1))));
    %min(min(efmap))
    %max(max(efmap))
    matplot=matplot+efmap.*df(jj(i));
    
end
matplot=4.*sqrt(double(matplot));

figure(2)   
clf
pcolor(lon,lat,matplot')
shading flat;     
colormap(jet);colorbar;
latmin=min(lat);
latmax=max(lat);
coslat=cos(0.5*(latmax+latmin)*pi/180);
set(gca,'DataAspectRatio',[1 coslat 1]);  %#   data aspect ratio

title(['Hs for T > 20 s (m)  on ' datestr(rundate,31)  ]);
%title(['F_{p3D}(2f,k~0)/(\rho_w^2 g^2) (m^4/Hz): ' datestr(rundate,31) ', f=' num2str(freq(jf)) 'Hz' ]);
xlabel('Longitude (deg)')
ylabel('Latitude (deg)')

%print(gcf,'-dpng',[filename '_it' num2str(it,'%3.3d') '.png'])


