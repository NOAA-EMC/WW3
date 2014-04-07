% example_plot_unst
%
% example of using read_WWNC_var and plotting some results
% This is for model output on a triangular mesh. 

% Here is how you can obtain the sample data: 
%	p1='http://tinyurl.com/iowagaftp/HINDCAST'
%	p2='2011_ECMWF/hs'
%   wget ${p1}/GLOBAL/${p2}/ww3.201102_hs.nc -O ww3.201102_hs-global.nc
%   wget ${p1}/ATNE/${p2}/ww3.201102_hs.nc -O ww3.201102_hs-atne.nc
%   wget ${p1}/NORGASUG/${p2}/ww3.201102_hs.nc -O ww3.201102_hs-ngug.nc


% 
it=0
%filename='ANTILLES_FORECAST.nc';it=10;         % This example uses a file with
                                                % many variables
filename='ww3.201102_hs-ngug.nc';               % This example uses a file with
                                                % with only hs in it
varname='hs';
[tri,lat,lon,time,mat1,var1,unit1]=read_WWNC_UG_var(filename,varname);

if it==0
% Looks for a specific time: here 2011/2/15 at 18:00:00 UTC 
tt=find(time >= datenum(2011,2,15,21,0,0));
it=tt(1);
end

rundate =  time(it);
figure(2)   
%clf
trisurf(tri',lon,lat,mat1(:,it))
view(0,90);shading interp;     
colormap(jet);colorbar;
caxis([0 10]);
latmin=min(lat);
latmax=max(lat);
coslat=cos(0.5*(latmax+latmin)*pi/180);
set(gca,'DataAspectRatio',[1 coslat 1]);  %#   data aspect ratio

title(['H_{m0} (m): ' datestr(rundate,31)]);
xlabel('Longitude (deg)')
ylabel('Latitude (deg)')

%print(gcf,'-dpng',[filename '_it' num2str(it,'%3.3d') '.png'])


