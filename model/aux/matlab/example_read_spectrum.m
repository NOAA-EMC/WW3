% 
% Example of reading and plotting directional spectra from NetCDF files. 
% Sample files can be found at : 
% ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/wavewatch3/HINDCAST/GLOBAL/2008_ECMWF/SPECTRA_NC/
%  Note: you may have to wait for the listing to appear (over 9000 files ...) 
%
clear all
close all
addpath('~/TOOLS/MATLAB')
filename='ww3.62069_2008_spec.nc'
varname='efth';
[lat,lon,freq,dir,df,dates,Efth,depth,curr,currdir,unit1]=readWWNC_SPEC(filename,varname);
nth=size(Efth,1);
nk=size(Efth,2);
dth=360/nth;
%
% Reads a color table
%
col=load('doppler_modified_land_2.ct');
col(:,254)=col(:,253);
col(:,255)=col(:,253);
col(:,256)=col(:,253);
%
% Converting directions from nautical to trigonometric convention  x=90-w
% and from 'To' to 'From' convention: y = x + 180
% and shifts by half bin to have the polygons centered: z = z -dth/2;
%
dirmemo=dir;
dir=(90.-dir)+180-dth/2;
dtor=pi/180;
%
% Selects one of the time steps in the file     
%
%I=find (dates >= datenum(2008,3,12) );
%i=I(1)
i=1000;
Efth1(:,:)=double(Efth(:,:,1,i)');
% Displays the date onf that step (year month day hour minute second) 
datevec(dates(i))
%

%-------------------------------------------------------------
% 1. Polar plot display
%-------------------------------------------------------------
% The first direction is repeated at the end in order to make 
% a nice plot 
dirr=[dir' dir(1)]';
dir2=repmat(dirr',nk,1);
freq2=repmat(freq,1,nth+1);
Efth2(:,1:nth)=Efth1;
Efth2(:,nth+1)=Efth1(:,1);
x2=cos(dir2.*dtor).*freq2;  %  - sign converts direction "to" to direction "from"
y2=sin(dir2.*dtor).*freq2;  
%
figure(1);
colormap(col'./255);
pcolor(x2,y2,Efth2);axis equal;shading flat;
hold on;
for i=1:7 
    plot(0.1*i*cos(linspace(0,2*pi,25)),0.1*i*sin(linspace(0,2*pi,25)))
end

% Integrates over frequencies and plots the freq. spectrum 
figure(2);
clf
dth=360./real(nth);
dthr=2.*pi./real(nth);

Ef=sum(Efth1,2)*dthr;
% Computes the directional integral that comes into the seismic noise
% source
DirInt = 2*sum(Efth1(:,1:nth/2).*Efth1(:,nth/2+1:nth),2)*dthr./(Ef.^2);


Hs=4.*sum(Ef.*df)
plot(freq,Ef,'k-+','LineWidth',2);
set(gca,'FontSize',15);
xlabel('Frequency (Hz)');
ylabel('E(f) (m^2/Hz)');

dir=90+dirmemo;
% Computes mean directions and directional spread
dir2=repmat(dir',nk,1);
a1=sum(Efth1.*cos(dir2.*dtor),2).*dthr./Ef;
b1=sum(Efth1.*sin(dir2.*dtor),2).*dthr./Ef;

m1=sqrt(a1.^2+b1.^2);
sth1m=sqrt(2*(1-m1))*180/pi;

figure(5);
clf
th1m=atan2(b1,a1)*180/pi;
plot(freq,th1m,'k-','LineWidth',2);
set(gca,'FontSize',15);
xlabel('Frequency (Hz)');
ylabel('mean direction th1m (deg)');


figure(6);
clf
plot(freq,sth1m,'k-','LineWidth',2);
set(gca,'FontSize',15);
xlabel('Frequency (Hz)');
ylabel('directional spread (deg)');




%-------------------------------------------------------------
% 2. Simulates a surface assuming linear superposition 
%-------------------------------------------------------------
% a . gets phases from random draw
phases=rand(nk,nth)*2*pi;

% b.   Computes amplitudes
% b.1  frequency increment 
df=freq.*(1.1-1/1.1)./2;
% b.2  direction increment in radians 
as=zeros(nk,nth);
for i=1:nk
    as(i,:)=sqrt(2.*Efth1(i,:)*df(i)*dthr);
    %as(:,2:end)=0;
end

% c. definition du domaine de realisation de la surface (carre de 1 km par 1km)
nx=201;
x=linspace(0,1000,nx);
y2=repmat(x',1,nx);
x2=repmat(x,nx,1);
y=x;
g=9.81;
%%%  Computes the wave numbers
sig=2*pi.*freq;
ks=sig.^2./g;% ...  ??    use the dispersion relation. Here for deep water.
             %  How would it be for a different depth?

% d. Prepares for a movie with nt timesteps 
nt=20;
t=linspace(0,(nt-1)*0.5,nt);
% Loads a "nice" color table
col=load('doppler_modified_land_2.ct');
col(:,254)=col(:,253);
col(:,255)=col(:,253);
col(:,256)=col(:,253);
colormap(col'./255);

nfig=3;
figure(nfig);
clf
set(nfig,'Position',[1 1 3*nx+40 3*nx+40])
colormap(col'./255);
M=struct([]);
mov = avifile('example_surface.avi');
for ii=1:nt
%
% initialise la surface a zero
%
dirt=(90.-dirmemo);
zeta=zeros(nx,nx);
E=0;
for i=1:nk
    for j=1:nth
        zeta(:,:)= zeta(:,:)+as(i,j)*cos( ks(i)*cos(dirt(j).*dtor).*x2 ...
            +ks(i)*sin(dirt(j).*dtor).*y2 + phases(i,j)-sig(i)*t(ii) );
    E=E+as(i,j).^2.;
    end
end
pcolor(x2,y2,zeta);
if ii == 1
hs=4*sqrt(E);
caxis([-Hs,Hs]);
end
axis equal;shading interp;colorbar;
xlabel('x (m)');
ylabel('y (m)');
N(ii)=getframe;
F=getframe;
mov = addframe(mov,F);
end
% Checks that the Hs is indeed 4 x stdev(elevation)  
hs=4*sqrt(E)

mov = close(mov);
movie(N,1,1)
