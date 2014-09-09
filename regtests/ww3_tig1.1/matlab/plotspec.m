[lat,lon,freq,dir,df,time,spectra,depth,curr,currdir,unit1]=readWWNC_SPEC('ww3.196806_spec.nc','efth');
Efth=double(spectra(:,:,1,1))';
figure(1)
pcolor(freq,circshift(dir,9),log10(Efth'));
colorbar;
caxis([-4 0])