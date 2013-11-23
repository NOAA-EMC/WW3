
clear

%          !  ICECOEF1 = H_ICE
%          !  ICECOEF2 = VISC
ni=101;
nj=51;

parameter_number=3;

if parameter_number==1
  variable='mud_dens';
  nonzero_ice_value=[1310 1310];
  CAX=1.5;
  FORMAT='%6.0f';
elseif parameter_number==2
  variable='mud_thickn';
  nonzero_ice_value=[0.4 0.4];
  CAX=1.5;
  FORMAT='%6.2f';
elseif parameter_number==3
  variable='mud_visc';
  nonzero_ice_value=[0.0076  0.0076];
  CAX=1.5;
  FORMAT='%8.4f';
end

itime=0;HH=-1;
nt=length(nonzero_ice_value);

disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
disp('making fields and plotting')
disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')

for itime=1:nt
  HH=HH+1;
  time_field(itime)=datenum(1968,06,06,HH,00,00);

  ice_parameter=zeros(ni,nj);

% for jy=1:nj % uniform
%   for ix=1:ni % uniform 
  for jy=round(nj/2):nj % high y (non-uniform)
    for ix=1:round(ni/2) % low x (non-uniform)
      ice_parameter(ix,jy)=nonzero_ice_value(itime);
    end
  end

  figure(1),clf,hold off
  imagesc(ice_parameter')
  axis xy
  caxis([-CAX CAX])
  colorbar
  disp(['mean(mean(ice_parameter)) = ' num2str(mean(mean(ice_parameter)))])
  
  title(datestr(time_field(itime),0))
% disp([datestr(time_field(itime),0) ' ; pausing']);pause
  disp([datestr(time_field(itime),0)]);pause(0.1)
  
  ice_parameter_time{itime}=ice_parameter;

end

disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
disp('finished making fields and plotting')
disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')

disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
disp('writing to file')
disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')

filename=[variable '.txt'];  
disp(['opening ' filename])
fid = fopen(filename,'w');

for itime=1:nt
  
  DS=datestr(time_field(itime),30);
  disp(DS)
  I1=str2num(DS(1:8));
  I2=str2num(DS(10:15));
  dateheader=[sprintf('%10d',I1) ' ' sprintf('%10d',I2)];
  fprintf(fid,'%s\n',dateheader);
  
  ice_parameter=ice_parameter_time{itime};

  for jy=1:nj
    for ix=1:(ni-1)
      fprintf(fid,FORMAT,[ice_parameter(ix,jy)]);
    end
    fprintf(fid,[FORMAT '\n'],[ice_parameter(ix,jy)]);
  end
  
end

disp(['closing ' filename])
fclose(fid);


disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
disp('finished writing to file')
disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
