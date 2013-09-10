function [xgrid,ygrid,height,year,month,day,hour,minute]=read_scalar(filename,icheck)
% usage: function [xgrid,ygrid,height,year,month,day,hour]=read_scalar(filename)
% example usage : 
% read_scalar('ww3.01011021.hs');

% Name: read_scalar.m
% Purpose: read .hs files, and similar files
% Origination: E Rogers 
% This header last updated: E Rogers Jan 11 2013

fid = fopen(filename,'r');
header=fgets(fid);
C = textscan(header,'%s');
YYYYMMDD=C{1}{3};
HHMMSS=C{1}{4};

year=str2num(YYYYMMDD(1:4));
month=str2num(YYYYMMDD(5:6));
day=str2num(YYYYMMDD(7:8));
hour=str2num(HHMMSS(1:2));
minute=str2num(HHMMSS(3:4));

xmin=str2num(C{1}{5});
xmax=str2num(C{1}{6});
nx=str2num(C{1}{7});
ymin=str2num(C{1}{8});
ymax=str2num(C{1}{9});
ny=str2num(C{1}{10});
variable=C{1}{11};
factors=str2num(C{1}{12});

if icheck>0
  str=['!head -n 1 ' filename];disp(str);eval(str)
  disp(['year = ' num2str(year)])
  disp(['month = ' num2str(month)])
  disp(['day = ' num2str(day)])
  disp(['hour = ' num2str(hour)])
  disp(['minute = ' num2str(minute)])
  disp(['xmin = ' num2str(xmin)])
  disp(['xmax = ' num2str(xmax)])
  disp(['nx = ' num2str(nx)])
  disp(['ymin = ' num2str(ymin)])
  disp(['ymax = ' num2str(ymax)])
  disp(['ny = ' num2str(ny)])
  disp(['factors = ' num2str(factors)])
end

dx=(xmax-xmin)/(nx-1);
dy=(ymax-ymin)/(ny-1);
xgrid=xmin:dx:xmax;
ygrid=ymin:dy:ymax;
if nx ~= size(xgrid,2);
  error('uh oh.')
end
if ny ~= size(ygrid,2);
  error('uh oh.')
end

disp('scanning data.....')
for j=1:ny
  for i=1:nx
    height(i,j)=fscanf(fid,'%f',1);
  end
end
disp('done scanning data.')
height=height .* factors;

fclose(fid);

disp(['variable read is ' variable])

if icheck>1
  beep
  iq=input('Do the variables look ok (1=yes)?    ');
  if iq ~= 1
    error('does not look ok, stopping')
  end
end
