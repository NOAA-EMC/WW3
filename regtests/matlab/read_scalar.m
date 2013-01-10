function [xlong,ylat,height,year,month,day,hour,minute]=read_scalar(filename,icheck,offset)
% usage: function [xlong,ylat,height,year,month,day,hour]=read_scalar(filename)
% example usage : 
% read_scalar('ww3.01011021.hs');

if exist('offset')~=1
  offset=0; % offset=12 req'd sometimes: in that case, provide as input argument
end

fid = fopen(filename,'r');
header=fgets(fid);
year=str2num(header(16:19));
month=str2num(header(20:21));
day=str2num(header(22:23));
hour=str2num(header(25:26));
minute=str2num(header(27:28));
some_variables_string=header(31:71+offset);
some_variables=str2num(some_variables_string);
xmin=some_variables(1);
xmax=some_variables(2);
nx=some_variables(3);
ymin=some_variables(4);
ymax=some_variables(5);
ny=some_variables(6);
variable=header(72+offset:74+offset);
factors=header(76+offset:84+offset);

if icheck>0
  disp(['xmin = ' num2str(xmin)])
  disp(['xmax = ' num2str(xmax)])
  disp(['nxs = ' num2str(nx)])
  disp(['ymin = ' num2str(ymin)])
  disp(['ymax = ' num2str(ymax)])
  disp(['ny = ' num2str(ny)])
  disp(['factors = ' factors])
end

factor=str2num(factors);

dx=(xmax-xmin)/(nx-1);
dy=(ymax-ymin)/(ny-1);
xlong=xmin:dx:xmax;
ylat=ymin:dy:ymax;
if nx ~= size(xlong,2);
  error('uh oh.')
end
if ny ~= size(ylat,2);
  error('uh oh.')
end

disp('scanning data.....')
for j=1:ny
  for i=1:nx
    height(i,j)=fscanf(fid,'%f',1);
  end
end
disp('done scanning data.')
height=height .* factor;

fclose(fid);

if variable ~= '.hs'
  disp('warning: this is not a .hs file')
end

if icheck>1
  beep
  iq=input('Do the variables look ok (1=yes)?    ');
  if iq ~= 1
    error('does not look ok, stopping')
  end
end
