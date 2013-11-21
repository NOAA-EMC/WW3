
clear

% Name: write_x_and_y_example.m
% Purpose: Make example curvilinear grid
% Origination: E Rogers March 11 2008
% Updated: E Rogers Jan 8 2013
% Notes: 
% ..1) This grid is used with /regtests/ww3_tp2.1/input/ww3_grid_c.inp
% ..2) This test case (tp2.1c) is an idealized deepwater case: 
% .......depths are specified in ww3_grid_c.inp
% ..3) General remarks about irregular grids:
% .......irregular grids are logically rectangular, specified as 
% .......x(i,j) and y(i,j). (This script creates these two arrays.)
% .......x varies in i or j or both.
% .......y varies in i or j or both.
% .......Grid lines can be straight. For example, you might have a grid
% ...........where the spacing becomes smaller as you get closer to shore.
% ...........In this case the grid is irregular but not curvilinear. 
% .......Grid lines can be curved. That would be an irregular and curvilinear
% ...........grid. 
% .......It is possible to specify a rotated regular grid in WW3 by treating
% ...........it as an irregular grid. (SWAN users may be familiar with the
% ..........."CGRID...alpc" variable which is another way to specify a 
% ...........rotated regular grid)

% theta-dimension is i
dtheta=0.4;
theta=180:(-1*dtheta):90;
theta=theta .* (pi/180);
ni=length(theta);

% radial dimension is j
radius(1)=600000;
nj=331;

% offset is arbitrary
xoffset=7e+6;
yoffset=2e+6;

disp('calculating xgrd ygrd')

for jj=1:nj
  if jj > 1
    ds=dtheta*(pi/180)*radius(jj-1); % arc-length of cells
    dr=ds;  % make each cell roughly square
    radius(jj)=radius(jj-1)+dr;
  end
  for ii=1:ni
    xgrd(ii,jj)=radius(jj)*cos(theta(ii));
    ygrd(ii,jj)=radius(jj)*sin(theta(ii));
  end
end 

xgrd=xgrd+xoffset;
ygrd=ygrd+yoffset;

disp('finished calculating xgrd ygrd')

figure(1),clf,hold off

subplot(2,2,1)
plot(xgrd,ygrd,'k.')
axis equal
axis([min(min(xgrd)) max(max(xgrd)) min(min(ygrd)) max(max(ygrd))])
grid on

subplot(2,2,2)
plot(xgrd,ygrd,'k.')
axis equal
axis([6220000 6460000 1980000 2220000])
grid on

subplot(2,2,3)
plot(xgrd,ygrd,'k.')
axis equal
axis([974000 1210000 1930000 2170000])
grid on

disp('saving grid')

xgrd=xgrd';
ygrd=ygrd';

save xgrd.2.1c.dat xgrd -ascii
save ygrd.2.1c.dat ygrd -ascii

disp('finished saving grid')
