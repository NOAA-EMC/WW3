function read_outf_hs_generic(time_filename,hmax,dt,axisin,ext,variablename,units,plot_bathy)

% Purpose: scan depth (if ploty_bath==1) and Hs files 
%     and make simple x,y plots and save fields
%     to a .mat file. The x,y plots are just for simple diagnostics.  It is
%     assumed that the .mat file will subsequently be used to make better 
%     plots (especially in case of curvilinear grid, since plot will be 
%     distorted here).

% Name: read_outf_hs_generic.m
% Origination: E Rogers 
% This header last updated: E Rogers Jan 11 2013

icheck=1;

fz=12;
set(0,'defaultaxesfontsize',fz);

BUFFER=1;

wermap=jet;
wermap(1,:)=[0 0.6 0];

v2=[-10 0 10];

for itime=1:1000

  Z1=datestr(time_filename,30);
  Z2=[Z1(3:8) Z1(10:11)];
  
  if itime==1 & plot_bathy==1
    
    filename=['./ww3.' Z2 '.dpt'];
    
    if exist(filename) == 0
      disp([filename ' does not exist'])
      break
    end
    if exist(filename) == 2
      disp([filename ' does exist.'])
      
      [xgrd,ygrd,depth,year,month,day,hour,minute]=read_scalar(filename,icheck);
      time(itime)=datenum(year,month,day,hour,minute,0);
      icheck=0;
      
      figure(1),clf,hold off
      imagesc(xgrd,ygrd,depth')
      colormap(jet)
      axis xy
      axis equal
      if isempty(axisin)==1
	axis1=[min(min(xgrd))-BUFFER max(max(xgrd))+BUFFER min(min(ygrd))-BUFFER max(max(ygrd))+BUFFER];
      else
	axis1=axisin;
      end
      axis(axis1)
      xlabel('x (degrees)','fontsize',(fz+2))
      ylabel('y (degrees)','fontsize',(fz+2))
      set(gca,'fontsize',fz)
      caxis([-300 300])
      colorbar
      title(['depth (m) ; ' datestr(time(itime),0)])
      pause(0.1)
    end
    
  end
  
  filename=['./ww3.' Z2 '.' ext];
  
  if exist(filename) == 0
    disp([filename ' does not exist'])
    break
  end
  if exist(filename) == 2
    disp([filename ' does exist; itime = ' num2str(itime)])
    
    str=['[xgrd,ygrd,' variablename ',year,month,day,hour,minute]=read_scalar(filename,icheck);'];eval(str)
    filenames{itime}=filename; % for error checking
    time_filenames(itime)=time_filename; % for error checking
    time(itime)=datenum(year,month,day,hour,minute,0);
    icheck=0;
    
    if plot_bathy==1
      [i]=find(depth<0);
      str=['    ' variablename '(i)=-99;'];eval(str)
    end
    figure(2),clf,hold off
    str=['    imagesc(xgrd,ygrd,' variablename ''')'];eval(str)
    colormap(wermap)
    axis xy
    axis equal
    if isempty(axisin)==1
      axis1=[min(min(xgrd))-BUFFER max(max(xgrd))+BUFFER min(min(ygrd))-BUFFER max(max(ygrd))+BUFFER];
    else
      axis1=axisin;
    end
    axis(axis1) 
    xlabel('x (degrees)','fontsize',(fz+2))
    ylabel('y (degrees)','fontsize',(fz+2))
    set(gca,'fontsize',fz)
    caxis([-hmax/63 hmax])
    colorbar
    if plot_bathy==1
      hold on
      contour(xgrd,ygrd,depth',v2,'w-')
    end
    title([variablename ' (' units ') ; ' datestr(time(itime),0)])
    pause(0.1)
           
    time_filename=time_filename+dt;
%   round to nearest minute to avoid limits of precision in dt that causes creepage over many time steps
    time_filename=round(time_filename*1440)/1440;
    
    str=['    ' variablename '_t{itime}=' variablename ';'];eval(str)
    
  end
    
end

%print -dpsc2 pcolor.final.ps
%print -dpng pcolor.final.png

if exist('xgrd')==0
  error('did you put in the correct date?')
end

disp('saving final .mat file....')
str=['save ' variablename '.OUTF.mat xgrd ygrd ' variablename '_t time'];eval(str)
disp('....done')
