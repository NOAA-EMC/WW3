function read_outf_hs_generic(hmax,dt,units,runid,time_filename,offset)

% dt, time_filename:   just for filename

icheck=1;

fz=14;
set(0,'defaultaxesfontsize',fz);

%offset=12;
psfile=['Hs.' runid '.ps'];

for itime=1:1000

  Z1=datestr(time_filename,30);
  Z2=[Z1(3:8) Z1(10:11)];
  
  filename=['ww3.' Z2 '.hs'];
  
  if exist(filename) == 0
    disp([filename ' does not exist'])
    break
  end
  if exist(filename) == 2
    disp([filename ' does exist.'])
    
    [xgrd,ygrd,height,year,month,day,hour,minute]=read_scalar(filename,icheck,offset);
    time(itime)=datenum(year,month,day,hour,minute,0);
    icheck=0;

    figure(1),clf,hold off
    imagesc(xgrd,ygrd,height')
    colormap(jet)
    axis xy
    axis equal
    axis([min(xgrd) max(xgrd) min(ygrd) max(ygrd)])
    caxis([0 hmax])
    colorbar_ml5
    xlabel(['x (' units ')'])
    ylabel(['y (' units ')'])
    title(['height (m) ; ' datestr(time(itime),0)])
    disp(['max(max(height))  = ' num2str(max(max(height)))])
    
    add_date_pwd
    
    pause(0.1)
%   disp('pausing');pause
    
    if itime==1
      str=['print -dpsc2 ' psfile];disp(str);eval(str)
    else
      str=['print -dpsc2 -append ' psfile];disp(str);eval(str)
    end
      
    disp('note to self: improve this to avoid potential problems with round-off error in dt')
    time_filename=time_filename+dt;
    height_t(:,:,itime)=height;
    
  end
  
end

save HS.OUTF.mat xgrd ygrd height_t time

nt=length(time);

