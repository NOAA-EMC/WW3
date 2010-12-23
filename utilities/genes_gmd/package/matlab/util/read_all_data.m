function out = read_all_data(dir,fname)
%
% Author      : H.L. Tolman
% Last update : 27-Nov-2008
%
%     27-Nov-2008 : Origination.                        ( version 1.00 )
%     04-Dec-2008 : Adding depth to all_data.ww3        ( version 1.01 )
%     10-Dec-2008 : Adding relative depth computation.  ( version 1.02 )
%
%  1. Purpose :
%
%     Read data from file all_data.ww3 from the genetic optimization
%     package for the Generalized Multiple DIA (GMD).
%
%  2. Method :
%
%     Formatted data read.
%
%  3. Input and output :
%
%       dir   string I  Directory of input file.
%       fname string I  File name.
%       out   struc  O  Structure with output data.
%
%  4. Subroutines used :
%
%     ----------------------------------------------------------------
%      wavnu2.m       Solve the dispersion relation.
%     ----------------------------------------------------------------
%
%  5. Error messages :
%
%  6. Remarks :
%
%  7. Structure :
%
%  8. Source code :
%
% -------------------------------------------------------------------- %
%  0. Initializations
%
  if exist('dir','var')
      fullname = dir ;
  else
      fullname = '.' ;
  end
%
  if exist('fname','var')
      fullname = [ fullname '/' fname ] ;
  else
      fullname = [ fullname '/all_data.ww3' ] ;
  end
%
  fid = fopen (fullname,'r') ;
  clear fullname ;
%
% -------------------------------------------------------------------- %
%  1. Read mean parameters
%
  test = fscanf(fid, '%d%d%d', 3) ;
  np = test(1) ; nk = test(2) ; nth = test(3) ;
  out.counts = test ;
  clear test 
%
  data = fscanf (fid,'%d%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e', ...
                 [ 21 np ] ) ;
%
  out.time = data( 2,:)' ;
  out.x    = data( 3,:)' ;
  out.y    = data( 4,:)' ;
  out.dpt  = data( 5,:)' ;
  out.u10  = data( 6,:)' ;
  out.ud   = data( 7,:)' ;
  out.hs0  = data( 8,:)' ;
  out.hs1  = data( 9,:)' ;
  out.hs2  = data(10,:)' ;
  out.fp0  = data(11,:)' ;
  out.fp1  = data(12,:)' ;
  out.fp2  = data(13,:)' ;
  out.tm0  = data(14,:)' ;
  out.tm1  = data(15,:)' ;
  out.tm2  = data(16,:)' ;
  out.si0  = data(17,:)' ;
  out.si1  = data(18,:)' ;
  out.si2  = data(19,:)' ;
  out.alph = data(20,:)' ;
  out.fr0  = data(21,:)' ;
%
  for i=1:np
      sigma = out.fp0(i,1) .* 2 .* pi ;
      [k,cg,icon,it] = wavenu2(sigma,out.dpt(i,1)) ;
      out.kd0(i,1) = k .* out.dpt(i,1) ;
      if ( out.fp1(i,1) ~= NaN )
          sigma = out.fp1(i,1) .* 2 .* pi ;
          [k,cg,icon,it] = wavenu2(sigma,out.dpt(i,1)) ;
          out.kd1(i,1) = k .* out.dpt(i,1) ;
      else
          out.kd1(i,1) = k .* out.dpt(i,1) ;
      end
      if ( out.fp2(i,1) ~= NaN )
          sigma = out.fp2(i,1) .* 2 .* pi ;
          [k,cg,icon,it] = wavenu2(sigma,out.dpt(i,1)) ;
          out.kd2(i,1) = k .* out.dpt(i,1) ;
      else
          out.kd2(i,1) = k .* out.dpt(i,1) ;
      end
  end
  clear i sigma k cp icon it
%
  clear data
%
% -------------------------------------------------------------------- %
%  2. Read 1-D spectral data
%
  out.freq = fscanf (fid,'%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
  out.dir  = fscanf (fid,'%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e', nth ) ;
%
  for i=1:np
      out.time1(:,i) = fscanf (fid,'%d%d', 2 ) ;
      out.spec1(:,i) = fscanf (fid,'%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
      out.sspc1(:,i) = fscanf (fid,'%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
      out.spth1(:,i) = fscanf (fid, ...
                   '%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
      out.spsi1(:,i) = fscanf (fid, ...
                   '%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
      out.snl1 (:,i) = fscanf (fid,'%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
      out.fnl1 (:,i) = fscanf (fid,'%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
      out.wn1  (:,i) = fscanf (fid,'%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
  end
%
% -------------------------------------------------------------------- %
%  3. Read 2-D spectral data
%
  for i=1:np
      out.time2(:,i) = fscanf (fid,'%d%d', 2 ) ;
      for j=1:nth
          out.spec2(:,j,i) = fscanf (fid,...
                               '%e%e%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
      end
      for j=1:nth
          out.snl2(:,j,i) = fscanf (fid,...
                                 '%e%e%e%e%e%e%e%e%e%e%e%e', nk ) ;
      end
  end
%
  clear i j
%
% -------------------------------------------------------------------- %
%  x. Clean up
%
  check = fclose (fid) ;
  clear np nk nth check fid
%
% - end of read_all_data --------------------------------------------- %
