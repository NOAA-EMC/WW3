function out = read_pop_clean(dir1,dir2,dir3)
%
% Author      : H.L. Tolman
% Last update : 16-Nov-2009
%
%     16-Nov-2009 : Origination.                        ( version 1.00 )
%     08-Jan-2010 : Add check on existence of file.     ( version 1.01 )
%
%  1. Purpose :
%
%     Read data from file pop_clean from the genetic optimization
%     package for the Generalized Multiple DIA (GMD).
%
%  2. Method :
%
%     Formatted data read.
%
%  3. Input and output :
%
%       dirN  string I  Directory of input file.
%       out   struc  O  Structure with output data.
%
%  4. Subroutines used :
%
%     None.
%
%  5. Error messages :
%
%  6. Remarks :
%
%     - Using scripts pop_extract.sh and pop_clean.sh.
%
%  7. Structure :
%
%  8. Source code :
%
% -------------------------------------------------------------------- %
%  0. Initializations
%
  !grep genes_main ~/.genes.env | sed 's/=/ /g' | awk '{ print $3}' > tempfile
  base_dir = textread ( 'tempfile', '%c' )' ;
  !rm -f tempfile
%
  !grep genes_data ~/.genes.env | sed 's/=/ /g' | awk '{ print $3}' > tempfile
  data_dir = textread ( 'tempfile', '%c' )' ;
  !rm -f tempfile
%
  full_data_dir = [ data_dir '/' dir1 '/' dir2 '/' ] ;
  full_gens_dir = [ full_data_dir dir3 '/' ] ;
  full_base_dir = [ base_dir '/matlab/' ] ;
  clear base_dir data_dir
%
  fid = fopen ('temp_1.sh','w') ;
  count = fprintf ( fid, '%s \n', '#!/bin/sh' ) ;
  count = fprintf ( fid, '%s \n', ...
      [full_base_dir 'pop_extract.sh ' dir1 ' ' dir2 ] ) ;
  !chmod 700 temp_1.sh
  check = fclose (fid) ;
%
  fid = fopen ('temp_2.sh','w') ;
  count = fprintf ( fid, '%s \n', '#!/bin/sh' ) ;
  count = fprintf ( fid, '%s \n', ...
      [full_base_dir 'pop_clean.sh ' dir1 ' ' dir2 ] ) ;
  !chmod 700 temp_2.sh
  check = fclose (fid) ;
%
  clear check count fid
%
% -------------------------------------------------------------------- %
%  1. Prepare data
%  1.a Parse files in data directory
%
  !temp_1.sh > temp_1.out
  !rm -f temp_1.sh temp_1.out
%
% 1.b Get parameter counts
%
  fid = fopen ( [ full_data_dir 'pop.count' ],'r') ;
%
  test = fscanf ( fid, '%d%d%d', 3) ;
  nq = test(1) ;
  np = test(2) ;
  ng = test(3) ;
%
  check = fclose (fid) ;
  clear fid check test
%  
  !temp_2.sh > temp_2.out
  !rm -f temp_2.sh temp_2.out
%
% 1.c Make dummy data structure
%
  out.count = [ nq np ng ] ;
%
  out.error  = zeros ( [ np 1 ] ) ;
  out.lambda = zeros ( [ np nq ] ) ;
  out.mu     = zeros ( [ np nq ] ) ;
  out.Dtheta = zeros ( [ np nq ] ) ;
  out.Cd     = zeros ( [ np nq ] ) ;
  out.Cs     = zeros ( [ np nq ] ) ;
  out.m      = zeros ( [ np 1 ] ) ;
  out.n      = zeros ( [ np 1 ] ) ;
%
  out.error (:,:) = NaN ;
  out.lambda(:,:) = NaN ;
  out.mu    (:,:) = NaN ;
  out.Dtheta(:,:) = NaN ;
  out.Cd    (:,:) = NaN ;
  out.Cs    (:,:) = NaN ;
  out.m     (:,:) = NaN ;
  out.n     (:,:) = NaN ; 
%
% -------------------------------------------------------------------- %
%  2. Read parameter values
%
  fid = fopen ( [ full_gens_dir 'pop_clean' ],'r') ;
  if ( fid ~= -1 )
%
      for ip=1:np
%
% 2.a Single quadruplet reading
%
          if ( nq == 1 )
%
              test = fscanf(fid, '%g%g%g%g%g%g%g%g', 8) ;
%
              out.error (ip,1) = test(1) ;
              out.lambda(ip,1) = test(2) ;
              out.mu    (ip,1) = test(3) ;
              out.Dtheta(ip,1) = test(4) ;
              out.Cd    (ip,1) = test(5) ;
              out.Cs    (ip,1) = test(6) ;
              out.m     (ip,1) = test(7) ;
              out.n     (ip,1) = test(8) ;
%
% 2.b Multiple quadruplet reading
%
          else
%
              for iq=1:nq
%
                  if ( iq == 1 ) 
%
                      test = fscanf(fid, '%g%g%g%g%g%g', 6) ;
%
                      out.error (ip,iq) = test(1) ;
                      out.lambda(ip,iq) = test(2) ;
                      out.mu    (ip,iq) = test(3) ;
                      out.Dtheta(ip,iq) = test(4) ;
                      out.Cd    (ip,iq) = test(5) ;
                      out.Cs    (ip,iq) = test(6) ;
%
% 2.b.1 Frist of multiple quads
%
                  elseif ( iq == nq )
%
                      test = fscanf(fid, '%g%g%g%g%g%g%g', 7) ;
%
                      out.lambda(ip,iq) = test(1) ;
                      out.mu    (ip,iq) = test(2) ;
                      out.Dtheta(ip,iq) = test(3) ;
                      out.Cd    (ip,iq) = test(4) ;
                      out.Cs    (ip,iq) = test(5) ;
                      out.m     (ip,1 ) = test(6) ;
                      out.n     (ip,1 ) = test(7) ;
%
% 2.b.2 Last of multiple quads
%
                  else
%
                      test = fscanf(fid, '%g%g%g%g%g', 5) ;
%
                      out.lambda(ip,iq) = test(1) ;
                      out.mu    (ip,iq) = test(2) ;
                      out.Dtheta(ip,iq) = test(3) ;
                      out.Cd    (ip,iq) = test(4) ;
                      out.Cs    (ip,iq) = test(5) ;
%
% 2.b.3 in-betweens of multiple quads
%
                  end
              end
          end
      end
%
      check = fclose (fid) ;
%
  end
%
  clear fid test ip iq
%
% -------------------------------------------------------------------- %
%  4. Clean up
%
  clear full_data_dir full_base_dir full_gens_dir nq np ng check
%
% - end of read_pop_clean -------------------------------------------- %
