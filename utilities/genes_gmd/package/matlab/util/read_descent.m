function out = read_descent(dir1,dir2,dir3,fileID)
%
% Author      : H.L. Tolman
% Last update : 19-Jan-2010
%
%     19-Jan-2010 : Origination.                        ( version 1.00 )
%
%  1. Purpose :
%
%     Read data from file descent.fileID until best quadruplet is found.
%
%  2. Method :
%
%     Formatted data read, check for EOF/read error.
%
%  3. Input and output :
%
%       dirN     string I  Directory of input file.
%       fileID   string I  Extension of descent file.
%       out      struc  O  Structure with output data.
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
  full_gens_dir = [ full_data_dir  dir3 ] ;
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
  out.nq     = nq ;
%
  out.error        = NaN ;
  out.lambda(1:nq) = NaN ;
  out.mu    (1:nq) = NaN ;
  out.Dtheta(1:nq) = NaN ;
  out.Cd    (1:nq) = NaN ;
  out.Cs    (1:nq) = NaN ;
  out.m            = NaN ;
  out.n            = NaN ;
%
% -------------------------------------------------------------------- %
%  2. Read parameter values
%
  fid = fopen ( [ full_gens_dir '/descent.' fileID ],'r') ;
%
  if ( fid == -1 )
      disp ( [ full_gens_dir '/descent.' fileID ' not found' ] )
  else
      disp ( [ full_gens_dir '/descent.' fileID ' opened' ] )
      OK = 0 ; ip = 0 ;
%
      while ( OK == 0 )
%
% 2.a Single quadruplet reading
%
          if ( nq == 1 )
%
              test = fscanf(fid, '%g%g%g%g%g%g%g%g', 8) ;
              OK = isempty(test) ;
%
              if ( OK == 0 )
                  out.error (1) = test(1) ;
                  out.lambda(1) = test(2) ;
                  out.mu    (1) = test(3) ;
                  out.Dtheta(1) = test(4) ;
                  out.Cd    (1) = test(5) ;
                  out.Cs    (1) = test(6) ;
                  out.m     (1) = test(7) ;
                  out.n     (1) = test(8) ;
              end
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
                      OK = isempty(test) ;
%
                      if ( OK == 0 )
                          out.error (1) = test(1) ;
                          out.lambda(1) = test(2) ;
                          out.mu    (1) = test(3) ;
                          out.Dtheta(1) = test(4) ;
                          out.Cd    (1) = test(5) ;
                          out.Cs    (1) = test(6) ;
                      end
%
% 2.b.1 Frist of multiple quads
%
                  elseif ( iq == nq )
%
                      if ( OK == 0 )
                          test = fscanf(fid, '%g%g%g%g%g%g%g', 7) ;
%
                          out.lambda(iq) = test(1) ;
                          out.mu    (iq) = test(2) ;
                          out.Dtheta(iq) = test(3) ;
                          out.Cd    (iq) = test(4) ;
                          out.Cs    (iq) = test(5) ;
                          out.m     (1 ) = test(6) ;
                          out.n     (1 ) = test(7) ;
                      end
%
% 2.b.2 Last of multiple quads
%
                  else
%
                      if ( OK == 0 )
                          test = fscanf(fid, '%g%g%g%g%g', 5) ;
%
                          out.lambda(iq) = test(1) ;
                          out.mu    (iq) = test(2) ;
                          out.Dtheta(iq) = test(3) ;
                          out.Cd    (iq) = test(4) ;
                          out.Cs    (iq) = test(5) ;
                      end
%
% 2.b.3 in-betweens of multiple quads
%
                  end
              end
          end
          if ( OK == 0 )
              ip = ip + 1 ;
              string = sprintf ( '%s %.0f %s', '     configuration ', ...
                  ip, ' scanned' ) ;
              disp (string)
          end
      end
%
      check = fclose (fid) ;
%
  end
%
  clear fid test iq OK check string
%
% -------------------------------------------------------------------- %
%  4. Clean up
%
  clear full_data_dir full_base_dir full_gens_dir nq np ng  
%
% - end of read_descent ---------------------------------------------- %
