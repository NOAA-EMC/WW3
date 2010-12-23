function out = read_mapping(dir1,dir2)
%
% Author      : H.L. Tolman
% Last update : 07-Oct-2009
%
%     07-Oct-2009 : Origination.                        ( version 1.00 )
%
%  1. Purpose :
%
%     Read data from file mapping.pars from the genetic optimization
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
%     - Using scripts maps_extract.sh and maps_clean.sh.
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
  full_base_dir = [ base_dir '/matlab/' ] ;
  clear base_dir data_dir
%
  fid = fopen ('temp_1.sh','w') ;
  count = fprintf ( fid, '%s \n', '#!/bin/sh' ) ;
  count = fprintf ( fid, '%s \n', ...
      [full_base_dir 'maps_extract.sh ' dir1 ' ' dir2 ] ) ;
  !chmod 700 temp_1.sh
  check = fclose (fid) ;
%
  fid = fopen ('temp_2.sh','w') ;
  count = fprintf ( fid, '%s \n', '#!/bin/sh' ) ;
  count = fprintf ( fid, '%s \n', ...
      [full_base_dir 'maps_clean.sh ' dir1 ' ' dir2 ] ) ;
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
  fid = fopen ( [ full_data_dir 'maps.count' ],'r') ;
%
  test = fscanf ( fid, '%d%d%d%d%d%d%d', 7) ;
  nr_la = test(1) ;
  nr_mu = test(2) ;
  nr_Dt = test(3) ;
  nr_Cd = test(4) ;
  nr_Cs = test(5) ;
  nr_m  = test(6) ;
  nr_n  = test(7) ;
%
  check = fclose (fid) ;
  clear fid check
%
% 1.c Make dummy data structure
%
  out.count = test ;
  out.data  = zeros ( [ test' 20 ] ) ;
  out.data(:,:,:,:,:,:,:,:) = NaN ;
  clear test  
%
% -------------------------------------------------------------------- %
% 2. Read parameter values
% 2.a lambda
%
  fid = fopen ( [ full_data_dir 'maps.lambda' ],'r') ;
%
  for i=1:nr_la
      test = fscanf(fid, '%g', 1) ;
      out.lambda(i) = test ;
  end
%
  check = fclose (fid) ;
%
% 2.b mu
%
  fid = fopen ( [ full_data_dir 'maps.mu' ],'r') ;
%
  for i=1:nr_mu
      test = fscanf(fid, '%g', 1) ;
      out.mu(i) = test ;
  end
%
  check = fclose (fid) ;
%
% 2.c Dtheta
%
  fid = fopen ( [ full_data_dir 'maps.Dtheta' ],'r') ;
%
  for i=1:nr_Dt
      test = fscanf(fid, '%g', 1) ;
      out.Dtheta(i) = test ;
  end
%
  check = fclose (fid) ;
%
% 2.d Cd
%
  fid = fopen ( [ full_data_dir 'maps.Cd' ],'r') ;
%
  for i=1:nr_Cd
      test = fscanf(fid, '%g', 1) ;
      out.Cd(i) = test ;
  end
%
  check = fclose (fid) ;
%
% 2.e Cs
%
  fid = fopen ( [ full_data_dir 'maps.Cs' ],'r') ;
%
  for i=1:nr_Cs
      test = fscanf(fid, '%g', 1) ;
      out.Cs(i) = test ;
  end
%
  check = fclose (fid) ;
%
% 2.f m
%
  fid = fopen ( [ full_data_dir 'maps.m' ],'r') ;
%
  for i=1:nr_m
      test = fscanf(fid, '%g', 1) ;
      out.m(i) = test ;
  end
%
  check = fclose (fid) ;
%
% 2.g n
%
  fid = fopen ( [ full_data_dir 'maps.n' ],'r') ;
%
  for i=1:nr_n
      test = fscanf(fid, '%g', 1) ;
      out.n(i) = test ;
  end
%
  check = fclose (fid) ;
%
  clear fid check test i
%
% -------------------------------------------------------------------- %
%  3. Read error data
%
  fid = fopen ( [ full_data_dir 'mapping.pars' ],'r') ;
%
  for i7=1:nr_n
      for i6=1:nr_m
          for i5=1:nr_Cs
              for i4=1:nr_Cd
                  for i3=1:nr_Dt
                      for i2=1:nr_mu
                          for i1=1:nr_la
                                test = fscanf(fid, ...
                        '%g%g%g%g%g%g%g%g%g%g%g%g%g%g%g%g%g%g%g%g', 20) ;
                                out.data(i1,i2,i3,i4,i5,i6,i7,1:20) = ...
                                    test ;
%                               out.data(i1,i2,i3,i4,i5,i6,i7,1) = ...
%                                   out.n(i7) ;
                          end
                      end
                  end
              end
          end
      end
  end
%
  check = fclose (fid) ;
%
  clear fid check test i1 i2 i3 i4 i5 i6 i7
%
% -------------------------------------------------------------------- %
%  4. Clean up
%
  !temp_2.sh > temp_2.out
  !rm -f temp_2.sh temp_2.out
%
  clear full_data_dir full_base_dir nr_la nr_mu nr_Dt nr_Cd nr_Cs nr_m nr_n
%
% - end of read_mapping ---------------------------------------------- %
