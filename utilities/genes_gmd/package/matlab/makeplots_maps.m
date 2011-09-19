% makeplots_maps
%
% Author      : H.L. Tolman
% Last update : 03-Nov-2009
%
%     03-Nov-2009 : Origination.                        ( version 1.00 )
%
%  1. Purpose :
%
%     Plot results of error mapping excercise for GMD optimization
%     packages. Assuming that lambda and Cd are varied only.
%
%  2. Method :
%
%     Get data from mapping files and use Matlab to create plots.
%     Requests set up in section 0.
%
%  3. Input and output :
%
%     None, main program.
%
%  4. Subroutines used :
%
%     ----------------------------------------------------------------
%      read_mapping.m    Read the mapping.par.
%     ----------------------------------------------------------------
%
%  5. Error messages :
%
%  6. Remarks :
%
%     - Assuming that genes_nq = 1.
%
%  7. Structure :
%
%  8. Source code :
%
% -------------------------------------------------------------------- %
%  0. Initializations
%  0.a Set user run time options
%
  clear ; clc ; close all ;
%
  dir1 = 'mapping' ;                      % select test case
  dir2 = 'test_13b' ;
  nr_m = 5 ;                              % m counter for shallow w.
%
% 0.b
%
  d_m(1:20) = 100 ; d_m(10) = 200 ; d_m(12:20) = 200 ;
  d_s(1:20) = 100 ; d_s(10) = 200 ; d_s(12:20) = 200 ;
                    d_s(15) = 500 ; d_s(20) = 500 ;
  ytb = 9.15 ; yta = 9.10 ; ytd(1:20) = ytb ;
%  
% flag( 1,:) = 'yes' ; txt( 1,:) = '     (a) test\_11 (%)     ' ;
  flag( 1,:) = 'ye ' ; txt( 1,:) = '(-) Total error (%)       ' ;
  flag( 2,:) = 'ye ' ; txt( 2,:) = '(a) Error H_s (%)         ' ; ytd( 2) = yta ;
  flag( 3,:) = ' e ' ; txt( 3,:) = '(b) Error H_s (swell,%)   ' ; ytd( 3) = yta ;
  flag( 4,:) = 'ye ' ; txt( 4,:) = '(b) Error f_p (%)         ' ; ytd( 4) = yta ;
  flag( 5,:) = ' e ' ; txt( 5,:) = '( ) Error f_p (swell,%)   ' ; ytd( 5) = yta ;
  flag( 6,:) = 'ye ' ; txt( 6,:) = '(c) Error \theta (%)      ' ;
  flag( 7,:) = ' e ' ; txt( 7,:) = '( ) Error \theta (dwell,%)' ;
  flag( 8,:) = 'ye ' ; txt( 8,:) = '(d) Error \sigma (%)      ' ;
  flag( 9,:) = ' e ' ; txt( 9,:) = '( ) Error \sigma (swell,%)' ;
  flag(10,:) = 'ye ' ; txt(10,:) = '(a) Error \alpha (%)      ' ;
  flag(11,:) = 'ye ' ; txt(11,:) = '(a) Error f_0 (%)         ' ; ytd(11) = yta ;
  flag(12,:) = 'ye ' ; txt(12,:) = '(a) Error \beta (%)       ' ;
  flag(13,:) = 'yes' ; txt(13,:) = '(a) Error F(f) (%)        ' ;
  flag(14,:) = 'yes' ; txt(14,:) = '(b) Error G(f) (%)        ' ;
  flag(15,:) = 'yes' ; txt(15,:) = '(c) Error S_{nl}(f)       ' ; ytd(15) = yta ;
  flag(16,:) = 'ye ' ; txt(16,:) = '(d) Error \theta(f)       ' ;
  flag(17,:) = 'ye ' ; txt(17,:) = '(e) Error \sigma(f)       ' ;
  flag(18,:) = 'ye ' ; txt(18,:) = '(b) Error F(f,\theta)     ' ;
  flag(19,:) = 'ye ' ; txt(19,:) = '(c) Error G(f,\theta)     ' ;
  flag(20,:) = 'yes' ; txt(20,:) = '(d) Error S_{nl}(f,\theta)' ; ytd(20) = yta ;
%
% -------------------------------------------------------------------- %
%  1. Fill the data structures and check contents of file
%
  data = read_mapping (dir1,dir2) ;
  clear dir1 dir2
%
  nr_deep = data.count(1) .* data.count(4) ;
  nr_deep_i = data.count(2) .* data.count(3) .* data.count(5) .* ...
              data.count(6) .* data.count(7) ;
  if ( nr_deep > 1 ) && ( nr_deep_i == 1 )
      deep = 'yes' ;
  else
      deep = 'no' ;
  end
%
  nr_shal = data.count(5) .* data.count(6) .* data.count(7) ;
  nr_shal_i = data.count(1) .* data.count(2) .*  ...
              data.count(3) .* data.count(4) ;
  if ( nr_shal > 1 ) && ( nr_shal_i == 1 )
      shal_1 = 'yes' ;
  else
      shal_1 = 'no' ;
  end
%
  nr_shal = data.count(5) .* data.count(6) .* data.count(7) ;
  nr_shal_i = data.count(1) .* data.count(2) .*  ...
              data.count(3) .* data.count(4) ;
  if ( nr_shal > 1 ) && ( nr_shal_i == 1 )
      shal_1 = 'yes' ;
  else
      shal_1 = 'no' ;
  end
%
  nr_shal = data.count(1) .* data.count(5)  ;
  nr_shal_i = data.count(2) .* data.count(2) .*  data.count(4) .* ...
              data.count(6) .* data.count(7) ;
  if ( nr_shal > 1 ) && ( nr_shal_i == 1 )
      shal_2 = 'yes' ;
  else
      shal_2 = 'no' ;
  end
%
  clear nr_deep nr_deep_i nr_shal nr_shal_i
%
% -------------------------------------------------------------------- %
%  2. Plot for deep water parameter combinations
%
  if strcmp ( deep , 'yes' )
%
      nx = data.count(1) ;
      ny = data.count(4) ;
%
      var_x = data.lambda ;
      var_y = log10(data.Cd) ;
%
      error = ones ( ny, nx ) ;
%
      for ip=1:20
         if strcmp ( flag(ip,:) , 'yes' )
              for ix=1:nx
                  error(:,ix) = data.data(ix,1,1,:,1,1,1,ip) ;
              end
              fig_d(ip) = figure ;
              axi_d(ip) = axes ( 'Parent', fig_d(ip), ...
                  'xlim' , [0.1 0.35], 'ylim', [6 9], ...
                  'Position',[0.12 0.10 0.75 0.80],...
                  'YTick', [6 7 8 9], 'Layer','top','FontSize',14);
              box('on') ; hold('all') ;
              imagesc ( var_x, var_y,error,'Parent', axi_d(ip));
              caxis ( [0 d_m(ip) ] ) ;
              colorbar ('peer', axi_d(ip), ...
                        [0.90 0.10 0.04 0.80], 'FontSize', 14 );
              text ( 0.31, 5.75,'\lambda (-)', 'FontSize', 18 )
              text ( 0.062, 8.60,'log C_d', 'FontSize', 18 )
              text ( 0.070, 8.30,'(-)', 'FontSize', 18 )
              text ( 0.18, ytd(ip), txt(ip,:), 'FontSize', 18 )
         end
      end
  end
%
% -------------------------------------------------------------------- %
%  3. Plot for shallow water parameter combinations
%  3.a First optimization excersises
%
  if strcmp ( shal_1 , 'yes' )
%
      nx = data.count(7) ;
      ny = data.count(5) ;
%
      var_x = data.n ;
      var_y = log10(data.Cs) ;
%
      error = ones ( ny, nx ) ;
      ytd = ytd - 2. ;
 %
      for ip=1:20
         if strcmp ( flag(ip,:) , 'yes' )
              for ix=1:nx
                  error(:,ix) = data.data(1,1,1,1,:,nr_m,ix,ip) ;
              end
              fig_s(ip) = figure ;
              axi_s(ip) = axes ( 'Parent', fig_s(ip), ...
                  'xlim' , [-5 2], 'ylim', [4 7], ...
                  'Position',[0.12 0.10 0.75 0.80],...
                  'YTick', [4 5 6 7], 'Layer','top','FontSize',14);
              box('on') ; hold('all') ;
              imagesc ( var_x, var_y,error,'Parent', axi_s(ip));
%             caxis ( [0 d_m(ip) ] ) ;
%             caxis ( [ 20 40 ] ) ;
%             caxis ( [ 25 55 ] ) ;
%             caxis ( [ 70 240 ] ) ;
              colorbar ('peer', axi_s(ip), ...
                        [0.90 0.10 0.04 0.80], 'FontSize', 14 );
              text ( 0.45, 3.75,'n        (-)', 'FontSize', 18 )
              text ( -6.00, 6.60,'log C_s', 'FontSize', 18 )
              text ( -5.75, 6.30,'(-)', 'FontSize', 18 )
              text ( -3.0, ytd(ip), txt(ip,:), 'FontSize', 18 )
              IDtext = sprintf ('m =%4.1f', data.m(nr_m)) ;
              text ( 0.5, 4.15, IDtext, 'FontSize', 18 )
%             IDtext = sprintf ('(d)  m =%4.1f', data.m(nr_m)) ;
%             text ( -2.0, ytd(ip), IDtext, 'FontSize', 18 )
          end
      end
  end
%
%  3.a Second optimization excersises
%
  if strcmp ( shal_2 , 'yes' )
%
      nx = data.count(1) ;
      ny = data.count(5) ;
%
      var_x = data.lambda ;
      var_y = log10(data.Cs) ;
%
      error = ones ( ny, nx ) ;
      ytd = ytd - 2. ;
%
      for ip=1:20
         if strcmp ( flag(ip,:) , 'yes' )
              for ix=1:nx
                  error(:,ix) = data.data(ix,1,1,1,:,1,1,ip) ;
              end
              fig_d(ip) = figure ;
              axi_d(ip) = axes ( 'Parent', fig_d(ip), ...
                  'xlim' , [0.1 0.35], 'ylim', [4 7], ...
                  'Position',[0.12 0.10 0.75 0.80],...
                  'YTick', [4 5 6 7], 'Layer','top','FontSize',14);
              box('on') ; hold('all') ;
              imagesc ( var_x, var_y,error,'Parent', axi_d(ip));
              caxis ( [0 d_s(ip) ] ) ;
%             caxis ( [30 60] ) ;
%             caxis ( [70 220] ) ;
              colorbar ('peer', axi_d(ip), ...
                        [0.90 0.10 0.04 0.80], 'FontSize', 14 );
              text ( 0.31, 3.75,'\lambda (-)', 'FontSize', 18 )
              text ( 0.062, 6.60,'log C_s', 'FontSize', 18 )
              text ( 0.070, 6.30,'(-)', 'FontSize', 18 )
              text ( 0.18, ytd(ip), txt(ip,:), 'FontSize', 18 )
         end
      end
  end
%
% -------------------------------------------------------------------- %
%  x. End of program
%
  clear ; clc
  close all
%
% - end of makeplots_maps -------------------------------------------- %
