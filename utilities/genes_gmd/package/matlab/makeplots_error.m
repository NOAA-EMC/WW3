% makeplots_error
%
% Author      : H.L. Tolman
% Last update : 14-Dec-2009
%
%     14-Dec-2009 : Origination.                        ( version 1.00 )
%     14-Dec-2009 : Adding label option.                ( version 1.01 )
%
%  1. Purpose :
%
%     Make scatter plots of error evolution per generation. 
%
%  2. Method :
%
%     Get data from pop_clean files and use Matlab to create plots.
%     Requests set up in section 0.
%
%  3. Input and output :
%
%     None, main program.
%
%  4. Subroutines used :
%
%     ----------------------------------------------------------------
%      read_pop_clean.m  Read the pop_clean files.
%     ----------------------------------------------------------------
%
%  5. Error messages :
%
%  6. Remarks :
%
%     - All points are plottes in backward order, to get the best points 
%       on top in the graphics.
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
  dir1 = '3P5Q' ;
% dir2 = [ 'shal_b_1' ; 'shal_b_2' ; 'shal_b_3' ] ;
% dir2 = [ 'shal_c_1' ; 'shal_c_2' ; 'shal_c_3' ] ;
% dir2 = [ 'shal_d_1' ; 'shal_d_2' ; 'shal_d_3' ] ;
  dir2 = [ 'shal_e_1' ; 'shal_e_2' ; 'shal_e_3' ] ;
% dir2 = [ 'shal_e_1'  ] ;
  [ ncases nchar ] = size (dir2) ;
%
  gen_1 =   1 ;
  gen_n = 120 ;
  gen_s =   1 ;
%
  fract = 0.50 ;
%
% label = ' ' ;
  label = '(d)' ;
% label = '(b) GMD1' ;
%
% err_lim = [ 15 35 ] ; ETick = [15 20 25 30 35 40] ;
% err_lim = [ 15 25 ] ; ETick = [15 17.5 20 22.5 25] ;
% err_lim = [ 10 30 ] ; ETick = [10 15 20 25 30] ;
% err_lim = [  5 25 ] ; ETick = [ 5 10 15 20 25] ;
  err_lim = [ 32 36 ] ; ETick = [30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48] ;
% err_lim = [ 30 38 ] ; ETick = [30 32 34 36 38 40 42 44 46 48] ;
%
  v_xlab = err_lim(1) - 0.12 .* ( err_lim(2) - err_lim(1) ) ;
  h_ylab = -0.10 * gen_n ;
  v_ylab1 = err_lim(1) + 0.93 .* ( err_lim(2) - err_lim(1) );
  v_ylab2 = err_lim(1) + 0.84 .* ( err_lim(2) - err_lim(1) );
% h_llab = 0.05 * gen_n ;
% v_llab = err_lim(1) + 0.07 .* ( err_lim(2) - err_lim(1) );
  h_llab = 0.92 * gen_n ;
  v_llab = err_lim(1) + 0.93 .* ( err_lim(2) - err_lim(1) );
%
% -------------------------------------------------------------------- %
%  1. Set up plot
%
  fig = figure ;
  axi = axes ( 'Parent',fig,'FontSize',14, 'YTick',ETick,...
               'LineWidth',2,'Position',[0.12 0.13 0.80 0.80],...
               'Layer','top');
  xlim([0 gen_n]);
  ylim(err_lim);
%
  box('on') ; hold('all') ;
%      
  text ( 0.7.*gen_n, v_xlab,'generation', 'FontSize', 18 )
  text ( 0.8.*h_ylab, v_ylab1,'\epsilon', 'FontSize', 18 )
  text ( h_ylab, v_ylab2,'(%)', 'FontSize', 18 )
  text ( h_llab, v_llab, label, 'FontSize', 18 )
%
% -------------------------------------------------------------------- %
%  2. Loop over cases
%
  for icase=1:ncases
      icase
%
% -------------------------------------------------------------------- %
%  3. Loop over generations
%
      err_min = ones(gen_n,1) ;
      err_avg = ones(gen_n,1) ;
      err_axi = ones(gen_n,1) ;
      err_min(:,1) = NaN ;
      err_avg(:,1) = NaN ;
      err_axi(:,1) = NaN ;
%
      for i_gen=gen_1:gen_s:gen_n
          err_axi(i_gen,1) = i_gen ;
%
% 3.a File name and plot ID
%
          [ filename , error ] = sprintf ( 'gen%04g', i_gen ) ;
          clear error
%
% 3.b Read data
%
          data = read_pop_clean (dir1,dir2(icase,:),filename) ;
          ndata = data.count(2) ;
%
          err_min(i_gen,1) = data.error(1,1) ;
          err_avg(i_gen,1) = mean(data.error(1:round(fract.*gen_n))) ;
%
      end
%
% 3.c Plot
%
      if ( icase == 1 )
          color = 'r' ;
      else if ( icase == 2 )
              color = 'g' ;
          else if ( icase == 3 )
                  color = 'b' ;
              end
          end
      end
%
      plot (err_axi,err_min,'LineWidth',1,'Color',color,...
                            'LineStyle','-');
      plot (err_axi,err_avg,'LineWidth',1,'Color',color,...
                            'LineStyle','--');
%
  end
%
% -------------------------------------------------------------------- %
%  4. End of program
%
% clear ; clc
% close all
%
% - end of makeplots_error ------------------------------------------- %
