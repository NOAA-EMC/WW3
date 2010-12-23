% makeplots_co_5
%
% Author      : H.L. Tolman
% Last update : 01-Dec-2008
%
%     12-Nov-2009 : Origination.                        ( version 1.01 )
%                   Starting from makeplots_base.m
%     22-Oct-2010 : Adding Great Lakes test.
%
%  1. Purpose :
%
%     Graphics for comparison of five different model runs for which data
%     has been gemerated using run_base.sh. Can be used for any number of
%     data sets (see remarks).
%
%  2. Method :
%
%     Get data from all_data.ww3 files and use Matlab to create plots.
%     Requests set up in section 0.
%
%  3. Input and output :
%
%     None, main program.
%
%  4. Subroutines used :
%
%     ----------------------------------------------------------------
%      read_all_data.m   Read the all_data.ww3 files.
%     ----------------------------------------------------------------
%
%  5. Error messages :
%
%  6. Remarks :
%
%  7. Structure :
%
%     - Number of lines can be changes simply by modifying the arrays sizes
%       in Section 0.b.
%
%  8. Source code :
%
% -------------------------------------------------------------------- %
%  0. Initializations
%  0.a Set user run time options
%
  clear ; clc ; close all
%
  test = 'test_LM' ;                      % select test case
%
  plot_d  = ' es' ; lab_d  = '(a)' ;      % Make water depth plot
  plot_Hs = ' es' ; lab_Hs = '(a)' ;      % Make wave height plot
  plot_fp = ' es' ; lab_fp = '(b)' ;      % Make peak frequency plot
  plot_kd = ' es' ; lab_kd = '(b)' ;      % Make relative depth plot
  plot_tm = ' es' ; lab_tm = '(c)' ;      % Make mean direction plot
  plot_si = ' es' ; lab_si = '(d)' ;      % Make direct. spread  plot
%
  j1 = 1 ; js = 1 ; jn = 40 ;             % Step info for spectra 
% j1 = 1 ; js = 1 ; jn = 47 ;             % Step info for spectra
% j1 = 1 ; js = 1 ; jn = 50 ;             % Step info for spectra
% j1 = 1 ; js = 1 ; jn =  5 ;             % Step info for spectra
% j1 =12 ; js = 1 ; jn = 12 ;             % Step info for spectra
% j1 =37 ; js = 1 ; jn = 37 ;             % Step info for spectra
%
  plot_F1  = 'yes' ; lab_F1  = '(b)' ;    % Make F(f) plot
  plot_G1  = 'yes' ; lab_G1  = '(c)' ;    % Make G(f) plot
  plot_th1 = 'yes' ; lab_th1 = '(d)' ;    % Make th(f) plot
  plot_si1 = 'yes' ; lab_si1 = '(e)' ;    % Make si(f) plot
  plot_s1  = 'yes' ; lab_s1  = '(f)' ;    % Make s(f) plot
  plot_M1  = ' es' ; lab_M1  = '(f)' ;    % Make M(f) plot
%
% 0.b Cases and line attributes
%
  cases(1,:) = 'WRT   ' ;
  lnwdt(1)   = 3 ;
  lnsty(1,:) = '- ' ;
  lncol(1,:) = [0.0 0.7 0.0 ] ;
%
  cases(2,:) = 'G35d  ' ;
  lnwdt(2)   = 2 ;
  lnsty(2,:) = '--' ;
  lncol(2,:) = [0.0 0.3 0.0 ] ;
%
  cases(3,:) = 'GMD3  ' ;
  lnwdt(3)   = 2 ;
  lnsty(3,:) = '- ' ;
  lncol(3,:) = [0.0 0.0 0.7 ] ;
% 
  cases(4,:) = 'GMD1  ' ;
  lnwdt(4)   = 2 ;
  lnsty(4,:) = '- ' ;
  lncol(4,:) = [0.7 0.0 0.0 ] ;
%
  cases(5,:) = 'WW3   ' ;
  lnwdt(5)   = 2 ;
  lnsty(5,:) = '--' ;
  lncol(5,:) = [0.7 0.0 0.0 ] ;  
%
  cases(6,:) = 'WAM   ' ;
  lnwdt(6)   = 3 ;
  lnsty(6,:) = ': ' ;
  lncol(6,:) = [0.7 0.0 0.0 ] ;  
%
% cases(7,:) = 'GMD3s3' ;
% lnwdt(7)   = 2 ;
% lnsty(7,:) = '--' ;
% lncol(7,:) = [0.7 0.0 0.7 ] ;  
%
% 0.c Setup that should remain unchanged for this script
%
  fname = 'all_data.ww3' ;
%
  !grep 'genes_data' ~/.genes.env | sed 's/=/ /g' | awk '{ print $3} ' > tempfile
  base_dir = textread ( 'tempfile', '%c' )' ;
  !rm -f tempfile
%
  Position_mean = [0.10 0.15 0.88 0.80] ;
  Position_spc1 = [0.14 0.17 0.80 0.78] ;
%
% 0.d Setup dependent upon use setup in 0.a
%
  swell = 'no ' ;
%
  switch test
      case 'test_0X'
          x_axis = 't' ; XLim_m = [0 48] ; XTick_m = [0 12 24 36 48] ;
          x_text = '{\it t} (h)' ; x_tx = 40 ; y_tx = -4 ;
          l_tx = 1 ; l_txr = 44 ;
          Hs_max = 16 ; YTick_Hs = [0 4 8 12 16] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.24 ; YTick_fp = [0 0.06 0.12 0.18 0.24] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = 70 ; tm_max = 110 ; YTick_tm = [70 80 90 100 110] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = 70 ; tmf_max = 110 ; YTick_tmf = [70 80 90 100 110] ;
      case 'test_01'
          x_axis = 't' ; XLim_m = [0 48] ; XTick_m = [0 12 24 36 48] ;
          x_text = '{\it t} (h)' ; x_tx = 40 ; y_tx = -4 ;
          l_tx = 1 ; l_txr = 44 ;
          Hs_max = 16 ; YTick_Hs = [0 4 8 12 16] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.24 ; YTick_fp = [0 0.06 0.12 0.18 0.24] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = 70 ; tm_max = 110 ; YTick_tm = [70 80 90 100 110] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = 70 ; tmf_max = 110 ; YTick_tmf = [70 80 90 100 110] ;
      case 'test_02'
          x_axis = 'x' ; XLim_m = [0 510] ; XTick_m = [0 125 250 375 500] ;
          x_text = '{\it x} (km)' ; x_tx = 395 ; y_tx = -50 ;
          l_tx = 10 ; l_txr = 470 ;
          Hs_max = 8 ; YTick_Hs = [0 2 4 6 8] ;
          fp_max = 0.24 ; YTick_fp = [0 0.06 0.12 0.18 0.2] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = 70 ; tm_max = 110 ; YTick_tm = [70 80 90 100 110] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = 70 ; tmf_max = 110 ; YTick_tmf = [70 80 90 100 110] ;
      case 'test_03'
          x_axis = 't' ; XLim_m = [0 24] ; XTick_m = [0 6 12 18 24] ;
          x_text = '{\it t} (h)' ; x_tx = 20 ; y_tx = -2 ;
          l_tx = 0.5 ; l_txr = 22 ;
          Hs_max = 8 ; YTick_Hs = [0 2 4 6 8] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.32 ; YTick_fp = [0 0.08 0.16 0.24 0.32] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = -45 ; tm_max = 135 ; YTick_tm = [-45 0 45 90 135] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = -45 ; tmf_max = 135 ; YTick_tmf = [-45 0 45 90 135] ;
      case 'test_04'
          x_axis = 't' ; XLim_m = [0 24] ; XTick_m = [0 6 12 18 24] ;
          x_text = '{\it t} (h)' ; x_tx = 20 ; y_tx = -2 ;
          l_tx = 0.5 ; l_txr = 22 ;
          Hs_max = 8 ; YTick_Hs = [0 2 4 6 8] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.32 ; YTick_fp = [0 0.08 0.16 0.24 0.32] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = -90 ; tm_max = 90 ; YTick_tm = [-90 -45 0 45 90] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = -90 ; tmf_max = 90 ; YTick_tmf = [-135 -90 -45 0 45 90 135] ;
      case 'test_05'
          x_axis = 'x' ; XLim_m = [0 510] ; XTick_m = [0 125 250 375 500] ;
          x_text = '{\it x} (km)' ; x_tx = 395 ; y_tx = -50 ;
          l_tx = 10 ; l_txr = 470 ;
          Hs_max = 12 ; YTick_Hs = [0 3 6 9 12] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.16 ; YTick_fp = [0 0.04 0.08 0.12 0.16] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = -20 ; tm_max = 60 ; YTick_tm = [-20 0 20 40 60] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = -90 ; tmf_max = 90 ; YTick_tmf = [-135 -90 -45 0 45 90 135] ;
      case 'test_06'
          swell = 'yes' ;
          x_axis = 't' ; XLim_m = [0 48] ; XTick_m = [0 12 24 36 48] ;
          x_text = '{\it t} (h)' ; x_tx = 40 ; y_tx = -4 ;
          l_tx = 1 ; l_txr = 44 ;
          Hs_max = 16 ; YTick_Hs = [0 4 8 12 16] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.24 ; YTick_fp = [0 0.06 0.12 0.18 0.24] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = -60 ; tm_max = 120 ; YTick_tm = [-60 -15 30 75 120] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = -60 ; tmf_max = 120 ; YTick_tmf = [-60 -15 30 75 120] ;
      case 'test_11'
          x_axis = 't' ; XLim_m = [0 24] ; XTick_m = [0 6 12 18 24] ;
          x_text = '{\it t} (h)' ; x_tx = 20 ; y_tx = -2 ;
          l_tx = 0.5 ; l_txr = 22 ;
          Hs_max = 8 ; YTick_Hs = [0 2 4 6 8] ;
          d_max = 60 ; YTick_d = [0 15 30 45 60] ;
          fp_max = 0.24 ; YTick_fp = [0 0.06 0.12 0.18 0.24] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = 70 ; tm_max = 110 ; YTick_tm = [70 80 90 100 110] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = 70 ; tmf_max = 110 ; YTick_tmf = [70 80 90 100 110] ;
      case 'test_12'
          x_axis = 'x' ; XLim_m = [0 32] ; XTick_m = [0 8 16 24 32] ;
          x_text = '{\it x} (km)' ; x_tx = 25.3 ; y_tx = -2.8 ;
          l_tx = 0.64 ; l_txr = 29.3 ;
          Hs_max = 12 ; YTick_Hs = [0 3 6 9 12] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.16 ; YTick_fp = [0 0.04 0.08 0.12 0.16] ;
          kd_max = 2. ; YTick_kd = [0 0.5 1 1.5 2] ;
          tm_min = -130 ; tm_max = -50 ; YTick_tm = [-130 -110 -90 -70 -50] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = -130 ; tmf_max = -50 ; YTick_tmf = [-130 -110 -90 -70 -50] ;
      case 'test_13'
          x_axis = 'x' ; XLim_m = [0 32] ; XTick_m = [0 8 16 24 32] ;
          x_text = '{\it x} (km)' ; x_tx = 25.3 ; y_tx = -2.8 ;
          l_tx = 0.64 ; l_txr = 29.3 ;
          Hs_max = 6 ; YTick_Hs = [0 1.5 3 4.5 6] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.16 ; YTick_fp = [0 0.04 0.08 0.12 0.16] ;
          kd_max = 2. ; YTick_kd = [0 0.5 1 1.5 2] ;
          tm_min = -130 ; tm_max = -50 ; YTick_tm = [-130 -110 -90 -70 -50] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = -130 ; tmf_max = -50 ; YTick_tmf = [-130 -110 -90 -70 -50] ;
      case 'test_14'
          swell = 'yes' ;
          x_axis = 'x' ; XLim_m = [0 32] ; XTick_m = [0 8 16 24 32] ;
          x_text = '{\it x} (km)' ; x_tx = 25.3 ; y_tx = -2.8 ;
          l_tx = 0.64 ; l_txr = 29.3 ;
          Hs_max = 12 ; YTick_Hs = [0 3 6 9 12] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.16 ; YTick_fp = [0 0.04 0.08 0.12 0.16] ;
          kd_max = 2. ; YTick_kd = [0 0.5 1 1.5 2] ;
          tm_min = -130 ; tm_max = -50 ; YTick_tm = [-130 -110 -90 -70 -50] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = -130 ; tmf_max = -50 ; YTick_tmf = [-130 -110 -90 -70 -50] ;
        case 'test_hr'
          tmf_min = -180 ; tmf_max = 180 ; YTick_tmf = [ -180 -90 0 90 180] ;
        case 'test_LM'
          x_axis = 't' ; XLim_m = [9 48] ; XTick_m = [0 12 24 36 48] ;
          x_text = '{\it t} (h)' ; x_tx = 40 ; y_tx = 4;
          l_tx = 10 ; l_txr = 44 ;
          Hs_max = 4 ; YTick_Hs = [0 1 2 3 4] ;
          d_max = 100 ; YTick_d = [0 25 50 75 100] ;
          fp_max = 0.36 ; YTick_fp = [0 0.09 0.18 0.27 0.36] ;
          kd_max = 12. ; YTick_kd = [0 3 6 9 12] ;
          tm_min = -180 ; tm_max = 180 ; YTick_tm = [-180 -90 0 90 180] ;
          si_max = 60 ; YTick_si = [0 15 30 45 60] ;
          tmf_min = -180 ; tmf_max = 180 ; YTick_tmf = [-180 -90 0 90 180] ;
     end
%
% fr1_min = 0.00 ; fr1_max = 1.00 ; 
% XTick_sp1 = [0 0.2 0.4 0.6 0.8 1.0] ;
%
  fr1_min = 0.04 ; fr1_max = 0.40 ; 
  XTick_sp1 = [0 0.1 0.2 0.3 0.4 0.5] ;
%
% fr1_min = 0.04 ; fr1_max = 0.25 ;
% XTick_sp1 = [0 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50]' ;
%
% fr1_min = 0.04 ; fr1_max = 0.11 ;
% XTick_sp1 = [0.05 0.07 0.09 0.11 0.13 0.15]' ;
%
  sif_max = 60 ; YTick_sif = [0 15 30 45 60] ;
%
% -------------------------------------------------------------------- %
%  1. Fill the data structures
%
  [ ncase str_len ] = size(cases) ;
%
  for i=1:ncase
      case_name = cases(i,1:1) ;
      for j=2:str_len
          if ( cases(i,j:j) ~= ' ' )
              case_name = [ case_name cases(i,j:j) ] ;
          end
      end
      dir = [ base_dir '/' case_name '/' test ] ;
      data(i) = read_all_data (dir,fname) ;
  end
%
  [ nfr iii ] = size ( data(1).freq(:) ) ;
  clear iii base_dir dir fname i str_len
%
% -------------------------------------------------------------------- %
%  2. Plot of depth and wave heights
%
  if strcmp ( plot_d , 'yes' )
%
      if ( x_axis == 'x' )
          var_x = data(1).x ;
      else
          var_x = data(1).time ;
      end
%      
      for i=1:ncase
          var_y(:,i) = data(i).dpt ;
      end
%
      YLim_m = [ 0 d_max ] ; x_ty = 1.1 .* d_max ;
      y_ty1 = 0.15 .* d_max ; y_ty2 = 0.38 .* d_max ;
      l_ty = 0.08 .* d_max ;
%
      figure_d = figure ;
      axes_d = axes('Parent',figure_d, 'LineWidth', 2, 'YDir','reverse', ...
                    'XLim',XLim_m,'XTick',XTick_m,'XGrid','off',...
                    'YLim',YLim_m,'YTick',YTick_d, 'YGrid','off',...
                    'Position',Position_mean,'FontSize',20);
      box('on') ; hold('all') ;
      text (x_tx,x_ty,x_text,'FontSize',24)
      text (y_tx,y_ty1,'{\it d}','FontSize',24)
      text (y_tx.*1.1,y_ty2,'(m)','FontSize',24)
      if ~strcmp ( lab_d , '   ' )
          text (l_txr,l_ty,lab_d,'FontSize',24)
      end
%
      plot_d = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_d) ;
%
      for j=1:ncase
          set ( plot_d (j) , 'LineWidth', lnwdt(j) , ...
                             'LineStyle', lnsty(j,:) , ...
                                 'Color', lncol(j,:) );
      end
%
      clear var_x var_y
%
  end
%
  if strcmp ( plot_Hs , 'yes' )
%
      if ( x_axis == 'x' )
          var_x = data(1).x ;
      else
          var_x = data(1).time ;
      end
%
      if ~strcmp ( swell , 'yes' )
          for i=1:ncase
              var_y(:,i) = data(i).hs0 ;
          end
      else
          for i=1:ncase
              var_y(:,i) = data(i).hs1 ; 
              var_y(:,i+ncase) = data(i).hs2 ;
          end
      end
%
      YLim_m = [ 0 Hs_max ] ; x_ty = -0.1 .* Hs_max ;
      y_ty1 = 0.85 .* Hs_max ; y_ty2 = 0.62 .* Hs_max ;
      l_ty = 0.92 .* Hs_max ;
%
      figure_Hs = figure ;
      axes_Hs = axes('Parent',figure_Hs, 'LineWidth', 2, ...
                     'XLim',XLim_m,'XTick',XTick_m,'XGrid','off',...
                     'YLim',YLim_m,'YTick',YTick_Hs, 'YGrid','off',...
                     'Position',Position_mean,'FontSize',20);
      box('on') ; hold('all') ;
      text (x_tx,x_ty,x_text,'FontSize',24)
      text (1.2.*y_tx,y_ty1,'{\it H_s}','FontSize',24)
      text (1.2.*y_tx,y_ty2,'(m)','FontSize',24)
      if ~strcmp ( lab_Hs , '   ' )
          text (l_tx,l_ty,lab_Hs,'FontSize',24)
      end
%
      plot_Hs = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_Hs) ;
%
      for j=1:ncase
          set ( plot_Hs(j) , 'LineWidth', lnwdt(j) , ...
                             'LineStyle', lnsty(j,:) , ...
                                 'Color', lncol(j,:) );
      end
      if strcmp ( swell , 'yes' )
          for j=1:ncase
              set ( plot_Hs(j+ncase) , 'LineWidth', lnwdt(j) , ...
                                       'LineStyle', lnsty(j,:) , ...
                                           'Color', lncol(j,:) );
          end
      end
%
      clear var_x var_y
%
  end
%
% -------------------------------------------------------------------- %
%  3. Plot peak frequencies and relative depths
%
  if strcmp ( plot_fp , 'yes' )
%
      if ( x_axis == 'x' )
          var_x = data(1).x ;
      else
          var_x = data(1).time ;
      end
%
      if ~strcmp ( swell , 'yes' )
          for i=1:ncase
              var_y(:,i) = data(i).fp0 ;
          end
      else
          for i=1:ncase
              var_y(:,i) = data(i).fp1 ;
              var_y(:,i+ncase) = data(i).fp2 ;
          end
      end
%
      YLim_m = [ 0 fp_max ] ; x_ty = -0.1 .* fp_max ;
      y_ty1 = 0.85 .* fp_max ; y_ty2 = 0.62 .* fp_max ;
      l_ty = 0.92 .* fp_max ;
%
      figure_fp = figure ;
      axes_fp = axes('Parent',figure_fp, 'LineWidth', 2, ...
                     'XLim',XLim_m,'XTick',XTick_m,'XGrid','off',...
                     'YLim',YLim_m,'YTick',YTick_fp, 'YGrid','off',...
                     'Position',Position_mean,'FontSize',20);
      box('on') ; hold('all') ;
      text (x_tx,x_ty,x_text,'FontSize',24)
      text (1.2 * y_tx,y_ty1,'{\it f_p}','FontSize',24)
%     text (y_tx,y_ty1,'{\it f_p}','FontSize',24)
      text (1.2 .* y_tx,y_ty2,'(Hz)','FontSize',24)
      text (l_txr,l_ty,lab_fp,'FontSize',24)
%
      plot_fp = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_fp) ;
%
      for j=1:ncase
          set ( plot_fp(j) , 'LineWidth', lnwdt(j) , ...
                             'LineStyle', lnsty(j,:) , ...
                                 'Color', lncol(j,:) );
      end
      if strcmp ( swell , 'yes' )
          for j=1:ncase
              set ( plot_fp(j+ncase) , 'LineWidth', lnwdt(j) , ...
                                       'LineStyle', lnsty(j,:) , ...
                                           'Color', lncol(j,:) );
          end
      end
%
      clear var_x var_y
%      
  end
%
  if strcmp ( plot_kd , 'yes' )
%
      if ( x_axis == 'x' )
          var_x = data(1).x ;
      else
          var_x = data(1).time ;
      end
%
      if ~strcmp ( swell , 'yes' )
          for i=1:ncase
              var_y(:,i) = data(i).kd0 ;
          end
      else
          for i=1:ncase
              var_y(:,i) = data(i).kd1 ;
              var_y(:,i+ncase) = data(i).kd2 ;
          end
      end
%
      YLim_m = [ 0 kd_max ] ; x_ty = -0.1 .* kd_max ;
      y_ty1 = 0.85 .* kd_max ; y_ty2 = 0.62 .* kd_max ;
      l_ty = 0.92 .* kd_max ;
%
      figure_kd = figure ;
      axes_kd = axes('Parent',figure_kd, 'LineWidth', 2, ...
                     'XLim',XLim_m,'XTick',XTick_m,'XGrid','off',...
                     'YLim',YLim_m,'YTick',YTick_kd, 'YGrid','off',...
                     'Position',Position_mean,'FontSize',20);
      box('on') ; hold('all') ;
      text (x_tx,x_ty,x_text,'FontSize',24)
      text (1.4.*y_tx,y_ty1,'{\it k_pd}','FontSize',24)
      text (y_tx,y_ty2,'(-)','FontSize',24)
      text (l_tx,l_ty,lab_kd,'FontSize',24)
%
      plot_kd = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_kd) ;
%
      for j=1:ncase
          set ( plot_kd(j) , 'LineWidth', lnwdt(j) , ...
                             'LineStyle', lnsty(j,:) , ...
                                 'Color', lncol(j,:) );
      end
      if strcmp ( swell , 'yes' )
          for j=1:ncase
              set ( plot_kd(j+ncase) , 'LineWidth', lnwdt(j) , ...
                                       'LineStyle', lnsty(j,:) , ...
                                           'Color', lncol(j,:) );
          end
      end
%
      clear var_x var_y
%      
  end
%
% -------------------------------------------------------------------- %
%  4. Plot mean directions
%
  if strcmp( plot_tm , 'yes' )
%
      if ( x_axis == 'x' )
          var_x = data(1).x ;
      else
          var_x = data(1).time ;
      end
%
      var_y(:,1) = data(1).ud ;
      if ~strcmp( swell , 'yes' )
          for i=1:ncase
              var_y(:,i+1) = data(i).tm0 ;
          end
      else
          for i=1:ncase
              var_y(:,i+1) = data(i).tm1 ;
              var_y(:,i+1+ncase) = data(i).tm2 ;
          end
      end
%
      YLim_m = [ tm_min tm_max ] ; x_ty = tm_min - 0.1 .* (tm_max-tm_min) ;
      y_ty1 = tm_min + 0.85 .* ( tm_max - tm_min ) ;
      y_ty2 = tm_min + 0.62 .* ( tm_max -tm_min ) ;
      l_ty = tm_min + 0.92 .* ( tm_max -tm_min ) ;
%
      figure_tm = figure ;
      axes_tm = axes('Parent',figure_tm, 'LineWidth', 2, ...
                     'XLim',XLim_m,'XTick',XTick_m,'XGrid','off',...
                     'YLim',YLim_m,'YTick',YTick_tm, 'YGrid','off',...
                     'Position',Position_mean,'FontSize',20);
      box('on') ; hold('all') ;
      text (x_tx,x_ty,x_text,'FontSize',24)
%     text (0.85 .* y_tx,y_ty1+0.04.*(tm_max-tm_min),'{-}','FontSize',24)
%     text (0.85 .* y_tx,y_ty1,'{\theta}','FontSize',24)
%     text (y_tx,y_ty2,'(^o)','FontSize',24)
      text (1.4 .* y_tx,y_ty1+0.04.*(tm_max-tm_min),'{-}','FontSize',24)
      text (1.4 .* y_tx,y_ty1,'{\theta}','FontSize',24)
      text (1.3 .* y_tx,y_ty2,'(^o)','FontSize',24)
      text (l_tx,l_ty,lab_tm,'FontSize',24)
%
      plot_tm = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_tm) ;
%
      set ( plot_tm(1) , 'LineWidth', 0.1 ) ;
      for j=1:ncase
          set ( plot_tm(j+1) , 'LineWidth', lnwdt(j) , ...
                               'LineStyle', lnsty(j,:) , ...
                                   'Color', lncol(j,:) );
      end
      if strcmp ( swell , 'yes' )
          for j=1:ncase
              set ( plot_tm(j+ncase+1) , 'LineWidth', lnwdt(j) , ...
                                        'LineStyle', lnsty(j,:) , ...
                                            'Color', lncol(j,:) );
          end
      end
%
      clear var_x var_y
%      
  end
%
% -------------------------------------------------------------------- %
%  5. Plot directional spread
%
  if strcmp( plot_si , 'yes' )
%
      if ( x_axis == 'x' )
          var_x = data(1).x ;
      else
          var_x = data(1).time ;
      end
%
      if ~strcmp( swell , 'yes' )
          for i=1:ncase
              var_y(:,i) = data(i).si0 ;
          end
      else
          for i=1:ncase
              var_y(:,i) = data(i).si1 ;
              var_y(:,i+ncase) = data(i).si2 ;
          end
      end
%
      YLim_m = [ 0 si_max ] ; x_ty = -0.1 .* si_max ;
      y_ty1 = 0.85 .* si_max ; y_ty2 = 0.62 .* si_max ;
      l_ty = 0.92 .* si_max ;
%
      figure_si = figure ;
      axes_si = axes('Parent',figure_si, 'LineWidth', 2, ...
                     'XLim',XLim_m,'XTick',XTick_m,'XGrid','off',...
                     'YLim',YLim_m,'YTick',YTick_si, 'YGrid','off',...
                     'Position',Position_mean,'FontSize',20);
      box('on') ; hold('all') ;
      text (x_tx,x_ty,x_text,'FontSize',24)
%     text (y_tx,y_ty1,'{\sigma_\theta}','FontSize',24)
%     text (y_tx,y_ty2,'(^o)','FontSize',24)
      text (1.3 .* y_tx,y_ty1,'{\sigma_\theta}','FontSize',24)
      text (1.3 .* y_tx,y_ty2,'(^o)','FontSize',24)
      text (l_tx,l_ty,lab_si,'FontSize',24)
%
      plot_si = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_si) ;
%
      for j=1:ncase
          set ( plot_si(j) , 'LineWidth', lnwdt(j) , ...
                             'LineStyle', lnsty(j,:) , ...
                                 'Color', lncol(j,:) );
      end
      if strcmp ( swell , 'yes' )
          for j=1:ncase
              set ( plot_si(j+ncase) , 'LineWidth', lnwdt(j) , ...
                                       'LineStyle', lnsty(j,:) , ...
                                           'Color', lncol(j,:) );
          end
      end
%
      clear var_x var_y
%      
  end
%
% -------------------------------------------------------------------- %
%  6. Plot F(f)
%
  if strcmp( plot_F1 , 'yes' )
%
      var_x = data(1).freq ;
      XLim_sp1 = [ fr1_min fr1_max ] ;
%
      for j=j1:js:jn
%
          for i=1:ncase
              var_y(:,i) = data(i).spec1(:,j) ;
          end
          norm = max(var_y(:,1)) ;
          var_y = var_y ./ norm ;
%
          figure_F1 = figure ;
          axes_F1 = axes('Parent',figure_F1, 'LineWidth', 2, ...
                         'XLim',XLim_sp1,'XTick',XTick_sp1,...
                         'YLim',[0 1.2],'YTick',[0 0.2 0.4 0.6 0.8 1.0 1.2], ...
                         'Position',Position_spc1,'FontSize',24);
%         axes_F1 = axes('Parent',figure_F1, 'LineWidth', 2, ...
%                        'XLim',XLim_sp1,'XTick',XTick_sp1,...
%                        'YLim',[0 2.4],'YTick',[0 0.4 0.8 1.2 1.6 2.0 2.4], ...
%                        'Position',Position_spc1,'FontSize',24);
          box('on') ; hold('all') ;
          text (fr1_min+0.80.*(fr1_max-fr1_min), ...
                                      -0.18,'{\it{f}} (Hz)','FontSize',24)
          text (fr1_min-0.17.*(fr1_max-fr1_min), ...
                                1.08,'{\it{F }}({\it{f }})','FontSize',24)
          text (fr1_min-0.14.*(fr1_max-fr1_min), ...
                                                 0.91,'(-)','FontSize',24)
          text (fr1_min+0.88.*(fr1_max-fr1_min),1.05,lab_F1,'FontSize',30)
%         text (fr1_min+0.80.*(fr1_max-fr1_min), ...
%                                     -0.36,'{\it{f}} (Hz)','FontSize',24)
%         text (fr1_min-0.17.*(fr1_max-fr1_min), ...
%                               2.16,'{\it{F }}({\it{f }})','FontSize',24)
%         text (fr1_min-0.14.*(fr1_max-fr1_min), ...
%                                                1.82,'(-)','FontSize',24)
%
%         text (fr1_min+0.88.*(fr1_max-fr1_min),2.10,lab_F1,'FontSize',30)
%         text (fr1_min+0.05.*(fr1_max-fr1_min),1.05,lab_F1,'FontSize',30)
%
          plot_F1 = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_F1) ;
          for i=1:ncase
              set ( plot_F1(i) , 'LineWidth', lnwdt(i) , ...
                                 'LineStyle', lnsty(i,:) , ...
                                     'Color', lncol(i,:) );
          end
%
          pname = [ test '_F1_' int2str(j) '.eps' ] ;
          print ('-depsc', pname)
%
          clear var_y
%
      end
%
      clear var_x
%      
  end
%
% -------------------------------------------------------------------- %
%  7. Plot G(f)
%
  if strcmp( plot_G1 , 'yes' )
%
      var_x = data(1).freq ;
      XLim_sp1 = [ fr1_min fr1_max ] ;
%
      for j=j1:js:jn
%          
          for i=1:ncase
              var_y(:,i) = data(i).sspc1(:,j) ;
          end
          norm = max(var_y(:,1)) ;
          var_y = var_y ./ norm ;
%
          figure_G1 = figure ;
          axes_G1 = axes('Parent',figure_G1, 'LineWidth', 2, ...
                         'XLim',XLim_sp1,'XTick',XTick_sp1,...
                         'YLim',[0 1.2],'YTick',[0 0.2 0.4 0.6 0.8 1.0 1.2], ...
                         'Position',Position_spc1,'FontSize',24);
          box('on') ; hold('all') ;
          text (fr1_min+0.80.*(fr1_max-fr1_min), ...
                                      -0.18,'{\it{f}} (Hz)','FontSize',24)
          text (fr1_min-0.17.*(fr1_max-fr1_min), ...
                                1.08,'{\it{G }}({\it{f }})','FontSize',24)
          text (fr1_min-0.14.*(fr1_max-fr1_min), ...
                                                 0.91,'(-)','FontSize',24)
          text (fr1_min+0.88.*(fr1_max-fr1_min),1.05,lab_G1,'FontSize',30)
%         text (fr1_min+0.05.*(fr1_max-fr1_min),1.05,lab_G1,'FontSize',30)
%
          plot_G1 = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_G1) ;
          for i=1:ncase
              set ( plot_G1(i) , 'LineWidth', lnwdt(i) , ...
                                 'LineStyle', lnsty(i,:) , ...
                                     'Color', lncol(i,:) );
          end
%
          pname = [ test '_G1_' int2str(j) '.eps' ] ;
          print ('-depsc', pname)
%
          clear var_y
%
      end
%
      clear var_x      
%      
  end
%
% ------------.-------------------------------------------------------- %
%  8. Plot theta(f)
%
  if strcmp( plot_th1 , 'yes' )
%
      var_x = data(1).freq ;
      XLim_sp1 = [ fr1_min fr1_max ] ;
%
      for j=j1:js:jn
%          
          for i=1:ncase
              var_y(:,i) = data(i).spth1(:,j) ;
          end
          var_y(:,ncase+1) = ones(nfr,1) .* data(1).ud(j) ;
%
          figure_th1 = figure ;
          axes_th1 = axes('Parent',figure_th1, 'LineWidth', 2, ...
                          'XLim',XLim_sp1,'XTick',XTick_sp1,...
                          'YLim',[tmf_min tmf_max],'YTick',YTick_tmf, ...
                          'Position',Position_spc1,'FontSize',24);
          box('on') ; hold('all') ;
          x_ty = tmf_min - 0.15 .* ( tmf_max - tmf_min ) ;
          y_ty0 = tmf_min + 0.92 .* ( tmf_max - tmf_min ) ;
          y_ty1 = tmf_min + 0.87 .* ( tmf_max - tmf_min ) ;
          y_ty2 = tmf_min + 0.62 .* ( tmf_max -tmf_min ) ;
          l_ty = tmf_min + 0.88 .* ( tmf_max -tmf_min ) ;
          text (fr1_min+0.80.*(fr1_max-fr1_min), ...
                                       x_ty,'{\it{f}} (Hz)','FontSize',24)
          text (fr1_min-0.15.*(fr1_max-fr1_min), ...
                                                  y_ty0,'-','FontSize',24)
          text (fr1_min-0.15.*(fr1_max-fr1_min), ...
                                  y_ty1,'\theta({\it{f }})','FontSize',24)
          text (fr1_min-0.14.*(fr1_max-fr1_min), ...
                                               y_ty2,'(^o)','FontSize',24)
          text (fr1_min+0.88.*(fr1_max-fr1_min),l_ty,lab_th1,'FontSize',30)
%         text (fr1_min+0.05.*(fr1_max-fr1_min),l_ty,lab_th1,'FontSize',30)
%
          plot_th1 = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_th1) ;
          for i=1:ncase
              set ( plot_th1(i) , 'LineWidth', lnwdt(i) , ...
                                  'LineStyle', lnsty(i,:) , ...
                                      'Color', lncol(i,:) );
          end
          set(plot_th1(ncase+1),'LineWidth',1,'LineStyle','-.');
%
          pname = [ test '_th1_' int2str(j) '.eps' ] ;
          print ('-depsc', pname)
%
          clear var_y
%
      end
%
      clear var_x
%      
  end
%
% ------------.-------------------------------------------------------- %
%  9. Plot sigma(f)
%
  if strcmp( plot_si1 , 'yes' )
%
      var_x = data(1).freq ;
      XLim_sp1 = [ fr1_min fr1_max ] ;
%
      for j=j1:js:jn
%          
          for i=1:ncase
              var_y(:,i) = data(i).spsi1(:,j) ;
          end
%
          figure_si1 = figure ;
          axes_si1 = axes('Parent',figure_si1, 'LineWidth', 2, ...
                          'XLim',XLim_sp1,'XTick',XTick_sp1,...
                          'YLim',[0 sif_max],'YTick',YTick_sif, ...
                          'Position',Position_spc1,'FontSize',24);
          box('on') ; hold('all') ;
          text (fr1_min+0.80.*(fr1_max-fr1_min), ...
                             -0.15.*sif_max,'{\it{f}} (Hz)','FontSize',24)
          text (fr1_min-0.15.*(fr1_max-fr1_min), ...
                        0.87.*sif_max,'{\sigma}({\it{f }})','FontSize',24)
          text (fr1_min-0.14.*(fr1_max-fr1_min), ...
                                       0.62.*sif_max,'(^o)','FontSize',24)
%         text (fr1_min+0.05.*(fr1_max-fr1_min), ...
          text (fr1_min+0.88.*(fr1_max-fr1_min), ...
                                      0.88.*sif_max,lab_si1,'FontSize',30)
%
          plot_si1 = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_si1) ;
          for i=1:ncase
              set ( plot_si1(i) , 'LineWidth', lnwdt(i) , ...
                                  'LineStyle', lnsty(i,:) , ...
                                      'Color', lncol(i,:) );
          end
% 
          pname = [ test '_si1_' int2str(j) '.eps' ] ;
          print ('-depsc', pname)
%
          clear var_y
%
      end
%
      clear var_x      
%      
  end
%
% ------------.-------------------------------------------------------- %
%  10. Plot s_nl(f)
%
  if strcmp( plot_s1 , 'yes' )
%
      var_x = data(1).freq ;
      XLim_sp1 = [ fr1_min fr1_max ] ;
%
      for j=j1:js:jn
%          
          for i=1:ncase
              var_y(:,i) = data(i).snl1(:,j) ;
          end
          norm = max(abs(var_y(:,1))) ;
          var_y = var_y ./ norm ;
          var_y(:,ncase+1) = zeros(nfr,1) ;
%
          figure_s1 = figure ;
          axes_s1 = axes('Parent',figure_s1, 'LineWidth', 2, ...
                         'XLim',XLim_sp1,'XTick',XTick_sp1,...
                         'YLim',[-1.2 1.2],'YTick', ...
                         [-1.0 -0.5 0 0.5  1.0], ...
                        'Position',Position_spc1,'FontSize',24);
%         axes_s1 = axes('Parent',figure_s1, 'LineWidth', 2, ...
%                        'XLim',XLim_sp1,'XTick',XTick_sp1,...
%                        'YLim',[-3.6 3.6],'YTick', ...
%                        [-3.0 -1.5 0 1.5  3.0], ...
%                        'Position',Position_spc1,'FontSize',24);
          box('on') ; hold('all') ;
          text (fr1_min+0.80.*(fr1_max-fr1_min), ...
                                      -1.56,'{\it{f}} (Hz)','FontSize',24)
          text (fr1_min-0.17.*(fr1_max-fr1_min), ...
                            0.67,'{\it{s_{nl}}}({\it{f }})','FontSize',24)
          text (fr1_min-0.14.*(fr1_max-fr1_min), ...
                                                 0.23,'(-)','FontSize',24)
          text (fr1_min+0.88.*(fr1_max-fr1_min),0.90,lab_s1,'FontSize',30)
%         text (fr1_min+0.80.*(fr1_max-fr1_min), ...
%                                     -4.68,'{\it{f}} (Hz)','FontSize',24)
%         text (fr1_min-0.17.*(fr1_max-fr1_min), ...
%                           2.01,'{\it{s_{nl}}}({\it{f }})','FontSize',24)
%         text (fr1_min-0.14.*(fr1_max-fr1_min), ...
%                                                0.69,'(-)','FontSize',24)
%         text (fr1_min+0.88.*(fr1_max-fr1_min),2.70,lab_s1,'FontSize',30)
%         text (fr1_min+0.05.*(fr1_max-fr1_min),0.90,lab_s1,'FontSize',30)
%
          plot_s1 = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_s1) ;
          for i=1:ncase
              set ( plot_s1(i) , 'LineWidth', lnwdt(i) , ...
                                 'LineStyle', lnsty(i,:) , ...
                                     'Color', lncol(i,:) );
          end
%
          pname = [ test '_s1_' int2str(j) '.eps' ] ;
          print ('-depsc', pname)
%
          clear var_y
%
      end
%
      clear var_x
%      
  end
%
% ------------.-------------------------------------------------------- %
%  11. Plot M(f)
%
  if strcmp( plot_M1 , 'yes' )
%
      var_x = data(1).freq ;
      XLim_sp1 = [ fr1_min fr1_max ] ;
%
      for j=j1:js:jn
%          
          for i=1:ncase
              var_y(:,i) = data(i).fnl1(:,j) ;
          end
          norm = max(abs(var_y(:,1))) ;
          var_y = var_y ./ norm ;
          var_y(:,ncase+1) = zeros(nfr,1) ;
%
          figure_M1 = figure ;
          axes_M1 = axes('Parent',figure_M1, 'LineWidth', 2, ...
                         'XLim',XLim_sp1,'XTick',XTick_sp1,...
                         'YLim',[-1.2 1.2],'YTick', ...
                         [-1.0 -0.5 0 0.5  1.0], ...
                         'Position',Position_spc1,'FontSize',24);
          box('on') ; hold('all') ;
          text (fr1_min+0.80.*(fr1_max-fr1_min), ...
                                      -1.56,'{\it{f}} (Hz)','FontSize',24)
          text (fr1_min-0.17.*(fr1_max-fr1_min), ...
                                0.75,'{\it{M }}({\it{f }})','FontSize',24)
          text (fr1_min-0.14.*(fr1_max-fr1_min), ...
                                                 0.23,'(-)','FontSize',24)
          text (fr1_min+0.88.*(fr1_max-fr1_min),0.90,lab_M1,'FontSize',30)
%         text (fr1_min+0.05.*(fr1_max-fr1_min),0.90,lab_M1,'FontSize',30)
%
          plot_M1 = plot(var_x,var_y,'Color',[0 0 0],'Parent',axes_M1) ;
          for i=1:ncase
              set ( plot_M1(i) , 'LineWidth', lnwdt(i) , ...
                                 'LineStyle', lnsty(i,:) , ...
                                     'Color', lncol(i,:) );
          end
%
          pname = [ test '_M1_' int2str(j) '.eps' ] ;
          print ('-depsc', pname)
%
          clear var_y
%
      end
%
      clear var_x
%      
  end
%
% --------------------------------------------------------------------- %
%  12. Plot F(f,theta)
%
% Later dude !
%
% --------------------------------------------------------------------- %
%  13. Plot F(f,theta)
%
% Later dude !
%
% --------------------------------------------------------------------- %
%  14. End of program
%
% clear ; clc
% close all
%
% - end of makeplots_co_5 --------------------------------------------- %
