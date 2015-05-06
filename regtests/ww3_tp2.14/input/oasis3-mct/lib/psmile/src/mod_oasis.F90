MODULE mod_oasis

! !USES:
  USE mod_oasis_kinds  ,ONLY: ip_single_p
  USE mod_oasis_kinds  ,ONLY: ip_double_p
  USE mod_oasis_kinds  ,ONLY: ip_realwp_p
  USE mod_oasis_kinds  ,ONLY: ll_single
  USE mod_oasis_kinds  ,ONLY: ip_i2_p
  USE mod_oasis_kinds  ,ONLY: ip_i4_p
  USE mod_oasis_kinds  ,ONLY: ip_i8_p
  USE mod_oasis_kinds  ,ONLY: ip_intwp_p

  USE mod_oasis_parameters

  USE mod_oasis_namcouple ,ONLY: namflddti

  USE mod_oasis_method ,ONLY: oasis_init_comp    
  USE mod_oasis_method ,ONLY: oasis_terminate
  USE mod_oasis_method ,ONLY: oasis_get_localcomm
  USE mod_oasis_method ,ONLY: oasis_set_couplcomm
  USE mod_oasis_method ,ONLY: oasis_create_couplcomm
  USE mod_oasis_method ,ONLY: oasis_get_intracomm
  USE mod_oasis_method ,ONLY: oasis_get_intercomm 
  USE mod_oasis_method ,ONLY: oasis_set_debug     
  USE mod_oasis_method ,ONLY: oasis_get_debug     
  USE mod_oasis_method ,ONLY: oasis_enddef        

  USE mod_oasis_part   ,ONLY: oasis_def_partition 
  
  USE mod_oasis_var    ,ONLY: oasis_def_var      
  
  USE mod_oasis_getput_interface ,ONLY: oasis_get 
  USE mod_oasis_getput_interface ,ONLY: oasis_put 
  
  USE mod_oasis_grid   ,ONLY: oasis_start_grids_writing 
  USE mod_oasis_grid   ,ONLY: oasis_write_grid 
  USE mod_oasis_grid   ,ONLY: oasis_write_angle
  USE mod_oasis_grid   ,ONLY: oasis_write_corner        
  USE mod_oasis_grid   ,ONLY: oasis_write_mask          
  USE mod_oasis_grid   ,ONLY: oasis_write_area          
  USE mod_oasis_grid   ,ONLY: oasis_terminate_grids_writing 
  
  USE mod_oasis_sys    ,ONLY: oasis_abort       

  IMPLICIT NONE

!===============================================================================

END MODULE mod_oasis
