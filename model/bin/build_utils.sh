#!/bin/bash -e
# --------------------------------------------------------------------------- #
# build_utils.sh : Shell functions that can be used by multiple scripts       # 
#                  for building the model                                     #
#                                                                             #
# programs used : w3_new  Touches the correct files if compiler switches      #
#                         have been changed.                                  #
#                                                                             #
# error codes : all error output goes directly to screen in w3_make.          #
#                                                                             #
#                                                      Hendrik L. Tolman      #
#                                                      May 2009               #
#                                                      March 2014             #
#                                                                             #
#    Copyright 2009-2014 National Weather Service (NWS),                      #
#       National Oceanic and Atmospheric Administration.  All rights          #
#       reserved.  WAVEWATCH III is a trademark of the NWS.                   #
#       No unauthorized use without permission.                               #
#                                                                             #
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 1. Check switches                                                           #
#    Performs quality check on switches defined in variable $switch. Checks   #
#    to make sure that swtich choices are compatible and that only the valid  #
#    switches have been identified. Groups switched into different variables  #
#    by type                                                                  # 
# --------------------------------------------------------------------------- #



check_switches()
 {

 # 1.a Step through categories 

  for type in nco grib scrip scripnc \
              shared mpp mpiexp thread GSE prop smcg \
              stress s_ln sterm stab s_nl snls s_bot s_db miche s_tr s_bs \
              dstress s_ice s_is reflection \
              wind windx wcor rwind curr currx mgwind mgprop mggse \
              subsec tdyn dss0 pdif tide refrx ig rotag nnt mprf \
              cou oasis agcm ogcm igcm trknc setup pdlib memck uost rstwind b4b
  do

# 1.a.1 Group switches by category

    case $type in
#sort:nco:
      nco    ) TY='upto1'
               ID='NCO modifications'
               TS='NCO'
               OK='NCO' ;;
#sort:grib:
      grib   ) TY='one'
               ID='GRIB package'
               OK='NOGRB NCEP2' ;;
#sort:scrip:
      scrip  ) TY='upto1'
               ID='grid to grid interpolation'
               TS='SCRIP'
               OK='SCRIP' ;;
#sort:scripnc:
      scripnc ) TY='upto1'
               ID='use of netcdf in grid to grid interpolation'
               TS='SCRIPNC'
               OK='SCRIPNC' ;;
#sort:shared:
      shared ) TY='one'
               ID='shared / distributed memory'
               OK='SHRD DIST' ;;
#sort:mpp:
      mpp    ) TY='one'
               ID='message passing protocol'
               OK='SHRD MPI' ;;
#sort:mpiexp:
      mpiexp ) TY='upto1'
               ID='experimental MPI option'
               TS='MPIBDI'
               OK='MPIBDI' ;;
#sort:thread:
      thread ) TY='upto2'
               ID='directive controlled threading'
               TS='OMP'
               OK='OMPG OMPH' ;;
#sort:GSE:
      GSE    ) TY='one'
               ID='GSE aleviation'
               OK='PR0 PR1 PR2 PR3' ;;
#sort:prop:
      prop   ) TY='one'
               ID='propagation scheme'
               OK='PR0 PR1 UQ UNO' ;;
#sort:smcg:
      smcg   ) TY='upto1'
               ID='SMC grid'
               TS='SMC'
               OK='SMC' ;;
#sort:stress:
      stress ) TY='one'
               ID='stress computation'
               OK='FLX0 FLX1 FLX2 FLX3 FLX4 FLX5' ;;
#sort:dstress:
      dstress) TY='upto1'
               ID='Diagnostic stress comp'
               TS='FLD'
               OK='FLD0 FLD1 FLD2' ;;
#sort:s_ln:
      s_ln   ) TY='one'
               ID='linear input'
               OK='LN0 SEED LN1' ;;
#sort:sterm:
      sterm  ) TY='one'
               ID='input/whitecapping'
               OK='ST0 ST1 ST2 ST3 ST4 ST6' ;;
#sort:stab:
      stab   ) TY='upto1'
               ID='stability correction'
               TS='STAB'
               OK='STAB0 STAB2 STAB3' ;;
#sort:s_nl:
      s_nl   ) TY='one'
               ID='quadruplet interactions'
               OK='NL0 NL1 NL2 NL3 NL4 NL5' ;;
#sort:snls:
      snls   ) TY='upto1'
               ID='quadruplet smoother'
               TS='NLS'
               OK='NLS' ;;
#sort:s_bot:
      s_bot  ) TY='one'
               ID='bottom friction'
               OK='BT0 BT1 BT4 BT8 BT9' ;;
#sort:s_db:
      s_db   ) TY='one'
               ID='depth-induced breaking'
               OK='DB0 DB1' ;;
#sort:miche:
      miche  ) TY='upto1'
               ID='Miche style limiter'
               TS='MLIM'
               OK='MLIM' ;;
#sort:s_tr:
      s_tr   ) TY='one'
               ID='triad interactions'
               OK='TR0 TR1' ;;
#sort:s_bs:
      s_bs   ) TY='one'
               ID='bottom scattering'
               OK='BS0 BS1' ;;
#sort:s_ice:
      s_ice  ) TY='one'
               ID='ice sink term'
               OK='IC0 IC1 IC2 IC3 IC4 IC5' ;;
#sort:s_is:
      s_is  ) TY='one'
               ID='ice scattering term'
               OK='IS0 IS1 IS2' ;;
#sort:reflection:
  reflection ) TY='one'
               ID='wave reflections'
               OK='REF0 REF1' ;;
#sort:wind:
      wind   ) TY='one'
               ID='wind interpolation in time'
               OK='WNT0 WNT1 WNT2' ;;
#sort:windx:
      windx  ) TY='one'
               ID='wind interpolation in space'
               OK='WNX0 WNX1 WNX2' ;;
#sort:wcor:
      wcor   ) TY='upto1'
               ID='wind speed correction'
               TS='WCOR'
               OK='WCOR' ;;
#sort:rwind:
      rwind  ) TY='upto1'
               ID='wind vs. current definition'
               TS='RWND'
               OK='RWND' ;;
#sort:rstwind:
      rstwind  ) TY='upto1'
               ID='wind in restart for wmesmf'
               TS='WRST'
               OK='WRST' ;;
#sort:curr:
      curr   ) TY='one'
               ID='current interpolation in time'
               OK='CRT0 CRT1 CRT2' ;;
#sort:currx:
      currx  ) TY='one'
               ID='current interpolation in space'
               OK='CRX0 CRX1 CRX2' ;;
#sort:mgwind:
      mgwind ) TY='upto1'
               ID='moving grid wind correction'
               TS='MGW'
               OK='MGW' ;;
#sort:mgprop:
      mgprop ) TY='upto1'
               ID='moving grid propagation correction'
               TS='MGP'
               OK='MGP' ;;
#sort:mggse:
      mggse  ) TY='upto1'
               ID='moving grid GSE correction'
               TS='MGG'
               OK='MGG' ;;
#sort:subsec:
      subsec ) TY='upto1'
               ID='sub-second time stepping'
               TS='SEC1'
               OK='SEC1' ;;
#sort:tdyn:
      tdyn   ) TY='upto1'
               ID='dynamic diffusion time'
               TS='TDYN'
               OK='TDYN' ;;
#sort:dss0:
      dss0   ) TY='upto1'
               ID='diffusion tensor'
               TS='DSS0'
               OK='DSS0' ;;
#sort:pdif:
      pdif   ) TY='upto1'
               ID='propagation diffusion'
               TS='XW'
               OK='XW0 XW1' ;;
#sort:tide:
      tide   ) TY='upto1'
               ID='use of tidal analysis'
               TS='TIDE'
               OK='TIDE' ;;
#sort:refrx:
      refrx  ) TY='upto1'
               ID='use of spectral refraction @C/@x'
               TS='REFRX'
               OK='REFRX' ;;
#sort:ig:
      ig     ) TY='upto1'
               ID='infragravity waves'
               TS='IG1'
               OK='IG1' ;;
#sort:rotag:
      rotag  ) TY='upto1'
               ID='rotated grid'
               TS='RTD'
               OK='RTD' ;;
#sort:nnt:
      nnt    ) TY='upto1'
               ID='NN training/test data generation'
               TS='NNT'
               OK='NNT' ;;
#sort:mprf:
      mprf   ) TY='upto1'
               ID='multi-grid model profiling'
               TS='MPRF'
               OK='MPRF' ;;
#sort:agcm:
      agcm   ) TY='upto1'
               ID='atmospheric circulation model'
               TS='OASACM'
               OK='OASACM' ;;
#sort:ogcm:
      ogcm   ) TY='upto1'
               ID='ocean circulation model'
               TS='OASOCM'
               OK='OASOCM' ;;
#sort:igcm:
      igcm   ) TY='upto1'
               ID='ice model'
               TS='OASICM'
               OK='OASICM' ;;
#sort:cou:
      cou    ) TY='upto1'
               ID='use of the coupler'
               TS='COU'
               OK='COU' ;;
#sort:oasis:
      oasis  ) TY='upto1'
               ID='type of coupler'
               TS='OASIS'
               OK='OASIS' ;;
#sort:trknc:
      trknc  ) TY='upto1'
               ID='use of netcdf for tracking of wave systems'
               TS='TRKNC'
               OK='TRKNC' ;;
#sort:pdlib:
      pdlib  ) TY='upto1'
               ID='use pdlib'
               TS='PDLIB'
               OK='PDLIB' ;;
#sort:memck: 
      memck  ) TY='upto1'
               ID='check memory use'
               TS='MEMCHECK'
               OK='MEMCHECK' ;;
#sort:setup:
      setup  ) TY='upto1'
               ID='switch to zeta setup'
               TS='SETUP'
               OK='SETUP' ;;
#sort:uost:
      uost   ) TY='upto1'
               ID='unresolved obstacles source term'
               TS='UOST'
               OK='UOST' ;;
#sort:b4b:
      b4b    ) TY='upto1'
               ID='bit-for-bit reproducability'
               TS='B4B'
               OK='B4B' ;;
   esac

# 1.a.2 Check to make sure the correct amount of switches identified per category

    n_found='0'
    s_found=
    for check in $OK
    do
      if [ "`grep $check $switch | wc -w | awk '{print $1}'`" -gt '1' ]
      then
        n_found=$(($n_found + 1))
        s_found="$s_found $check"
      fi
    done

    if [ "$n_found" != '1' ] && [ "$TY" = 'one' ]
    then
      echo ' '
      echo "   *** No valid $ID switch found  ***"
      echo "       valid : $OK"
      echo "       found : $s_found"
      echo ' ' ; exit 1
    fi

    if [ "$n_found" -gt '1' ] && [ "$TY" = 'upto1' ]
    then
      echo ' '
      echo "   *** Too many $ID switches found (max 1) ***"
      echo "       valid : $OK"
      echo "       found : $s_found"
      echo ' ' ; exit 2
    fi

    if [ "$n_found" -gt '2' ] && [ "$TY" = 'upto2' ]
    then
      echo ' '
      echo "   *** Too many $ID switches found (max 2) ***"
      echo "       valid : $OK"
      echo "       found : $s_found"
      echo ' ' ; exit 3
    fi


# 1.a.3 Refresh files where switches have changed

    if [ "$TY" = 'man' ]
    then
      echo "   w3_new $type has to be run manually"
    else
      if [ "$n_found" = '1' ]
      then
        sw="`echo $s_found | awk '{ print $1 }'`"
        if [ "`grep $sw $old_sw | wc -w | awk '{print $1}'`" -lt '1' ]
        then
          $new_sw $type
          echo "   new $ID"
        fi
      else
        if [ "`grep $TS $old_sw | wc -w | awk '{print $1}'`" -gt '1' ]
        then
          $new_sw $type
          echo "   new $ID"
        fi
      fi
    fi

# 1.a.4 Put switch names in category

    if [ "$n_found" = '1' ]
    then
      sw="`echo $s_found | awk '{ print $1 }'`"
    else
      sw=
    fi

    if [ "$type" = 'thread' ]
    then
      sw1="`echo $s_found | awk '{ print $1 }'`"
      sw2="`echo $s_found | awk '{ print $2 }'`"
    fi


    case $type in
      shared ) shared=$sw ;;
      mpp    ) mpp=$sw ;;
      mpiexp ) mpiexp=$sw ;;
      thread ) thread1=$sw1 ; thread2=$sw2 ;;
      GSE    ) g_switch=$sw ;;
      prop   ) p_switch=$sw ;;
      smcg   ) smcg=$sw ;;
      s_ln   ) s_ln=$sw ;;
      sterm  ) s_inds=$sw ;;
      stab   ) stab=$sw ;;
      stress ) stress=$sw ;;
      dstress) dstress=$sw ;;
      scrip  ) scrip=$sw ;;
      scripnc) scripnc=$sw ;;
      s_nl   ) s_nl=$sw ;;
      snls   ) snls=$sw ;;
      s_bot  ) s_bt=$sw ;;
      s_ice  ) s_ic=$sw ;;
      s_is   ) s_is=$sw ;;
      s_db   ) s_db=$sw ;;
      s_tr   ) s_tr=$sw ;;
      s_bs   ) s_bs=$sw ;;
      reflection    ) reflection=$sw ;;
      refrx  ) refrx=$sw ;;
      ig     ) ig=$sw ;;
      tide   ) tide=$sw ;;
      mprf   ) mprf=$sw ;;
      cou    ) cou=$sw ;;
      oasis  ) oasis=$sw ;;
      agcm   ) agcm=$sw ;;
      ogcm   ) ogcm=$sw ;;
      igcm   ) igcm=$sw ;;
      trknc  ) trknc=$sw ;;
      pdlib  ) pdlib=$sw ;;
      memck  ) memck=$sw ;;
      setup  ) setup=$sw ;;
      uost   ) uost=$sw ;;
      b4b    ) b4b=$sw ;;
              *    ) ;;
    esac
  done

# 1.b Check switch compatibility 

  case $stress in
   FLX0) str_st1='no' ; str_st2='no' ; str_st3='OK' ; str_st6='no' ;;
   FLX1) str_st1='OK' ; str_st2='no' ; str_st3='no' ; str_st6='OK' ;;
   FLX2) str_st1='OK' ; str_st2='OK' ; str_st3='no' ; str_st6='OK' ;;
   FLX3) str_st1='OK' ; str_st2='OK' ; str_st3='no' ; str_st6='OK' ;;
   FLX4) str_st1='OK' ; str_st2='no' ; str_st3='no' ; str_st6='OK' ;;
   FLX5) str_st1='no' ; str_st2='no' ; str_st3='yes' ; str_st6='OK' ;;
  esac

  if [ -n "$thread2" ] && [ "$thread1" != 'OMPG' ]
  then
      echo ' '
      echo "   *** !/OMPH has to be used in combination with !/OMPG"
      echo ' ' ; exit 4
  fi

  if [ "$thread2" = 'OMPH' ] && [ "$mpp" != 'MPI' ]
  then
      echo ' '
      echo "   *** !/OMPH has to be used in combination with !/MPI"
      echo ' ' ; exit 5
  fi

  if [ -n "$b4b" ] && [ -z "$thread2" ]
  then
      echo ' '
      echo "   *** !/B4B should be used in combination with !/OMPG or !/OMPH"
      echo ' ' ; exit 7
  fi

  if [ "$stab" = 'STAB2' ] && [ "$s_inds" != 'ST2' ]
  then
      echo ' '
      echo "   *** !/STAB2 has to be used in combination with !/ST2"
      echo ' ' ; exit 8
  fi

  if [ "$stab" = 'STAB3' ] && [ "$s_inds" != 'ST3' ]
  then
      if [ "$s_inds" != 'ST4' ]
      then
        echo ' '
        echo "   *** !/STAB3 has to be used in combination with !/ST3 or !/ST4"
        echo ' ' ; exit 9
      fi
  fi

  if [ "$s_inds" = 'ST1' ] && [ "$str_st1" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST1 cannot be used in combination with !/$stress"
      echo "       Choose from FLX1, FLX2, FLX3, or FLX4."
      echo ' ' ; exit 10
  fi

  if [ "$s_inds" = 'ST2' ] && [ "$str_st2" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST2 cannot be used in combination with !/$stress"
      echo "       Choose from FLX2 or FLX3."
      echo ' ' ; exit 11
  fi

  if [ "$s_inds" = 'ST3' ] && [ "$str_st3" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST3 cannot be used in combination with !/$stress"
      echo "       Stresses embedded in source terms, use FLX0."
      echo ' ' ; exit 12
  fi
  if [ "$s_inds" = 'ST4' ] && [ "$str_st3" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST4 cannot be used in combination with !/$stress"
      echo "       Stresses embedded in source terms, use FLX0."
      echo ' ' ; exit 13
  fi
  if [ "$s_inds" = 'ST6' ] && [ "$str_st6" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST6 cannot be used in combination with !/$stress"
      echo "       Choose from FLX1, FLX2, FLX3, or FLX4 or FLX5."
      echo ' ' ; exit 14
  fi

  if [ "$oasis" = 'OASIS' ] && [ "$str_st3" = 'no' ]
  then
      echo ' '
      echo "   *** !/OASIS cannot be used in combination with !/$stress"
      echo "       Stresses embedded in source terms, use FLX0."
      echo ' ' ; exit 15
  fi

  if [ -n "$thread1" ] && [ "$s_nl" = 'NL2' ]
  then
      echo ' '
      echo "   *** The present version of the WRT interactions"
      echo "       cannot be run under OpenMP (OMPG, OMPH). Use"
      echo "       SHRD or MPI options instead.                    ***"
      echo ' ' ; exit 16
  fi

  if [ "$pdlib" = 'PDLIB' ] && [ "$mpp" != 'MPI' ]
  then 
      echo ' '
      echo "   *** For PDLIB, we need to have MPI as well."
      echo ' ' ; exit 17
  fi 


} #end of check_switches


# --------------------------------------------------------------------------- #
# 2. Get files associated with switches                                       #
#    Use the switches sorted and grouped in check_switches() to get the       #
#    associated files. This function should be called after check_switches()  #
# --------------------------------------------------------------------------- #

switch_files()
{

  case $g_switch in
   PR0) pr=$NULL ;;
   PR1) pr='w3profsmd w3pro1md' ;;
   PR2) pr='w3profsmd w3pro2md' ;;
   PR3) pr='w3profsmd w3pro3md' ;;
  esac 

  case $p_switch in
   UQ ) pr="$pr w3uqckmd" ;;
   UNO) pr="$pr w3uno2md" ;;
  esac 

  smcm=$NULL
  smco=$NULL
  case $smcg in
   SMC) smcm='w3psmcmd'; smco='w3smcomd w3psmcmd' ;;
  esac

  case $uost in
   UOST) uostmd="w3uostmd"
  esac

  case $stress in
   FLX0) flx='w3flx1md'
         flxx=$NULL ;;
   FLX1) flx='w3flx1md'
         flxx=$NULL ;;
   FLX2) flx='w3flx2md'
         flxx=$NULL ;;
   FLX3) flx='w3flx3md'
         flxx=$NULL ;;
   FLX4) flx='w3flx4md'
         flxx=$NULL ;;
   FLX5) flx='w3flx5md'
         flxx=$NULL ;;
  esac

  case $dstress in
   FLD0) ds=$NULL
         dsx=$NULL ;;
   FLD1) ds=$NULL
         dsx='w3fld1md' ;;
   FLD2) ds=$NULL
         dsx='w3fld1md w3fld2md' ;;
  esac

  case $s_ln in
   LN0) ln=$NULL
        lnx=$NULL ;;
   LN1) ln='w3sln1md'
        lnx=$NULL ;;
  esac

  case $s_inds in
   ST0) st='w3src0md'
        stx=$NULL ;;
   ST1) st='w3src1md'
        stx='w3src1md' ;;
   ST2) st='w3src2md'
        stx='w3src2md' ;;
   ST3) st='w3src3md'
        stx='w3src3md' ;;
   ST4) st='w3src4md'
        stx='w3src4md' ;;
   ST6) st='w3src6md w3swldmd'
        stx='w3src6md' ;;
  esac

  case $s_nl in
   NL0) nl=$NULL
        nlx=$NULL ;;
   NL1) nl='w3snl1md'
        nlx='w3snl1md' ;;
   NL2) nl='w3snl2md mod_xnl4v5 serv_xnl4v5 mod_constants mod_fileio'
        nlx="$nl" ;;
   NL3) nl='w3snl3md'
        nlx='w3snl3md' ;;
   NL4) nl='w3snl4md'
        nlx='w3snl4md' ;;
   NL5) nl='w3snl5md w3gkemd'
        nlx="$nl" ;;
  esac

  case $snls in
   NLS) nl="$nl w3snlsmd"
        nlx="$nlx w3snlsmd" ;;
  esac

  case $s_bt in
   BT0) bt=$NULL ;;
   BT1) bt='w3sbt1md' ;;
   BT2) bt='w3sbt2md mod_btffac' ;;
   BT4) bt='w3sbt4md' ;;
   BT8) bt='w3sbt8md' ;;
   BT9) bt='w3sbt9md' ;;
  esac

  case $s_db in
   DB0) db=$NULL
        dbx=$NULL ;;
   DB1) db='w3sdb1md'
        dbx=$NULL ;;
  esac

  case $s_tr in
   TR0) tr=$NULL
        trx=$NULL ;;
   TR1) tr='w3str1md'
        trx=$NULL ;;
  esac

  case $s_bs in
   BS0) bs=$NULL
        bsx=$NULL ;;
   BS1) bs='w3sbs1md'
        bsx=$NULL ;;
  esac

  ic=$NULL
  case $s_ic in
   IC1) ic='w3sic1md' ;;
   IC2) ic='w3sic2md' ;;
   IC3) ic='w3sic3md' ;;
   IC4) ic='w3sic4md' ;;
   IC5) ic='w3sic5md' ;;
  esac

  is=$NULL
  case $s_is in
   IS1) is='w3sis1md' ;;
   IS2) is='w3sis2md' ;;
  esac

  refcode=$NULL
  case $reflection in
   REF1) refcode='w3ref1md'
  esac

  pdlibcode=$NULL 
  pdlibyow=$NULL 
  case $pdlib in
   PDLIB) pdlibcode='yowfunction pdlib_field_vec w3profsmd_pdlib'
          pdlibyow='yowsidepool yowdatapool yowerr yownodepool yowelementpool yowexchangeModule yowrankModule yowpdlibmain yowpd' ;;
  esac

  memcode=$NULL 
  case $memck in 
    MEMCHECK) memcode='w3meminfo' 
  esac

  setupcode=$NULL
  case $setup in
   SETUP) setupcode='w3wavset'
  esac

  tidecode=$NULL
  tideprog=$NULL
  case $tide in
   TIDE) tidecode='w3tidemd'
         tideprog='ww3_prtide'
  esac

  igcode=$NULL
  case $ig in
   IG1) igcode='w3gig1md w3canomd'
  esac

  oasismd=$NULL
  case $oasis in
   OASIS) oasismd='w3oacpmd'
  esac

  agcmmd=$NULL
  case $agcm in
   OASACM) agcmmd='w3agcmmd'
  esac

  ogcmmd=$NULL
  case $ogcm in
   OASOCM) ogcmmd='w3ogcmmd'
  esac

  igcmmd=$NULL
  case $igcm in
   OASICM) igcmmd='w3igcmmd'
  esac

  mprfaux=$NULL
  case $mprf in
    MPRF) mprfaux='w3getmem'
  esac

} #end of switch_files



# --------------------------------------------------------------------------- #
# 3. Create list of files for a prog                                          # 
#    For a particular prog create files and filesl a list of files and a      #
#    list of files to be linked.  The variable prog needs to be set and       #
#    the switch_files should be used first (which requires check_switches()   # 
# --------------------------------------------------------------------------- #

create_file_list() 
{
    case $prog in
      ww3_grid)
               core=
               data='w3wdatmd w3gdatmd w3adatmd w3idatmd w3odatmd wmmdatmd'
               prop=
            sourcet="w3parall w3triamd w3gridmd $stx $flx $nlx $btx $is $uostmd"
                 IO='w3iogrmd'
                aux="constants w3servmd w3arrymd w3dispmd w3gsrumd w3timemd w3nmlgridmd $pdlibyow $memcode"
                if [ "$scrip" = 'SCRIP' ]
                then
                  aux="$aux scrip_constants scrip_grids scrip_iounitsmod"
                  aux="$aux scrip_remap_vars scrip_timers scrip_errormod scrip_interface"
                  aux="$aux scrip_kindsmod scrip_remap_conservative wmscrpmd" 
                fi 
                if [ "$scripnc" = 'SCRIPNC' ]
                then
                  aux="$aux scrip_netcdfmod scrip_remap_write scrip_remap_read"
                fi ;;
      ww3_strt)
               core=
               data="$memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $tr $trx $bt $setupcode $stx $flx $nlx $btx $is wmmdatmd w3parall $uostmd"
                 IO='w3iogrmd w3iorsmd'
                aux="constants w3triamd w3servmd w3arrymd w3dispmd w3gsrumd w3timemd" ;;
      ww3_bound)
               core=
               data="w3adatmd $memcode w3gdatmd w3wdatmd w3idatmd w3odatmd"
               prop="$smcm"
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode $tr $trx $stx $nlx $btx $is wmmdatmd w3parall w3triamd $uostmd"
                 IO='w3iobcmd w3iogrmd w3dispmd w3gsrumd'
                aux="constants w3servmd w3timemd w3arrymd w3cspcmd" ;;
      ww3_bounc)
               core=
               data="w3adatmd $memcode w3gdatmd w3wdatmd w3idatmd w3odatmd"
               prop="$smcm"
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode $stx $nlx $btx $is wmmdatmd w3parall w3triamd $uostmd"
                 IO='w3iobcmd w3iogrmd w3dispmd w3gsrumd'
                aux="constants w3servmd w3arrymd w3timemd w3cspcmd w3nmlbouncmd" ;;
      ww3_prep) 
               core='w3fldsmd'
               data="$memcode w3gdatmd w3adatmd w3idatmd w3odatmd w3wdatmd wmmdatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode w3triamd $stx $flx $nlx $btx  $is $uostmd"
                 IO="w3iogrmd $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3gsrumd w3parall" ;;
      ww3_prnc)
               core='w3fldsmd'
               data="$memcode w3gdatmd w3adatmd w3idatmd w3odatmd w3wdatmd wmmdatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode w3triamd $stx $flx $nlx $btx $is w3parall $uostmd"
                 IO="w3iogrmd $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd w3tidemd w3nmlprncmd" ;;
      ww3_prtide) 
               core='w3fldsmd'
               data="wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop="$pr $smcm"
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode w3triamd $stx $nlx $btx $is w3parall $uostmd"
                 IO="w3iogrmd $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd $tidecode" ;;
      ww3_shel) 
               core='w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd'
               data="wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop="$pr $smcm"
            sourcet="$pdlibcode $setupcode w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $ic"
            sourcet="$sourcet $is $db $tr $bs $refcode $igcode w3parall $uostmd"
                 IO="w3iogrmd w3iogomd w3iopomd w3iotrmd w3iorsmd w3iobcmd $oasismd $agcmmd $ogcmmd $igcmmd"
                 IO="$IO w3iosfmd w3partmd"
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3cspcmd w3gsrumd"
                aux="$aux w3nmlshelmd $pdlibyow" ;;
      ww3_multi|ww3_multi_esmf)
               if [ "$prog" = "ww3_multi" ]
               then
                 core=''
               else
                 core='wmesmfmd'
               fi
               core="$core wminitmd wmwavemd wmfinlmd wmgridmd wmupdtmd wminiomd"
               core="$core w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd"
               data="wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop="$pr $smcm"
            sourcet="$pdlibcode $pdlibyow $setupcode w3parall w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $ic $is $db $tr $bs $refcode $igcode $uostmd"
                 IO='w3iogrmd w3iogomd w3iopomd wmiopomd'
                 IO="$IO w3iotrmd w3iorsmd w3iobcmd w3iosfmd w3partmd $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants $tidecode w3servmd w3timemd w3arrymd w3dispmd w3cspcmd w3gsrumd $mprfaux"
                aux="$aux  wmunitmd w3nmlmultimd" 
                if [ "$scrip" = 'SCRIP' ]
                then
                  aux="$aux scrip_constants scrip_grids scrip_iounitsmod"
                  aux="$aux scrip_remap_vars scrip_timers scrip_errormod scrip_interface"
                  aux="$aux scrip_kindsmod scrip_remap_conservative wmscrpmd"
                fi
                if [ "$scripnc" = 'SCRIPNC' ]
                then
                  aux="$aux scrip_netcdfmod scrip_remap_write scrip_remap_read"
                fi ;;
      ww3_sbs1) 
               core='wminitmd wmwavemd wmfinlmd wmgridmd wmupdtmd wminiomd' 
               core="$core w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd" 
               data="w3parall wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd" 
               prop="$pr $smcm" 
            sourcet="$pdlibcode $pdlibyow w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $db $tr $bs $refcode $igcode $is $ic $uostmd" 
                 IO='w3iogrmd w3iogomd w3iopomd wmiopomd' 
                 IO="$IO w3iotrmd w3iorsmd w3iobcmd w3iosfmd w3partmd $oasismd $agcmmd $ogcmmd $igcmmd" 
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3cspcmd w3gsrumd $mprfaux $tidecode" 
                aux="$aux  wmunitmd w3nmlmultimd"  
                if [ "$scrip" = 'SCRIP' ]
                then
                  aux="$aux scrip_constants scrip_grids scrip_iounitsmod"
                  aux="$aux scrip_remap_vars scrip_timers scrip_errormod scrip_interface"
                  aux="$aux scrip_kindsmod scrip_remap_conservative wmscrpmd"
                fi 
                if [ "$scripnc" = 'SCRIPNC' ]
                then
                  aux="$aux scrip_netcdfmod scrip_remap_write scrip_remap_read"
                fi ;;
      ww3_outf)
               core=
               data="w3parall wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode $tr $trx $stx $flx $nlx $btx $is $uostmd"
                 IO='w3iogrmd w3iogomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux" ;;
      ww3_ounf)
               core='w3initmd'
               data="wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode w3parall w3triamd $stx $flx $nlx $btx  $is $uostmd"
                 IO='w3iogrmd w3iogomd w3iorsmd w3iopomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux w3nmlounfmd $smco w3ounfmetamd w3metamd" ;;
      ww3_outp) 
               core=
               data="wmmdatmd w3parall w3triamd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $setupcode $flx $ln $st $nl $bt $ic $is $db $tr $bs $igcode $uostmd"
                 IO='w3bullmd w3iogrmd w3iopomd w3partmd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd" ;;
      ww3_ounp)
               core=
               data="wmmdatmd w3parall w3triamd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $setupcode $flx $ln $st $nl $bt $ic $is $db $tr $bs $igcode $uostmd"
                 IO='w3bullmd w3iogrmd w3iopomd w3partmd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux w3nmlounpmd" ;;
      ww3_trck) 
               core=
               data="$memcode w3gdatmd w3odatmd"
               prop=
            sourcet=
                 IO=
                aux="constants w3servmd w3timemd w3gsrumd" ;;
      ww3_trnc)
               core=
               data="$memcode w3gdatmd w3odatmd"
               prop=
            sourcet=
                 IO=
                aux="constants w3servmd w3timemd w3gsrumd w3nmltrncmd" ;;
      ww3_grib) 
               core=
               data="w3parall wmmdatmd w3triamd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode $stx $flx $nlx $btx $is $uostmd"
                 IO='w3iogrmd w3iogomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux" ;;
      ww3_gspl) 
               core='w3fldsmd'
               data="$memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode wmmdatmd w3parall w3triamd $stx $flx $nlx $btx $is $uostmd"
                 IO="w3iogrmd  $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd $tidecode" ;;
      ww3_gint) 
               core=
               data="w3parall wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
                 IO='w3iogrmd w3iogomd'
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $st $nl $is $uostmd"
                aux="constants w3triamd w3servmd  w3arrymd w3dispmd w3timemd w3gsrumd"
                aux="$aux" ;;
      gx_outf)
               core=
               data="$memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode wmmdatmd w3parall w3triamd $stx $flx $nlx $btx $tr $bs $is $uostmd"
                 IO='w3iogrmd w3iogomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux" ;;
      gx_outp) 
               core=
               data="$memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode wmmdatmd w3parall w3triamd $ln $flx $st $nlx $btx $tr $bs $is $ic $uostmd"
                 IO='w3iogrmd w3iopomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd" ;;
      ww3_systrk) 
               core='w3strkmd'
               data="$memcode w3gdatmd w3adatmd w3idatmd w3odatmd w3wdatmd"
               prop=
            sourcet="$pdlibcode $pdlibyow $db $bt $setupcode wmmdatmd w3dispmd w3triamd $ln $stx $flx $nlx $btx $tr $bs $is $uostmd"
                 IO=
                aux="constants w3servmd w3timemd w3arrymd w3gsrumd w3parall" ;;
      libww3|libww3.so) 
               core='w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd'
               data='wmmdatmd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop="$pr $smcm"
            sourcet="w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $ic $is $db $tr $bs $refcode $igcode $uostmd"
                 IO='w3iogrmd w3iogomd w3iopomd w3iotrmd w3iorsmd w3iobcmd w3iosfmd w3partmd'
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3cspcmd w3gsrumd" ;;
      ww3_uprstr) 
              core= 
	      data='wmmdatmd w3triamd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd' 
              prop= 
           sourcet="$memcode $pdlibcode $pdlibyow $flx $ln $st $nl $bt $ic $is $db $tr $bs $uostmd"
                IO='w3iogrmd w3iogomd w3iorsmd' 
               aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd" 
               aux="$aux w3parall w3nmluprstrmd" ;; 
    esac

    # if esmf is included in program name or if 
    # the target is compile and create archive
    if [ -n "`echo $prog | grep esmf 2>/dev/null`" ]
    then
      files="$aux $core $data $prop $sourcet $IO"
      filesl="$data $core $prop $sourcet $IO $aux"
    # if program name is libww3, then
    # the target is compile and create archive
    elif [ "$prog" = "libww3" ] ||  [ "$prog" = "libww3.so" ]
    then
      files="$aux $core $data $prop $sourcet $IO"
      filesl="$data $core $prop $sourcet $IO $aux"
    else
      files="$aux $core $data $prop $sourcet $IO $prog"
      filesl="$prog $data $core $prop $sourcet $IO $aux"
    fi
} #end of create_file_list
