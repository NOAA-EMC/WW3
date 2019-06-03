#!/bin/bash -e
# --------------------------------------------------------------------------- #
# make_makefile.sh : Generates the makefile for WAVEWATCH based on switch     #
#                    settings                                                 #
#                    This script is called by w3_make and therefore does      #
#                    not print header information.                            #
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
# 1. Preparations                                                             #
# --------------------------------------------------------------------------- #


# 1.a ID header  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  echo ' '
  echo '                *****************************'
  echo '              ***   WAVEWATCH III makefile  ***'
  echo '                *****************************'
  echo ' '


# 1.b Internal variables - - - - - - - - - - - - - - - - - - - - - - - - - - - -


  # ar_cmd
  os=`uname -s 2>/dev/null`
  if [ "$os" = "AIX" ]
  then
    ar_cmd="ar -X32_64 rv"
  else
    ar_cmd="ar rv"
  fi


# 1.c Create makefile with header  - - - - - - - - - - - - - - - - - - - - - -

  echo '# -------------------------'              > makefile
  echo '# WAVEWATCH III makefile   '             >> makefile
  echo '# -------------------------'             >> makefile


# 1.d Get data from setup file - - - - - - - - - - - - - - - - - - - - - - - - 

  source $(dirname $0)/w3_setenv
  main_dir=$WWATCH3_DIR
  temp_dir=$WWATCH3_TMP
  source=$WWATCH3_SOURCE
  list=$WWATCH3_LIST


# --------------------------------------------------------------------------- #
# 2. Part 1, subroutine dependencies                                          #
# --------------------------------------------------------------------------- #
# 2.a File ID

  cd $temp_dir
  rm -f filelist.tmp

# 2.b Get info from switch file  - - - - - - - - - - - - - - - - - - - - - - -

  if [ -z "$(env | grep switch_file)" ]
  then
    switch_file=$main_dir/bin/switch
  fi

  switch=$switch_file
  old_sw=${switch_file}.old
  new_sw=$main_dir/bin/w3_new

  if [ ! -f $old_sw ]
  then
    old_sw=$switch
    $new_sw all
  fi

  echo switch = $switch

# NOTE: comment line with '#sort:key:" used by sort_switches, including ':'

  for type in mach nco grib mcp c90 nec netcdf scrip scripnc \
              shared mpp mpiexp thread GSE prop \
              stress s_ln source stab s_nl snls s_bot s_db miche s_tr s_bs \
              dstress s_ice s_is reflection s_xx \
              wind windx wcor rwind curr currx mgwind mgprop mggse \
              subsec tdyn dss0 pdif tide refrx ig rotag arctic nnt mprf \
              cou oasis agcm ogcm igcm trknc setup pdlib memck uost
  do
    case $type in
#sort:mach:
      mach   ) TY='one'
               ID='hardware or compiler'
               OK='DUM F90' ;;
#sort:nco:
      nco    ) TY='upto1'
               ID='NCO modifications'
               TS='NCO'
               OK='NCO' ;;
#sort:grib:
      grib   ) TY='one'
               ID='GRIB package'
               OK='NOGRB NCEP1 NCEP2' ;;
#sort:mcp:
      mcp    ) TY='upto1'
               ID='model coupling protocol'
               TS='NCC'
               OK='NCC' ;;
#sort:c90:
      c90    ) TY='upto1'
               ID='Cray C90 compiler directives'
               TS='C90'
               OK='C90' ;;
#sort:nec:
      nec    ) TY='upto1'
               ID='NEC compiler directives'
               TS='NEC'
               OK='NEC' ;;
#sort:netcdf:
      netcdf ) TY='upto1'
               ID='netcdf api type'
               TS='NC4'
               OK='NC4' ;;
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
               OK='OMPG OMPX OMPH' ;;
#sort:GSE:
      GSE    ) TY='one'
               ID='GSE aleviation'
               OK='PR0 PR1 PR2 PR3 PRX SMC' ;;
#sort:prop:
      prop   ) TY='one'
               ID='propagation scheme'
               OK='PR0 PR1 UQ UNO SMC' ;;
#sort:stress:
      stress ) TY='one'
               ID='stress computation'
               OK='FLX0 FLX1 FLX2 FLX3 FLX4 FLXX' ;;
#sort:dstress:
      dstress) TY='upto1'
               ID='Diagnostic stress comp'
               TS='FLD'
               OK='FLD0 FLD1 FLD2' ;;
#sort:s_ln:
      s_ln   ) TY='one'
               ID='linear input'
               OK='LN0 SEED LN1 LNX' ;;
#sort:source:
      source ) TY='one'
               ID='input/whitecapping'
               OK='ST0 ST1 ST2 ST3 ST4 ST6 STX' ;;
#sort:stab:
      stab   ) TY='upto1'
               ID='stability correction'
               TS='STAB'
               OK='STAB0 STAB2 STAB3' ;;
#sort:s_nl:
      s_nl   ) TY='one'
               ID='quadruplet interactions'
               OK='NL0 NL1 NL2 NL3 NL4 NLX' ;;
#sort:snls:
      snls   ) TY='upto1'
               ID='quadruplet smoother'
               TS='NLS'
               OK='NLS' ;;
#sort:s_bot:
      s_bot  ) TY='one'
               ID='bottom friction'
               OK='BT0 BT1 BT4 BT8 BT9 BTX' ;;
#sort:s_db:
      s_db   ) TY='one'
               ID='depth-induced breaking'
               OK='DB0 DB1 DBX' ;;
#sort:miche:
      miche  ) TY='upto1'
               ID='Miche style limiter'
               TS='MLIM'
               OK='MLIM' ;;
#sort:s_tr:
      s_tr   ) TY='one'
               ID='triad interactions'
               OK='TR0 TR1 TRX' ;;
#sort:s_bs:
      s_bs   ) TY='one'
               ID='bottom scattering'
               OK='BS0 BS1 BSX' ;;
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
#sort:s_xx:
      s_xx   ) TY='one'
               ID='arbitrary source'
               OK='XX0 XXX' ;;
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
#sort:arctic:
      arctic ) TY='upto1'
               ID='Arctic grid'
               TS='ARC'
               OK='ARC' ;;
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
   esac

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
      echo ' ' ; exit 3
    fi

    if [ "$n_found" -gt '1' ] && [ "$TY" = 'upto1' ]
    then
      echo ' '
      echo "   *** Too many $ID switches found (max 1) ***"
      echo "       valid : $OK"
      echo "       found : $s_found"
      echo ' ' ; exit 4
    fi

    if [ "$n_found" -gt '2' ] && [ "$TY" = 'upto2' ]
    then
      echo ' '
      echo "   *** Too many $ID switches found (max 2) ***"
      echo "       valid : $OK"
      echo "       found : $s_found"
      echo ' ' ; exit 5
    fi

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
      s_ln   ) s_ln=$sw ;;
      source ) s_inds=$sw ;;
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
      s_xx   ) s_xx=$sw ;;
      reflection    ) reflection=$sw ;;
      refrx  ) refrx=$sw ;;
      ig     ) ig=$sw ;;
      mcp    ) mcp=$sw ;;
      netcdf ) netcdf=$sw;;
      tide   ) tide=$sw ;;
      arctic ) arctic=$sw ;;
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
              *    ) ;;
    esac
  done

  if [ -n "$thread1" ] && [ -z "$thread2" ]
  then
      echo ' '
      echo "   *** !/OMPX or !/OMPH has to be used in combination with !/OMPG"
      echo ' ' ; exit 6
  fi

  if [ -n "$thread2" ] && [ "$thread1" != 'OMPG' ]
  then
      echo ' '
      echo "   *** !/OMPX or !/OMPH has to be used in combination with !/OMPG"
      echo ' ' ; exit 6
  fi

  if [ "$thread2" = 'OMPX' ] && [ "$shared" != 'SHRD' ]
  then
      echo ' '
      echo "   *** !/OMPX has to be used in combination with !/SHRD"
      echo ' ' ; exit 7
  fi

  if [ "$thread2" = 'OMPH' ] && [ "$mpp" != 'MPI' ]
  then
      echo ' '
      echo "   *** !/OMPH has to be used in combination with !/MPI"
      echo ' ' ; exit 8
  fi

  if [ "$arctic" = 'ARC' ] && [ "$p_switch" != 'SMC' ]
  then
      echo ' '
      echo "   *** !/ARC has to be used in combination with !/SMC"
      echo ' ' ; exit 9
  fi

  smco=$NULL
  case $g_switch in
   PR0) pr=$NULL ;;
   PR1) pr='w3profsmd w3pro1md' ;;
   PR2) pr='w3profsmd w3pro2md' ;;
   PR3) pr='w3profsmd w3pro3md' ;;
   SMC) pr='w3psmcmd'; smco='w3smcomd w3psmcmd' ;;
  esac 

  case $p_switch in
   UQ ) pr="$pr w3uqckmd" ;;
   UNO) pr="$pr w3uno2md" ;;
  esac 

  case $uost in
   UOST) uostmd="w3uostmd"
   esac

  case $stress in
   FLX0) str_st1='no' ; str_st2='no' ; str_st3='yes' ; str_st6='no'
         flx='w3flx1md'
         flxx=$NULL ;;
   FLX1) str_st1='OK' ; str_st2='no' ; str_st3='no' ; str_st6='OK'
         flx='w3flx1md'
         flxx=$NULL ;;
   FLX2) str_st1='OK' ; str_st2='OK' ; str_st3='no' ; str_st6='OK'
         flx='w3flx2md'
         flxx=$NULL ;;
   FLX3) str_st1='OK' ; str_st2='OK' ; str_st3='no' ; str_st6='OK'
         flx='w3flx3md'
         flxx=$NULL ;;
   FLX4) str_st1='OK' ; str_st2='no' ; str_st3='no' ; str_st6='OK'
         flx='w3flx4md'
         flxx=$NULL ;;

   FLXX) str_st1='no' ; str_st2='no' ; str_st3='no'
         flx='w3flxxmd'
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
   LNX) ln='w3slnxmd'
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
   STX) st='w3srcxmd'
        stx=$NULL ;;
  esac

  if [ "$stab" = 'STAB2' ] && [ "$s_inds" != 'ST2' ]
  then
      echo ' '
      echo "   *** !/STAB2 has to be used in combination with !/ST2"
      echo ' ' ; exit 10
  fi

  if [ "$stab" = 'STAB3' ] && [ "$s_inds" != 'ST3' ]
  then
      if [ "$s_inds" != 'ST4' ]
      then
        echo ' '
        echo "   *** !/STAB3 has to be used in combination with !/ST3 or !/ST4"
        echo ' ' ; exit 10
      fi
  fi

  if [ "$s_inds" = 'ST1' ] && [ "$str_st1" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST1 cannot be used in combination with !/$stress"
      echo "       Choose from FLX1, FLX2, FLX3, or FLX4."
      echo ' ' ; exit 11
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
      echo ' ' ; exit 11
  fi
  if [ "$s_inds" = 'ST4' ] && [ "$str_st3" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST4 cannot be used in combination with !/$stress"
      echo "       Stresses embedded in source terms, use FLX0."
      echo ' ' ; exit 11
  fi
  if [ "$s_inds" = 'ST6' ] && [ "$str_st6" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST6 cannot be used in combination with !/$stress"
      echo "       Choose from FLX1, FLX2, FLX3, or FLX4."
      echo ' ' ; exit 11
  fi

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
   NLX) nl='w3snlxmd'
        nlx='w3snlxmd' ;;
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
   BTX) bt='w3sbtxmd' ;;
  esac

  case $s_db in
   DB0) db=$NULL
        dbx=$NULL ;;
   DB1) db='w3sdb1md'
        dbx=$NULL ;;
   DBX) db='w3sdbxmd'
        dbx=$NULL ;;
  esac

  case $s_tr in
   TR0) tr=$NULL
        trx=$NULL ;;
   TR1) tr='w3str1md'
        trx=$NULL ;;
   TRX) tr='w3strxmd'
        trx=$NULL ;;
  esac

  case $s_bs in
   BS0) bs=$NULL
        bsx=$NULL ;;
   BS1) bs='w3sbs1md'
        bsx=$NULL ;;
   BSX) bs='w3sbsxmd'
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

  case $s_xx in
   XX0) xx=$NULL
        xxx=$NULL ;;
   XXX) xx='w3sxxxmd'
        xxx=$NULL ;;
  esac

  tidecode=$NULL
  tideprog=$NULL
  case $tide in
   TIDE) tidecode='w3tidemd'
         tideprog='ww3_prtide'
   esac

  case $ig in
   IG1) igcode='w3gig1md w3canomd'
   esac

  oasismd=$NULL
  case $oasis in
   OASIS) oasismd='w3oacpmd'
   esac

  if [ "$oasis" = 'OASIS' ] && [ "$str_st3" = 'no' ]
  then
      echo ' '
      echo "   *** !/OASIS cannot be used in combination with !/$stress"
      echo "       Stresses embedded in source terms, use FLX0."
      echo ' ' ; exit 11
  fi

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

  cplcode=$NULL
  case $mcp in 
   NCC) cplcode='cmp.comm ww.comm'
  esac

  if [ -n "$thread1" ] && [ "$s_nl" = 'NL2' ]
  then
      echo ' '
      echo "   *** The present version of the WRT interactions"
      echo "       cannot be run under OpenMP (OMPG OMPX, OMPH). Use"
      echo "       SHRD or MPI options instead.                    ***"
      echo ' ' ; exit 12
  fi

  if [ "$mprf" = "MPRF" ]
  then
    mprfaux='w3getmem'
  else
    mprfaux=$NULL
  fi

  mpi_mode=no
  if [ -n "`grep MPI $switch`" ]
  then
    mpi_mode=yes
  fi

  if [ "$pdlib" = 'PDLIB' ] && [ "$mpi_mode" = no ]
  then
      echo ' '
      echo "   *** For PDLIB, we need to have MPI as well."
      echo ' ' ; exit 8
  fi


# 2.c Make makefile and file list  - - - - - - - - - - - - - - - - - - - - - -

  echo ' '                                       >> makefile
  echo '# WAVEWATCH III executables'             >> makefile
  echo '# -------------------------'             >> makefile

  progs="ww3_grid ww3_strt ww3_prep ww3_prnc ww3_shel ww3_multi ww3_sbs1
         ww3_outf ww3_outp ww3_trck ww3_trnc ww3_grib gx_outf gx_outp ww3_ounf 
         ww3_ounp ww3_gspl ww3_gint ww3_bound ww3_bounc ww3_systrk $tideprog"
  progs="$progs ww3_multi_esmf  ww3_uprstr"
  progs="$progs libww3"
  progs="$progs libww3.so"

  for prog in $progs
  do
    case $prog in
     ww3_grid) IDstring='Grid preprocessor'
               core=
               data='w3wdatmd w3gdatmd w3adatmd w3idatmd w3odatmd wmmdatmd'
               prop=
             source="w3parall w3triamd $stx $nlx $btx $is $uostmd"
                 IO='w3iogrmd'
                aux="constants w3servmd w3arrymd w3dispmd w3gsrumd w3timemd w3nmlgridmd $pdlibyow $memcode" ;;
     ww3_strt) IDstring='Initial conditions program'
               core=
               data="$memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $tr $trx $bt $setupcode $stx $nlx $btx $is wmmdatmd w3parall $uostmd"
                 IO='w3iogrmd w3iorsmd'
                aux="constants w3triamd w3servmd w3arrymd w3dispmd w3gsrumd w3timemd" ;;
     ww3_bound) IDstring='boundary conditions program'
               core=
               data="w3adatmd $memcode w3gdatmd w3wdatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode $tr $trx $stx $nlx $btx $is wmmdatmd w3parall w3triamd $uostmd"
                 IO='w3iobcmd w3iogrmd w3dispmd w3gsrumd'
                aux="constants w3servmd w3timemd w3arrymd w3cspcmd" ;;
     ww3_bounc) IDstring='NetCDF boundary conditions program'
               core=
               data="w3adatmd $memcode w3gdatmd w3wdatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode $stx $nlx $btx $is wmmdatmd w3parall w3triamd $uostmd"
                 IO='w3iobcmd w3iogrmd w3dispmd w3gsrumd'
                aux="constants w3servmd w3arrymd w3timemd w3cspcmd w3nmlbouncmd" ;;
     ww3_prep) IDstring='Field preprocessor'
               core='w3fldsmd'
               data="$memcode w3gdatmd w3adatmd w3idatmd w3odatmd w3wdatmd wmmdatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode w3triamd $stx $nlx $btx  $is $uostmd"
                 IO="w3iogrmd $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3gsrumd w3parall" ;;
     ww3_prnc) IDstring='NetCDF field preprocessor'
               core='w3fldsmd'
               data="$memcode w3gdatmd w3adatmd w3idatmd w3odatmd w3wdatmd wmmdatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode w3triamd $stx $nlx $btx $is w3parall $uostmd"
                 IO="w3iogrmd $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd $tidecode w3nmlprncmd" ;;
    ww3_prtide) IDstring='Tide prediction'
               core='w3fldsmd'
               data="wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop="$pr"
             source="$pdlibcode $pdlibyow $db $bt $setupcode w3triamd $stx $nlx $btx $is w3parall $uostmd"
                 IO="w3iogrmd $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd $tidecode" ;;
     ww3_shel) IDstring='Generic shell'
               core='w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd'
               data="wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop="$pr"
             source="$pdlibcode $setupcode w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $ic"
             source="$source $is $db $tr $bs $xx $refcode $igcode w3parall $uostmd"
                 IO="w3iogrmd w3iogomd w3iopomd w3iotrmd w3iorsmd w3iobcmd $oasismd $agcmmd $ogcmmd $igcmmd"
                 IO="$IO w3iosfmd w3partmd"
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3cspcmd w3gsrumd $cplcode"
                aux="$aux w3nmlshelmd $pdlibyow" ;;
    ww3_multi|ww3_multi_esmf)
               if [ "$prog" = "ww3_multi" ]
               then
                 IDstring='Multi-grid shell'
                 core=''
               else
                 IDstring='Multi-grid ESMF module'
                 core='wmesmfmd'
               fi
               core="$core wminitmd wmwavemd wmfinlmd wmgridmd wmupdtmd wminiomd"
               core="$core w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd"
               data="wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop="$pr"
             source="$pdlibcode $pdlibyow $setupcode w3parall w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $ic $is $db $tr $bs $xx $refcode $igcode $uostmd"
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
   ww3_sbs1) IDstring='Multi-grid shell sbs version' 
               core='wminitmd wmwavemd wmfinlmd wmgridmd wmupdtmd wminiomd' 
               core="$core w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd" 
               data="w3parall wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd" 
               prop="$pr" 
               source="$pdlibcode $pdlibyow w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $db $tr $bs $xx $refcode $igcode $is $ic $uostmd" 
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
     ww3_outf) IDstring='Gridded output'
               core=
               data="w3parall wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode $tr $trx $stx $nlx $btx $is $uostmd"
                 IO='w3iogrmd w3iogomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux" ;;
     ww3_ounf) IDstring='Gridded NetCDF output'
               core='w3initmd'
               data="wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode w3parall w3triamd $stx $nlx $btx  $is $uostmd"
                 IO='w3iogrmd w3iogomd w3iorsmd w3iopomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux w3nmlounfmd $smco" ;;
     ww3_outp) IDstring='Point output'
               core=
               data="wmmdatmd w3parall w3triamd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $setupcode $flx $ln $st $nl $bt $ic $is $db $tr $bs $xx $igcode $uostmd"
                 IO='w3bullmd w3iogrmd w3iopomd w3partmd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd" ;;
     ww3_ounp) IDstring='Point NetCDF output'
               core=
               data="wmmdatmd w3parall w3triamd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $setupcode $flx $ln $st $nl $bt $ic $is $db $tr $bs $xx $igcode $uostmd"
                 IO='w3bullmd w3iogrmd w3iopomd w3partmd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux w3nmlounpmd" ;;
     ww3_trck) IDstring='Track output post'
               core=
               data="$memcode w3gdatmd w3odatmd"
               prop=
             source=
                 IO=
                aux="constants w3servmd w3timemd w3gsrumd" ;;
     ww3_trnc) IDstring='Track NetCDF output post'
               core=
               data="$memcode w3gdatmd w3odatmd"
               prop=
             source=
                 IO=
                aux="constants w3servmd w3timemd w3gsrumd w3nmltrncmd" ;;
     ww3_grib) IDstring='Gridded output (GRIB)'
               core=
               data="w3parall wmmdatmd w3triamd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode $stx $nlx $btx $is $uostmd"
                 IO='w3iogrmd w3iogomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux" ;;
     ww3_gspl) IDstring='Grid splitting'
               core='w3fldsmd'
               data="$memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode wmmdatmd w3parall w3triamd $stx $nlx $btx $is $uostmd"
                 IO="w3iogrmd  $oasismd $agcmmd $ogcmmd $igcmmd"
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd $tidecode" ;;
     ww3_gint) IDstring='Grid Interpolation'
               core=
               data="w3parall wmmdatmd $memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
                 IO='w3iogrmd w3iogomd'
               prop=
             source="$pdlibcode $pdlibyow $db $bt $st $nl $is $uostmd"
                aux="constants w3triamd w3servmd  w3arrymd w3dispmd w3timemd w3gsrumd"
                aux="$aux" ;;
      gx_outf) IDstring='GrADS input file generation (gridded fields)'
               core=
               data="$memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode wmmdatmd w3parall w3triamd $stx $nlx $btx $tr $bs $xx $is $uostmd"
                 IO='w3iogrmd w3iogomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd"
                aux="$aux" ;;
      gx_outp) IDstring='GrADS input file generation for point output'
               core=
               data="$memcode w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode wmmdatmd w3parall w3triamd $ln $flx $st $nlx $btx $tr $bs $xx $is $ic $uostmd"
                 IO='w3iogrmd w3iopomd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd" ;;
      ww3_systrk) IDstring='Wave system tracking postprocessor'
               core='w3strkmd'
               data="$memcode w3gdatmd w3adatmd w3idatmd w3odatmd w3wdatmd"
               prop=
             source="$pdlibcode $pdlibyow $db $bt $setupcode wmmdatmd w3dispmd w3triamd $ln $stx $nlx $btx $tr $bs $xx $is $uostmd"
                 IO=
                aux="constants w3servmd w3timemd w3arrymd w3gsrumd w3parall" ;;
     libww3) IDstring='Object file archive'
               core='w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd'
               data='wmmdatmd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop="$pr"
             source="w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $ic $is $db $tr $bs $xx $refcode $igcode $uostmd"
                 IO='w3iogrmd w3iogomd w3iopomd w3iotrmd w3iorsmd w3iobcmd w3iosfmd w3partmd'
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3cspcmd w3gsrumd" ;;
     libww3.so) IDstring='Object file archive'
               core='w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd'
               data='wmmdatmd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop="$pr"
             source="w3triamd w3srcemd $dsx $flx $ln $st $nl $bt $ic $is $db $tr $bs $xx $refcode $igcode $uostmd"
                 IO='w3iogrmd w3iogomd w3iopomd w3iotrmd w3iorsmd w3iobcmd w3iosfmd w3partmd'
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3cspcmd w3gsrumd" ;;  
     ww3_uprstr) IDstring='Update Restart File' 
              core= 
	          data='wmmdatmd w3triamd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd' 
              prop= 
            source="$memcode $pdlibcode $pdlibyow $flx $ln $st $nl $bt $ic $is $db $tr $bs $xx $uostmd"
                IO='w3iogrmd w3iogomd w3iorsmd' 
               aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd" 
               aux="$aux w3parall" ;; 
    esac

    # if esmf is included in program name, then
    # the target is compile and create archive
    if [ -n "`echo $prog | grep esmf 2>/dev/null`" ]
    then
      d_string="$prog"' : $(aPo)/'
      files="$aux $core $data $prop $source $IO"
      filesl="$data $core $prop $source $IO $aux"
    # if program name is libww3, then
    # the target is compile and create archive
    elif [ "$prog" = "libww3" ] ||  [ "$prog" = "libww3.so" ]
    then
      d_string="$prog"' : $(aPo)/'
      files="$aux $core $data $prop $source $IO"
      filesl="$data $core $prop $source $IO $aux"
    else
      d_string='$(aPe)/'"$prog"' : $(aPo)/'
      files="$aux $core $data $prop $source $IO $prog"
      filesl="$prog $data $core $prop $source $IO $aux"
    fi
    echo "# $IDstring"                           >> makefile
    echo ' '                                     >> makefile
    for file in $files
    do
      echo "$d_string$file.o"                    >> makefile
      if [ -z "`echo $file | grep scrip 2>/dev/null`" ]
      then
        echo "$file"                             >> filelist.tmp
      fi
    done

    # if esmf is included in program name, then
    # the target is compile and create archive
    if [ -n "`echo $prog | grep esmf 2>/dev/null`" ]
    then
      lib=lib$prog.a
      objs=""
      for file in $filesl
      do
        objs="$objs $file.o"
      done
      echo "	@cd \$(aPo); $ar_cmd $lib $objs" >> makefile
      echo ' '                                   >> makefile
    # if program name is libww3, then
    # the target is compile and create archive
    elif [ "$prog" = "libww3" ]
    then
      lib=$prog.a
      objs=""
      for file in $filesl
      do
        objs="$objs $file.o"
      done
      echo "	@cd \$(aPo); $ar_cmd $lib $objs" >> makefile
      echo ' '                                   >> makefile
    # if program name is libww3.so, then
    # the target is compile and create archive
    elif [ "$prog" = "libww3.so" ]
    then
      lib=$prog
      objs=""
      for file in $filesl
      do
        objs="$objs $file.o"
      done
      echo "	@cd \$(aPo); ld -o $lib -shared $objs" >> makefile
      echo ' '                                   >> makefile
      
    else
      echo '	@$(aPb)/link '"$filesl"          >> makefile
      echo ' '                                   >> makefile
    fi

  done

  sort -u filelist.tmp                            > filelist
  rm -f filelist.tmp

# --------------------------------------------------------------------------- #
# 3. Part 2, includes in subroutines                                          #
# --------------------------------------------------------------------------- #
# 3.a File ID

  echo '   Checking all subroutines for modules (this may take a while) ...'

  echo ' '                                       >> makefile
  echo '# WAVEWATCH III subroutines'             >> makefile
  echo '# -------------------------'             >> makefile
  echo ' '                                       >> makefile

# 3.b Loop over files

  for file in `cat filelist`
  do

    suffixes="ftn f F f90 F90 c"
    fexti=none
    ispdlibi=no 
    for s in $suffixes
    do
      if [ -f $main_dir/ftn/$file.$s ]
      then
        fexti=$s
        break
      fi
      if [ -f $main_dir/ftn/PDLIB/$file.$s ]
      then
        fexti=$s
        ispdlibi=yes
        break
      fi
    done
    if [ "$fexti" = 'none' ]
    then
      echo '      *** make_makefile.sh error ***'
      echo "          Source file $main_dir/ftn/$file.* "
      echo "                   or $main_dir/ftn/PDLIB/$file.* not found"
      echo "          Source file suffixes checked: $suffixes"
      exit 2
    fi
    if [ "$fexti" = 'ftn' ]
    then
      fexto=F90
    else
      fexto=$fexti
    fi

    string1='$(aPo)/'$file'.o : '$file.$fexti' 'w3macros.h' '
    string2='	@$(aPb)/ad3'" $file"
    string3="$NULL"

    if [ "$ispdlibi" = 'yes' ]
    then 
      string1='$(aPo)/'$file'.o : PDLIB/'$file.$fexti' '
    fi 

    $main_dir/bin/ad3 $file 0 1 > ad3.out 2>&1

    if [ -n "`grep error ad3.out`" ]
    then
      cat ad3.out
      exit 20
    fi
    rm -f ad3.out

    if [ "$fexto" = 'c' ]
    then
      touch check_file
    else
      check_file=`grep -i '^[[:blank:]]*use' $file.$fexto | awk '{print toupper($2)}' | \
                     sed -e 's/,//' | sort -u`
    fi
    rm -f $file.$fexto

      for mod in $check_file
      do
      modfound=yes
      case $mod in
         'W3UOSTMD'     ) modtest=w3uostmd.o ;;
         'W3INITMD'     ) modtest=w3initmd.o ;;
         'W3WAVEMD'     ) modtest=w3wavemd.o ;;
         'W3WDASMD'     ) modtest=w3wdasmd.o ;;
         'W3UPDTMD'     ) modtest=w3updtmd.o ;;
         'W3FLDSMD'     ) modtest=w3fldsmd.o ;;
         'W3CSPCMD'     ) modtest=w3cspcmd.o ;;
         'MALLOCINFO_M' ) modtest=w3meminfo.o ;;
         'W3GDATMD'     ) modtest=w3gdatmd.o ;;
         'W3WDATMD'     ) modtest=w3wdatmd.o ;;
         'W3ADATMD'     ) modtest=w3adatmd.o ;;
         'W3ODATMD'     ) modtest=w3odatmd.o ;;
         'W3IDATMD'     ) modtest=w3idatmd.o ;;
         'W3FLD1MD'     ) modtest=w3fld1md.o ;;
         'W3FLD2MD'     ) modtest=w3fld2md.o ;;
         'W3IOGRMD'     ) modtest=w3iogrmd.o ;;
         'W3IOGOMD'     ) modtest=w3iogomd.o ;;
         'W3IOPOMD'     ) modtest=w3iopomd.o ;;
         'W3IOTRMD'     ) modtest=w3iotrmd.o ;;
         'W3IORSMD'     ) modtest=w3iorsmd.o ;;
         'W3IOBCMD'     ) modtest=w3iobcmd.o ;;
         'W3IOSFMD'     ) modtest=w3iosfmd.o ;;
         'W3PARTMD'     ) modtest=w3partmd.o ;;
         'W3BULLMD'     ) modtest=w3bullmd.o ;;
         'W3TIDEMD'     ) modtest=w3tidemd.o ;;
         'W3CANOMD'     ) modtest=w3canomd.o ;;
         'W3GIG1MD'     ) modtest=w3gig1md.o ;;
         'W3STRKMD'     ) modtest=w3strkmd.o ;;
         'W3PRO1MD'     ) modtest=w3pro1md.o ;;
         'W3PRO2MD'     ) modtest=w3pro2md.o ;;
         'W3PRO3MD'     ) modtest=w3pro3md.o ;;
         'W3PROXMD'     ) modtest=w3proxmd.o ;;
         'W3UQCKMD'     ) modtest=w3uqckmd.o ;;
         'W3UNO2MD'     ) modtest=w3uno2md.o ;;
         'W3PSMCMD'     ) modtest=w3psmcmd.o ;;
         'W3PROFSMD'    ) modtest=w3profsmd.o ;;
         'W3SRCEMD'     ) modtest=w3srcemd.o ;;
         'W3FLX1MD'     ) modtest=w3flx1md.o ;;
         'W3FLX2MD'     ) modtest=w3flx2md.o ;;
         'W3FLX3MD'     ) modtest=w3flx3md.o ;;
         'W3FLX4MD'     ) modtest=w3flx4md.o ;;
         'W3FLXXMD'     ) modtest=w3flxxmd.o ;;
         'W3SLN1MD'     ) modtest=w3sln1md.o ;;
         'W3SLNXMD'     ) modtest=w3slnxmd.o ;;
         'W3SRC0MD'     ) modtest=w3src0md.o ;;
         'W3SRC1MD'     ) modtest=w3src1md.o ;;
         'W3SRC2MD'     ) modtest=w3src2md.o ;;
         'W3SRC3MD'     ) modtest=w3src3md.o ;;
         'W3SRC4MD'     ) modtest=w3src4md.o ;;
         'W3SRC6MD'     ) modtest=w3src6md.o ;;
         'W3SRCXMD'     ) modtest=w3srcxmd.o ;;
         'W3SNL1MD'     ) modtest=w3snl1md.o ;;
         'W3SNL2MD'     ) modtest=w3snl2md.o ;;
         'W3SNL3MD'     ) modtest=w3snl3md.o ;;
         'W3SNL4MD'     ) modtest=w3snl4md.o ;;
         'W3SNLXMD'     ) modtest=w3snlxmd.o ;;
         'W3SNLSMD'     ) modtest=w3snlsmd.o ;;
         'M_XNLDATA'    ) modtest=mod_xnl4v5.o ;;
         'SERV_XNL4V5'  ) modtest=serv_xnl4v5.o ;;
         'M_FILEIO'     ) modtest=mod_fileio.o ;;
         'M_CONSTANTS'  ) modtest=mod_constants.o ;;
         'W3SWLDMD'     ) modtest=w3swldmd.o ;;
         'W3SBT1MD'     ) modtest=w3sbt1md.o ;;
         'W3SBT4MD'     ) modtest=w3sbt4md.o ;;
         'W3SBT8MD'     ) modtest=w3sbt8md.o ;;
         'W3SBT9MD'     ) modtest=w3sbt9md.o ;;
         'W3SBTXMD'     ) modtest=w3sbtxmd.o ;;
         'W3SDB1MD'     ) modtest=w3sdb1md.o ;;
         'W3SDBXMD'     ) modtest=w3sdbxmd.o ;;
         'W3STR1MD'     ) modtest=w3str1md.o ;;
         'W3STRXMD'     ) modtest=w3strxmd.o ;;
         'W3SBS1MD'     ) modtest=w3sbs1md.o ;;
         'W3SBSXMD'     ) modtest=w3sbsxmd.o ;;
         'W3SIC1MD'     ) modtest=w3sic1md.o ;;
         'W3SIC2MD'     ) modtest=w3sic2md.o ;;
         'W3SIC3MD'     ) modtest=w3sic3md.o ;;
         'W3SIC4MD'     ) modtest=w3sic4md.o ;;
         'W3SIC5MD'     ) modtest=w3sic5md.o ;;
         'W3SIS1MD'     ) modtest=w3sis1md.o ;;
         'W3SIS2MD'     ) modtest=w3sis2md.o ;;
         'W3REF1MD'     ) modtest=w3ref1md.o ;;
         'W3SXXXMD'     ) modtest=w3sxxxmd.o ;;
         'CONSTANTS'    ) modtest=constants.o ;;
         'W3SERVMD'     ) modtest=w3servmd.o ;;
         'W3TIMEMD'     ) modtest=w3timemd.o ;;
         'W3ARRYMD'     ) modtest=w3arrymd.o ;;
         'W3DISPMD'     ) modtest=w3dispmd.o ;;
         'W3GSRUMD'     ) modtest=w3gsrumd.o ;;
         'W3TRIAMD'     ) modtest=w3triamd.o ;;
         'WMINITMD'     ) modtest=wminitmd.o ;;
         'WMWAVEMD'     ) modtest=wmwavemd.o ;;
         'WMFINLMD'     ) modtest=wmfinlmd.o ;;
         'WMMDATMD'     ) modtest=wmmdatmd.o ;;
         'WMGRIDMD'     ) modtest=wmgridmd.o ;;
         'WMUPDTMD'     ) modtest=wmupdtmd.o ;;
         'WMINIOMD'     ) modtest=wminiomd.o ;;
         'WMUNITMD'     ) modtest=wmunitmd.o ;;
         'WMIOPOMD'     ) modtest=wmiopomd.o ;;
         'WMSCRPMD'     ) modtest=wmscrpmd.o ;;
         'WMESMFMD'     ) modtest=wmesmfmd.o ;;
         'W3GETMEM'     ) modtest=w3getmem.o ;;
         'WW_CC'        ) modtest=ww.comm.o  ;;
         'CMP_COMM'     ) modtest=cmp.comm.o  ;;
         'W3OACPMD'     ) modtest=w3oacpmd.o ;;
         'W3AGCMMD'     ) modtest=w3agcmmd.o ;;
         'W3OGCMMD'     ) modtest=w3ogcmmd.o ;;
         'W3IGCMMD'     ) modtest=w3igcmmd.o ;;
         'W3NMLMULTIMD' ) modtest=w3nmlmultimd.o ;;
         'W3NMLPRNCMD'  ) modtest=w3nmlprncmd.o ;;
         'W3NMLOUNFMD'  ) modtest=w3nmlounfmd.o ;;
         'W3NMLOUNPMD'  ) modtest=w3nmlounpmd.o ;;
         'W3NMLTRNCMD'  ) modtest=w3nmltrncmd.o ;;
         'W3NMLBOUNCMD' ) modtest=w3nmlbouncmd.o ;;
         'W3NMLSHELMD'  ) modtest=w3nmlshelmd.o ;;
         'W3NMLGRIDMD'  ) modtest=w3nmlgridmd.o ;;
         'W3NETCDF'     ) modtest=w3netcdf.o ;;
         'YOWFUNCTION'  ) modtest=yowfunction.o ;;
         'YOWDATAPOOL'  ) modtest=yowdatapool.o ;;
         'YOWNODEPOOL'  ) modtest=yownodepool.o ;;
         'YOWSIDEPOOL'  ) modtest=yowsidepool.o ;;
         'YOWRANKMODULE'     ) modtest=yowrankModule.o ;;
         'YOWERR'            ) modtest=yowerr.o ;;
         'YOWELEMENTPOOL'    ) modtest=yowelementpool.o ;;
         'YOWEXCHANGEMODULE' ) modtest=yowexchangeModule.o ;;
         'YOWPDLIBMAIN'      ) modtest=yowpdlibmain.o ;;
         'PDLIB_FIELD_VEC'   ) modtest=pdlib_field_vec.o ;;
         'PDLIB_W3PROFSMD'   ) modtest=w3profsmd_pdlib.o ;;
         'W3PARALL'     ) modtest=w3parall.o ;;
         'W3SMCOMD'     ) modtest=w3smcomd.o ;;
         *              ) modfound=no ;; 
      esac

      if [ "$modfound" == "yes" ]
      then
        if [ "$modtest" != "$file.o" ]
        then
          string3="$string3 "'$(aPo)/'"$modtest"
        fi
      fi
    done
    rm -f check_file

    string_scripnc='$(aPo)/scrip_netcdfmod.o $(aPo)/scrip_remap_write.o $(aPo)/scrip_remap_read.o'
    if  [ "$scrip" = 'SCRIP' ] && [ "$file" = 'wmscrpmd' ]
    then
       S_string2='$(aPo)/scrip_constants.o $(aPo)/scrip_grids.o $(aPo)/scrip_iounitsmod.o  $(aPo)/scrip_remap_vars.o $(aPo)/scrip_timers.o $(aPo)/scrip_errormod.o $(aPo)/scrip_interface.o $(aPo)/scrip_kindsmod.o $(aPo)/scrip_remap_conservative.o'
       if  [ "$scripnc" = 'SCRIPNC' ]
       then
           S_string2="$S_string2 $string_scripnc"
       fi
       string3="$string3 $S_string2"
    fi

    if  [ "$scrip" = 'SCRIP' ] && [ "$file" = 'wmgridmd' ]
    then
        S_string2='$(aPo)/scrip_constants.o $(aPo)/scrip_grids.o $(aPo)/scrip_iounitsmod.o  $(aPo)/scrip_remap_vars.o $(aPo)/scrip_timers.o $(aPo)/scrip_errormod.o $(aPo)/scrip_interface.o $(aPo)/scrip_kindsmod.o $(aPo)/scrip_remap_conservative.o'
       if  [ "$scripnc" = 'SCRIPNC' ]
       then
           S_string2="$S_string2 $string_scripnc"
       fi
        string3="$string3 $S_string2"
    fi

    echo "$string1$string3"                      >> makefile
    echo "$string2"                              >> makefile
    echo ' '                                     >> makefile

  done

  echo '# end of WAVEWATCH III subroutines'      >> makefile
  rm -f filelist

  if  [ "$scrip" = 'SCRIP' ]
  then
    echo ' '                                       >> makefile
    echo ' '                                       >> makefile
    echo '# SCRIP subroutines'                     >> makefile
    echo '# -----------------'                     >> makefile
    echo ' '                                       >> makefile

    scrip_dir=$main_dir/ftn/SCRIP
    if [ ! -d $scrip_dir ]
    then
      echo "*** SCRIP directory $scrip_dir not found ***"
      exit 21
    fi

    if  [ "$scripnc" = 'SCRIPNC' ]
    then 
       scrip_mk=$scrip_dir/SCRIP_NC.mk
    else 
       scrip_mk=$scrip_dir/SCRIP.mk
    fi
    if [ ! -e $scrip_mk ]
    then
      echo "*** SCRIP makefile fragment $scrip_mk not found ***"
      exit 22
    fi

    cat $scrip_mk >> makefile

    echo '# end of SCRIP subroutines'              >> makefile
  fi

# --------------------------------------------------------------------------- #
# 4. Move makefile to proper place                                            #
# --------------------------------------------------------------------------- #

  mv makefile $main_dir/ftn/makefile

# end of script ------------------------------------------------------------- #
