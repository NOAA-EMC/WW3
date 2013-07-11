#!/bin/sh
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
#                                                      Sep 2012               #
#                                                                             #
#    Copyright 2009-2012 National Weather Service (NWS),                      #
#       National Oceanic and Atmospheric Administration.  All rights          #
#       reserved.  WAVEWATCH III is a trademark of the NWS.                   #
#       No unauthorized use without permission.                               #
#                                                                             #

# --------------------------------------------------------------------------- #
# 1. Preparations                                                             #
# --------------------------------------------------------------------------- #
# 1.a Internal variables

  ww3_env="${HOME}/.wwatch3.env"                           # setup file
  if [ ${WWATCH3_ENV} ]; then ww3_env="${WWATCH3_ENV}"; fi # alternate setup file

# 1.b ID header  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 1.c Read data from the environment file  - - - - - - - - - - - - - - - - - -

  if [ ${WWATCH3_DIR} ] && [ ${WWATCH3_TMP} ]
  then
    main_dir=${WWATCH3_DIR}
    temp_dir=${WWATCH3_TMP}
  elif [ -f $ww3_env ]
  then
    set `grep WWATCH3_DIR $ww3_env` ; shift
    main_dir="$*"
    set `grep WWATCH3_TMP $ww3_env` ; shift
    temp_dir="$*"
  else
     "*** Set-up file $ww3_env not found ***"
    exit 1
  fi

# 1.d Check / make directories   - - - - - - - - - - - - - - - - - - - - - - -

  if [ -d $main_dir ]
  then
    cd $main_dir
  else
    echo "*** Directory $main_dir not found ***"
    exit 2
  fi

  if [ ! -d $temp_dir ]
  then
    mkdir $temp_dir
  fi
  cd $temp_dir

# --------------------------------------------------------------------------- #
# 2. Part 1, subroutine dependencies                                          #
# --------------------------------------------------------------------------- #
# 2.a File ID

  echo ' '                                        > makefile
  echo '# WAVEWATCH III executables'             >> makefile
  echo '# -------------------------'             >> makefile

  rm -f filelist.tmp
  nr_thr=0

# 2.b Get info from switch file  - - - - - - - - - - - - - - - - - - - - - - -

  switch=$main_dir/bin/switch
  old_sw=$main_dir/bin/switch.old
  new_sw=$main_dir/bin/w3_new

  if [ ! -f $old_sw ]
  then
    old_sw=$switch
    $new_sw all
  fi

  for type in mach nco grib shared mpp thr0 thr1 c90 nec lrecl grid \
              prop stress s_ln source stab s_nl snls s_bot s_db s_tr s_bs s_xx \
              wind windx rwind curr currx tdyn dss0 pdif miche \
              mgwind mgprop mggse nnt mprf reflection mcp netcdf scrip scripnc tide
  do
    case $type in
      mach   ) TY='one'
               ID='hardware or compiler'
               OK='DUM F90' ;;
      nco    ) TY='upto1'
               ID='NCO modifications'
               TS='NCO'
               OK='NCO' ;;
      grib   ) TY='one'
               ID='GRIB package'
               OK='NOGRB NCEP1 NCEP2' ;;
      shared ) TY='one'
               ID='shared / distributed memory'
               OK='SHRD DIST' ;;
      mpp    ) TY='one'
               ID='message passing protocol'
               OK='SHRD MPI' ;;
      thr0   ) TY='upto1'
               ID='directive controlled threading (subs)'
               TS='OMP0'
               OK='OMP0' ;;
      thr1   ) TY='upto1'
               ID='directive controlled threading (loop)'
               TS='OMP1'
               OK='OMP1' ;;
      c90    ) TY='upto1'
               ID='Cray C90 compiler directives'
               TS='C90'
               OK='C90' ;;
      nec    ) TY='upto1'
               ID='NEC compiler directives'
               TS='NEC'
               OK='NEC' ;;
      lrecl  ) TY='one'
               ID='word length in record length'
               OK='LRB4 LRB8' ;;
      prop   ) TY='one'
               ID='propagation scheme'
               OK='PR0 PR1 PR2 PR3 PRX' ;;
      stress ) TY='one'
               ID='stress computation'
               OK='FLX0 FLX1 FLX2 FLX3 FLXX' ;;
      s_ln   ) TY='one'
               ID='linear input'
               OK='LN0 SEED LN1 LNX' ;;
      source ) TY='one'
               ID='input/whitecapping'
               OK='ST0 ST1 ST2 ST3 ST4 STX' ;;
      stab   ) TY='upto1'
               ID='stability correction'
               TS='STAB'
               OK='STAB0 STAB2 STAB3' ;;
      s_nl   ) TY='one'
               ID='quadruplet interactions'
               OK='NL0 NL1 NL2 NL3 NLX' ;;
      snls   ) TY='upto1'
               ID='quadruplet smoother'
               TS='NLS'
               OK='NLS' ;;
      s_bot  ) TY='one'
               ID='bottom friction'
               OK='BT0 BT1 BT4 BTX' ;;
      s_db   ) TY='one'
               ID='depth-induced breaking'
               OK='DB0 DB1 DBX' ;;
      s_tr   ) TY='one'
               ID='triad interactions'
               OK='TR0 TRX' ;;
      s_bs   ) TY='one'
               ID='bottom scattering'
               OK='BS0 BS1 BSX' ;;
      s_xx   ) TY='one'
               ID='arbitrary source'
               OK='XX0 XXX' ;;
      wind   ) TY='one'
               ID='wind interpolation in time'
               OK='WNT0 WNT1 WNT2' ;;
      windx  ) TY='one'
               ID='wind interpolation in space'
               OK='WNX0 WNX1 WNX2' ;;
      rwind  ) TY='upto1'
               ID='wind vs. current definition'
               TS='RWND'
               OK='RWND' ;;
      curr   ) TY='one'
               ID='current interpolation in time'
               OK='CRT1 CRT2' ;;
      currx  ) TY='one'
               ID='current interpolation in space'
               OK='CRX0 CRX1 CRX2' ;;
      tdyn   ) TY='upto1'
               ID='dynamic diffusion time'
               TS='TDYN'
               OK='TDYN' ;;
      dss0   ) TY='upto1'
               ID='diffusion tensor'
               TS='DSS0'
               OK='DSS0' ;;
      pdif   ) TY='upto1'
               ID='propagation diffusion'
               TS='XW'
               OK='XW0 XW1' ;;
      miche  ) TY='upto1'
               ID='Miche style limiter'
               TS='MLIM'
               OK='MLIM' ;;
      mgwind ) TY='upto1'
               ID='moving grid wind correction'
               TS='MGW'
               OK='MGW' ;;
      mgprop ) TY='upto1'
               ID='moving grid propagation correction'
               TS='MGP'
               OK='MGP' ;;
      mggse  ) TY='upto1'
               ID='moving grid GSE correction'
               TS='MGG'
               OK='MGG' ;;
      nnt    ) TY='upto1'
               ID='NN training/test data generation'
               TS='NNT'
               OK='NNT' ;;
      mprf   ) TY='upto1'
               ID='multi-grid model profiling'
               TS='MPRF'
               OK='MPRF' ;;
      reflection ) TY='upto1'
               ID='wave reflections'
               TS='REF1'
               OK='REF1' ;;
      mcp    ) TY='one'
               ID='model coupling protocol'
               TS='NOPA'
               OK='NOPA PALM' ;;
      netcdf ) TY='upto1'
               ID='netcdf api type'
               TS='NC4'
               OK='NC4' ;;
      scrip  ) TY='upto1'
               ID='grid to grid interpolation '
               TS='SCRIP'
               OK='SCRIP' ;;
      scripnc ) TY='upto1'
               ID='use of netcdf in grid to grid interpolation '
               TS='SCRIPNC'
               OK='SCRIPNC' ;;
      tide   ) TY='upto1'
               ID='use of tidal analysis'
               TS='TIDE'
               OK='TIDE' ;;
    esac

    n_found='0'
    s_found=
    for check in $OK
    do
      if [ "`grep $check $switch | wc -w | awk '{print $1}'`" -gt '1' ]
      then
        n_found=`expr $n_found + 1`
        s_found="$check $s_found"
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

    case $type in
      shared ) shared=$sw ;;
      mpp    ) mpp=$sw ;;
      thr0   ) nr_thr=`expr $nr_thr + $n_found` ;;
      thr1   ) nr_thr=`expr $nr_thr + $n_found` ;;
      prop   ) p_switch=$sw ;;
      s_ln   ) s_ln=$sw ;;
      source ) s_inds=$sw ;;
      stab   ) stab=$sw ;;
      stress ) stress=$sw ;;
      scrip  ) scrip=$sw ;;
      scripnc) scripnc=$sw ;;
      s_nl   ) s_nl=$sw ;;
      snls   ) snls=$sw ;;
      s_bot  ) s_bt=$sw ;;
      s_db   ) s_db=$sw ;;
      s_tr   ) s_tr=$sw ;;
      s_bs   ) s_bs=$sw ;;
      s_xx   ) s_xx=$sw ;;
      reflection    ) reflection=$sw ;;
      mcp    ) mcp=$sw ;;
      netcdf ) netcdf=$sw;;
      tide   ) tide=$sw ;;
              *    ) ;;
    esac
  done

  if [ "$nr_thr" != '0' ] && [ "$shared" != 'SHRD' ]
  then
      echo ' '
      echo "   *** !/OMPn has to be used in combination with !/SHRD"
      echo ' ' ; exit 5
  fi

  case $p_switch in
   PR0) pr=$NULL ;;
   PR1) pr='w3profsmd w3pro1md' ;;
   PR2) pr='w3profsmd w3uqckmd w3pro2md' ;;
   PR3) pr='w3profsmd w3uqckmd w3pro3md' ;;
  esac 

  case $stress in
   FLX0) str_st1='no' ; str_st2='no' ; str_st3='yes'
         flx='w3flx1md'
         flxx=$NULL ;;
   FLX1) str_st1='OK' ; str_st2='no' ; str_st3='no'
         flx='w3flx1md'
         flxx=$NULL ;;
   FLX2) str_st1='OK' ; str_st2='OK' ; str_st3='no'
         flx='w3flx2md'
         flxx=$NULL ;;
   FLX3) str_st1='OK' ; str_st2='OK' ; str_st3='no'
         flx='w3flx3md'
         flxx=$NULL ;;
   FLXX) str_st1='no' ; str_st2='no' ; str_st3='no'
         flx='w3flxxmd'
         flxx=$NULL ;;
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
        stx=$NULL ;;
   ST2) st='w3src2md'
        stx='w3src2md' ;;
   ST3) st='w3src3md'
        stx='w3src3md' ;;
   ST4) st='w3src4md'
        stx='w3src4md' ;;
   STX) st='w3srcxmd'
        stx=$NULL ;;
  esac

  if [ "$stab" = 'STAB2' ] && [ "$s_inds" != 'ST2' ]
  then
      echo ' '
      echo "   *** !/STAB2 has to be used in combination with !/ST2"
      echo ' ' ; exit 6
  fi

  if [ "$stab" = 'STAB3' ] && [ "$s_inds" != 'ST3' ]
  then
      if [ "$s_inds" != 'ST4' ]
      then
        echo ' '
        echo "   *** !/STAB3 has to be used in combination with !/ST3 or !/ST4"
        echo ' ' ; exit 6
      fi
  fi

  if [ "$s_inds" = 'ST1' ] && [ "$str_st1" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST1 cannot be used in combination with !/$stress"
      echo "       Choose from FLX1, FLX2 or FLX3."
      echo ' ' ; exit 7
  fi

  if [ "$s_inds" = 'ST2' ] && [ "$str_st2" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST2 cannot be used in combination with !/$stress"
      echo "       Choose from FLX2 or FLX3."
      echo ' ' ; exit 7
  fi

  if [ "$s_inds" = 'ST3' ] && [ "$str_st3" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST3 cannot be used in combination with !/$stress"
      echo "       Stresses embedded in source terms, use FLX0."
      echo ' ' ; exit 7
  fi
  if [ "$s_inds" = 'ST4' ] && [ "$str_st3" = 'no' ]
  then
      echo ' '
      echo "   *** !/ST4 cannot be used in combination with !/$stress"
      echo "       Stresses embedded in source terms, use FLX0."
      echo ' ' ; exit 7
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

  case $s_xx in
   XX0) xx=$NULL
        xxx=$NULL ;;
   XXX) xx='w3sxxxmd'
        xxx=$NULL ;;
  esac

  refcode=$NULL
  case $reflection in
   REF1) refcode='w3ref1md'
   esac

  tidecode=$NULL
  case $tide in
   TIDE) tidecode='w3tidemd'
   esac

  if [ "$nr_thr" != '0' ] && [ "$s_nl" = 'NL2' ]
  then
      echo ' '
      echo "   *** The present version of the WRT interactions"
      echo "       cannot be run under OpenMP (OMP0, OMP1). Use"
      echo "       SHRD or MPI options instead.                    ***"
      echo ' ' ; exit 8
  fi

# 2.c Make makefile and file list  - - - - - - - - - - - - - - - - - - - - - -

  progs='ww3_grid ww3_strt ww3_prep ww3_prnc ww3_shel ww3_multi ww3_sbs1
         ww3_outf ww3_outp ww3_trck ww3_grib gx_outf gx_outp ww3_ounf 
         ww3_ounp ww3_gspl ww3_gint ww3_bound ww3_bounc ww3_systrk'

  for prog in $progs
  do
    case $prog in
     ww3_grid) IDstring='Grid preprocessor'
               core=
               data='w3gdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="w3triamd $stx $nlx $btx"
                 IO='w3iogrmd'
                aux='constants w3servmd w3arrymd w3dispmd w3gsrumd' ;;
     ww3_strt) IDstring='Initial conditions program'
               core=
               data='w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="$stx $nlx $btx"
                 IO='w3iogrmd w3iorsmd'
                aux='constants w3servmd w3arrymd w3dispmd w3gsrumd' ;;
     ww3_bound) IDstring='boundary conditions program'
               core=
               data='w3adatmd w3gdatmd w3wdatmd w3idatmd w3odatmd'
               prop=
             source="$stx $nlx $btx w3triamd"
                 IO='w3iobcmd w3iogrmd w3dispmd w3gsrumd'
                aux='constants w3servmd w3timemd w3cspcmd' ;;
     ww3_prep) IDstring='Field preprocessor'
               core='w3fldsmd'
               data='w3gdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="w3triamd $stx $nlx $btx"
                 IO='w3iogrmd'
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3gsrumd" ;;
     ww3_prnc) IDstring='NetCDF field preprocessor'
               core='w3fldsmd'
               data='w3gdatmd w3adatmd w3idatmd w3odatmd w3wdatmd'
               prop=
             source="w3triamd $stx $nlx $btx"
                 IO='w3iogrmd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd w3tidemd" ;;
     ww3_shel) IDstring='Generic shell'
               core='w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd'
               data='w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop="$pr"
             source="w3triamd w3srcemd $flx $ln $st $nl $bt $db $tr $bs $xx $refcode"
                 IO='w3iogrmd w3iogomd w3iopomd w3iotrmd w3iorsmd w3iobcmd'
                 IO="$IO w3iosfmd w3partmd"
                aux="constants w3servmd w3timemd $tidecode w3arrymd w3dispmd w3cspcmd w3gsrumd" ;;
    ww3_multi) IDstring='Multi-grid shell'
               core='wminitmd wmwavemd wmfinlmd wmgridmd wmupdtmd wminiomd'
               core="$core w3fldsmd w3initmd w3wavemd w3wdasmd w3updtmd"
               data='wmmdatmd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop="$pr"
             source="w3triamd w3srcemd $flx $ln $st $nl $bt $db $tr $bs $xx $refcode"
                 IO='w3iogrmd w3iogomd w3iopomd wmiopomd'
                 IO="$IO w3iotrmd w3iorsmd w3iobcmd w3iosfmd w3partmd"
                aux="constants $tidecode w3servmd w3timemd w3arrymd w3dispmd w3cspcmd w3gsrumd"
                aux="$aux  wmunitmd" 
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
               data='wmmdatmd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd' 
               prop="$pr" 
             source="w3triamd w3srcemd $flx $ln $st $nl $bt $db $tr $bs $xx $refcode" 
                 IO='w3iogrmd w3iogomd w3iopomd wmiopomd' 
                 IO="$IO w3iotrmd w3iorsmd w3iobcmd w3iosfmd w3partmd" 
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3cspcmd w3gsrumd $tidecode" 
                aux="$aux  wmunitmd"  
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
               data='w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="$stx $nlx $btx"
                 IO='w3iogrmd w3iogomd'
                aux='constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd' ;;
     ww3_ounf) IDstring='Gridded NetCDF output'
               core='w3initmd'
               data='w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="w3triamd $stx $nlx $btx"
                 IO='w3iogrmd w3iogomd w3iorsmd w3iopomd'
                aux='constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd' ;;
     ww3_outp) IDstring='Point output'
               core=
               data='w3triamd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="$flx $ln $st $nl $bt $db $tr $bs $xx"
                 IO='w3bullmd w3iogrmd w3iopomd w3partmd'
                aux='constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd' ;;
     ww3_ounp) IDstring='Point NetCDF output'
               core=
               data='w3triamd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="$flx $ln $st $nl $bt $db $tr $bs $xx"
                 IO='w3bullmd w3iogrmd w3iopomd w3partmd'
                aux='constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd' ;;
     ww3_trck) IDstring='Track output post'
               core=
               data='w3gdatmd w3odatmd'
               prop=
             source=
                 IO=
                aux="w3servmd w3timemd w3gsrumd" ;;
     ww3_grib) IDstring='Gridded output (GRIB)'
               core=
               data='w3triamd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="$stx $nlx $btx"
                 IO='w3iogrmd w3iogomd'
                aux='constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd' ;;
     ww3_gspl) IDstring='Grid splitting'
               core='w3fldsmd'
               data='w3gdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="w3triamd $stx $nlx $btx"
                 IO='w3iogrmd'
                aux="constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd $tidecode" ;;
     ww3_gint) IDstring='Grid Interpolation'
               core=
               data='w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
                 IO='w3iogrmd w3iogomd'
               prop=
             source="$st $nl"
               aux='constants w3servmd  w3arrymd w3dispmd w3timemd w3gsrumd' ;;
      gx_outf) IDstring='GrADS input file generation (gridded fields)'
               core=
               data='w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="w3triamd $stx $nlx $btx $db $tr $bs $xx"
                 IO='w3iogrmd w3iogomd'
                aux='constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd' ;;
      gx_outp) IDstring='GrADS input file generation for point output'
               core=
               data='w3triamd w3gdatmd w3wdatmd w3adatmd w3idatmd w3odatmd'
               prop=
             source="$flx $ln $st $nl $bt $db $tr $bs $xx"
                 IO='w3iogrmd w3iopomd'
                aux='constants w3servmd w3timemd w3arrymd w3dispmd w3gsrumd' ;;
      ww3_systrk) IDstring='Wave system tracking postprocessor'
               core='w3strkmd'
               data=
               prop=
             source=
                 IO=
                aux= ;;
    esac

    d_string='$(aPe)/'"$prog"' : $(aPo)/'

    files="$aux $core $data $prop $source $IO $prog"
    filesl="$prog $data $core $prop $source $IO $aux"

    echo "# $IDstring"                           >> makefile
    echo ' '                                     >> makefile
    for file in $files
    do
      echo "$d_string$file.o"                    >> makefile
      if [ -z "`echo $file | grep scrip 2>/dev/null`" ]
      then
      echo "$file"                               >> filelist.tmp
      fi
    done

    echo '	@$(aPb)/link '"$filesl"          >> makefile
    echo ' '                                     >> makefile
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
    if [ -f $main_dir/ftn/$file.ftn ]
    then
      fext=ftn
    else
      if [ -f $main_dir/ftn/$file.f90 ]
      then
        fext=f90
      else
        fext=f
      fi
    fi

    string1='$(aPo)/'$file'.o : '$file.$fext' '
    string2='	@$(aPb)/ad3'" $file"
    string3="$NULL"

    $main_dir/bin/ad3 $file 0 1 > ad3.out 2>&1

    if [ -n "`grep error ad3.out`" ]
    then
      cat ad3.out
      exit 8
    fi
    rm -f ad3.out

    if [ -f $file.f90 ]
    then
      fext=f90
    else
      fext=f
    fi

    grep USE $file.$fext  > check_file
    grep use $file.$fext >> check_file
    rm -f $file.$fext

    for mod in W3GDATMD W3WDATMD W3ADATMD W3ODATMD W3IDATMD \
               CONSTANTS W3SERVMD W3TIMEMD W3ARRYMD W3DISPMD W3GSRUMD \
               W3TRIAMD \
               W3IOGRMD W3IOGOMD W3IOPOMD W3IOTRMD W3IORSMD W3IOBCMD \
               W3IOSFMD W3PARTMD \
               W3PRO1MD W3PRO2MD W3PRO3MD W3PRO4MD W3PROXMD W3UQCKMD W3PROFSMD \
               W3SRCEMD W3FLX1MD W3FLX2MD W3FLX3MD W3FLXXMD \
               W3SLN1MD W3SLNXMD W3SRC0MD W3SRC1MD W3SRC2MD W3SRC3MD W3SRC4MD W3SRCXMD \
               W3SNL1MD W3SNL2MD W3SNL3MD W3SNLXMD W3SNLSMD \
               m_xnldata serv_xnl4v5 m_fileio m_constants \
               W3SBT1MD W3SBT4MD W3SBTXMD W3SDB1MD W3SDBXMD \
               W3STRXMD W3SBS1MD W3SBSXMD W3SXXXMD W3REF1MD \
               W3INITMD W3WAVEMD W3WDASMD W3UPDTMD W3FLDSMD W3CSPCMD \
               WMMDATMD WMINITMD WMWAVEMD WMFINLMD WMGRIDMD WMUPDTMD \
               WMUNITMD WMINIOMD WMIOPOMD W3BULLMD WMSCRPMD m_btffac W3STRKMD W3TIDEMD  
      do
      case $mod in
         'CONSTANTS'    ) modtest=constants.o ;;
         'W3GDATMD'     ) modtest=w3gdatmd.o ;;
         'W3WDATMD'     ) modtest=w3wdatmd.o ;;
         'W3ADATMD'     ) modtest=w3adatmd.o ;;
         'W3ODATMD'     ) modtest=w3odatmd.o ;;
         'W3IDATMD'     ) modtest=w3idatmd.o ;;
         'W3TRIAMD'     ) modtest=w3triamd.o ;;
         'W3WAVEMD'     ) modtest=w3wavemd.o ;;
         'W3INITMD'     ) modtest=w3initmd.o ;;
         'W3WDASMD'     ) modtest=w3wdasmd.o ;;
         'W3UPDTMD'     ) modtest=w3updtmd.o ;;
         'W3PRO1MD'     ) modtest=w3pro1md.o ;;
         'W3PRO2MD'     ) modtest=w3pro2md.o ;;
         'W3PRO3MD'     ) modtest=w3pro3md.o ;;
         'W3PRO4MD'     ) modtest=w3pro4md.o ;;
         'W3UQCKMD'     ) modtest=w3uqckmd.o ;;
         'W3PROFSMD'     ) modtest=w3profsmd.o ;;
         'W3PROXMD'     ) modtest=w3proxmd.o ;;
         'W3SRCEMD'     ) modtest=w3srcemd.o ;;
         'W3FLX1MD'     ) modtest=w3flx1md.o ;;
         'W3FLX2MD'     ) modtest=w3flx2md.o ;;
         'W3FLX3MD'     ) modtest=w3flx3md.o ;;
         'W3FLXXMD'     ) modtest=w3flxxmd.o ;;
         'W3SLN1MD'     ) modtest=w3sln1md.o ;;
         'W3SLNXMD'     ) modtest=w3slnxmd.o ;;
         'W3SRC0MD'     ) modtest=w3src0md.o ;;
         'W3SRC1MD'     ) modtest=w3src1md.o ;;
         'W3SRC2MD'     ) modtest=w3src2md.o ;;
         'W3SRC3MD'     ) modtest=w3src3md.o ;;
         'W3SRC4MD'     ) modtest=w3src4md.o ;;
         'W3SRCXMD'     ) modtest=w3srcxmd.o ;;
         'W3SNL1MD'     ) modtest=w3snl1md.o ;;
         'W3SNL2MD'     ) modtest=w3snl2md.o ;;
         'm_xnldata'    ) modtest=mod_xnl4v5.o ;;
         'serv_xnl4v5'  ) modtest=serv_xnl4v5.o ;;
         'm_fileio'     ) modtest=mod_fileio.o ;;
         'm_constants'  ) modtest=mod_constants.o ;;
         'W3SNL3MD'     ) modtest=w3snl3md.o ;;
         'W3SNLXMD'     ) modtest=w3snlxmd.o ;;
         'W3SNLSMD'     ) modtest=w3snlsmd.o ;;
         'W3SBT1MD'     ) modtest=w3sbt1md.o ;;
         'W3SBT2MD'     ) modtest=w3sbt2md.o ;;
         'W3SBT4MD'     ) modtest=w3sbt4md.o ;;
         'm_btffac'     ) modtest=mod_btffac.o ;;
         'W3SBTXMD'     ) modtest=w3sbtxmd.o ;;
         'W3SDB1MD'     ) modtest=w3sdb1md.o ;;
         'W3SDBXMD'     ) modtest=w3sdbxmd.o ;;
         'W3STRXMD'     ) modtest=w3strxmd.o ;;
         'W3SBS1MD'     ) modtest=w3sbs1md.o ;;
         'W3SBSXMD'     ) modtest=w3sbsxmd.o ;;
         'W3SXXXMD'     ) modtest=w3sxxxmd.o ;;
         'W3IOGRMD'     ) modtest=w3iogrmd.o ;;
         'W3IOGOMD'     ) modtest=w3iogomd.o ;;
         'W3IOPOMD'     ) modtest=w3iopomd.o ;;
         'W3IOTRMD'     ) modtest=w3iotrmd.o ;;
         'W3IORSMD'     ) modtest=w3iorsmd.o ;;
         'W3IOBCMD'     ) modtest=w3iobcmd.o ;;
         'W3IOSFMD'     ) modtest=w3iosfmd.o ;;
         'W3PARTMD'     ) modtest=w3partmd.o ;;
         'W3DISPMD'     ) modtest=w3dispmd.o ;;
         'W3TIMEMD'     ) modtest=w3timemd.o ;;
         'W3TIDEMD'     ) modtest=w3tidemd.o ;;
         'W3SERVMD'     ) modtest=w3servmd.o ;;
         'W3ARRYMD'     ) modtest=w3arrymd.o ;;
         'W3GSRUMD'     ) modtest=w3gsrumd.o ;;
         'W3FLDSMD'     ) modtest=w3fldsmd.o ;;
         'W3CSPCMD'     ) modtest=w3cspcmd.o ;;
         'W3BULLMD'     ) modtest=w3bullmd.o ;;
         'WMMDATMD'     ) modtest=wmmdatmd.o ;;
         'WMINITMD'     ) modtest=wminitmd.o ;;
         'WMWAVEMD'     ) modtest=wmwavemd.o ;;
         'WMFINLMD'     ) modtest=wmfinlmd.o ;;
         'WMGRIDMD'     ) modtest=wmgridmd.o ;;
         'WMUPDTMD'     ) modtest=wmupdtmd.o ;;
         'WMINIOMD'     ) modtest=wminiomd.o ;;
         'WMUNITMD'     ) modtest=wmunitmd.o ;;
         'WMIOPOMD'     ) modtest=wmiopomd.o ;;
         'WMSCRPMD'     ) modtest=wmscrpmd.o ;;
         'W3REF1MD'     ) modtest=w3ref1md.o ;;
         'W3STRKMD'     ) modtest=w3strkmd.o ;;
      esac
      nr=`grep $mod check_file | wc -c | awk '{ print $1 }'`
      if [ "$nr" -gt '8' ]
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
      echo "*** SCRIPT directory $scrip_dir not found ***"
      exit 2
    fi

    if  [ "$scripnc" = 'SCRIPNC' ]
    then 
       scrip_mk=$scrip_dir/SCRIP_NC.mk
    else 
       scrip_mk=$scrip_dir/SCRIP.mk
    fi
    if [ ! -e $scrip_mk ]
    then
      echo "*** SCRIPT makefile fragment $scrip_mk not found ***"
      exit 2
    fi

    cat $scrip_mk >> makefile

    echo '# end of SCRIP subroutines'              >> makefile
  fi

# --------------------------------------------------------------------------- #
# 4. Move makefile to proper place                                            #
# --------------------------------------------------------------------------- #

  mv makefile $main_dir/ftn/makefile

# end of script ------------------------------------------------------------- #
