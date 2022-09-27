#!/bin/bash -e
# --------------------------------------------------------------------------- #
# make_makefile.sh : Generates the makefile for WAVEWATCH based on switch     #
#                    settings                                                 #
#                    This script is called by w3_make and therefore does      #
#                    not print header information.                            #
#                                                                             #
# programs used : w3_new  Touches the correct files if compiler switches      #
#                         have been changed.                                  #
#                build_utils.sh functions that are used for building ww3      #
#                w3_setenv  Sets environment variables for ww3                #
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

# 1.b Get data from setup file - - - - - - - - - - - - - - - - - - - - - - - -

  source $(dirname $0)/w3_setenv
  main_dir=$WWATCH3_DIR
  temp_dir=$WWATCH3_TMP
  list=$WWATCH3_LIST

# 1.c Source the utility functions - - - - - - - - - - - - - - - - - - - - - -

. ${main_dir}/bin/build_utils.sh


# 1.d Go to temp dir
  cd $temp_dir


# --------------------------------------------------------------------------- #
# 2. Part 1, subroutine dependencies                                          #
# --------------------------------------------------------------------------- #

# 2.a Get info from switch file  - - - - - - - - - - - - - - - - - - - - - - -

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

# 2.b Check switches

   check_switches

# 2.c Get list of file names from switches

   switch_files


# 3.b Internal variables for makefile - - - - - - - - - - - - - - - - - - - - -

  # ar_cmd
  os=`uname -s 2>/dev/null`
  if [ "$os" = "AIX" ]
  then
    ar_cmd="ar -X32_64 rv"
  else
    ar_cmd="ar rv"
  fi


# 3.c Create makefile header - - - - - - - - - - - - - - - - - - - - - - - - -

  echo '# -------------------------'              > makefile
  echo '# WAVEWATCH III makefile   '             >> makefile
  echo '# -------------------------'             >> makefile


# 3.d Go through programs to create file list - - - - - - - - - - - - - - -

  echo ' '                                       >> makefile
  echo '# WAVEWATCH III executables'             >> makefile
  echo '# -------------------------'             >> makefile

  rm -f filelist.tmp



  progs="ww3_grid ww3_strt ww3_prep ww3_prnc ww3_shel ww3_multi ww3_sbs1
         ww3_outf ww3_outp ww3_trck ww3_trnc ww3_grib gx_outf gx_outp ww3_ounf
         ww3_ounp ww3_gspl ww3_gint ww3_bound ww3_bounc ww3_systrk $tideprog"
  progs="$progs ww3_multi_esmf  ww3_uprstr"
  progs="$progs libww3"
  progs="$progs libww3.so"

  for prog in $progs
  do

    # Get file - the list of files to be compiled
    # and filel - list of files for linking a particular prog
    create_file_list

    case $prog in
      ww3_grid) IDstring='Grid preprocessor' ;;
      ww3_strt) IDstring='Initial conditions program' ;;
      ww3_bound) IDstring='boundary conditions program' ;;
      ww3_bounc) IDstring='NetCDF boundary conditions program' ;;
      ww3_prep) IDstring='Field preprocessor' ;;
      ww3_prnc) IDstring='NetCDF field preprocessor' ;;
      ww3_prtide) IDstring='Tide prediction' ;;
      ww3_shel) IDstring='Generic shell' ;;
      ww3_multi) IDstring='Multi-grid shell' ;;
      ww3_multi_esmf) IDstring='Multi-grid ESMF module' ;;
      ww3_sbs1) IDstring='Multi-grid shell sbs version' ;;
      ww3_outf) IDstring='Gridded output' ;;
      ww3_ounf) IDstring='Gridded NetCDF output' ;;
      ww3_outp) IDstring='Point output' ;;
      ww3_ounp) IDstring='Point NetCDF output' ;;
      ww3_trck) IDstring='Track output post' ;;
      ww3_trnc) IDstring='Track NetCDF output post' ;;
      ww3_grib) IDstring='Gridded output (GRIB)' ;;
      ww3_gspl) IDstring='Grid splitting' ;;
      ww3_gint) IDstring='Grid Interpolation' ;;
      gx_outf) IDstring='GrADS input file generation (gridded fields)' ;;
      gx_outp) IDstring='GrADS input file generation for point output' ;;
      ww3_systrk) IDstring='Wave system tracking postprocessor' ;;
      libww3) IDstring='Object file archive' ;;
      libww3.so) IDstring='Object file archive' ;;
      ww3_uprstr) IDstring='Update Restart File' ;;
    esac


    echo "# $IDstring"                           >> makefile
    echo ' '                                     >> makefile

    # if esmf is included in program name or if
    # the target is compile and create archive
    if [ -n "`echo $prog | grep esmf 2>/dev/null`" ]
    then
      d_string="$prog"' : $(aPo)/'
    # if program name is libww3, then
    # the target is compile and create archive
    elif [ "$prog" = "libww3" ] ||  [ "$prog" = "libww3.so" ]
    then
      d_string="$prog"' : $(aPo)/'
    else
      d_string='$(aPe)/'"$prog"' : $(aPo)/'
    fi


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
      if [ -f $main_dir/src/$file.$s ]
      then
        fexti=$s
        break
      fi
      if [ -f $main_dir/src/PDLIB/$file.$s ]
      then
        fexti=$s
        ispdlibi=yes
        break
      fi
    done
    if [ "$fexti" = 'none' ]
    then
      echo '      *** make_makefile.sh error ***'
      echo "          Source file $main_dir/src/$file.* "
      echo "                   or $main_dir/src/PDLIB/$file.* not found"
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
         'W3UQCKMD'     ) modtest=w3uqckmd.o ;;
         'W3UNO2MD'     ) modtest=w3uno2md.o ;;
         'W3PSMCMD'     ) modtest=w3psmcmd.o ;;
         'W3PROFSMD'    ) modtest=w3profsmd.o ;;
         'W3SRCEMD'     ) modtest=w3srcemd.o ;;
         'W3FLX1MD'     ) modtest=w3flx1md.o ;;
         'W3FLX2MD'     ) modtest=w3flx2md.o ;;
         'W3FLX3MD'     ) modtest=w3flx3md.o ;;
         'W3FLX4MD'     ) modtest=w3flx4md.o ;;
         'W3FLX5MD'     ) modtest=w3flx5md.o ;;
         'W3SLN1MD'     ) modtest=w3sln1md.o ;;
         'W3SRC0MD'     ) modtest=w3src0md.o ;;
         'W3SRC1MD'     ) modtest=w3src1md.o ;;
         'W3SRC2MD'     ) modtest=w3src2md.o ;;
         'W3SRC3MD'     ) modtest=w3src3md.o ;;
         'W3SRC4MD'     ) modtest=w3src4md.o ;;
         'W3SRC6MD'     ) modtest=w3src6md.o ;;
         'W3SNL1MD'     ) modtest=w3snl1md.o ;;
         'W3SNL2MD'     ) modtest=w3snl2md.o ;;
         'W3SNL3MD'     ) modtest=w3snl3md.o ;;
         'W3SNL4MD'     ) modtest=w3snl4md.o ;;
         'W3SNL5MD'     ) modtest=w3snl5md.o ;;
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
         'W3SDB1MD'     ) modtest=w3sdb1md.o ;;
         'W3STR1MD'     ) modtest=w3str1md.o ;;
         'W3SBS1MD'     ) modtest=w3sbs1md.o ;;
         'W3SIC1MD'     ) modtest=w3sic1md.o ;;
         'W3SIC2MD'     ) modtest=w3sic2md.o ;;
         'W3SIC3MD'     ) modtest=w3sic3md.o ;;
         'W3SIC4MD'     ) modtest=w3sic4md.o ;;
         'W3SIC5MD'     ) modtest=w3sic5md.o ;;
         'W3SIS1MD'     ) modtest=w3sis1md.o ;;
         'W3SIS2MD'     ) modtest=w3sis2md.o ;;
         'W3REF1MD'     ) modtest=w3ref1md.o ;;
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
         'W3NMLBOUNDMD' ) modtest=w3nmlboundmd.o ;;
         'W3NMLSHELMD'  ) modtest=w3nmlshelmd.o ;;
         'W3NMLGRIDMD'  ) modtest=w3nmlgridmd.o ;;
         'W3NMLUPRSTRMD' ) modtest=w3nmluprstrmd.o ;;
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
         'W3OUNFMETAMD' ) modtest=w3ounfmetamd.o ;;
         'W3METAMD'     ) modtest=w3metamd.o ;;
         'W3GRIDMD'     ) modtest=w3gridmd.o ;;
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

    scrip_dir=$main_dir/src/SCRIP
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

  mv makefile $main_dir/src/makefile

# end of script ------------------------------------------------------------- #
