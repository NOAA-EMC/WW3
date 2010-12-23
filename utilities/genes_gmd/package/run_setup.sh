#!/bin/sh
# ---------------------------------------------------------------------------- #
# run_setup.sh : Set up the run time environment for the genetic optimization  #
#                package for the Generalized Multiple DIA.                     #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 27-Oct-2008 : Origination.                                                   #
# 16-Dec-2008 : Two-level experiment directory setup.                          #
# 22-Dec-2008 : Add base run and engin identifier.                             #
# 17-Nov-2009 : Add copying of setup files if exist.                           #
#                                                                              #
#    Copyright 2008-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e

  echo ' '
  echo 'run_setup.sh :'
  echo '--------------'
  echo '   Set up run time environment for GMD optimization'

  file='.genes.env'

  genes_main=$NULL
  genes_work=$NULL
  genes_data=$NULL
  genes_exp1=$NULL
  genes_exp2=$NULL
  genes_base=$NULL
  genes_engn=$NULL

# ---------------------------------------------------------------------------- #
# 2. Test setup file

  if [ -f ~/$file ]
  then
    echo "   Setup file $file found."
    . ~/$file
  else
    echo "   Setup file $file NOT found."
  fi

# ---------------------------------------------------------------------------- #
# 3. Set defaults if needed

  if [ -z "$genes_main" ] ; then
    genes_main=`pwd` ; fi

  cd $genes_main ; cd .. ; genes_pwd=`pwd`

  if [ -z "$genes_data" ] ; then
    genes_data=$genes_pwd/genes_data ; fi
  mkdir -p $genes_data

  if [ -z "$genes_work" ] ; then
    if [ -d /tmp ] && [ -n "$USER" ] ; then
      mkdir -p /tmp/$USER ; fi
    if [ -d /tmp/$USER ] 
    then
      genes_work=/tmp/$USER/genes
    else
      genes_work=$genes_pwd/genes_work
    fi
  fi
  mkdir -p $genes_work

  if [ -z "$genes_exp1" ] ; then
    genes_exp1=main_exp ; fi

  if [ -z "$genes_exp2" ] ; then
    genes_exp2=sub_exp ; fi

  if [ -z "$genes_base" ] ; then
    genes_base=WRT ; fi

  if [ -z "$genes_engn" ] ; then
    genes_engn=single ; fi

# ---------------------------------------------------------------------------- #
# 4. Show present settings

  echo ' '
  echo "   Present settings :"
  echo "   ------------------"
  echo "      genes_main : $genes_main"
  echo "      genes_data : $genes_data"
  echo "      genes_work : $genes_work"
  echo "      genes_exp1 : $genes_exp1"
  echo "      genes_exp2 : $genes_exp2"
  echo "      genes_base : $genes_base"
  echo "      genes_engn : $genes_engn"
  echo ' '

# ---------------------------------------------------------------------------- #
# 5. Modify present settings

  echo -n "   Change settings [y/-] ? "
  read OK
  change='no'

  if [ "$OK" = 'y' ] || [ "$OK" = 'Y' ]
  then
    change='yes'

    echo ' '
    echo "   NOTE: directories not starting with '/' are taken relative"
    echo "         to $genes_data ..."
    echo ' '

    echo -n "   genes_main [$genes_main] : "
    read OK
    if [ -n "$OK" ] 
    then
      root_tst=`echo $OK | cut -c1-1`
      if [ "$root_tst" != '/' ] ; then
        OK=$genes_base/$OK ; fi
      if [ ! -d $OK ]
      then
        echo "   directory $OK does not exist."
      else
        genes_main=$OK
      fi
    fi
    echo ' '

    echo -n "   genes_data [$genes_data] : "
    read OK
    if [ -n "$OK" ] 
    then
      root_tst=`echo $OK | cut -c1-1`
      if [ "$root_tst" != '/' ] ; then
        OK=$genes_base/$OK ; fi
      if [ ! -d $OK ]
      then
        echo "   directory $OK does not exist, create [y/-] ? :"
        read OK2
        if [ "$OK2" = 'y' ] || [ "$OK2" = 'Y' ]
        then
          mkdir -p $OK
          genes_data=$OK
        fi
      else
        genes_data=$OK
      fi
    fi
    echo ' '

    echo -n "   genes_work [$genes_work] : "
    read OK
    if [ -n "$OK" ] 
    then
      root_tst=`echo $OK | cut -c1-1`
      if [ "$root_tst" != '/' ] ; then
        OK=$genes_base/$OK ; fi
      if [ ! -d $OK ]
      then
        echo "   directory $OK does not exist, create [y/-] ? :"
        read OK2
        if [ "$OK2" = 'y' ] || [ "$OK2" = 'Y' ]
        then
          mkdir -p $OK
          genes_work=$OK
        fi
      else
        genes_work=$OK
      fi
    fi
    echo ' '

    echo -n "   genes_exp1 [$genes_exp1] : "
    read OK
    if [ -n "$OK" ] 
    then
      genes_exp1=$OK
    fi
    echo ' '

    echo -n "   genes_exp2 [$genes_exp2] : "
    read OK
    if [ -n "$OK" ] 
    then
      genes_exp2=$OK
    fi
    echo ' '

    echo -n "   genes_base [$genes_base] : "
    read OK
    if [ -n "$OK" ] 
    then
      genes_base=$OK
    fi
    echo ' '

    echo -n "   genes_engn [$genes_engn] : "
    read OK
    if [ -n "$OK" ] 
    then
      genes_engn=$OK
    fi
    echo ' '

  fi

# ---------------------------------------------------------------------------- #
# 6. Write the setup file

  if [ "$change" = 'yes' ]
  then
    echo "   New setup file :"
    echo "-------------------------------------------------------"
    echo "#!/bin/sh"
    echo "# $file : setup file for GMD optimization"
    echo " "
    echo "  export genes_main=$genes_main"
    echo "  export genes_data=$genes_data"
    echo "  export genes_work=$genes_work"
    echo "  export genes_exp1=$genes_exp1 "
    echo "  export genes_exp2=$genes_exp2 "
    echo "  export genes_base=$genes_base "
    echo "  export genes_engn=$genes_engn "
    echo "-------------------------------------------------------"
    echo -n "   OK to write file [y/-] ? :"
    read OK
    if [ "$OK" = 'y' ] || [ "$OK" = 'Y' ]
    then
      write='yes'
    else
      write='no'
    fi
  else
    write='yes'
  fi

  if [ "$write" = 'yes' ]
  then
    echo ' '
    echo "   Writing setup file ..."
    echo "#!/bin/sh"                                                > ~/$file
    echo "# $file : setup file for GMD optimization"               >> ~/$file
    echo " "                                                       >> ~/$file
    echo "  export genes_main=$genes_main"                         >> ~/$file
    echo "  export genes_data=$genes_data"                         >> ~/$file
    echo "  export genes_work=$genes_work"                         >> ~/$file
    echo "  export genes_exp1=$genes_exp1"                         >> ~/$file
    echo "  export genes_exp2=$genes_exp2"                         >> ~/$file
    echo "  export genes_base=$genes_base"                         >> ~/$file
    echo "  export genes_engn=$genes_engn"                         >> ~/$file
  fi

# ---------------------------------------------------------------------------- #
# 7. Copy setup files as needed and acknowledged
#

  if [ -d $genes_data/$genes_exp1/$genes_exp2 ]
  then
    cd $genes_data/$genes_exp1/$genes_exp2
    echo ' '
    echo "   Checking setup files in data directory ...."
    set +e
    ls genes.* > filelist 2> /dev/null
    set -e
    if [ "`wc -l filelist | awk '{ print $1 }'`" != '0' ]
    then
      for file in `cat filelist`
      do
        chmod 644 $file
        if [ "$file" != 'genes.expdef.env' ]
        then
          if [ -f $genes_main/$file ]
          then
            set +e
            tststr="`diff $file $genes_main/.`"
            set -e
            if [ -n "$tststr" ]
            then
              echo -n "      $file differs, replace [y/-] ? "
              read OK
              if [ "$OK" = 'y' ] || [ "$OK" = 'Y' ]
              then
                echo "         replacing ..."
                rm -f $genes_main/$file
                cp $file $genes_main/.
              fi
            fi
          fi
        fi
      done
    fi
    rm -f filelist
  fi

# ---------------------------------------------------------------------------- #
# 8. End of script

  echo ' '
  echo 'End of run_setup.sh' 

# End of run_setup.sh -------------------------------------------------------- #
