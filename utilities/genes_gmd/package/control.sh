#!/bin/sh
# ---------------------------------------------------------------------------- #
# control.sh   : Manage the GMD optimization from initialization to the final  #
#                processing of the final generation.                           #
#                                                                              #
# usage: control.sh                                                            #
#                                                                              #
# remarks: For now set up not to cycle.                                        #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 29-Dec-2008 : Origination.                                                   #
# 24-Sep-2009 : Set test error level if not yet defined.                       #
# 02-Nov-2009 : Adding clean stop of engine.                                   #
# 16-Feb-2010 : Adapt for up to 999 threads.                                   #
# 03-Mar-2010 : Additional cleanup for IBM added.                              #
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
  echo 'control.sh : '`date`
  echo '------------------------------------------------'
  echo '   managing GMD genetic optimization experiments'
  echo ' '

# if [ "$#" -lt "1" ] ; then
#   echo "   No base_ID defined (abort)" ; exit 1 ; fi

  setup='.genes.env'
  sleep=5

# ---------------------------------------------------------------------------- #
# 2. Get base setup for the runs

  if [ -f ~/$setup ]
  then
    echo "   Setup file $setup found."
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 2
  fi
  all_cases=`$genes_main/ush/get_cases.sh`

  echo "   Save directory  : $genes_data/$genes_exp1/$genes_exp2"
  echo "   Cases to run    : $all_cases"
  echo "   Testing directories and files ..."
  echo ' '

# ---------------------------------------------------------------------------- #
# 3.  Checking directories and files.
# 3.a General data directory

  if [ ! -d "$genes_data" ]
  then
    echo "   Main data directory $genes_data not found (abort) *** "
    exit 3 
  fi

  cd $genes_data

# 3.b First level experiment directory

  if [ ! -d "$genes_exp1" ]
  then
    echo -n "   Experiment directory $genes_exp1 not found, create [y/-] ? "
    read OK
    if [ "$OK" = 'y' ] || [ "$OK" = 'Y' ]
    then
      mkdir $genes_exp1
    else
      echo "   *** aborting script ***" ; exit 4
    fi
  fi

  cd $genes_exp1

# 3.c Second level experiment directory

  if [ ! -d "$genes_exp2" ]
  then
    echo -n "   Experiment directory $genes_exp2 not found, create [y/-] ? "
    read OK
    if [ "$OK" = 'y' ] || [ "$OK" = 'Y' ]
    then
      mkdir $genes_exp2
    else
      echo "   *** aborting script ***" ; exit 5
    fi
  fi

  cd $genes_exp2

# 3.d Experiment setup file

  if [ -f "genes.expdef.env" ]
  then
    . genes.expdef.env
  else
    echo "   Experiment definition file not found :"
    echo -n "      Number of quadruplets    : " ; read genes_nq
    echo -n "      Size of first generation : " ; read genes_npop0
    echo -n "      Size of next generations : " ; read genes_npop
    echo -n "      Number of generations    : " ; read genes_ngen
    echo -n "      Initial random seed      : " ; read genes_seed

    echo ' '
    echo 'New file genes.expdef.env : '
    echo '----------------------------------------------------'
    echo "#!/bin/sh"
    echo "# genes.expdef.env : setup file "
    echo ' '
    echo "  export    genes_nq=$genes_nq"
    echo "  export genes_npop0=$genes_npop0"
    echo "  export  genes_npop=$genes_npop"
    echo "  export  genes_ngen=$genes_ngen"
    echo "  export  genes_seed=$genes_seed"
    echo '----------------------------------------------------'
    echo -n "Save this file [y/-] ? " ; read OK

    if [ "$OK" = 'y' ] || [ "$OK" = 'Y' ]
    then
      echo "#!/bin/sh"                                   > genes.expdef.env
      echo "# genes.expdef.env : setup file "           >> genes.expdef.env
      echo ' '                                          >> genes.expdef.env
      echo "  export    genes_nq=$genes_nq"             >> genes.expdef.env
      echo "  export genes_npop0=$genes_npop0"          >> genes.expdef.env
      echo "  export  genes_npop=$genes_npop"           >> genes.expdef.env
      echo "  export  genes_ngen=$genes_ngen"           >> genes.expdef.env
      echo "  export  genes_seed=$genes_seed"           >> genes.expdef.env
    fi
    echo ' '
    echo 'Out of control.sh '`date`
    exit 0
  fi

  . genes.expdef.env

  echo "   Number of quadruplets : $genes_nq"
  echo "   Size of population    : $genes_npop [$genes_npop0]"
  echo "   Number of generations : $genes_ngen"
  echo "   Initial random seed   : $genes_seed"
  echo ' '

# 3.e Test other files in second level directory

  for file in mask stats spec source cases
  do
    if [ ! -f "genes.$file.env" ]
    then
      echo "   Copy file genes.$file.env"
      cp $genes_main/genes.$file.env .
    else
      if [ ! -z "`diff $genes_main/genes.$file.env .`" ]
      then
        echo "   File genes.$file.env does not match main directory."
        echo "   *** abort ***" ; exit 7
      fi
    fi
  done

# 3.f Test weight files

# rm -f genes.w_??.env
  for case in `$genes_main/ush/get_cases.sh`
  do
    file=`echo $case | sed 's/test/w/g'`
    if [ ! -f "genes.$file.env" ]
    then
      echo "   Copy file genes.$file.env"
      cp $genes_main/genes.$file.env .
    else
      if [ ! -z "`diff $genes_main/genes.$file.env .`" ]
      then
        echo "   File genes.$file.env does not match main directory."
        echo "   *** abort ***" ; exit 7
      fi
    fi
  done

# 3.g Set generation to work on

# rm -fr gen0007

  set +e
  gens=`ls -d gen???? 2> /dev/null`
  set -e
 
  if [ "$ngen" != '0' ]
  then
    for gen in $gens
    do
      if [ ! -f $gen/population ]
      then
        echo "   Generation $gen has no population, removing it ..."
        rm -rf $gen
      fi
    done
  fi

  set +e
  ngen=`ls -d gen???? 2> /dev/null | wc -w | awk '{ print $1 }'`
  set -e

  if [ "$ngen" = '0' ]
  then
    gen=gen0001
    mkdir -p $gen
  else
    gen=`ls -d gen???? | tail -1 | awk '{ print $1 }'`
  fi

  echo "   Working on $gen ..."
  cd $gen

# ---------------------------------------------------------------------------- #
# 4.  Set up initial population

  if [ "$gen" = 'gen0001' ] && [ ! -f population ]
  then
    echo "   Making intial generation ..."
    $genes_main/ush/make_init.sh > make_init.out
  fi

# ---------------------------------------------------------------------------- #
# 5.  Test status of population

  echo "   Check status of population ..."

# back=`pwd`
# cd $genes_main/progs
# rm -f chckgen.o
# pgf90 chckgen.f90 -byteswapio -o chckgen.x -Mlist *.o
# rm -f chckgen.o
# mv chckgen.x $genes_main/exe/.
# cd $back

  rm -f snl.????
  rm -f snl.????.out

  echo "  $genes_nq"                                        > input
  sed -n '/^\$.*/!p'  ../genes.mask.env | head -$genes_nq  >> input

  cat input | $genes_main/exe/chckgen.x > chckgen.out
  rm -f input

  size_old=`wc -w population      | awk '{print $1}'`
  size_new=`wc -w population.updt | awk '{print $1}'`

  if [ "$size_old" != "$size_new" ]
  then
    echo "   Files population and population.updt have different size"
    echo "   *** abort ***" ; exit 8
  else
    rm -f population
    mv population.updt population
    rm -f chckgen.out
  fi

  set +e
  nsnl=`ls -f snl.???? 2> /dev/null | wc -w | awk '{ print $1 }'`
  set -e

  echo "      There are $nsnl members that still need to be processed"

  TERR=

  if [ ! -f genes.terr.env ]
  then
    $genes_main/ush/get_terr.sh
  fi

  if [ -f genes.terr.env ]
  then
    . genes.terr.env
  fi

  if [ -z "$TERR" ] || [ "$TERR" = '999.999' ]
  then
    echo "      Test error level not set."
  else
    echo "      Test error level set at $TERR"
  fi

# ---------------------------------------------------------------------------- #
# 6.  Process errors for selected members of population

  if [ "$nsnl" != '0' ]
  then
    echo ' '
    echo "   Compute errors against $genes_base ..."

# 6.a Check baseline data sets

    for test in `$genes_main/ush/get_cases.sh`
    do
      for file in spec source part
      do
        if [ ! -f $genes_data/$genes_base/$test/$file.ww3 ]
        then
          echo "   File .../$genes_base/$test/$file.ww3 not found"
          echo "   *** abort ***" ; exit 9
#       else
#         echo "      File .../$genes_base/$test/$file.ww3 OK"
        fi
      done
    done

# 6.b Check the engine

    back=`pwd`
    cd $genes_main/ush

    for file in start stop wait check
    do
      script="thread_$file.sh.$genes_engn"
      s_link="thread_$file.sh"
      if [ -f $script ]
      then
        ln -sf $script $s_link
        echo "      Script $script found and linked"
      else
        echo "      File .../$script not found"
        echo "      *** abort ***" ; exit 10
      fi
    done

    cd $back

    echo "#!/bin/sh"                > $genes_main/control.status
    echo " "                       >> $genes_main/control.status
    echo "  export status=OK"      >> $genes_main/control.status

    chmod 700 $genes_main/control.status

    . $genes_main/control.status

# 6.c Start the engine

    echo "      Prepping the engine ..."
    $genes_main/ush/thread_stop.sh

    echo "      Starting the engine ..."
    $genes_main/ush/thread_start.sh

    echo "      Starting the engine check ..."
    $genes_main/ush/thread_check.sh $$ > $genes_main/thread_check.out \
                                               2> /dev/null &

    nr_threads=`head -1 threads.data | awk '{print $1}'`
    set +e
    nr_test=`ls tdata.??? 2> /dev/null | wc -w | awk '{print $1}'`
    set -e
    if [ "$nr_threads" = "$nr_test" ]
    then
      echo "         Engine with $nr_threads threads"
    else
      echo "      Threads inconsistency [$nr_threads $nr_test]"
      echo "      *** abort ***" ; exit 11
    fi

# 6.d Feed the engine

    echo ' '
    rm -f filelist
    ls snl.???? > filelist

    i=1
    while [ "$i" -le "$nsnl" ]
    do
      . $genes_main/control.status
      if [ "$status" = 'stop' ]
      then
        i=`expr $nsnl + 1`
      fi

      j=1
      while [ "$j" -le "$nr_threads" ]
      do
        if [ "$j" -lt '100' ] ; then
          j="0$j" ; fi
        if [ "$j" -lt '10' ] ; then
          j="0$j" ; fi
        key=`head -1  tdata.$j`
        if [ "$key" = 'ready to go' ]
        then
          if [ "$i" -le "$nsnl" ]
          then
            sed -n $i,${i}p filelist > tdata.$j
            echo "      Assigned `cat tdata.$j |awk '{print $1}'` to thread $j"
          fi
          i=`expr $i + 1`
        fi
        j=`expr $j + 1`
      done

      if [ "$i" -le "$nsnl" ] ; then
        sleep $sleep ; fi
    done

# 6.e Stop the engine

    i=0
    while [ "$i" -lt "$nr_threads" ]
    do
      j=1
      while [ "$j" -le "$nr_threads" ]
      do
        if [ "$j" -lt '100' ] ; then
          j="0$j" ; fi
        if [ "$j" -lt '10' ] ; then
          j="0$j" ; fi
        if [ -f tdata.$j ]
        then
          key=`head -1  tdata.$j`
          if [ "$key" = 'ready to go' ]
          then
            echo 'done' > tdata.$j
            echo "      Shutting down thread $j"
            i=`expr $i + 1`
          fi
        fi
        j=`expr $j + 1`
      done
      if [ "$i" -lt "$nr_threads" ] ; then
        sleep $sleep ; fi
    done

    rm -f filelist

# 6.f Wait for the engine to finish

    echo "      Waiting for the engine to finish ..."
    $genes_main/ush/thread_wait.sh
    echo "      Engine done"
    echo "      Stopping engine check ..."
    killall sleep
    sleep 1
    set +e
    killall thread_check.sh 2> /dev/null
    set -e
    echo ' '

    rm -f tdata.???
    rm -f threads.data
    rm -f thread_???.out
    rm -f $genes_main/thread_check.out

# 6.g Recheck population

    echo "   Recheck status of population ..."

    echo "  $genes_nq"                                        > input
    sed -n '/^\$.*/!p'  ../genes.mask.env | head -$genes_nq  >> input

    cat input | $genes_main/exe/chckgen.x > chckgen.out
    rm -f input

    size_old=`wc -w population      | awk '{print $1}'`
    size_new=`wc -w population.updt | awk '{print $1}'`

    if [ "$size_old" != "$size_new" ]
    then
      echo "   Files population and population.updt have different size"
      echo "   *** abort ***" ; exit 12
    else
      rm -f population
      mv population.updt population
      rm -f chckgen.out
    fi

    set +e
    nsnl=`ls -f snl.???? 2> /dev/null | wc -w | awk '{ print $1 }'`
    set -e

    echo "      There are $nsnl members that still need to be processed"
    
  fi

# ---------------------------------------------------------------------------- #
# 7.  Finalize population

  if [ "$nsnl" = '0' ]
  then
    rm -f snl.????.out
    rm -f thread_???.out
    rm -f genes.terr.env

    echo ' '
    echo "   Finalize the present population ..."

# back=`pwd`
# cd $genes_main/progs
# rm -f sortgen.o
# pgf90 sortgen.f90 -byteswapio -o sortgen.x -Mlist *.o
# rm -f sortgen.o
# mv sortgen.x $genes_main/exe/.
# cd $back

    npop=`wc -l population | awk '{print $1}'`
    npop=`expr $npop / $genes_nq`

    if [ "$npop" != "$genes_npop" ] && [ "$npop" != "$genes_npop0" ]
    then
      echo "      Unexpeced population size $npop $genes_npop [$genes_npop0]"
      echo "   *** abort ***" ; exit 13
    fi

    echo "$genes_nq $npop" | $genes_main/exe/sortgen.x > sortgen.out

    size_old=`wc -w population      | awk '{print $1}'`
    size_new=`wc -w population.sort | awk '{print $1}'`

    if [ "$size_old" != "$size_new" ]
    then
      echo "   Files population and population.updt have different size"
      echo "   *** abort ***" ; exit 14
    else
      rm -f population
      mv population.sort population
      rm -f sortgen.out
    fi

    echo "      Population sorted and finalized"

    set +e
    ls err.???? > filelist 2> /dev/null
    set -e
    nerr=`wc -l filelist | awk '{ print $1}'`

    if [ "$nerr" != '0' ]
    then
      echo "      Processing $nerr error files err.NNNN ..."

      set `cat filelist`
      while [ "$#" != '0' ]
      do
        sed -n 1,1p $1  >> errors.test
        shift
      done
      sort -un errors.test | sed -n 1,${npop}p > tempfile
      mv tempfile errors.test
      echo "         File errors.test updated"

      set `cat filelist`
      while [ "$#" != '0' ]
      do
        sed -n 2,2p $1  >> errors.pars
        shift
      done
      sort -un errors.pars | sed -n 1,${npop}p > tempfile
      mv tempfile errors.pars
      echo "         File errors.pars updated"

      set `cat filelist`
      while [ "$#" != '0' ]
      do
        rm -f $1
        shift
      done
      echo "         File err.NNNN removed."

    fi

    rm -f filelist

    rm -rf thread_???
    rm -f thread_???.out tdata.* temp.sh threads.data

    rm -f g_engine*
    rm -f cmdfile

# ---------------------------------------------------------------------------- #
# 8.  Start next generation

    igen=`echo $gen | sed 's/gen//g'`

    if [ "$igen" -ge "$genes_ngen" ]
    then
      echo ' '
      echo "   This was the last generation."
    else
      igen=`expr $igen + 1`
      if [ "$igen" -lt '10' ] 
      then
        igen="000$igen"
      else
        if [ "$igen" -lt '100' ] 
        then
          igen="00$igen"
        else
          if [ "$igen" -lt '1000' ] 
          then
            igen="0$igen"
          fi
        fi
      fi
      newgen=gen$igen
      . seed.env
      mkdir -p ../$newgen
      cd ../$newgen
      echo ' '

      echo "   Starting the next generation [$newgen from $gen] ..."
      echo "       Seed = $genes_seed "

      $genes_main/ush/make_next.sh $gen $newgen > make_next.out

#     rm -f make_next.out

    fi

  fi

# ---------------------------------------------------------------------------- #
# 9. End of script

  echo ' '
  echo 'Out of control.sh '`date`

# End of control.sh ---------------------------------------------------------- #
