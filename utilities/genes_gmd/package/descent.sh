#!/bin/sh
# ---------------------------------------------------------------------------- #
# descent.sh   : Do a steepest descent search from a selected member of a      #
#                selected population.                                          #
#                                                                              #
# usage: descent.sh igen ipop/ID                                               #
#                                                                              #
# remarks:                                                                     #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 30-Dec-2008 : Origination.                                                   #
# 19-Jan-2010 : Set up to work of predifined descent.ID file.                  #
# 16-Feb-2010 : Add engine check, adapt for 100+ threads.                      #
# 05-Mar-2010 : Start from sorted quadruplet, make recompute of first error    #
#               work on IBM.                                                   #
# 10-Sep-2010 : Swicth sorting off again to avoid issues with non-homogeneous  #
#               optimization masks.                                            #
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
  echo 'descent.sh : '`date`
  echo '-----------------------------------------------------'
  echo '   steepest descent search from GMD population member'
  echo ' '

  if [ "$#" -lt "2" ] ; then
    echo "   usage: descent igen ipop/ID" ; exit 1 ; fi

  setup='.genes.env'
  sleep=5
# incs='0.2 0.1 0.05 0.025 0.0125 0.0064 0.0032 0.0016'
  incs='0.1 0.05 0.025 0.0125 0.0064 0.0032 0.0016'
# incs='0.05 0.025 0.0125 0.0064 0.0032 0.0016'
# incs='0.025 0.0125 0.0064 0.0032 0.0016'
# incs='0.0125 0.0064 0.0032 0.0016'

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
  igen=$1
  ipop=$2

  set +e
  test=`expr $ipop + 1 2> /dev/null`
  OK=$?
  set -e

  if [ "$OK" = '0' ]
  then
    pop_is_nr='yes'
  else
    pop_is_nr='no'
  fi

  echo "   Save directory  : $genes_data/$genes_exp1/$genes_exp2"
  echo "   Cases to run    : $all_cases"
  echo "   Generation      : $1"
  if [ "$pop_is_nr" = 'yes' ]
  then
    echo "   Member          : $2"
  else
    echo "   Case            : $2"
  fi
  echo "   Increment facs  : $incs"
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
    echo "   Experiment directory $genes_exp1 not found (abort)"
    exit 4 
  fi

  cd $genes_exp1

# 3.c Second level experiment directory

  if [ ! -d "$genes_exp2" ]
  then
    echo "   Experiment directory $genes_exp2 not found (abort)"
    exit 5
  fi

  cd $genes_exp2

# 3.d Second level experiment directory

  if [ ! -f "genes.expdef.env" ]
  then
    echo "   File genes.expdef.env not found (abort)"
    exit 6
  fi

  . genes.expdef.env

# 3.e Generational directory

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
  gen=gen$igen

  if [ ! -d "$gen" ]
  then
    echo "   Generation directory $gen not found (abort)"
    exit 7
  fi

  cd $gen

# 3.f Population check

  if [ "$pop_is_nr" = 'yes' ] && [ ! -f "population" ]
  then
    echo "   Population file not found (abort)"
    exit 8
  fi

# 3.g Member check

  if [ "$pop_is_nr" = 'yes' ]
  then

    mpop=`wc -l population | awk '{ print $1}'`
    mpop=`expr $mpop / $genes_nq`

    if [ "$ipop" -lt '1' ] || [ "$ipop" -gt "$mpop" ]
    then
      echo "   Population member $ipop [$mpop] not found (abort)"
      exit 9
    fi

  else

    if [ ! -f descent.$ipop ]
    then
      echo "   File descent.$ipop not found (abort)"
      exit 9
    fi

  fi

# 3.h Get first member data

# back=`pwd`
# cd $genes_main/progs
# rm -f getmember.o
# pgf90 getmember.f90 -byteswapio -o getmember.x -Mlist *.o
# rm -f getmember.o
# mv getmember.x $genes_main/exe/.
# cd $back

  if [ "$pop_is_nr" = 'yes' ] 
  then

    if [ "$ipop" -lt '10' ]
    then
      ipop="000$ipop"
    else
      if [ "$ipop" -lt '100' ]
      then
        ipop="00$ipop"
      else
        if [ "$ipop" -lt '1000' ]
        then
          ipop="0$ipop"
        fi
      fi
    fi

    if [ ! -f descent.$ipop ]
    then
      echo "   Extracting member $ipop [$mpop]"

      echo "$ipop $genes_nq" | $genes_main/exe/getmember.x > getmember.out

      rm -f getmember.out namelist

      mv member descent.$ipop
    fi

  fi

  ln -sf descent.$ipop descent
  error=`cut -c1-10 descent.$ipop | sort -nu | sed -n '2,2p' | awk '{ print $1}'`

# ---------------------------------------------------------------------------- #
# 4.  Start the engine
# 4.a Engine setup

  back=`pwd`
  cd $genes_main/ush
  echo "   Set up the engine ..."

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

# 4.b Start the engine

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

# 4.c Recompute first error as needed

  if [ "$error" = '999.999' ]
  then

    mkdir work
    cd work
    if [ -f ../err.0001 ]
    then
      mv ../err.0001 .
    fi

    ln -sf ../descent.$ipop population

    echo "  $genes_nq"                                          > input
    sed -n '/^\$.*/!p'  ../../genes.mask.env | head -$genes_nq >> input

    cat input | $genes_main/exe/chckgen.x > chckgen.out
    rm -f input

    set +e
    nsnl=`ls snl.???? 2> /dev/null | wc -w | awk '{ print $1 }'`
    set -e

    echo "         There is $nsnl member in descent.$ipop that still need to be processed"

    if [ "$nsnl" -gt '1' ]
    then
      echo "         *** Oops, why is the number not 0 or 1 ???? ***"
      echo "         *** abort ***" ; exit 12
    fi

    if [ "$nsnl" = '0' ]
    then
      mv population.updt ../descent.$ipop
    else
      mv snl.* ../.

      cd ..
      rm -rf work

      key=
      while [ "$key" != 'ready to go' ]
      do
        sleep $sleep
        key=`head -1  tdata.001`
      done

      echo 'snl.0001' > tdata.001

      key=
      while [ "$key" != 'ready to go' ]
      do
        sleep $sleep
        key=`head -1  tdata.001`
      done

      mkdir work
      cd work
      if [ -f ../err.0001 ]
      then
        mv ../err.0001 .
      fi
  
      ln -sf ../descent.$ipop population

      echo "  $genes_nq"                                          > input
      sed -n '/^\$.*/!p'  ../../genes.mask.env | head -$genes_nq >> input

      cat input | $genes_main/exe/chckgen.x > chckgen.out
      rm -f input
  
      set +e
      nsnl=`ls -f snl.???? 2> /dev/null | wc -w | awk '{ print $1 }'`
      set -e   

      mv population.updt ../descent.$ipop

      cd ..
      rm -rf work

    fi

    error=`head -1 descent.$ipop | awk '{ print $1}'`

    if [ "$error" = '999.999' ]
    then
      echo "         Could not establish base error ..."
      echo "         *** abort ***" ; exit 13
    fi

  fi

# ---------------------------------------------------------------------------- #
# 5.  Loop over increment factors

  rm -f mini_pop line_pop final_pop best_member snl.* err.*

large='999.999'

while [ "$error" != "$large" ]
do
  large="$error"

  for inc in $incs
  do
    echo ' '
    echo "   Working on increment factor $inc , error = $error ..."
    best='999.99'

    while [ "$error" != "$best" ]
    do

# ---------------------------------------------------------------------------- #
# 6.  Search loop
# 6.a Make mini population for partial derivatives ...

      echo "      Computing partial derivatives ..."

# back=`pwd`
# cd $genes_main/progs
# rm -f descent1.o
# pgf90 descent1.f90 -byteswapio -o descent1.x -Mlist *.o
# rm -f descent1.o
# mv descent1.x $genes_main/exe/.
# cd $back

      if [ -f mini_pop ]
      then
        echo "         mini_pop already exists"
      else
        echo "$genes_nq $inc "                                        > input
        sed -n '/^\$.*/!p'  ../genes.mask.env                        >> input

        cat input | $genes_main/exe/descent1.x > descent1.out

        rm -f input descent1.out
      fi

# 6.b Generating snl files as needed

      rm -rf work
      rm -f snl.????
      rm -f snl.????.out
      mkdir work
      cd work

      set +e
      files=`ls ../err.* 2> /dev/null`
      set -e

      if [ -n "$files" ]
      then
        mv ../err.* .
      fi
      ln -sf ../mini_pop population

      echo "  $genes_nq"                                          > input
      sed -n '/^\$.*/!p'  ../../genes.mask.env | head -$genes_nq >> input

      cat input | $genes_main/exe/chckgen.x > chckgen.out
      rm -f input

      size_old=`wc -w population      | awk '{print $1}'`
      size_new=`wc -w population.updt | awk '{print $1}'`

      if [ "$size_old" != "$size_new" ]
      then
        echo "         error in size of updated mini_pop [$size_old $size_new]"
        echo "         *** abort ***" ; exit 14
      else
        mv population.updt ../mini_pop
        rm -f population
        rm -f chckgen.out
      fi

      set +e
      nsnl=`ls -f snl.???? 2> /dev/null | wc -w | awk '{ print $1 }'`
      set -e

      echo "         There are $nsnl members that still need to be processed"

      if [ "$nsnl" != '0' ]
      then
        mv snl.* ../.
      fi

      cd ..
      rm -rf work

# 6.c Run the engine

      if [ "$nsnl" != '0' ]
      then
        rm -f filelist
        ls snl.???? > filelist
        sleep 2
        i=1
        while [ "$i" -le "$nsnl" ]
        do
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
#               echo "Assigned `cat tdata.$j |awk '{print $1}'` to thread $j"
              fi
              i=`expr $i + 1`
            fi
            j=`expr $j + 1`
          done
          if [ "$i" -le "$nsnl" ] ; then
            sleep $sleep ; fi
        done
      fi

      i=0
      until [ "$i" = "$nr_threads" ]
      do
        j=1
        i=0
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
              i=`expr $i + 1`
            fi
          fi
          j=`expr $j + 1`
        done 
        if [ "$i" -lt "$nr_threads" ] ; then
          sleep $sleep ; fi
      done

      rm -f filelist snl.????.out

# 6.d See if there is a line to check

      echo "      Computing search line  ..."

# back=`pwd`
# cd $genes_main/progs
# rm -f descent2.o
# pgf90 descent2.f90 -byteswapio -o descent2.x -Mlist *.o
# rm -f descent2.o
# mv descent2.x $genes_main/exe/.
# cd $back

      echo "$genes_nq $inc "                                        > input
      sed -n '/^\$.*/!p'  ../genes.mask.env                        >> input

      cat input | $genes_main/exe/descent2.x > descent2.out

      rm -f input descent2.out err.????

      set +e
      nsnl=`ls -f snl.???? 2> /dev/null | wc -w | awk '{ print $1 }'`
      set -e

      echo "         There are $nsnl members that still need to be processed"

# 6.e Run the engine

      if [ "$nsnl" != '0' ]
      then
        rm -f filelist
        ls snl.???? > filelist
        i=1
        while [ "$i" -le "$nsnl" ]
        do
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
#               echo "Assigned `cat tdata.$j |awk '{print $1}'` to thread $j"
              fi
              i=`expr $i + 1`
            fi
            j=`expr $j + 1`
          done
          if [ "$i" -le "$nsnl" ] ; then
            sleep $sleep ; fi
        done
      fi

      i=0
      until [ "$i" = "$nr_threads" ]
      do
        j=1
        i=0
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
              i=`expr $i + 1`
            fi
          fi
          j=`expr $j + 1`
        done 
        if [ "$i" -lt "$nr_threads" ] ; then
          sleep $sleep ; fi
      done

      rm -f filelist snl.????.out

# 6.f Get final best guess

      echo "      Select best member  ..."

# back=`pwd`
# cd $genes_main/progs
# rm -f descent3.o
# pgf90 descent3.f90 -byteswapio -o descent3.x -Mlist *.o
# rm -f descent3.o
# mv descent3.x $genes_main/exe/.
# cd $back

      echo "$genes_nq" | $genes_main/exe/descent3.x > descent3.out

      rm -f descent3.out err.????

      best=`head -1 best_member | awk '{ print $1}'`

      if [ "$best" != "$error" ]
      then
        echo "         Error improved from $error to $best"
        cat best_member >> descent
        error=$best
        best='999.99'
      else
        echo "         No more improvement for this increment"
      fi

      rm -f mini_pop line_pop final_pop best_member
 
# ... End loops 5.
 
    done
 
  done

# large="$error" ; echo ' ' ; echo '   *** Stop of descent.sh hardwired ***'

done

# ---------------------------------------------------------------------------- #
# 7. Stop the engine

  for file in `ls tdata.???`
  do
    echo 'done' > $file
  done

  echo ' '
  echo "   Waiting for the engine to finish ..."
  $genes_main/ush/thread_wait.sh
  echo "   Engine done"
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
  rm -f descent
  rm -f $genes_main/thread_check.out

# ---------------------------------------------------------------------------- #
# 8. End of script

  echo ' '
  echo 'Out of descent.sh '`date`

# End of descent.sh ---------------------------------------------------------- #
