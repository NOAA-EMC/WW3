#!/bin/sh
# ---------------------------------------------------------------------------- #
# get_cases.sh  : Evaluate contents of genes.cases.env for use in scripts.     #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 27-Oct-2008 : Origination.                                                   #
# 10-Dec-2008 : Add shallow water cases.                                       #
# 21-Jan-2009 : Add cheap version of test_01 (test_0X).                        #
# 29-Sep-2010 : Add Hurricane test (baselining only).                          #
# 14-Oct-2010 : Add Lake Michigan test (baselining only).                      #
#                                                                              #
#    Copyright 2008-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e
  setup='.genes.env'
  cases='genes.cases.env'

  all_cases="0X 01 02 03 04 05 06 11 12 13 hr LM"

# ---------------------------------------------------------------------------- #
# 2. Test setup file

  if [ -f ~/$setup ]
  then
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 1
  fi

# ---------------------------------------------------------------------------- #
# 3. Set cases files

  if [ ! -f $genes_main/$cases ]
  then
    echo "   Cases file $setup NOT found (abort)."
    exit 2
  fi

# ---------------------------------------------------------------------------- #
# 4. Check for all possible cases

  output=

  for case in $all_cases
  do
    OK=`grep "test_$case" $genes_main/$cases | awk '{ print $2 }'`
    if [ "$OK" = 'yes' ] 
    then
      output="$output test_$case"
    fi
  done

# ---------------------------------------------------------------------------- #
# 5. End of script

  echo $output

# End of get_cases.sh -------------------------------------------------------- #
