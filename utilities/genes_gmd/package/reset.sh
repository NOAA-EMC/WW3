#!/bin/sh
# ---------------------------------------------------------------------------- #
# reset.sh     : Run setup.sh to restore previous experiment.                  #
#                                                                              #
# usage: reset.sh Dir1 Dir2                                                    #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 01-Jan-2010 : Origination.                                                   #
#                                                                              #
#    Copyright 2010 National Weather Service (NWS),                            #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #


cat > data << EOF
y



$1
$2


y
y
y
y
y
y
y
EOF

  ./run_setup.sh < data

  rm -f data
