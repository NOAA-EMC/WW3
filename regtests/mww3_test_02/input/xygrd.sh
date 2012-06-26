#! /bin/sh

# outer : 
#       Dimensions                  :     43      43
#       Increments             (km) :   25.00   25.00
#       X range                (km) :  -25.00 1025.00
#       Y range                (km) :  -25.00 1025.00

pgf90 -r8 xygrd.f90
./a.out<<EOF
43,43 # nx,ny
25000.0,25000.0 # sx,sy
-25000.0,-25000.0 # x0,y0
EOF
mv xgrd.inp xgrd.outer.inp
mv ygrd.inp ygrd.outer.inp

# respec (identical to coarse):
#      Dimensions                  :     13      13
#      Increments             (km) :   25.00   25.00
#      X range                (km) :  600.00  900.00
#      Y range                (km) :  600.00  900.00

# coarse :
#       Dimensions                  :     13      13
#       Increments             (km) :   25.00   25.00
#       X range                (km) :  600.00  900.00
#       Y range                (km) :  600.00  900.00

pgf90 -r8 xygrd.f90
./a.out<<EOF
13,13 # nx,ny
25000.0,25000.0 # sx,sy
600000.0,600000.0 # x0,y0
EOF
mv xgrd.inp xgrd.coarse.inp
mv ygrd.inp ygrd.coarse.inp

# fine :
#       Dimensions                  :     25      25
#       Increments             (km) :   12.50   12.50
#       X range                (km) :  600.00  900.00
#       Y range                (km) :  600.00  900.00

pgf90 -r8 xygrd.f90
./a.out<<EOF
25,25 # nx,ny
12500.0,12500.0 # sx,sy
600000.0,600000.0 # x0,y0
EOF
mv xgrd.inp xgrd.fine.inp
mv ygrd.inp ygrd.fine.inp

# tiny :
#      Dimensions                  :     61      61
#      Increments             (km) :    5.00    5.00
#      X range                (km) :  600.00  900.00
#      Y range                (km) :  600.00  900.00

pgf90 -r8 xygrd.f90
./a.out<<EOF
61,61 # nx,ny
05000.0,05000.0 # sx,sy
600000.0,600000.0 # x0,y0
EOF
mv xgrd.inp xgrd.tiny.inp
mv ygrd.inp ygrd.tiny.inp
