#!/bin/sh
# ---------------------------------------------------------------------------- #
# convert_LM.sh  : Convert hurricane grads files from big to little endian     #
#                  and from absolute to difference.                            #
#                                                                              #
# usage: convert_LM.sh runID [compID]                                          #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 14-Oct-2010 : Origination.                                                   #
#                                                                              #
#    Copyright 2010 National Weather Service (NWS),                            #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e

  swap='-byteswapio'
# swap=

  echo ' '
  echo 'convert_LM.sh :'
  echo '---------------'

  if [ "$#" -lt "1" ] ; then
    echo "   Base case not defined (abort)" ; exit 1 ; fi

  echo "   Convert LM run GrADS files and place result in main directory."

  setup='.genes.env'

# ---------------------------------------------------------------------------- #
# 2. Test setup file

  if [ -f ~/$setup ]
  then
    echo "   Setup file $setup found."
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 2
  fi

# ---------------------------------------------------------------------------- #
# 3. Test command line input

  case1=$1
  dir1=$genes_data/$case1/test_LM

  if [ -d $dir1 ]
  then
    echo "   Data directory $dir1 found."
  else
    echo "   Data directory $dir1 NOT found (abort)."
    exit 3
  fi

  if [ "$#" = '1' ]
  then
    two='F'
  else
    two='T'
    case2=$2
    dir2=$genes_data/$case2/test_LM

    if [ -d $dir2 ]
    then
      echo "   Data directory $dir2 found."
    else
      echo "   Data directory $dir2 NOT found (abort)."
      exit 4
    fi
  fi

  diro=$genes_main

  if [ -d $diro ]
  then
    echo "   Data directory $diro found."
  else
    echo "   Data directory $diro NOT found (abort)."
    exit 5
  fi

# ---------------------------------------------------------------------------- #
# 4. Make programs to convert

  cd $genes_work

  echo ' '
  echo '   Making programs ...'

  rm -f read_it.*

cat > read_it.f90 << EOF
      subroutine read_it ( nds, array, nx, ny, end )
!
      integer, intent(in)  :: nds, nx, ny
      real, intent(out)    :: array(nx,ny)
      logical, intent(out) :: end
!
      read (nds,end=100,err=100) array
      end    = .false.
      return
!
  100 continue
      end    = .true.
      return
!
      end subroutine read_it
EOF

  pgf90 -c -Mlist $swap read_it.f90
  rm -f read_it.f90 read_it.lst

cat > convert.f90 << EOF
      program convert
!
      implicit none
      integer             :: nx, ny ,nf, if, ib, ix, iy
      logical             :: second, spec, end
      real, allocatable   :: fld1(:,:), fld2(:,:)
!
      write (*,900)
!
      read (*,*) nx, ny, second, spec, nf
      write (*,901) nx, ny, second, spec, nf
!
                    open (10,file='input1',form='UNFORMATTED')
      if ( second ) open (11,file='input2',form='UNFORMATTED')
                    open (50,file='output',form='UNFORMATTED')
!
      allocate ( fld1(nx,ny), fld2(nx,ny) )
!
      ib     = 0
      do
        ib     = ib + 1
        do if=1, nf
          call read_it ( 10, fld1, nx, ny, end )
          if ( end ) exit
          if ( second ) then
              call read_it ( 11, fld2, nx, ny, end )
              if ( end ) exit
            end if
          if ( (if.eq.1 .and. .not.spec)  .or. .not.second ) then
              write (50) fld1
            else
              do ix=1, nx
                do iy=1, ny
                  if ( fld1(ix,iy).gt.-999.8 .and. fld2(ix,iy).gt.-999.8 ) then
!
!                     fld2(ix,iy) = fld2(ix,iy) - fld1(ix,iy)
!
                      fld2(ix,iy) = ( fld2(ix,iy) - fld1(ix,iy) ) / &
                                       max ( 0.25 , fld1(ix,iy) ) * 100.
!
                    else
                      fld2(ix,iy) = -999.9
                    end if
                  end do
                end do
              write (50) fld2
            end if
          end do
        if ( end ) exit
        write (*,902) ib
        end do
!
      write (*,999)
!
  900 format (/' convert test_LM output'/ &
               '------------------------')
  901 format ( '    dimensions    : ',2i6/ &
               '    second field  : ',l6/  &
               '    spec          : ',l6/  &
               '    fields        : ',i6/)
  902 format ( '    finished with batch',i4)
  999 format (/' end of program'/)
!
      end program convert
EOF

  pgf90 -Mlist -o convert.x convert.f90 read_it.o 
  rm -f convert.f90 convert.lst *.o

# ---------------------------------------------------------------------------- #
# 5.  Convert data sets and place in main directory

  echo ' '
  rm -f input1 input2 output

  for file in grl_mich
  do
    echo "    Working on $file ..."

    echo "      copying $dir1/$file.ctl ..."
    cp $dir1/$file.ctl $diro/.

    echo "      creating $diro/ww3.$file ..."
    echo "         linking $dir1/ww3.$file to input1"
    ln -sf $dir1/ww3.$file ./input1

    if [ "$two" = 'T' ]
    then
      echo "         linking $dir2/ww3.$file to input2"
      ln -sf $dir2/ww3.$file ./input2
    fi

    input="66 133 $two F 16"

    echo "         linking $diro/ww3.$file to output"
    touch $diro/ww3.$file
    ln -sf $diro/ww3.$file ./output

    echo "$input" | ./convert.x > convert.out

    rm -f input1 input2 output convert.out

  done

  echo "    Working on spectra ..."

  echo "      copying $dir1/ww3.spec.ctl ..."
  cp $dir1/ww3.spec.ctl $diro/.

  echo "      copying $dir1/ww3.mean.grads ..."
  cp $dir1/ww3.mean.grads $diro/.

  echo "      creating $diro/ww3.spec.grads ..."
  echo "         linking $dir1/ww3.spec.grads to input1"
  ln -sf $dir1/ww3.spec.grads ./input1

  if [ "$two" = 'T' ]
  then
    echo "         linking $dir2/ww3.spec.grads to input2"
    ln -sf $dir2/ww3.spec.grads ./input2
  fi

  nx=`grep 'XDEF' $diro/ww3.spec.ctl | awk '{ print $2}'`
  ny=`grep 'YDEF' $diro/ww3.spec.ctl | awk '{ print $2}'`
  input="$nx $ny $two F 6"

  echo "         linking $diro/ww3.spec.grads to output"
  touch $diro/ww3.spec.grads
  ln -sf $diro/ww3.spec.grads ./output

  echo $input | ./convert.x > convert.out

  rm -f input1 input2 output convert.out

  rm -f convert.x

# ---------------------------------------------------------------------------- #
# 6. End of script

  echo ' '
  echo 'End of convert_LM.sh' 

# End of convert_LM.sh ------------------------------------------------------- #
