 module use ../../../NEMS/src/conf 
 module load modules.nems              ; module list   
 ulimit -S -s $stack ; set -x ; cd /scratch4/NCEPDEV/ocean/noscrub/Henrique.Alves/EMC_FV3-WW3/WW3/model/esmf
 export COMP_SRCDIR="/scratch4/NCEPDEV/ocean/noscrub/Henrique.Alves/EMC_FV3-WW3/WW3/model" COMP_BINDIR="/scratch4/NCEPDEV/ocean/noscrub/Henrique.Alves/EMC_FV3-WW3/WW3/WW3_INSTALL" WW3_COMP="theia"
 set -e
 exec make -j 1 WW3_COMP="theia" ww3_nems 1> make.out 2>&1

