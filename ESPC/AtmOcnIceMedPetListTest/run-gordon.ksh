#!/bin/ksh

#PBS -e err
#PBS -o out
#PBS -A NRLMR03795024
#PBS -q debug
#PBS -j oe
#PBS -l walltime=00:05:00
#PBS -N proto
#PBS -l select=6:ncpus=6
#PBS -V

#
#unalias rm
#umask 022
#set -x
#

mkdir -p $WORKDIR/runs/$PBS_JOBID
cd $WORKDIR/runs/$PBS_JOBID
cp $WORKDIR/NESM/ESPC-v01-ss18/ESPC-SRC/proto/AtmOcnIceMedPetListTest/esmApp .

export ESMF_RUNTIME_COMPLIANCECHECK=ON:JSON=ON

#cd $WORKDIR/nuopc/AtmOcnIceMedPetListTest
#cp $HOME/nuopc/AtmOcnIceMedPetListTest/esmApp .
#rm PET* out.log core
date
aprun -n 6 ./esmApp > out.list
date

