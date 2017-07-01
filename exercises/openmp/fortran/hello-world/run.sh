#!/bin/bash
#SBATCH -J omp-test
#SBATCH -p test
#SBATCH -N 1
#SBATCH -t 00:10:00

export OMP_NUM_THREADS=4

./omp
