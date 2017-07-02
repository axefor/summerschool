COMP=gnu

ifeq ($(COMP),cray)
CC=cc
FC=ftn 
FCFLAGS=-efmw -hcpu=sandybridge
endif

ifeq ($(COMP),gnu)
CC=gcc
FC=gfortran-mp-6
FCFLAGS=-pg -g -fbacktrace -fbounds-check -Wall -fopenmp
endif

ifeq ($(COMP),intel)
CC=icc
FC=ifort
endif

EXE=sum.x
OBJ=sum.o


all: $(EXE)

sum.o: sum.F90

$(EXE): $(OBJ)
	$(FC) $(FCFLAGS) $(OBJ) -o $@ 

%.o: %.F90
	$(FC) $(FCFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

clean:
	rm -f $(EXE)  a.out *.o *.mod *.png *~
