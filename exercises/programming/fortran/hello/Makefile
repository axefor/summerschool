COMP=cray

ifeq ($(COMP),cray)
CC=cc
FC=ftn 
FCFLAGS=-efmw -hcpu=sandybridge
endif

ifeq ($(COMP),gnu)
CC=gcc
FCFLAGS=-Wall
FC=gfortran-mp-6
FCFLAGS=-pg -g -fbacktrace -fbounds-check
endif

ifeq ($(COMP),intel)
CC=icc
FC=ifort
endif

EXE=hello
OBJ=hello.o


all: $(EXE)

hello.o: hello.F90

$(EXE): $(OBJ)
	$(FC) $(FCFLAGS) $(OBJ) -o $@ 

%.o: %.F90
	$(FC) $(FCFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

clean:
	-/bin/rm -f $(EXE)  a.out *.o *.mod *.png *~
	rm -f $(EXE)  a.out *.o *.mod *.png *~
