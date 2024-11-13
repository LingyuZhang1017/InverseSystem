.SUFFIXES:
.SUFFIXES: .o .f90 .F90

MODEL = postInv.exe

## Usage: Uncomment the settings for your system/compiler below and type "make" to build MCIP

#...IBM
#FC     = /usr/bin/xlf90
#FFLAGS = -qfree -O3 -qstrict -bmaxstack:512000000 -bmaxdata:2000000000 -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#FFLAGS = -qfree -O0 -g -qcheck -qextchk -qflttrap=ov:und:zero:inv:imp:en -qsigtrap -qinitauto=FF911299 -bmaxstack:512000000 -bmaxdata:2000000000 -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#LIBS   = -L$(IOAPI_ROOT)/AIX -lioapi \
#	 -L$(NETCDF)/lib -lnetcdf
#
#...Linux (PGF90)
#FC     = pgf90
#FFLAGS = -O4 -fastsse -pc 32 -Mfree -byteswapio -I/home/Forecast/lib/include -I/home/Forecast/lib/fixed_src1
#LIBS   = -L/home/Forecast/lib/lib -lioapi -lnetcdf 
#
#...Linux (Intel)
FC     = ifort
FFLAGS = -g -O0 -check all -C -pc32 -traceback -FR -openmp -I/software/netcdf-3.6.3-intel/include
#FFLAGS = -g -O3 -FR -vec-report0
#LIBS   = -L/software/szip/lib -lsz -L/software/hdf4/lib -lmfhdf -ldf -ljpeg -lz -L/software/netcdf-3.6.3-intel/lib -lnetcdf 
LIBS   = -L/software/szip/lib -lsz -L/software/hdf4/lib -lmfhdf -ldf -lz -L/software/netcdf-3.6.3-intel/lib -lnetcdf 
#
#...Linux (Intel 64)
#FC	= ifort
#FFLAGS	= -g -I$(M3LIB)/ioapi  -I$(NETCDF)/include
#LIBS	= -L$(M3LIB)/ioapi -lioapi -L$(NETCDF)/lib -lnetcdf
#
#...SGI Altix (using Intel Fortran)
#FC      = ifort
#FFLAGS = -g -O0 -check all -C -traceback -FR -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#FFLAGS  = -FR -O3 -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#LIBS    = -L$(IOAPI_ROOT)/Linux2_ia64ifortdbg -lioapi \
#          -L$(NETCDF)/lib -lnetcdf
#LIBS    = -L$(IOAPI_ROOT)/Linux2_ia64ifort -lioapi \
#          -L$(NETCDF)/lib -lnetcdf
#
#i...gfortran
#FC	= gfortran
#FFLAGS	= -O3 -I$(NETCDF)/include -I$(IOAPI_ROOT)/include/ioapi
#LIBS    = -L$(IOAPI_ROOT)/Linux2_x86_64gfort -lioapi \
#          -L$(NETCDF)/lib -lnetcdf

DEFS   = 

MODULES =\
 module_go_fu.o \
 module_go_print.o \
 module_go_file.o \
 module_go_string.o \
 module_global.o    \
 module_rc.o  
 
OBJS =\
 postInv.o \
 Inv_init.o 

all:
	@$(MAKE) $(MODULES)
	@$(MAKE) $(MODEL)

$(MODEL): $(OBJS)
	$(FC) -o $(MODEL) $(FFLAGS) $(OBJS) $(MODULES) $(LIBS)

.f90.o:
	$(FC) $(FFLAGS) $(INCLUDES) -c -o $@ $<

clean:
	rm -f *.o *.mod $(MODEL)

