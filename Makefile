F90=ifort
FCFLAGS=-O3

TARGET= PARTICLE_COMBINE.exe
OBJECT= IO_module.o mod_particle_combine.o read_prt_data.o

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm PARTICLE_COMBINE.exe
	rm -f *.o
	rm -f *.mod
