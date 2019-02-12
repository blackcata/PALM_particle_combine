F90=ifort
FCFLAGS=-O3

TARGET= PARTICLE.exe
OBJECT= IO_module.f90 mod_particle_combine.f90 read_prt_data.f90

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
