F90=ifort
FCFLAGS=-O3

TARGET= PARTICLE_PDF.exe
OBJECT= mod_particle_PDF.o main_PDF.o

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm PARTICLE_PDF.exe
	rm -f *.o
	rm -f *.mod
