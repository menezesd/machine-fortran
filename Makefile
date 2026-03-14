FC = gfortran-mp-15
FFLAGS = -O2 -Wall -Wextra -std=f2008 -fall-intrinsics
FFLAGS_DEBUG = -g -O0 -Wall -Wextra -std=f2008 -fall-intrinsics -fcheck=all -fbacktrace

OBJS = memory_mod.o header_mod.o stack_mod.o text_mod.o object_mod.o \
       decode_mod.o quetzal_mod.o execute_mod.o zmachine.o

TARGET = zmachine

all: $(TARGET)

debug: FFLAGS = $(FFLAGS_DEBUG)
debug: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# Module dependencies
memory_mod.o: memory_mod.f90
	$(FC) $(FFLAGS) -c $<

header_mod.o: header_mod.f90 memory_mod.o
	$(FC) $(FFLAGS) -c $<

stack_mod.o: stack_mod.f90 memory_mod.o header_mod.o
	$(FC) $(FFLAGS) -c $<

text_mod.o: text_mod.f90 memory_mod.o header_mod.o
	$(FC) $(FFLAGS) -c $<

object_mod.o: object_mod.f90 memory_mod.o header_mod.o text_mod.o
	$(FC) $(FFLAGS) -c $<

decode_mod.o: decode_mod.f90 memory_mod.o
	$(FC) $(FFLAGS) -c $<

quetzal_mod.o: quetzal_mod.f90 memory_mod.o header_mod.o stack_mod.o
	$(FC) $(FFLAGS) -c $<

execute_mod.o: execute_mod.f90 memory_mod.o header_mod.o stack_mod.o text_mod.o object_mod.o decode_mod.o quetzal_mod.o
	$(FC) $(FFLAGS) -c $<

zmachine.o: zmachine.f90 memory_mod.o header_mod.o stack_mod.o text_mod.o object_mod.o decode_mod.o execute_mod.o
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f $(TARGET) *.o *.mod

.PHONY: all debug clean
