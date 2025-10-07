FC = gfortran
FLAGS = -Wall -O2
SRCS = src/list_sub.f90 src/soiltemperature.f90 src/SoiltempComponent.f90 main.f90
OBJS = $(SRCS:.f90=.o)
EXEC = build/my_program

all: $(EXEC)

$(EXEC): $(OBJS)
	$(FC) $(FLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(EXEC)
