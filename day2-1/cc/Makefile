FLAGS   = -Wall -O2 -std=c++14
TARGETS = main



all: $(TARGETS)

main: main.cc
	clang++ $(FLAGS) $< -o $@

clean:
	rm -fv $(TARGETS) *.hi *.o
