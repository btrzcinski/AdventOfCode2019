FLAGS   = -Wall -O2 -threaded -feager-blackholing
TARGETS = main



all: $(TARGETS)

main: Main.hs
	ghc $(FLAGS) $< -o $@

clean:
	rm -fv $(TARGETS) *.hi *.o
