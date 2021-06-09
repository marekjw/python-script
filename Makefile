.PHONY: all clean grammar

all: grammar interpreter

grammar: ./grammar/PythonScript.cf
	cd src; bnfc --haskell -d   -m ../grammar/PythonScript.cf  &&  make

interpreter:
	cd src; stack ghc main.hs
	mv src/main interpreter

clean:
	- cd src/PythonScript; rm *
	- cd src; rm *.hi *.o
	- rm interpreter
