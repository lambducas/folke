all: parser

ifeq ($(OS),Windows_NT) # Windows
parser:
		cd  ./src && C:\\cabal\\bin\\bnfc.exe -d -m logic.cf && make
else # Linux
parser:
	cd  ./src && bnfc -d -m logic.cf && make
endif

clean: 
	$(MAKE) -C src clean
	$(MAKE) -C src distclean