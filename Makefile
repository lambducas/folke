all: parser

ifeq ($(OS),Windows_NT) # Windows
parser:
		cd ./src && \
			C:\\cabal\\bin\\bnfc.exe -d -m logic.cf && \
			make ALEX=C:\\cabal\\bin\\alex.exe HAPPY=C:\\cabal\\bin\\happy.exe GHC_OPTS='-package array' all
else # Linux
parser:
	cd  ./src && bnfc -d -m logic.cf && make GHC_OPTS='-package array' all
endif

clean: 
	$(MAKE) -C src clean
	$(MAKE) -C src distclean