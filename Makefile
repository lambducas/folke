all: parser

parser:
	cd  ./src/Parser && bnfc -d -m logic.cf &&  make

clean: 
	$(MAKE) -C src/Parser clean
	$(MAKE) -C src/Parser distclean