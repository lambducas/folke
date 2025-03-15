all: parser

parser:
	cd  ./src && bnfc -d -m logic.cf &&  make

clean: 
	$(MAKE) -C src clean
	$(MAKE) -C src distclean