#
# For bootstrapping
#
.PHONY: all boot install default

#
# Bootstrap program is omake.boot
#
default:
	@echo "If you have already built omake, you should use it instead of make."
	@echo "If you need to bootstrap, use "
	@echo " - 'make boot',"
	@echo "       to build the bootstrapping (feature-limited) OMake binary './omake-boot'."
	@echo " - 'make all',"
	@echo "       to bootstrap and then build everything"
	@echo " - 'make install',"
	@echo "       to bootstrap, build, and install everything"
	@exit 1

boot: boot/Makefile
	@touch boot/Makefile.dep
	@cd boot; $(MAKE) Makefile.dep; $(MAKE) omake
	@ln -sf boot/omake omake-boot

boot/Makefile: src/Makefile
	mkdir -p boot
	ln -sf ../src/Makefile boot/Makefile

all: boot
	touch .config
	OMAKEFLAGS= ./omake-boot --dotomake .omake --force-dotomake -j2 -S --progress main
	OMAKEFLAGS= src/main/omake --dotomake .omake --force-dotomake -j2 -S --progress all

install: all
	OMAKEFLAGS= src/main/omake --dotomake .omake --force-dotomake -j2 install
