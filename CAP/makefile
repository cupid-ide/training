# User must customize the following make variable
INSTALLDIR=$(HOME)/CAP-INSTALLS/model_cap_$(installdate)

installdate := $(shell date '+%Y-%m-%d-%H-%M-%S')

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif
include         $(ESMFMKFILE)
ESMF_INC        = $(ESMF_F90COMPILEPATHS)
ESMF_LIB        = $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)
UTILINCS        = 

.SUFFIXES: .F90

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(UTILINCS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<

.PRECIOUS: %.o

PWDDIR := $(shell pwd)

MAKEFILE = makefile

LIBRARY  = libmodel_cap.a

MODULES  = model_cap.o

all default: 
	@make -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY): $(MODULES)
	$(AR) $(ARFLAGS) $@ $?

	rm -f model.mk.install
	@echo "# ESMF self-describing build dependency makefile fragment" > model.mk.install
	@echo "# src location Zeus: $pwd" >> model.mk.install
	@echo  >> model.mk.install
	@echo "ESMF_DEP_FRONT     = model_cap_mod" >> model.mk.install
	@echo "ESMF_DEP_INCPATH   = $(INSTALLDIR)" >> model.mk.install
	@echo "ESMF_DEP_CMPL_OBJS = " >> model.mk.install
	@echo "ESMF_DEP_LINK_OBJS = $(INSTALLDIR)/libmodel.a $(INSTALLDIR)/liblanl_model.a" >> model.mk.install
	mkdir -p $(INSTALLDIR)
	cp -f libmodel_cap.a model_cap.mod $(INSTALLDIR) 
	cp -f model.mk.install $(INSTALLDIR)/model.mk

clean:

	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst depend
