STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
BOOST_NOT_IN_BH_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'boost_not_in_BH', package = 'rstan'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -I"$(BOOST_NOT_IN_BH_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_NO_CXX11_RVALUE_REFERENCES

CXX_STD = CXX11
SOURCES = $(wildcard stan_files/*.stan)
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)

clean:
		RM -rf stan_files/*.o
		RM -rf *.so *.o
		RM -rf stan_files/*.cc
		RM -rf stan_files/*.hpp

%.cc: %.stan
				"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<


.phony: clean
