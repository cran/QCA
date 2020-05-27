#PKG_CFLAGS += -O3 -Wall -pipe -pedantic -std=gnu99
#PKG_CXXFLAGS += -O3 -Wall -pipe -Wno-unused -pedantic
PKG_CPPFLAGS=-I . -Ibfp -Ibfp/bfp_LUSOL -Ibfp/bfp_LUSOL/LUSOL -Icolamd -Ilp_solve -Ishared -DINTEGERTIME -DPARSER_LP -DBUILDING_FOR_R -DYY_NEVER_INTERACTIVE -DUSRDLL -DCLOCKTIME -DRoleIsExternalInvEngine -DINVERSE_ACTIVE=INVERSE_LUSOL -DINLINE=static -DParanoia

LIB = ../lpsolve.a

SRCS := $(wildcard *.c) $(wildcard shared/*.c) bfp/bfp_LUSOL/lp_LUSOL.c bfp/bfp_LUSOL/LUSOL/lusol.c colamd/colamd.c
OBJS := $(patsubst %.c,%.o,$(SRCS))

#$(info $$SRCS is [${SRCS}])
#$(info $$OBJS is [${OBJS}])

library: $(LIB) clean0

#include make_o.mk

%.o: %.cpp
	$(CC) $(CFLAGS) -c $<

$(LIB): $(OBJS)
	$(AR) -rucs $(LIB) $(OBJS)

mostlyclean: clean

clean0:
	@-rm -f *.o
	@-rm -f lp_solve/*.o
	@-rm -f shared/*.o
	@-rm -f colamd/*.o
	@-rm -f bfp/*.o
	@-rm -f bfp/bfp_LUSOL/*.o
	@-rm -f bfp/bfp_LUSOL/LUSOL/*.o

cleanLibs:
	@-rm -rf .libs _libs $(LIB)

clean: clean0 cleanLibs
