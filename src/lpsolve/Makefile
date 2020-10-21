PKG_CPPFLAGS=-I . -DINTEGERTIME -DPARSER_LP -DBUILDING_FOR_R -DYY_NEVER_INTERACTIVE -DUSRDLL -DCLOCKTIME -DRoleIsExternalInvEngine -DINVERSE_ACTIVE=INVERSE_LUSOL -DINLINE=static -DParanoia

LIB = ../lpSolve.a

SRCS := $(wildcard *.c)
OBJS := $(patsubst %.c,%.o,$(SRCS))

library: $(LIB)

%.o: %.cpp
	$(CC) $(CFLAGS) -c $<

$(LIB): $(OBJS)
	$(AR) -rucs $(LIB) $(OBJS)

mostlyclean: clean

clean0:
	@-rm -f *.o

cleanLibs:
	@-rm -rf .libs _libs $(LIB)

clean: clean0 cleanLibs
