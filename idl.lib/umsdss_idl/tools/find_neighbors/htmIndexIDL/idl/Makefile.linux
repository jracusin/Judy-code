#	PACKAGE:	SpatialIndex
#
# os: linux
# --- Macros -----------------------------------------------------------------

CCXX =		g++
LD = 		g++
INCLUDE =	-I../include
CCFLAGS =	${INCLUDE} -DLINKAGE= -fPIC -DSTAND_ALONE -DPIC -O
LDFLAGS = -shared -export-dynamic
CP      =       cp
RM      =       rm -f

LIBRARY =	../lib/libSpatialIndex.a
SOFILES = htmLookupRadec.so htmIntersectRadec.so
LOOKUP_OBJ     = htmLookupRadec.o
INTERSECT_OBJ  = htmIntersectRadec.o

LDLIBS  = ${LIBRARY} -lm

htmLookupRadec.so: ${LOOKUP_OBJ} ${LIBRARY}
	@ echo linking $@
	@ ${LD} -o $@ ${LDFLAGS} ${LOOKUP_OBJ} ${LDLIBS}

htmIntersectRadec.so: ${INTERSECT_OBJ} ${LIBRARY}
	@ echo linking $@
	@ ${LD} -o $@ ${LDFLAGS} ${INTERSECT_OBJ} ${LDLIBS}

apps: ${SOFILES}
	- cp ${SOFILES} ../bin

all: apps

clean:
	- ${RM} ${LOOKUP_OBJ} ${INTERSECT_OBJ} ${SOFILES}
	- ${RM} ../bin/htmLookupRadec.so ../bin/htmIntersectRadec.so

.SUFFIXES:
.SUFFIXES: .o .cpp

.cpp.o:
	@ echo compiling $<
	@ ${CCXX} -c ${CCFLAGS} $<
