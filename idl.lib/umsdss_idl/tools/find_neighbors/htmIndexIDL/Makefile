#
#
#	PACKAGE:	SpatialIndex
#

# --- Targets ----------------------------------------------------------------

default: all

all clean :
	@ cd src; ${MAKE} $@
	@ cd app; ${MAKE} $@
	@ cd idl; ${MAKE} $@

libs :
	@ cd src; ${MAKE} all

apps : libs
	@ cd app; ${MAKE} all

idl: libs
	@ cd idl; ${MAKE} all

linux :
	@ cd app; cp Makefile.linux Makefile
	@ cd src; cp Makefile.linux Makefile
	@ cd idl; cp Makefile.linux Makefile
	@ cd include; cp VarStr.hpp.linux VarStr.hpp; cp SpatialGeneral.h.linux SpatialGeneral.h

osf:
	@ cd app; cp Makefile.osf Makefile
	@ cd src; cp Makefile.osf Makefile
	@ cd idl; cp Makefile.osf Makefile
	@ cd include; cp VarStr.hpp.osf VarStr.hpp; cp SpatialGeneral.h.osf SpatialGeneral.h

make tidy :
	@ cd src; ${MAKE} $@
	@ cd app; ${MAKE} $@

