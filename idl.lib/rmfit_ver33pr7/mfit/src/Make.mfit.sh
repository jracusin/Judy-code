#!/bin/bash

#  Bash shell script to compile MFIT for Linux or Darwin (=Mac OS X).
#  Michael S. Briggs, UAH/NSSTC, 2009 April 11 -- 14, 23, 28, May 4 & 9 & 11,
#  June 4: fno-strict-aliasing option for compilation of mfit_idl.c

#  Prerequisites:
#  1) The environment variable IDL_DIR must be defined.
#  To test: ls ${IDL_DIR}/external/*.h should show "export.h".
#  2) The GNU Scientific Library (GSL) needs to be installed.
#  If libgsl.so is installed at /usr/lib the linker will automatically find it;
#  the linker will most likely find libraries at /usr/local/lib.  If libgsl.so
#  is located at a less standard place on your computer, provide the path
#  containing /lib/libgsl.so to this script with the environment variable GSL_PATH,
#  e.g., export GSL_PATH=/opt/local/ if libgsl.so is at /opt/local/lib/.
#  This will cause this script to specify the locations of both the library and
#  include files.

#  To run this script: invoke in the directory rmfit/mfit/src containing
#  the mfit source code via: ./Make.mfit.sh

#  If you have a name for your version of gcc other than "gcc", set the
#  environment variable "CC", e.g., "export CC=gcc-4.3".
#  If you have a name for your version of gfortran other than "gfortran",
#  set the environment variable "F95", e.g., "export F95=gfortran-4.3".

#  MFIT should compile & link with other C and Fortran 95 compilers, but
#  the option flags below are customized to gcc and gfortran.  If you wish
#  to use different compilers, you will have to edit this script.

#  MFIT should compile & link on other Unix-like operating systems, but the
#  option flags may need to be revised -- especially the linker flags
#  LINK_REG_FLAGS.

#  There are three OPTIONAL command line arguments for use by developers.
#  Normally users of rmfit/mfit should omit these arguments.
#  The optional arguments may be used on the command line in any number,
#  combination or order: they are recognized by their values.

#  One optional argument is "debug", "Debug" or "DEBUG".    Any of these will
#  cause the inclusion of debugging options on the compliation.   Omitting
#  these values causes a normal/default compilation without debug flags
#  to the compiler.
#  Another optional argument may be specified in two different forms.  The
#  first form is is "M" or "m" follwed by decimal digits.  The second is a
#  hexadecimal number 0x followed by digits from 0 to F (lower- or upper-case
#  letters).  The digits form a non-negative integer which functions as a
#  bit mask in MFIT -- non-zero bits turn on optional output.  Specifying 0
#  or omitting the argument turns off all optional output from MFIT.
#  The final optional argument is "clean", "Clean" or "CLEAN".  Any of these
#  will cause this script to delete the output files but to NOT make new
#  output files.


#  *** Script begins:


#  Process optional command line arguments:

#  Setup default results, for any optional argument not provided:
DEBUG=false
CLEAN_ONLY=false
declare -i OUTPUT_MASK=0


#  loop through the arguments, trying to identify them, then once identified,
#  procesing them:

for ARG in "$@" ; do

   # until the current argument is identified, mark as unidentified:
   ARG_ID=false

   #  Debug?
   if [ $ARG = "debug" ]  ||  [ $ARG = "Debug" ]  ||  [ $ARG = "DEBUG" ]; then
      ARG_ID=true
      DEBUG=true
   fi

   #  Clean?
   if [ $ARG = "clean" ]  ||  [ $ARG = "Clean" ]  ||  [ $ARG = "CLEAN" ]; then
      ARG_ID=true
      CLEAN_ONLY=true
   fi

   #  Output Mask (form 1)?  --  recognized by leading "M" or "m":
   LEADING_CHAR=${ARG:0:1}
   if [ $LEADING_CHAR = "M" ]  ||  [ $LEADING_CHAR = "m" ]; then
      MASK_STRING=${ARG:1}
      if [ -n "$(echo $MASK_STRING | grep '^[0-9]*$')" ]; then
         ARG_ID=true
         OUTPUT_MASK=MASK_STRING
      else
         echo "The characters after $LEADING_CHAR must be only digits!"
         echo $ARG
         exit
      fi
   fi

   #  Output Mask (form 2)?  --  recognized by leading "0x":
   LEADING_CHAR=${ARG:0:2}
   if [ $LEADING_CHAR = "0x" ]; then
      HEX_DIGITS=${ARG:2}
      if [ -n "$(echo $HEX_DIGITS | grep '^[0-9A-Fa-f]*$')" ]; then
         ARG_ID=true
         OUTPUT_MASK=$(($ARG))   # converts hex to decimal
      else
         echo "The characters after $LEADING_CHAR must be only digits and A to F / a to f !"
         echo $ARG
         exit
      fi
   fi


   #  Did we identify the argument?
   if ! $ARG_ID ; then
      echo -e "\nArgument $ARG is not recognized!"
      exit
   fi

done



if ! $CLEAN_ONLY ; then


   #  Setup most compile options that don't depend on the operating system:
   #  (Some additional non-OS dependent linker options below)
   
   F95_REG_FLAGS=" -c -O3  -fimplicit-none  -fno-backslash  -fPIC  -fno-underscoring "
   CC_REG_FLAGS=" -c  -O3  -std=c99  -fPIC  -DHAVE_INLINE  -DMFIT_DEBUG=$OUTPUT_MASK  -I${IDL_DIR}/external "
   
   if $DEBUG ; then

      F95_DEBUG_FLAGS=" -std=f2003  -Wall  -pedantic  -Wline-truncation  -Wimplicit-interface  -Wunused-parameter  -fbounds-check "
      CC_DEBUG_FLAGS=" -Wall  -Wextra "

   else

      F95_DEBUG_FLAGS=" "
      CC_DEBUG_FLAGS=" "

      #  GSL vector / matrix routines have subscript checking by default -- remove for non-debug compile:
      CC_REG_FLAGS="$CC_REG_FLAGS  -DGSL_RANGE_CHECK_OFF "

   fi
   
   
   #  Setup compile & link options that depend on the operating system:
   
   OS_NAME=$(uname)
   
   echo -e "\nThe Operating System is identified as $OS_NAME."
   
   if [ $OS_NAME = "Darwin" ] ; then
   
      echo "Will make MFIT for Darwin (Mac OS X)."
      echo -e "\nFor Darwin you need to explicitly select the bit-level of the MFIT executable to"
      echo "match the bit-level of that IDL is using -- see the rmfit README."
      echo "Or if the dynamic link fails when you try a spectral fit in rmfit, re-make MFIT and"
      echo "select the other bit-level."
      echo -e "\nPlease select a 32- or 64-bit executable:"
   
      select ANSWER in 32 64 ; do
   
         case "$REPLY" in
            32 )
               BIT_LEVEL=" "
               break ;;
            64 )
               BIT_LEVEL=" -m64 "
               break ;;
            * )
               echo "Please input 32 or 64." ;;
   
         esac
      done
   
      LINK_REG_FLAGS="  -bundle  -undefined dynamic_lookup "


      #  BSD command available in Darwin -- obtain machine's architecture type.
      #  "man arch" implies that the possibilties are: i386, ppc, ppc64, x86_64.
      #  Use this to add optimization options that are architecture dependent.
      #  For GNU compilers >=4.2 and for targets IA-32 / x86-64 (see comments under
      #  Linux, below, about these processor architectures), option march=native,
      #  produces code optimized for the host processor.   Since we require gfortran 
      #  version >= 4.3 (in order to obtain the ISO C Binding), this option should 
      #  always work with gfortran.
      
      ARCH=$(arch)
      
      if [ $ARCH = "i386" ]  ||  [ $ARCH = "x86_64" ]; then
         echo -e "\nAdding options for an Intel Processor: $ARCH"
         F95_REG_FLAGS="$F95_REG_FLAGS  -march=native "
      fi
   
   else     # Darwin?
   
      echo "Will make MFIT for Linux."
   
      BIT_LEVEL=" "
   
      LINK_REG_FLAGS=" -shared  --warn-once "
      
      #  It must be very rare that Linux is run on anything other than Intel/AMD processors.
      #  So we add an optimization flag that is only for the targets IA-32 (aka x86, x86-32 or i386)
      #  or x86-64 (aka amd64) (but the flag is not for IA-64 = Itanium).
      #  See additonal comments under Darwin, above.
      #  If someone is using Linux on a different processor, they will need to delete 
      #  or comment out the next line.
      F95_REG_FLAGS="$F95_REG_FLAGS  -march=native "
      
   fi     # Darwin?
   
   
   #  Back to options that don't depend on operating system:
   
   #  Flags for linking to GSL:
   #  1) Add paths to find GSL, if the user provided the environment varaiable GSL_PATH because GSL
   #  is in a non-standard location.   We have to setup both the include directory for the
   #  C-compilation, and the library directory for the link -- the "-I" and "-L" options respectively.
   #  2) We always have to add "-l" options to inform the linker to link to libgsl, libgslcblas and libm.
   
   if [ -n "${GSL_PATH:+x}" ]; then
      CC_REG_FLAGS="$CC_REG_FLAGS  -I${GSL_PATH}/include/ "
#      CC_REG_FLAGS="$CC_REG_FLAGS  -I${GSL_PATH}/ "
      LINK_REG_FLAGS="$LINK_REG_FLAGS  -L${GSL_PATH}/lib/  -lgsl  -lgslcblas  -lm "
   else
      LINK_REG_FLAGS="$LINK_REG_FLAGS  -lgsl  -lgslcblas  -lm "
   fi
   
   
   #  Output the compile and link options that have been setup:

   echo -e "\n\nThese are the flags that will be used with the compilers and the linker:"
   echo "C regular:" $CC_REG_FLAGS
   if [ ${#CC_DEBUG_FLAGS} != 1 ]; then
      echo "C debug:" $CC_DEBUG_FLAGS
   fi
   echo "F95 regular:" $F95_REG_FLAGS
   if [ ${#F95_DEBUG_FLAGS} != 1 ]; then
      echo "F95 debug:" $F95_DEBUG_FLAGS
   fi
   echo "Linker:" $LINK_REG_FLAGS
   echo "Bit Level:" $BIT_LEVEL
   echo " "
   
   
   
   #  Setup the commands for the compilers -- normally "gcc" and "gfortran", but
   #  if either environment variable CC or F95 is already defined, that selection
   #  takes precedence.
   
   if [ -n "${CC:+x}" ]; then
     echo -e "\nUsing environment variable CC for the C compiler:" $CC
   else
     CC="gcc"
   fi

   echo -e "\nC compiler version is:"
   $CC -v
   echo -e "\n\n"

   if [ -n "${F95:+x}" ]; then
     echo -e "\nUsing environment variable F95 for the Fortran 95 compiler:" $F95
   else
     F95="gfortran"
   fi
   
   echo -e "\nFortran 95 compiler version is:"
   $F95 -v
   echo -e "\n"



fi    # CLEAN_ONLY ?



#  Setup lists of files:

CC_SOURCE_FILE_LIST_ONE=" mfit_idl.c "
CC_SOURCE_FILE_LIST_TWO=" mfit_interface.c  gsl_interface_routines.c "

# .F invokes the preprocssor, .f doesn't:
F90_SOURCE_FILE_LIST=" mfit_kinds.f90  gsl_interfaces.f90  mfit_user_func_a.f90  mfit_user_func_b.f90  mfit.F90 "

OBJ_FILE_LIST=" mfit_idl.o  mfit_interface.o  gsl_interface_routines.o \
 mfit_user_func_a.o  mfit_user_func_b.o  mfit.o"

NON_LINK_OBJ_FILE_LIST=" mfit_kinds.o  gsl_interfaces.o "

MODULE_FILE_LIST=" mfit_kinds.mod  gsl_interfaces.mod  mfit_parameters.mod  mfit_module.mod \
 shr_vars_new_titarchuk.mod  shr_vars_sunyaev_titarchuk.mod "

OUTPUT_DYLIB=" ../dlm/mfit.so "



#  Setup over ....  perform the actual "Make" !

# The following have the form of: 1) message, 2) define command,
# 3) echo command to screen, 4) execute command without testing for sucess vs failure.

echo -e "\n\n *** *** Step 1: \"CLEAN\": Delete previous output files, if any:"
CMD="rm -fv  $NON_LINK_OBJ_FILE_LIST  $OBJ_FILE_LIST"
echo -e "\n"$CMD
($CMD)

CMD="rm -fv  $MODULE_FILE_LIST"
echo -e "\n"$CMD
($CMD)

CMD="rm -fv $OUTPUT_DYLIB"
echo -e "\n"$CMD
($CMD)


#  Was "clean [only]" requested?  If so, exit here before "making" new files:
if $CLEAN_ONLY ; then
   echo -e "\n\n *** *** The CLEAN option was requested, so no further actions...."
   exit
fi


# The following have the form of: 1) message, 2) define command,
# 3) echo command to screen, 4) execute command -- test for failure,
# 5) failure case -- message and exit.


#  The compilation of the C routines is broken into two groups because mfit_idl.c requires the -fno-strict-aliasing 
#  option to suppress a warning msg when the debug option is selected; perhaps -fno-strict-aliasing is required to
#  to obtain a correct executable.


echo -e "\n\n *** *** Step 2: \"COMPILE C\": Compile C routines of MFIT:"
CMD="$CC  $CC_REG_FLAGS  -fno-strict-aliasing  $CC_DEBUG_FLAGS  $BIT_LEVEL  $CC_SOURCE_FILE_LIST_ONE"
echo -e "\n"$CMD
if ! ($CMD) ; then
   echo -e "\nC Compilation one failed -- exiting!\n"
   exit
fi
CMD="$CC  $CC_REG_FLAGS  $CC_DEBUG_FLAGS  $BIT_LEVEL  $CC_SOURCE_FILE_LIST_TWO"
echo -e "\n"$CMD
if ! ($CMD) ; then
   echo -e "\nC Compilation two failed -- exiting!\n"
   exit
fi


echo -e "\n\n *** *** Step 3: \"COMPILE F95\": Compile Fortran 90/95/2003 routines of MFIT:"
CMD="$F95  $F95_REG_FLAGS  $F95_DEBUG_FLAGS  $BIT_LEVEL  $F90_SOURCE_FILE_LIST"
echo -e "\n"$CMD
if ! ($CMD) ; then
   echo -e "\nFortran Compilation failed -- exiting!\n"
   exit
fi


echo -e "\n\n *** *** Step 4: \"LINK\":"
CMD="$F95  $LINK_REG_FLAGS  $BIT_LEVEL  $OBJ_FILE_LIST  -o $OUTPUT_DYLIB"
echo -e "\n"$CMD
if ! ($CMD) ; then
   echo -e "\nLink failed -- exiting!\n"
   exit
fi


#  To be thourough, issue success message only if the output file was actually created.
#  Unfortunately the combination of reaching here and the output file existing isn't
#  proof of success -- the linker will proceed and create the output file despite
#  serious warning messages.

if [ -e $OUTPUT_DYLIB ]; then
   echo -e "\n\n *** *** DONE! : MFIT Make completed successfully!"
   echo -e "But check for warning messages from the commands...\n"
else
   echo -e "\n\n *** *** The output file was not created.  The Make failed!\n"
fi


exit