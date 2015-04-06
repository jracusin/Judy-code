#!/bin/bash

#  Bash shell script to compile MFIT for Linux (and Solaris).
#  This variant uses the Sun Studio Tools.
#  Tested with the Express Release of March 2009.

#  Michael S. Briggs, UAH/NSSTC, 2009 April 11 -- 14, 23, 28, May 10.


echo -e "\n\nThis version of the \"Make\" shell script compiles MFIT using Sun Studio C and Fortran 95"
echo -e "compilers.  But the Sun compilers are not supported -- the supported compilers are gcc & gfortran."
echo -e "You may need to make changes to this script or to the source code.  It's up to you.  Good luck.\n"
sleep 3



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

#  If you have a name for your version of sunc99 other than "sunc99", set the
#  environment variable "CC".".
#  If you have a name for your version of sunf95 other than "sunf95",
#  set the environment variable "F95".

#  MFIT should compile & link with other C and Fortran 95 compilers, but
#  the option flags below are customized to sunc99 and sunf95.  If you wish
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


 # Abort for Darwin OS -- not supported by Sun..
   
OS_NAME=$(uname)
   
echo -e "\nThe Operating System is identified as $OS_NAME."
   
if [ $OS_NAME = "Darwin" ] ; then
   echo -e "\n\nThe Sun Studio compilers are not available for Darwin (Mac OS X).\n"
   exit
else
   
   echo "Will make MFIT for Linux or Solaris (untested!)."
   
fi



if ! $CLEAN_ONLY ; then


   #  Setup most compile options that don't depend on the operating system:
   #  (Some additional non-OS dependent linker options below)
   
   F95_REG_FLAGS=" -c -O4  -native  -xautopar  -xipo  -xlibmopt  -xlibmil  -xvector=simd  -KPIC "
   CC_REG_FLAGS="  -c -O4  -native  -xautopar  -xipo  -xlibmopt  -xlibmil  -xvector=simd  -KPIC  \
-DMFIT_DEBUG=$OUTPUT_MASK  -I${IDL_DIR}/external  -I/usr/local/include/ "
   
   if $DEBUG ; then
     F95_DEBUG_FLAGS=" -v  -C "
     CC_DEBUG_FLAGS=" -v "
   else
     F95_DEBUG_FLAGS=" "
     CC_DEBUG_FLAGS=" "
   fi

   
   BIT_LEVEL=" "
   
   LINK_REG_FLAGS=" -G "
   
  
   
   
   #  More options...
   #  Add paths to find GSL, if the user provided the environment varaiable GSL_PATH because GSL
   #  is in a non-standard location.   We have to setup both the include directory for the
   #  C-compilation, and the library directory for the link -- the -I and -L options respectively.
   
   #  We always have to add the "little l" option to inform the linker to link to libgsl.
   
   if [ -n "${GSL_PATH:+x}" ]; then
      CC_REG_FLAGS="$CC_REG_FLAGS  -I${GSL_PATH}/include/ "
      LINK_REG_FLAGS="$LINK_REG_FLAGS  -L${GSL_PATH}/lib/  -lgsl  -lgslcblas  -lm "
   else
      LINK_REG_FLAGS="$LINK_REG_FLAGS  -lgsl  -lgslcblas  -lm "
   fi
   
   
   
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
   
   
   
   #  Setup the commands for the compilers -- normally "sunc99" and "sunf95", but
   #  if either environment variable CC or F95 is already defined, that selection
   #  takes precedence.
   
   if [ -n "${CC:+x}" ]; then
     echo -e "\nUsing environment variable CC for the C compiler:" $CC
   else
     CC="sunc99"
   fi
   
   echo -e "\nC compiler version is:"
   $CC -V
   echo -e "\n\n"
   
   if [ -n "${F95:+x}" ]; then
     echo -e "\nUsing environment variable F95 for the Fortran 95 compiler:" $F95
   else
     F95="sunf95"
   fi
   
   echo -e "\nFortran 95 compiler version is:"
   $F95 -V
   echo -e "\n\n"


fi    # CLEAN_ONLY ?



#  Lists of files:

CC_SOURCE_FILE_LIST=" mfit_idl.c mfit_interface.c  gsl_interface_routines.c "

# .F invokes C-preprocssor, .F doesn't:
F95_SOURCE_FILE_LIST=" mfit_kinds.f95  gsl_interfaces.f95  mfit_user_func_a.f95  mfit_user_func_b.f95  mfit.F95 "

OBJ_FILE_LIST=" mfit_idl.o  mfit_interface.o  gsl_interface_routines.o \
 mfit_user_func_a.o  mfit_user_func_b.o  mfit.o"

NON_LINK_OBJ_FILE_LIST=" mfit_kinds.o  GSL_interfaces.o "

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

echo -e "\n\n *** *** Step 2: \"COMPILE C\": Compile C routines of MFIT:"
CMD="$CC  $CC_REG_FLAGS  $CC_DEBUG_FLAGS  $BIT_LEVEL  $CC_SOURCE_FILE_LIST"
echo -e "\n"$CMD
if ! ($CMD) ; then
   echo -e "\nC Compilation failed -- exiting!\n"
   exit
fi

echo -e "\n\n *** *** Step 3: \"COMPILE F95\": Compile Fortran 95 routine of MFIT:"
CMD="$F95  $F95_REG_FLAGS  $F95_DEBUG_FLAGS  $BIT_LEVEL  $F95_SOURCE_FILE_LIST"
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