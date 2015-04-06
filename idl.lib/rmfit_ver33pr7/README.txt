; -----------------------------------------------------------------------------
; RMFIT
; A Lightcurve and Spectral Analysis Tool
; 
; Copyright (C) 2000 Robert S. Mallozzi
; Portions Copyright (C) 2009 Robert D. Preece & Michael S. Briggs
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
; -----------------------------------------------------------------------------


INSTALLATION and RUNNING

0. This README is for version 3.2.   At this time mfit has not been compiled
   under Microsoft Windows so spectral fitting is not possible under Windows.
   The interface between rmfit and mfit has changed so previous versions of mfit,
   and compilations of those versions, are not usable with rmfit 2.4 or higher.


1. To use rmfit in the IDL virtual machine (that is, without an IDL license), 
   you first need to download IDL - at least version 6.1.1 - to your machine and set it 
   up (demo mode is OK). Next, download the virtual machine from the ITT website 
   and install it, according to the instructions. 
   Please see the "RMFit Windows Installation Instructions" documentation for 
   details on how to install rmfit uder Windows.

   If you are going to run rmfit with a licensed copy of IDL:
   You must first install the IDL Astronomy User's Library and make sure that the 
   installation directory is mentioned in your IDL_PATH environment variable (see
   step 6 below). The IDL Astronomy User's Library is available from:
   http://idlastro.gsfc.nasa.gov/.    Download file astron.dir.tar.gz from
   http://idlastro.gsfc.nasa.gov/ftp; untar this file and make directory "pro"
   available on IDL_PATH (step 6 below).
   
   
2. You will probably find it convenient to add the shell commands defining IDL_DIR,
   IDL_PATH and IDL_DLM_PATH (see steps below) to the shell script that runs when
   you open a new terminal window, e.g., .bash_profile, .bashrc or .profile.
   
   
3. Be sure to setup IDL.   This will correctly define the environment variable
   IDL_DIR and setup the path that your operating system uses to find items related
   to IDL.   The environment variable IDL_DIR is now essential to the "make" process
   for mfit (step 4).  If IDL_DIR isn't defined follow IDL installation instructions
   (true also for the Virtual Machine).   Typical methods to setup IDL and define 
   IDL_DIR are:
   
      source  /Applications/itt/idl/bin/idl_setup.bash     [Mac OS X]
      source  /opt/itt/idl/bin/idl_setup.bash              [Linux]
   
   Earlier versions of IDL will use "rsi" instead of "itt".
 
   To test that IDL_DIR is defined, type the shell command: "echo $IDL_DIR", which
   should output the directory that you have defined, or type
   "ls ${IDL_DIR}/external", which should list various files including export.h.   

   On systems where the IDL windows remain blank if you cover them with another window
   and subsequently uncover them, you will have to change the 'backing store' method:
	
   1) Start up the IDL Developer Environment by typing 'idlde' at the command line
	
   (if you have the Virtual Machine, this puts you into 7 minute 'Timed Demo Mode').
	
   2) Select the 'Preferences:' menu command in the idlde.
	
   3) Select the disclosure triangle for the 'Graphics' item to reveal the subitems.
	
   4) Select 'IDL (2)' under 'Backing store method for window contents' (if this 
	
   does not work for you, try 'System (1)' instead).
	
   5) Save the changes by selecting 'Apply' and then 'OK'.



4. Steps 4 and 5 below are only necessary if you wish to do spectral fitting:

   Enter the ./rmfit/mfit/src directory and compile the MFIT Fortran shared object.

   Prerequisites:
   
   You will need C and Fortran 95 compilers.  The Fortran 95 compiler needs to
   support the Fortran 2003 ISO C Bindings, as most now do.   gcc (version not
   critical, >= 4.0 is reasonable) and gfortran >= 4.3 are suitable.
   Installation instructions for gfortran are given below in the section 
   "GFORTRAN NOTES".
   
   You will also need the GNU Scientific Library (GSL) -- installation 
   instructions are given in "GSL NOTES".
   
   IDL_DIR should be defined, as described above.

   The MFIT "make" Bash script that we provide is designed for gcc and gfortran.  
   This script works for Linux and Darwin (Mac OS X) operating systems.
   If you have a different C or Fortran 95 compiler, you will need to edit
   the make script Make.mfit.sh to change the compiler and linker options.
   If you have a different operating system, you might also need to revise the
   script Make.mfit.sh -- the options most likely to need revision are the linker
   options.
   
   Window users should follow the instructions in the file "RMFit Windows 
   Installation Instructions", although this is likely to be outdated...
 
 
   a) For Mac OS X, it is necessary to determine whether mfit should be a 32-
   or 64-bit dynamic library because there needs to be consistency between the bit-level
   of IDL and of the MFIT sharable executable, and furthermore, of the GSL shared library 
   that MFIT links to.
   
   (This isn't necessary with Linux because apparently gcc/gfortran automatically
   produce a 32- or 64-bit executable according the processor, while with Mac OS X 
   the bit-level must be specified with a compiler option.)
   
   If you have an Intel Mac with a 64-bit processor and IDL of version 7.0.4 or 
   higher, you are running 64-bit IDL, otherwise you will be using a 32-bit version 
   of IDL.  The best way to determine the "bit level" of IDL is to issue the command 
   "print, !version" inside of IDL.   If the output ends with 64 TWICE, you have a 
   64-bit version of IDL; the combination of "32 64" indicates 32-bit IDL.  
   
   Even if your installed IDL version and processor would normally default to
   a 64-bit IDL, you can force the running of a 32-bit IDL by adding the flag "-32"
   when you invoke IDL.  This may be the easiest solution since most Mac OS X
   binary executables and libraries being distributed in this era are 32-bit.
   If you select this approach, then obtain or compile a 32-bit GSL, and select
   "32" when requested by the MFIT make script.
   
   b) If libgsl.* is installed in a standard location, such as /usr/lib, the
   linker will automatically find it.  The link will most likely work if libgsl.*
   is at /usr/local/lib.  If your GSL library is located in a less standard directory,
   you will probably have to inform the MFIT make script of the directory using the
   environment variable GSL_PATH, e.g., "export GSL_PATH=/opt/local/" if libgsl.* is 
   located at /opt/local/lib (MacPorts) or "export GSL_PATH=/sw" (fink) if libgsl.* is
   located at /sw/lib.  The full path is not used because the shell script also must
   locate the include files. See the "GSL NOTES" section below for an explanation of 
   how to obtain and install the GSL.
   
   c) If your versions of gcc or gfortran have non-standard names, you can provide
   the names to the MFIT make script by defining the enviroment variables CC or F95, 
   e.g., "export CC=gcc-4.3", or "export F95=gfortran-4.3".
   
   d) cd to the directory rmfit/mfit/src and invoke the make script via: 
   ./Make.mfit.sh
   For Darwin (Mac OS X), the script will ask you to input the bit-level, either 32 or 64.
   
   If there are any issues compiling or linking, please see the section below on 
   "MAKE ERRORS".  There are a number of compatibility notes that we've accumulated 
   that should help.

   
5. Make sure IDL knows how to find the shared object for mfit.  On UNIX, including
   Mac OS X, IDL uses the environment variable IDL_DLM_PATH.  The DLM for mfit
   is located in subdirectory "mfit/dlm" of the rmfit directory.
   An example definition of IDL_DLM_PATH is:
   
       export IDL_DLM_PATH="<IDL_DEFAULT_DLM>:/home/mydir/rmfit/mfit/dlm"   
       
   The first field provides the directories that IDL needs to find its own shared
   objects.   <IDL_DEFAULT_DLM> is not a shell enviroment variable but rather
   a symbol internal to IDL -- IDL will expand it to the correct directory 
   containing the IDL DLMS.  The second field, after the colon, is the "mfit/dlm"
   subdirectory of your location of the rmfit directory.
   
   Windows users will need to add the MFIT and MFIT.dlm files to the bin 
   subdirectory of the RSI distribution tree. There is currently no way to 
   set the IDL_DLM_PATH environment variable in Windows.
   
   If you are using an old version of IDL, it might not have the symbol
   <IDL_DEFAULT_PATH>, in which case you can instead follow these more complicated
   instructions to explicitly include IDL's directories in the shell environment
   variable IDL_DLM_PATH:
   
   First, before configuring the environment variable IDL_DLM_PATH, determine
   IDL's default DLM path.   Enter IDL and type "print, !dlm_path".
   Typical outputs are:
   
      /Applications/rsi/idl61/bin/bin.darwin.i386/     [Mac OS X]
      /opt/local/rsi/idl61/bin/bin.linux.x86_64        [Linux]
      
   Use the directory that you found from "print, !dlm_path" in the definition of
   IDL_DLM_PATH (above) instead of <IDL_DEFAULT_PATH>.


6. Make sure IDL knows how to find the source files.   On UNIX, IDL uses
   the shell environment variable IDL_PATH:
   
      export IDL_PATH="<IDL_DEFAULT_PATH>\
      :+/home/mydir/rmfit\
      :/usr/local/IDL_AstronUsersLibrary/pro/"   

   The symbol <IDL_DEFAULT_PATH> is not a shell enviroment variable but rather a
   symbol internal to IDL -- IDL will expand it to be the directories containing
   IDL's own source files.   
   The second field after the colon is the directory where you have located rmfit;
   the "+" will cause IDL to search all subdirectories.
   
   Windows users will need to start up the IDLDE and add the rmfit directory to 
   the File->Preferences->Path... preference (make sure the "Search subfolders" 
   option is in effect - there should be a plus [+] in front of the new path).
   
   If you are using an old version of IDL, it might not have the symbol <IDL_DEFAULT_PATH>,
   in which case you can instead you the follow these more complicated instructions
   to explicitly include IDL's directories in the shell environment symbol IDL_PATH:

   First, before configuring the environment variable IDL_PATH, find out
   the default location for IDL's source files on your system by entering 
   "print, !path" at the IDL prompt. You will see a huge list of directories,
   however, they should all have top levels in common, up to /lib or /examples.   
   Typical examples for the common portion of the IDL Default directory are:
   
      /Applications/rsi/idl61/       [Mac OS X]
      /opt/local/rsi/idl61/lib       [Linux]

   Extract this common portion and include it in the definition of IDL_PATH (above),
   prepended with a "+" symbol, instead of using <IDL_DEFAULT_PATH>.  The '+' allows
   IDL to expand the given path to all subdirectories, which we want to do. 
  
  
7. If you plan to run the save file version, find the rmfit.sav file in the /rmfit
   directory and copy it into a useful directory for your analyses; i.e., at the
   root of a directory of FITS files.


8. To run rmfit:

   a) To run the save file version, which doesn't require an IDL license:
   Enter "idl -vm='/path_to_dir_w_rmfit_sav/rmfit.sav'" at your system prompt.
   (Or "idl -32 -vm='/path_to_dir_w_rmfit_sav/rmfit.sav'" to run in 32-bit mode
   on a 64-bit machine.)  If you will be doing this often, you can make an alias 
   and add it to your login file (in bash): 
   alias rmfit="idl -vm='/path_to_dir_w_rmfit_sav/rmfit.sav'"
   You will need to click through the banner ad for IDL to get to rmfit.
   
   b) To run rmfit directly, which requires an IDL license:
   Type "idl" (or "idl -32" to select 32-bit IDL on a 64-bit machine), then "rmfit".
   
   If you encounter errors running rmfit, especially when trying your first spectral
   fit, please check the section "RUNTIME ERRORS" below.
   
   
9. There is a user's guide available in the RMFIT window's Help menu; the same 
   help information can be found in the rmfit/help/help.hlp text file. If you are 
   unsure where to begin, this is the place.


_______________________________ GFORTRAN NOTES ______________________________________

   You will need C and Fortran 95 compilers.  The Fortran 95 compiler needs to
   support the Fortran 2003 ISO C Bindings, as most now do.   gcc (version not
   critical, >= 4.0 is reasonable) and gfortran >= 4.3 are suitable.  GNU Compilers 
   4.3.0 were released more than a year ago (2008 March 5).  Many Linux distributions
   provide gfortran 4.3 (e.g., Ubuntu 8.10 provides 4.3.2, information on some distributions 
   is at http://gcc.gnu.org/wiki/GFortranDistros).  For Mac OS X, MacPorts 
   (http://www.macports.org/) provides gcc43, the GNU Compiler Collection 4.3, which 
   includes gcc and gfortran 4.3.3.   Fink (http://www.finkproject.org/) package gcc43, 
   GNU Compiler Collection Version 4.3, has gcc and gfortran 4.3.1 (stable branch,
   currently only in source code form).  Recent gfortran binaries for both Linux and
   Mac OS X are provided at the GCC Wiki (http://gcc.gnu.org/wiki/GFortranBinaries).
   
   Additional information on the binaries available from the GCC Wiki:
   The Mac OS file is a dmg file which installs gfortran very easily.  gfortran will be
   installed at /usr/local/gfortran with a link "gfortran" at /usr/local/bin/.
   For Linux, links to various pages and versions for various needs are available from 
   http://gcc.gnu.org/wiki/GFortranBinaries/.   That page also has links to
   installation instructions.   In brief: ungzip and untar the file, for example
   in directory /opt.   It will expand to sub-directory gcc-trunk.
   Add /opt/gcc-trunk/lib to LD_LIBRARY_PATH.    Either run /opt/gcc-trunk/bin/gfortran
   or add /opt/gcc-trunk/bin to your PATH.
   
   Check your gfortran installation via:
   which gfortran
   gfortran -v
   
   Notes on gfortran versions:
   4.3.0   Documentation implies should work.  Failed on one machine -- did not recognize ISO C Binding.
   4.3.1   not tested
   4.3.2   Worked on Linux Ubuntu
   4.3.3   Worked on Mac OS X
   4.4.0   Obtained from GCC Wiki.  Worked on Mac OS X and SL Linux.


__________________________________ GSL NOTES ______________________________________

   Installing the GNU Scientific Library (GSL) on your system.
   
   Most versions of Linux provide the GSL through their package manager, either
   as an rpm or as a debian package (apt-get).   For Mac OS X, the GSL is
   available from both Fink (http://www.finkproject.org/) and MacPorts
   (http://www.macports.org/).    These Mac versions are likely to be 32-bit.
   
   For Darwin (Mac OS X), if you are running a 64-bit version of IDL (see above),
   you will need to have a GSL library compiled to be 64-bit (unless you invoke IDL
   with the "-32" flag).  This is so that the GSL shared library will be compatible
   with the MFIT shareable executable, which needs to be compatible with the IDL executable.
   
   GSL is also very easy to install from the source code (http://www.gnu.org/software/gsl/).
   
   i) ungzip and untar the file.
   ii) cd to the expanded directory, such as gsl-1.12
   iii) make clean   [optional on a fresh install]
   
   For Linux or a 32-bit version of IDL for Mac OS X, or for any IDL invoked with 
   the "-32" flag:
   iv) ./configure
   
   For a 64-bit version of IDL for Mac OS X:
   iv) ./configure CFLAGS="-m64" LDFLAGS="-m64"

   v) make
   
   If you get an error from the "make" step, try again from step iii), adding "--disable-shared"
   after the "./configure" of step iv).

   vi) sudo make install
   
   This will install the GSL in directory /usr/local/, e.g., the libraries libgsl.a and 
   libgsl.so (Linux) or libgsl.dylib (Mac OS X) in /usr/local/lib  (and only libgsl.a if
   you have used "--disable-shared") and various include files in /usr/local/include/gsl.
   Optional additional directions are in the file INSTALL.  That file also describes how 
   to run some tests.


__________________________________ MAKE ERRORS ______________________________________


The following errors have been reported during the running of Make.mfit.sh:

M-1.  If you see any messages similar to the followng:
   /var/tmp//ccvuR0IU.s:12294:operands given don't match any known 386 instruction
   
   Chances are, you do not have an appropriate linker from Apple.   The solution is to
   download and install the latest version of XCode from the Apple Developer's area
   (free to join).  For Mac OS X "Tiger" (10.4x) this is version 2.5. For more recent 
   builds of the OS, Leopard or Snow Leopard, get the latest version of XCode (version
   3.1.2 as of June 2009).
   
M-2. If you see multiple compiler errors related to 'libgsl', see the section 
   on installing the GSL, or set the environment variable GSL_PATH.
   
M-3. If the MFIT Link step fails with many error messages of the form:

   mfit_interface.o: In function `__sigismember': 
   mfit_interface.c:(.text+0x0): multiple definition of `__sigismember' 
   mfit_idl.o:mfit_idl.c:(.text+0x0): first defined here 
    
   The cause, as far as we understand, is a non-usable gcc obtained as part  
   of the gfortran binary from the gfortran wiki.   Use your older gcc  
   instead.   The gfortran binary is OK.   You can use the environment  
   variable CC (see instructions above) to point to a specific gcc by name  
   or path. 
   
M-4. The following error from the Make step,
  
    ld warning: in /usr/local/lib//libgfortran.dylib, file is not of required architecture
    
   was observed when gfortran 4.3 was installed without deleting gfortran 4.2.
   Apparently gfortran 4.3 was using the libraries of version 4.2, and those libraries
   were not compatible.   Solution: uninstall or delete the earlier version of gfortran.
   

__________________________________ RUNTIME ERRORS ______________________________________


The following errors have been reported when MFIT is first invoked, when performing the first
spectral fit of an rmfit session:


R-1.  If you get the error message listed below, it likely means that you have compiled MFIT
     for 32-bits, while you are running a 64-bit IDL, or vice-a-versa.  The instructions
     above discuss this issue.  Either remake MFIT and GSL for the other bit level, or
     force IDL to run as 32-bit program by invoking IDL with the "-32" flag.

        % MFIT: Error loading sharable executable.
        Symbol: IDL_Load, File =  ..../mfit.so
        dlopen(/...../mfit.so,           no suitable image found.
        Did find:    ...../mfit.so:    mach-o, but wrong architecture     % Execution halted at:

R-2. A very similar error message about the "wrong architecture" results when a Power PC version
    of IDL is used on an Intel Mac -- on an Intel Mac, you will normally have a gcc & gfortran
    that will compile MFIT to an Intel executaable.   This will result in IDL and MFIT executables 
    that are incompatible.  A solution is to download the current version of IDL and run that version
    without a license, using the virtual machine version.   You can still use your older version of
    IDL for other purposes.

R-3. If the GSL library is not in your LD_LIBRARY_PATH, then you will get the following 
   message when mfit is called:
   
   % MFIT: Error loading sharable executable.
           Symbol: IDL_Load, File = ...../mfit.so
           libgsl.so.0: cannot open shared object file: No such file or directory
   % Execution halted at: MFIT::FITMODEL   1655 
   
   Solution: Add an instruction in your login file to add the GSL location to LD_LIBRARY_PATH.
   e.g.: export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
   (or DYLD_LIBRARY_PATH for Mac OS X).
   

RDP and MSB
2009 Oct. 2
