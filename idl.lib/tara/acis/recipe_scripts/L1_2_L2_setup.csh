#!/bin/tcsh -fe
# $Id: L1_2_L2_setup.csh 3261 2008-12-02 19:11:31Z patb $

# This script contains the L1->L2 Recipe Commands prior to IDL work in astrometry recipe.

#  cd to a directory containing an ACIS data set, e.g. /Volumes/Bulk/projects/carina/data/SouthPillars/obsid_9831/
#  screen -S "`basename $PWD`" -L 

  set echo_style=both
  set pushdsilent
  set echo

  
# Establish a local CIAO parameter directory to avoid conflicts from simultaneous sessions.
# Since we start with an empty param dir we omit the punlearns from the recipe.
  if (! -e trash) mkdir trash
  if (  -e param) rm -r param 
  mkdir param
  setenv PFILES "$PWD/param;${ASCDS_INSTALL}/contrib/param:${ASCDS_INSTALL}/param"
  
  
  dmlist $CALDB/docs/chandra/caldb_version/caldb_version.fits"[cols  caldb_ver]" data,clean | tail -1
  ciao_info  
  
  chmod    u+w primary secondary
# The subshell and pipe to cat below are a trick to ignore non-zero exit status from this command.
( gunzip axaf*vv2.pdf.gz secondary/axaf*vvref2.pdf.gz primary/pcad*.gz primary/*bpix1*.gz primary/*evt2*.gz secondary/*evt1*.gz secondary/*flt1*.gz secondary/*pbk*.gz secondary/*msk*.gz secondary/*bias*.gz | cat )
    
  chmod -R a-w primary secondary
#( open axaf*vv2.pdf | cat )
  
( mv -f archive acis.evt1 acis.flt1 acis.pbk0 acis.msk1 acis.bpix1 acis.asol1 fix_astrometry/NOMAD.fits fix_astrometry/pipeline.evt2 trash |& cat > /dev/null )

  ln -s . archive
  ln -s   archive/secondary/acis*evt1.fits acis.evt1	
  ln -s   archive/secondary/acis*flt1.fits acis.flt1
  ln -s   archive/secondary/acis*pbk0.fits acis.pbk0
  ln -s   archive/secondary/acis*msk1.fits acis.msk1	
  ln -s   archive/primary/acis*bpix1.fits  acis.bpix1
   
  ls -1 archive/primary/pcad*asol1.fits | tee asol1_list.txt
  if ( `cat asol1_list.txt |wc -l` > 1 ) then
    echo "\nERROR!  You have more than one aspect file; you must find one that spans the data or merge them by hand!"
    exit 1
  endif
  
  ln -s archive/primary/pcad*asol1.fits acis.asol1
  
  cp -f ~townsley/acis_analysis_docs/lkt_badpix.fits .
  ls -1 secondary/*bias* > bias_list.txt
  mv -f acis.bpix1 trash
  acis_run_hotpix acis.evt1 acis.bpix1 pbkfile=acis.pbk0 badpixfile=lkt_badpix.fits biasfile="@bias_list.txt" maskfile=acis.msk1 clob+
  
  # If current directory is referenced by a LOCAL (not NFS) path, then prompt for an equivalent NFS path to put in ardlib.par so that it is usable on any machine.
  unset echo
  if ( `df . | egrep -c "^/dev"` == 1 ) then 
    printf "\nYour current directory is referenced by a LOCAL (not NFS) path:\n  %s\n" $PWD
    printf "\nEnter the equivalent NFS path so that ardlib.par will be portable: \n"
    set nfs_path="$<"
    if (! -e $nfs_path) then
      printf "\nERROR: that path does not exist!\n"
      exit 1
    endif
    pushd $nfs_path
  else 
    pushd $PWD
  endif
  
  echo "\n"
  set echo
  acis_set_ardlib acis.bpix1 absolutepath=yes	
  popd
  
  setenv ARDLIB_FILE `basename $PWD`_ardlib.par
  cp -f `paccess ardlib w` $ARDLIB_FILE
  chmod a-w                $ARDLIB_FILE
  ls -l                    $ARDLIB_FILE

  
  mkdir -p fix_astrometry/trash
  cd    fix_astrometry
  ln -s ../../../NOMAD.fits .
  ln -s ../archive/primary/acis*evt2.fits  pipeline.evt2
  
  pset wavdetect sigthresh=1e-6 log=yes
  pset wavdetect scales="1.0 1.414 2.0 2.828 4.0"
  wavdetect infile="pipeline.evt2[energy=500:7000][bin x=3584:4608:1,y=3584:4608:1]" outfile=central_1_evt2.sources regfile=trash/central_1_evt2.reg scellfile=trash/scellfile imagefile=trash/imagefile defnbkgfile=trash/defnbkgfile interdir=trash log=no clobber=yes
  
  unset echo
  echo "\nCheck that BADPIX_FILE entries printed above used NFS paths and refer to this obsid."
  echo "\nNow, you must execute IDL parts of astrometry_recipe.txt interactively.\n\n"

  echo "\nSCRIPT EXITED NORMALLY"





