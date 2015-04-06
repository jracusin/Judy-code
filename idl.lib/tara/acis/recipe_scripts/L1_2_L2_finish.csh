#!/bin/tcsh -fe
# $Id: L1_2_L2_finish.csh 3262 2008-12-02 19:12:17Z patb $

# This script contains the L1->L2 Recipe Commands following the astrometry recipe.

#  cd to a directory containing an ACIS data set, e.g. /Volumes/Bulk/projects/carina/data/SouthPillars/obsid_9831/
#  screen -S "`basename $PWD`" -L     

  set echo_style=both
  set echo
  
# Establish a local CIAO parameter directory to avoid conflicts from simultaneous sessions.
# Since we start with an empty param dir we omit the punlearns from the recipe.
  if (! -e trash) mkdir trash
  if (  -e param) rm -r param 
  mkdir param
  setenv PFILES "$PWD/param;${ASCDS_INSTALL}/contrib/param:${ASCDS_INSTALL}/param"


  setenv   DESTREAK_BITS_ZERO "xxxxxxxxxxxxxxxx0xxxxxxxxxxxxxxx"
  setenv AFTERGLOW2_BITS_ZERO "xxxxxxxxxxxx0000xxxxxxxxxxxxxxxx"
  setenv    CLEAN55_BITS_ZERO "xxxxxxxx0xxxxxxxxxxxxxxxxxxxxxxx"

  dmcopy         "acis.flare_filtered.evt2[status=0]" acis.validation.evt2 clobber=yes
  dmcopy "acis.stowed.calibrated.clean.evt[status=0]"     acis.stowed.evt2 clobber=yes

  dmcopy "acis.validation.evt2[ccd_id=0:3]"         iarray.validation.evt2 clobber=yes
  dmcopy     "acis.stowed.evt2[ccd_id=0:3]"             iarray.stowed.evt2 clobber=yes

  dmcopy "acis.flare_filtered.evt2[exclude status=${AFTERGLOW2_BITS_ZERO}]" bad_afterglow2.evt clobber=yes  
  dmcopy "acis.flare_filtered.evt2[exclude status=${CLEAN55_BITS_ZERO}]"    bad_clean55.evt    clobber=yes  
  dmcopy "acis.flare_filtered.evt2[exclude status=${DESTREAK_BITS_ZERO}]"   bad_destreak.evt   clobber=yes
  dmcopy "acis.flare_filtered.evt2[exclude status=0]"                       bad_validation.evt clobber=yes

    
# The subshell and pipe to cat below are a trick to ignore non-zero exit status from this command.
(  mv acis.stowed.calibrated.evt1 good.evt good.stowed.evt temp.asol1 temp.evt subpixres.* trash | cat )

  dmlist bad_afterglow2.evt   blocks | grep EVENTS
  dmlist bad_clean55.evt      blocks | grep EVENTS
  dmlist bad_destreak.evt     blocks | grep EVENTS
  dmlist bad_validation.evt   blocks | grep EVENTS
  dmlist acis.validation.evt2 blocks | grep EVENTS
  
  
  unset echo
  echo "\nPress return to spawn ds9:"
  read foobar
  set echo

  ds9 -bin factor 8 -log  acis.spectral.evt2 bad_afterglow2.evt bad_clean55.evt bad_destreak.evt bad_validation.evt acis.validation.evt2 &

  unset echo
  echo "\nCheck the dmlist results and the data shown in ds9.  Run L1_2_L2_emaps.pro."

  echo "\nSCRIPT EXITED NORMALLY"





