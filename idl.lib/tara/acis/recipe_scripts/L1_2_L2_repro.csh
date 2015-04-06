#!/bin/tcsh -fe
# $Id: L1_2_L2_repro.csh 3262 2008-12-02 19:12:17Z patb $

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

  
  # REPROJECT THE STOWED DATA
  cp -f ../../../procedures/vfaint_stowed.evt  acis.stowed.calibrated.evt1

  dmstat "acis.flt1[3][cols START]"
  set tstart=`pget dmstat out_min`
  dmstat "acis.flt1[3][cols STOP]"
  set  tstop=`pget dmstat out_max`
  
  dmhedit "acis.stowed.calibrated.evt1[2]" filelist=none operation=add key=TSTART  value=$tstart
  dmhedit "acis.stowed.calibrated.evt1[2]" filelist=none operation=add key=TSTOP   value=$tstop
  
  dmtcalc  acis.stowed.calibrated.evt1  temp.evt  expression="TIME=TSTART+((TSTOP-TSTART)*(#rand))" clobber=yes
  
  reproject_events  infile=temp.evt  aspect=acis.astrometric.asol1  match=acis.astrometric.evt1  outfile=acis.stowed.calibrated.evt1 random=-1 clobber=yes   
 

  # Reprocess the observation.
  dmtcalc             infile=acis.astrometric.evt1 outfile=temp.evt expression="status=status,status=X27F,status=X26F,status=X15F,status=X14F,status=X13F,status=X12F" clobber=yes
  
  acis_process_events infile=temp.evt              outfile=acis.astrometric.calibrated.evt1  eventdef=")stdlev1" check_vf_pha=yes badpixfile=acis.bpix1 apply_cti=yes apply_tgain=yes doevtgrade=yes calculate_pi=yes rand_pha=yes stop=none rand_pix_size=0.0 acaofffile=NONE clobber=yes verbose=2

  
#  IF THE DATAMODE of your observation is NOT VFAINT, then you must CLEAR the Clean55 bits in the stowed data so that the observation and stowed data sets will have equivalent filtering applied (later)!!!
  if (`dmkeypar acis.astrometric.calibrated.evt1 DATAMODE echo=yes` != 'VFAINT') then
       echo "CLEARING THE CLEAN55 BIT IN THE STOWED DATA!!"
       mv -f acis.stowed.calibrated.evt1 temp.evt
       dmtcalc  infile=temp.evt outfile=acis.stowed.calibrated.evt1 expression="status=status,status=X8F" clobber=yes
  endif
  
  
  
  
  dmtcalc               infile=acis.astrometric.calibrated.evt1 outfile=temp.evt                   expression="status=status,status=X0F,if(status==X15T)then(status=X0T)" clobber=yes

  acis_detect_afterglow infile=temp.evt                         outfile=acis.astrometric.calibrated.evt1  pha_rules=NONE fltgrade_rules=NONE clobber=yes

  cp -f acis.astrometric.calibrated.evt1 good.evt
  cp -f      acis.stowed.calibrated.evt1 good.stowed.evt

  mv -f good.evt temp.evt
  dmcopy "temp.evt[        grade=0,2,3,4,6]" good.evt      clobber=yes
  dmcopy "temp.evt[exclude grade=0,2,3,4,6]" bad_grade.evt clobber=yes

  mv -f good.stowed.evt temp.evt
  dmcopy "temp.evt[        grade=0,2,3,4,6]" good.stowed.evt    clobber=yes

  setenv       GRP1_BITS_ZERO "x0000000xxxxxxxxxx00xxxxxxxxxxxx"
  
  setenv       GRP2_BITS_ZERO "xxxxxxxxx00xxxxxxxxxxxxxxxxxxxxx"
  
  setenv AFTERGLOW1_BITS_ZERO "0xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  setenv     BADPIX_BITS_ZERO "xxxxxxxxxxxxxxxxxxxxxxxxxx00xxxx"
  setenv       GRP3_BITS_ZERO "0xxxxxxxxxx0xxxxx0xx000000000000"
  
  setenv        STD_BITS_ZERO "00000000x000xxxxx000000000000000"

  mv -f good.evt temp.evt
  dmcopy "temp.evt[        status=${STD_BITS_ZERO}]" good.evt       clobber=yes
 
  dmcopy "temp.evt[exclude status=${STD_BITS_ZERO}]"        bad_status.evt     clobber=yes
  dmcopy "temp.evt[exclude status=${BADPIX_BITS_ZERO}]"     bad_pix.evt        clobber=yes  
  dmcopy "temp.evt[exclude status=${AFTERGLOW1_BITS_ZERO}]" bad_afterglow1.evt clobber=yes  
 
  mv -f good.evt temp.evt
  dmcopy "temp.evt[@acis.flt1]"         good.evt    clobber=yes
  dmcopy "temp.evt[exclude @acis.flt1]" bad_flt.evt clobber=yes
  
  dmcopy "acis.asol1[@acis.flt1]" temp.asol1 clobber=yes

  dmcopy        "good.evt[energy<12000]" acis.astrometric.calibrated.clean.evt clobber=yes
  dmcopy "good.stowed.evt[energy<12000]"      acis.stowed.calibrated.clean.evt clobber=yes

  /usr/common/rsi/lib/apps/acis_psu/subpixel_resolution acis.astrometric.calibrated.clean.evt acis.astrometric.calibrated.clean.subpix.evt

  

# The subshell and pipe to cat below are a trick to ignore non-zero exit status from this command.
( dmdiff acis.astrometric.evt1 acis.astrometric.calibrated.evt1 keys=yes data=no subspace=no | cat )


  dmlist "acis.astrometric.calibrated.evt1[exclude status=${GRP1_BITS_ZERO}]" blocks | grep EVENTS
  dmlist "acis.astrometric.calibrated.evt1[exclude status=${GRP2_BITS_ZERO}]" blocks | grep EVENTS

  dmlist acis.astrometric.calibrated.clean.evt blocks 
   
  dmlist  acis.astrometric.calibrated.clean.evt header | egrep "ONTIME|LIVETIME|LIVTIME|EXPOSUR"

( dmlist  acis.astrometric.calibrated.clean.evt header | grep -i Merged | cat )
  
  dmlist bad_grade.evt        blocks | grep EVENTS
  dmlist bad_status.evt       blocks | grep EVENTS
  dmlist bad_pix.evt          blocks | grep EVENTS
  dmlist bad_afterglow1.evt   blocks | grep EVENTS
  dmlist bad_flt.evt          blocks | grep EVENTS

  unset echo
  echo "\nPress return to spawn ds9:"
  read foobar
  set echo

  ds9 -bin factor 8 -log acis.astrometric.calibrated.evt1 bad_grade.evt bad_status.evt "bad_pix.evt[bin=tdetx,tdety]" bad_afterglow1.evt  bad_flt.evt acis.astrometric.calibrated.clean.subpix.evt &


  unset echo
  echo "\nCheck the FITS keyword differences reported by dmdiff, the dmlist results following, and the data shown in ds9."
  echo "\nNow, you must execute background flare section of L1_2_L2_recipe.txt interactively.\n\n"

  echo "\nSCRIPT EXITED NORMALLY"




