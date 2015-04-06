#!/bin/tcsh -fe

# $Id: setup_AE_for_obsid_data.csh 3292 2008-12-17 19:14:34Z patb $

# Script to set up AE obsXXXX directories to point to an observation's Iarray+Sarray data. 
# Supply one parameter, a glob pattern in double quotes that finds obsid_XXXX directories,
# e.g.  "../../../*/pointing_*/obsid_*"

set echo_style=both
set pushdsilent
foreach obsid_dir ($1)
  set dir=`basename $obsid_dir | sed -e "s/obsid_/obs/"`
  echo "\n$dir"
  
  if (! -e $dir) mkdir $dir
  pushd $dir
  
  set source_files = (fullfield_1.emap acis.validation.evt2 acis.spectral.evt2 stowed.fullfield_1.emap acis.stowed.evt2 acis.astrometric.asol1 asphist obsid_\*_ardlib.par acis.pbk0    acis.msk1)
  set target_files = (        obs.emap      validation.evt       spectral.evt              stowed.emap      stowed.evt               obs.asol  asphist         ardlib.par  obs.pbkfile  obs.mskfile)
  
  @ loop=1
  while ( $loop <= ${#source_files} )
    
    if ( -e ${target_files[$loop]}) then
      echo "Link ${target_files[$loop]} already exists."
    else if ( -e ../$obsid_dir/${source_files[$loop]} ) then
      ln -s ../$obsid_dir/${source_files[$loop]} ${target_files[$loop]}
#     ls -l ${target_files[$loop]}
    else
      echo "Cannot find $obsid_dir/${source_files[$loop]}; skipping this symlink."
    endif
    
    
    @ loop++
  end
  
  popd
  end
  
setenv OBS_LIST ""
foreach dir (obs*)
  set obs=`basename $dir | sed -e "s/obs//"`
  setenv OBS_LIST "$obs $OBS_LIST"
  end
echo " "
echo "ObsId list: $OBS_LIST"  

