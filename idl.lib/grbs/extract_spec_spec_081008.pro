pro extract_spec_spec
  
;  cd,'~/Desktop/GRB080319B/ext_spec/'
  cd,'/bulk/wolverines/racusin/grbs/other_grbs/GRB081008/ext_spec'
;  trigtime=227599969.0
;  trigtime=227599971.904d
  trigtime=245188692.544d
;  evfiles=file_search('seg*_wt.evt')
;  bgfiles=file_search('*bg*evt')
;  tmin=75632;16496;10704;4969;544;320;65
;  tmax=752416                   ;377232;12864;6929;912;400;172
  
;  tmin=320
;  tmax=544
;  tmin=4000
;  tmax=8000
;  tmin=5e4
;  tmax=1e5
  tmin=95.9
  tmax=130.
  if tmax gt 420 then begin
     mode='pc' 
     bgregfile='bgpc.reg'
  endif else begin
     mode='wt'
     bgregfile='bg.reg' 
  endelse 
       
;  mode='wt'
;  tmin=3e5
;  tmax=1e6
;  tmins='1.5e4'
;  tmaxs='4e4'
;  tmins='5e4'
;  tmaxs='1e5'
  tmins='95.9'
  tmaxs='130'
;  tmins=ntostr(tmin)
;  tmaxs=ntostr(tmax)
;  mode='pc'
  
;  regfile=''
  stem='spec_'+tmins+'_'+tmaxs
  xselfile='src'+stem+'.xco'
  evname=stem+'.evt'
  phaname=stem+'.pha'
  bgxselfile='bg'+stem+'.xco'
  bgevname='bg'+evname
  bgphaname='bg'+phaname
  evfile=file_search('sw*x'+mode+'w2po_cl.evt')
  stop
  regfile='srcpc.reg'
  
  expomap=stem+'_ex.img'
  arfname=stem+'.arf'
  if mode eq 'wt' then begin
     grp='40'
;     respfile='/bulk/pkg/caldb/data/swift/xrt/cpf/rmf/swxwt0to2s6_20010101v010.rmf'
     respfile='swxwt0to2s6_20010101v010.rmf'
  endif 
  if mode eq 'pc' then begin
     respfile='/bulk/pkg/caldb/data/swift/xrt/cpf/rmf/swxpc0to12s6_20010101v010.rmf'
     grp='20'
  endif 
  phaname2=phaname+'.grpmin'+grp
  
;  w=indgen(4)
  w=indgen(n_elements(evfile))
  j=0
  
  ;;;src
  print,'XSELECT src'
  make_xselect_file,j,evfile,w,tmin+trigtime,tmax+trigtime,mode,regfile,xselfile,evname,phaname
  xselect='xselect @'+xselfile +' > xselect_'+tmins+'_'+tmaxs+'.out'
  print,xselect
  spawn,xselect
  
  ;;;bg
  texp=tmax-tmin
  if tmax lt 1000 then start=1000d else start=tmin
  print,'XSELECT bg'
  make_xselect_file,j,evfile,w,start+trigtime,start+texp+trigtime,mode,bgregfile,bgxselfile,bgevname,bgphaname,timefilter='time_filter_bg.flt'
  xselect='xselect @'+bgxselfile +' > xselect_bg_'+tmins+'_'+tmaxs+'.out'
  print,xselect
  spawn,xselect

;  goto,skip
  ;;;fappend
  fappend='fappend '+evfile[0]+'+3 '+evname
  print,fappend
  spawn,fappend
  
  ;;;xrtexpomap
  if mode eq 'wt' then begin 
     attfile=file_search('sw*sat*')
     hdfile=file_search('sw*hdtc.hk*')
  endif
  if mode eq 'pc' then begin
     attfile='merged_satfiles.fits'
     hdfile='merged_hkfiles.fits'
  endif 
  
  xrtexpomap='xrtexpomap infile='+evname+' attfile='+attfile+' hdfile='+hdfile+' outdir=./ stemout='+stem+' clobber=yes > xrtexpomap.out'
  print,xrtexpomap
  spawn,xrtexpomap
  
  ;;;grppha
  grppha='grppha infile="'+phaname+'" outfile="'+phaname2+'" chatter=0 comm="chkey ANCRFILE '+arfname+' & chkey RESPFILE '+respfile+' & chkey BACKFILE '+bgphaname+' & group min '+grp+' & exit" clobber=yes > grppha.out'
  print,grppha
  spawn,grppha

  ;;;xrtmkarf
  xrtmkarf='xrtmkarf outfile='+arfname+' phafile='+phaname+' srcx=-1 srcy=-1 psfflag=yes clobber=yes expofile='+expomap+' > xrtmkarf.out'
  print,xrtmkarf
  spawn,xrtmkarf
;skip:  
  tar='tar cvfz '+stem+'.tar.gz '+stem+'.pha.grpmin'+grp+' '+stem+'.arf bg'+stem+'.pha '+stem+'.evt'
  print,tar
  spawn,tar
  
  stop
  return
end 
