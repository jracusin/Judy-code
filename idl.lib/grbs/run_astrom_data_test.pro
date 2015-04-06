pro run_astrom_data,ps=ps,iter=iter,skipgv=skipgv,snr=snr,optfile=optfile,nospawn=nospawn,suffix=suffix,grb=grb

  if n_elements(suffix) eq 0 then suffix='';'_f100'; ''
  if n_elements(optfile) eq 0 then begin
     optfile='sdss.csv'
     sdss=1
  endif
  if optfile eq 'dposs.txt' then dposs=1
  if optfile eq 'sdss.csv' then sdss=1
  if optfile eq '2mass.txt' then twomass=1
  if optfile eq 'usno.txt' then usno=1
  
  astrom=create_struct('GRB','',$
                       'tid',0L,$
                       'xra',0d,$
                       'xdec',0d,$
                       'xerr',0d,$
                       'ora',0d,$
                       'odec',0d,$
                       'oerr',0d,$
                       'new_xra',0d,$
                       'new_xdec',0d,$
                       'new_xerr',0d,$
                       'dra',0d,$
                       'ddec',0d,$
                       'derr',0d,$
                       'sdra',0d,$
                       'sddec',0d,$
                       'xoff',0d,$
                       'xo_off',0d,$
                       'nsrc',0,$
                       'expotime',0L)

  ngrb=26
  astrom=replicate(astrom,ngrb)
  cd,'/bulk/shadow/racusin/grbs'
  readcol,'sdss_astrometry.csv',grbname,tid,xra,xdec,xerr,ora,odec,oerr,format='(a,l,d,d,d,d,d,d)',delim=','
  astrom.grb=grbname
  astrom.tid=tid
  astrom.xra=xra
  astrom.xdec=xdec
  astrom.xerr=xerr
  astrom.ora=ora
  astrom.odec=odec
  astrom.oerr=oerr
  
;  openr,lun,'sdss_astrometry.csv',/get_lun
;  line=readline(lun,delim=',')
;  for i=0,ngrb-1 do begin
;     line=readline(lun,delim=',')
;     astrom[i].grb=line[0]
;     astrom[i].tid=long(line[1])
;     astrom[i].xra=double(line[2])
;     astrom[i].xdec=double(line[5])
;     astrom[i].xerr=double(line[8])
;     astrom[i].ora=double(line[11])
;     astrom[i].odec=double(line[14])
;     astrom[i].oerr=0.5
;  endfor
;  close,lun
;  free_lun,lun

  w=where(astrom.ora ne 0,nw)

  grbs=astrom[w]

  g=0
  stop
  grb=intarr(nw)
;  grb[11]=5
  grb[9]=4
  for i=g,nw-1 do begin


     ralist=[grbs[i].xra,grbs[i].ora]
     declist=[grbs[i].xdec,grbs[i].odec]
     errlist=[grbs[i].xerr,grbs[i].oerr]
     names=['XRT refined','optical']

     dir='grb'+strlowcase(grbs[i].grb)
     print,'__________________________________________________________________'
     print,dir
     cd,dir

     if not exist(optfile) then begin
        optfile='dposs.txt'
        if not exist(optfile) then optfile='dss.txt'
        dposs=1 & sdss=0
     endif
     if not exist(optfile) then begin
        print,'no optical file'
        return
     endif else begin
;     if exist(optfile) then begin
        if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
        
        gb=grb[i]
        xrt_astrometry,'','',grbs[i].xra,grbs[i].xdec,newra,newdec,merr,ralist,declist,errlist,names,mra,mdec,mraerr,nsrc,expotime,sdra,sddec,sdss=sdss,dposs=dposs,dss=dss,usno=usno,twomass=twomass,dir='00*-xrt/',/filt,iter=iter,skipgv=skipgv,nospawn=nospawn,suffix=suffix,grb=gb
        print,'GRB'+grbs[i].grb

        ;need to filter out a few hotpixels after June-05
        grbs[i].new_xra=newra
        grbs[i].new_xdec=newdec
        grbs[i].new_xerr=merr
        grbs[i].dra=mra
        grbs[i].ddec=mdec
        grbs[i].derr=mraerr
        grbs[i].xoff=separation(newra,newdec,grbs[i].xra,grbs[i].xdec)
        grbs[i].xo_off=separation(newra,newdec,grbs[i].ora,grbs[i].odec)
        grbs[i].nsrc=nsrc
        grbs[i].expotime=expotime
        grbs[i].sdra=sdra
        grbs[i].sddec=sddec

        if keyword_set(ps) then endplot

;     endif else begin
;        print,'No sdss.csv'
;        stop
     endelse

     cd,'../'
;     stop
  endfor
  mwrfits,grbs,'grb_astrometry'+suffix+'.fits',/create

  stop
  return
end
