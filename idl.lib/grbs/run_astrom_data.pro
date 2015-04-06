pro run_astrom_data,ps=ps,iter=iter,skipgv=skipgv,snr=snr,nospawn=nospawn,suffix=suffix,grb=grb,noprompt=noprompt,wavdetect=wavdetect,only=only

  if n_elements(suffix) eq 0 then suffix='_f100' ; ''
  
  astrom=create_struct('GRB','',$
                       'tid',0L,$
                       'xra',0d,$
                       'xdec',0d,$
                       'xerr',0d,$
                       'ora',0d,$
                       'odec',0d,$
                       'oerr',0d,$
                       'ora2',0d,$
                       'odec2',0d,$
                       'oerr2',0d,$
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
                       'expotime',0L,$
                       'optcat','')

  ngrb=82
  astrom=replicate(astrom,ngrb)
  cd,'/bulk/shadow/racusin/grbs'
;  csvfile='sdss_astrometry.csv'
  csvfile='astrom_wopt.csv'
  readcol,csvfile,grbname,tid,xra,xdec,xerr,ora,odec,format='(a,l,d,d,d,d,d,d)',delim=',',/silent
  readcol,csvfile,grbname2,tid2,xra2,xdec2,xerr2,ora2,odec2,oerr2,format='(a,l,d,d,d,d,d,d)',delim=',',/silent
  match,odec,odec2,m1,m2
  n=n_elements(odec)
  oerr=dblarr(n)
  oerr[*]=0.5
  oerr[m2]=oerr2
  
  for i=0,n-1 do begin
     name1=str_sep(grbname[i],'"')
     name2=str_sep(name1[1],' ')
     astrom[i].grb=name2[1]
  endfor 
;  astrom.grb=grbname
  astrom.tid=tid
  astrom.xra=xra
  astrom.xdec=xdec
  astrom.xerr=xerr
  astrom.ora=ora
  astrom.odec=odec
  astrom.oerr=oerr
  
  readcol,'~/astrometry/butler/ot.txt',btid,grb,rah,ram,ras,decd,decm,decs,boerr,format='(l,a,a,a,a,a,a,a,a)'
  hms2radec,rah,ram,ras,decd,decm,decs,bora,bodec
  match,tid,btid,om1,om2
  astrom[om1].ora2=bora[om2]
  astrom[om1].odec2=bodec[om2]
  astrom[om1].oerr2=boerr[om2]
  
  
  if n_elements(only) gt 0 then begin
     astrom=astrom[only]
  endif 
     
  w=where(astrom.ora ne 0,nw)

  grbs=astrom[w]

  g=0
  stop
  grb=intarr(nw)
  grb[*]=-1
;  grb[11]=5
;  grb[9]=4
  
  but=mrdfits('~/astrometry/butler/butler.fits',1)
;  dontuse=['041223','050124','050126','050219B','050412','050502B','050509B','050716','050726',$
;           '050824','050904','051111','060111A','060115','060124','060204B','060323','060502B',$
;           '060906']
;  dont_match,grbs.grb,dontuse,mn1,mn2
;  grbs=grbs[mn1]
  
  match,grbs.tid,but.targid,m1,m2
  nw=n_elements(m1)
  
  for j=g,nw-1 do begin
     i=j;m1[j]
     k=m2[where(m1 eq i)]
     
     ralist=[grbs[i].xra,grbs[i].ora,but[k].ra,grbs[i].ora2]
     declist=[grbs[i].xdec,grbs[i].odec,but[k].dec,grbs[i].odec2]
     errlist=[grbs[i].xerr,grbs[i].oerr,but[k].err,grbs[i].oerr2]
     names=['XRT refined','optical','Butler','Butler Opt']

     dir='grb'+strlowcase(grbs[i].grb)
     print,'__________________________________________________________________'
     print,dir
     
     if not exist(dir) then spawn,'mkdir '+dir
     
     cd,dir
;     data=file_search('00*')
;     if not exist(data[0]) or data[0] eq '' then begin 
     grbdir='/bulk/shadow/stroh/GRB'+strupcase(grbs[i].grb)+'/'
     data=file_search(grbdir+'00*')
;     endif 
     if not exist(data[0]) or data[0] eq '' then data='';file_search('00*') ;;cheat to redownload data
     
     keep=intarr(n_elements(data))
     tmp=strpos(data,'-xrt')
     wtmp=where(tmp eq -1)
     keep[wtmp]=1
     tmp=strpos(data,'_o')
     wtmp=where(tmp eq -1)
     keep[wtmp]=keep[wtmp]+1
     w=where(keep gt 1,ndata)

     if ndata gt 0 then data=data[w] else begin 
        if not exist(grbdir) then get_sdc_data,'20'+year,month,grbs[i].tid else $
           data=file_search(grbdir+'00*')
     endelse
     name=grbs[i].grb
     year=strmid(name,0,2)
     month=strmid(name,2,2)
     if year ge 06*1 and month*1 ge 04 then teldef=1 else teldef=0

     output=file_search('00*-xrt')
     keep=intarr(n_elements(output))
     tmp=strpos(output,'-bat')
     wtmp=where(tmp eq -1)
     keep[wtmp]=1
     w=where(keep eq 1,nout)
     output=output[w]
     nout=n_elements(output)
     
     if output[0] eq '' or (ndata gt nout) then begin
        if output[0] ne '' then begin 
           mdata=strarr(ndata)
           for t=0,ndata-1 do begin
              tmp=str_sep(data[t],'/')
              mdata[t]=tmp[n_elements(tmp)-1]
           endfor
           moutput=strarr(nout)
           for t=0,nout-1 do begin
              tmp=str_sep(output[t],'-xrt')
              moutput[t]=tmp[0]
           endfor
           
           dont_match,mdata,moutput,mm1,mm2
           
           if mm1[0] ne -1 then w=mm1
           if mm2[0] ne -1 then w=mm2
           if mm1[0] eq -1 and mm2[0] eq -1 then w=indgen(n_elements(mdata))
        endif else w=indgen(ndata)
           
        run_xrtpipeline,indir=grbdir,teldef=teldef,w=w
        print,'Removing files from astrom run without full data set'
        spawn,'rm combined_image_wexpo*'
        spawn,'rm src_xrtcentroid*fits'
        spawn,'rm src_wavdetect*.fits'
     endif 
     if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
     
     gb=grb[i]
     xrt_astrometry,'','',grbs[i].xra,grbs[i].xdec,newra,newdec,merr,ralist,declist,errlist,names,mra,mdec,mraerr,nsrc,expotime,sdra,sddec,sdss=sdss,dposs=dposs,dss=dss,usno=usno,twomass=twomass,dir='00*-xrt/',/filt,iter=iter,skipgv=skipgv,nospawn=nospawn,suffix=suffix,noprompt=noprompt,indir=grbdir,grb=gb,wavdetect=wavdetect,name=grbs[i].grb,optcat=optcat
     print,'GRB'+grbs[i].grb
     
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
     grbs[i].optcat=optcat

     if keyword_set(ps) then endplot

     cd,'../'
     
     mwrfits,grbs,'grb_astrometry'+suffix+'.fits',/create
  endfor


  stop
  return
end
