pro collect_hotpix,daymin,daymax,file,tfile,npix,p1
  
  dir='/bulk/yankees2/xrt/pass_data/'
  outdir='/home/xrt/hotpix/'
  spawn,'dir '+dir+' -1 > '+dir+'dirs.txt'
  readcol,dir+'dirs.txt',passes,format='(a)'
  wp=where(strpos(passes,'pass_') eq 0 and strlen(passes) eq 16)
  passes=passes[wp]
  days=strmid(passes,5,7)
;  if daymin*1L lt 400 then daymin='2005'+ntostr(daymin)
;  if daymax*1L lt 400 then daymax='2005'+ntostr(daymax)
;  if daymin*1L gt 2010000L then days=strmid(passes,5,11)
  print,daymin[0],' ',daymax[0]
  w=where(days ge daymin[0] and days le daymax[0])
  files=file_search(dir+passes[w]+'/*hotpix*.fits')
  nfiles=n_elements(files)
  if files[0] eq '' then begin
     print,'No hotpix for day '+daymin+' '+daymax
     npix=0
     return
  endif
  hppos=strpos(files,'hotpix')
  ppos=strpos(files,'.fits')
  pix=strarr(nfiles)
  for i=0L,nfiles-1 do pix[i]=strmid(files[i],hppos[i]+7,ppos[i]-hppos[i]-7)
  rpix=pix[rem_dup(pix)]
  npix=n_elements(rpix)
  if nfiles eq 0 then begin
     print,'No hotpix files in day range'
     return
  endif 
  
;  f=''
  file=outdir+'hotpix_'+daymin+'_'+daymax+'.fits'
  tfile=outdir+'hotpix_'+daymin+'_'+daymax+'.txt'
  openw,lun,tfile,/get_lun
  for i=0L,npix-1 do begin 
     w=where(rpix[i] eq pix,nw)
     p1=mrdfits(files[w[0]],1,/silent)
     if nw gt 2 then begin 
        for j=1L,nw-1,2 do begin 
           p2=mrdfits(files[w[j]],1,/silent)
           
           concat_structs,p1,p2,tmp
           p1=tmp
           
        endfor 
     endif 
;     file=outdir+'hotpix_'+rpix[i]+'_'+daymin+'_'+daymax+'.fits'
;     f=[f,file]
;     print,file
     date=met2date(p1[0].time)
     date2=str_sep(date,'-')
     date=date2[0]+date2[1]
     pp=str_sep(rpix[i],'_')
     printf,lun,date+','+ntostr(i+1)+','+pp[0]+','+pp[1]
     
     if i eq 0 then mwrfits,p1,file,/create else mwrfits,p1,file
     
  endfor 
  close,lun
  free_lun,lun
;  f=f[1:*]
  
  return
end  
  
pro plot_hotpix,daymin,daymax,noplot=noplot,npix=npix
  
  collect_hotpix,daymin,daymax,files,tfile,npix
  outdir='/home/xrt/hotpix/'
  
  if not keyword_set(noplot) and npix ne 0 then begin 
     readcol,tfile,day,extt,pixx,pixy,format='(A,L,A,A)',delim=','
     ccdt=0.
     detx=0
     dety=0
     begplot,name=outdir+'hotpix_'+ntostr(daymin)+'_'+ntostr(daymax)+'.ps',/land
     print,npix
     for i=0L,npix-1 do begin 
        x=pixx[i]
        y=pixy[i]
        if npix lt 1000 then begin
           hp=mrdfits(files,i+1,/silent)
           np=n_elements(hp.time)    
;           x=hp[0].detx
;           y=hp[0].dety
           ccdt=[ccdt,hp.tccd]
        endif 
;           detx=[detx,hp.detx]
;           dety=[dety,hp.dety]
;        endif 
        detx=[detx,x]
        dety=[dety,y]
        
        if npix lt 1000 or (x lt 292 and x gt 294) then begin 
           if n_elements(hp) gt 1 then plot,hp.tccd,hp.pha,xtitle='T!LCCD!N',ytitle='PHA',title='Pix '+ntostr(x)+','+ntostr(y),psym=1,symsize=0.5,/ynozero,charsize=2 else begin 
              plot,[hp.tccd-1,hp.tccd+1],[hp.pha-10,hp.pha+10],/nodata,xtitle='T!LCCD!N',ytitle='PHA',title='Pix '+ntostr(x)+','+ntostr(y),/ynozero,charsize=2
              plots,hp.tccd,hp.pha,psym=1,symsize=0.5
           endelse 
           legend,['Days: '+ntostr(daymin)+' - '+ntostr(daymax)],/top,/left,box=0
           erase
        endif 
     endfor 
     erase
     if n_elements(ccdt) gt 1 then ccdt=ccdt[1:*]
     detx=detx[1:*]
     dety=dety[1:*]
     
     ytitle='N'
     title='N hotpix in central 200x200'
     xtitle='CCD Temp'
     if n_elements(rem_dup(ccdt)) gt 1 then begin 
        plothist,ccdt,xtitle=xtitle,ytitle=ytitle,title=title,bin=0.1,charsize=2
        legend,['Days: '+ntostr(daymin)+' - '+ntostr(daymax)],/top,/left,box=0
     endif else begin
        plot,[0,1],[0,1],/nodata,xtitle=xtitle,ytitle=ytitle,title=title,charsize=2
        if npix lt 1000 then xyouts,0.1,0.5,'No hot pixels in central 200x200',/data,charsize=2 else xyouts,0.1,0.5,'To many hot pixels in central 200x200 to track',/data,charsize=2
     endelse 
     erase
     plot,detx,dety,/iso,psym=1,title='Map of hot pixels',xtitle='Det x',ytitle='Det y',charsize=2,xrange=[0,600],yrange=[0,600]
;     map=intarr(600,600)
;     for i=0L,n_elements(detx)-1 do map[detx[i],dety[i]]=map[detx[i],dety[i]]+1
;     rdis,map,title='Central 200x200 hot pixels',xtitle='Det x',ytitle='Det y';,xmn=200,xmx=400,ymn=200,ymx=400
     
     endplot
  endif   
  
  
  return
end 
