pro plot_hotpix_trends,date,x,y
  
  dir='~xrt/hotpix/'
  
  jdnow=systime(/julian)
  daycnv,jdnow,yr,mn,day,hr
  dy = ymd2dn(yr,mn,day)
  if dy lt 100 then dy='0'+ntostr(dy)
  if dy lt 10 then dy='0'+ntostr(dy)
  today=ntostr(yr)+ntostr(dy)
  readcol,dir+'monthdays.txt',year,mstr,mno,fday,lday,format='(i4,a,i2,a,a)'
  
  ffday=ntostr(year)+ntostr(fday)
  llday=ntostr(year)+ntostr(lday)
  mnfiles=dir+'hotpix_'+ffday+'_'+llday+'.fits'
  mnfiles=file_search(mnfiles)
  w=where(mno eq mn and year eq yr,nw)
  
  plot_hotpix,ffday[w[0]],llday[w[0]],/noplot
  close,/all,/file
;  for i=1,99 do free_lun,i,/force
  pix=''
  ext=0L
  date=''
  x=''
  y=''
  tm=0
  for i=0,n_elements(mnfiles)-1 do begin 
     mnfile=str_sep(mnfiles[i],'.')
     mnfile=mnfile[0]+'.txt'
     readcol,mnfile,day,extt,pixx,pixy,format='(A,L,A,A)',delim=','
     date=[date,day]
     pix=[pix,pixx+'_'+pixy]
     x=[x,pixx]
     y=[y,pixy]
     ext=[ext,extt]
;     minday=min(day)
;     wwrap=where((minday*1L) mod year eq 365,nwrap)
;     if nwrap gt 0 then minday[wwrap]=
     tfday=[2005000,llday]
     m=where(min(day) ge tfday and max(day) le llday)
;     m=where(ffday ge min(day) and max(day) le llday)
     tm=[tm,replicate(mno[m],n_elements(pixx))]
  endfor 
;  files=findfile(dir+'hotpix_*.fits')
;  nfiles=n_elements(files)
;  dfile=findfile(dir+'hotpix_*.txt')
;  pix=''
;  ext=0L
;  date=''
;  for i=0,nfiles-1 do begin
;     readcol,dfile[i],day,extt,pixx,pixy,format='(A,L,A,A)',delim=','
;     pix=[pix,pixx+'_'+pixy]
;     ext=[ext,extt]
;     date=[date,day]
;  endfor 
  pix=pix[1:*]
  ext=ext[1:*]
  date=date[1:*]
  x=x[1:*]*1
  y=y[1:*]*1
  tm=tm[1:*]
  
  ;bad columns
  bc=find_bad_cols(x,y,nbc)
  nbc=n_elements(bc)
  
  ;bad pixel to not trend
  readcol,dir+'ignore_pix.txt',bpdate,bpx,bpy,format='(a,a,a)',delim=','  
  
  r=rem_dup(pix)
  rpix=pix[r]
  npix=n_elements(rpix)
  print,npix
  ccdt=0.
  tmon=0
  yr=0
  detx=0
  dety=0
  pha=0.
  
   k=0
   openw,klun,dir+'hotpix_trends_dat.txt',/get_lun
;   begplot,name=dir+'hotpix_trends.ps'
   for i=0,npix-1 do begin 
      if i ge 1 then openw,klun,dir+'hotpix_trends_dat.txt',/get_lun,/append
      w=where(pix eq rpix[i],nw)
      nw=n_elements(rem_dup(date[w]))
;      erase
      ff=0
      kk=0
      for j=0,nw-1 do begin 
         d=where(date[w[j]]*1L ge ffday*1L and date[w[j]]*1L le llday*1L)
         file=dir+'hotpix_'+ffday[d[0]]+'_'+llday[d[0]]+'.fits'

         hp=mrdfits(file,ext[w[j]],/silent)

         if n_elements(hp) gt 2 then begin 

            month=mstr[d[0]]
            tmp=str_sep(rpix[i],'_')
            x=tmp[0]
            y=tmp[1]
            time=met2date_judy(hp[0].time)
            day=ntostr(fix(time[0]))+ntostr(fix(time[1]))
          
            ccdt=[ccdt,hp.tccd]
            nt=n_elements(rem_dup(hp.tccd))
            pha=[pha,hp.pha]
            tmon=[tmon,replicate(mno[d[0]],n_elements(hp))]
            yr=[yr,replicate(year[d[0]],n_elements(hp))]
            
            detx=[detx,hp.detx]
            dety=[dety,hp.dety]
            
            wbc0=where(x eq bc,n0)
            wbc1=where((day eq bpdate or '*' eq bpdate) and (x eq bpx or '*' eq bpx) and (y eq bpy or '*' eq bpy),n1)
            nwbc=n0+n1
       
            if nwbc eq 0 then begin 

;               if ff eq 0 then begin 
;                  title='Pix '+rpix[i] 
;                  xrange=[-65,-50] ;[min(hp.tccd),max(hp.tccd)]
;                  erase
;                  multiplot,[1,nw],/init
;                  k=k+1
;                  ymax=max(hp.pha)
;                  wmax=where(hp.pha ne ymax)
;                  if wmax[0] eq -1 then yrange=[ymax-10,ymax+10] else $
;                     yrange=[min(hp.pha),max(hp[wmax].pha)]
;               endif else title=''
;               ff=1
              
;               if j eq nw-1 then begin 
;                  xtitle='T!LCCD!N'
;                  ytitle='PHA'
;               endif else begin 
;                  xtitle=''
;                  ytitle=''
;               endelse 
              
;               multiplot
;               plot,hp.tccd,hp.pha,xtitle=xtitle,ytitle=ytitle,title=title,/ynozero,psym=1,xrange=xrange,yrange=yrange,symsize=0.5,xstyle=1 ;else begin 
 ;;                 plot,[hp.tccd-1,hp.tccd+1],[hp.pha-10,hp.pha+10],/nodata,xtitle=xtitle,ytitle=ytitle,title=title,/ynozero,psym=1,xrange=xrange,yrange=yrange,symsize=0.5
 ;;                 plots,hp.tccd,hp.pha,psym=1,symsize=0.5
 ;;              endelse 
 ;              legend,[ntostr(year[d[0]])+' '+month],/top,/left,box=0
               print,rpix[i]
 ;;              printf,klun,day+','+x+','+y+','+ntostr(k)
               kk=kk+1
           endif else begin
              print,rpix[i],' skip'
           endelse 
        endif 
     endfor
     if kk gt 0 then printf,klun,ntostr(kk)+','+x+','+y+','+ntostr(k)
;     multiplot,/reset 
     close,/all,/file
  endfor  
  ccdt=ccdt[1:*]
  tmon=tmon[1:*]
  yr=yr[1:*]
  detx=detx[1:*]
  dety=dety[1:*]
  pha=pha[1:*]
  pix=ntostr(detx)+'_'+ntostr(dety)
;  endplot
;  close,klun
;  free_lun,klun
  close,/all,/file
  
  print,'PLOT_HOTPIX_TRENDS: plot bad columns'
  ;plot badcols together
  begplot,name=dir+'hotpix_trends_bc.ps'
  if nbc gt 0 then begin 
     for i=0,nbc-1 do begin 
        rp=where(detx eq bc[i],nrp)
        if nrp gt 0 then begin 
           tmoncol=tmon[rp]*1d3+yr[rp]
           mm=tmoncol[rem_dup(tmoncol)]
           nm=n_elements(mm)
           erase
           multiplot,[1,nm],/init
           for m=0,nm-1 do begin 
              print,'Bad Column ',detx[rp[0]]
              if m eq 0 then title='Bad Column '+ntostr(bc[i]) else title=''
;              w=where(tmon[rp] eq mm[m],nw)
              w=where(tmoncol eq mm[m],nw)
              help,w
              if m eq nm-1 then begin 
                 xtitle='T!LCCD!N'
                 ytitle='PHA'
              endif else begin 
                 xtitle=''
                 ytitle=''
              endelse 
              multiplot
              plot,ccdt[rp[w]],pha[rp[w]],xtitle=xtitle,ytitle=ytitle,title=title,/ynozero,psym=1,xrange=xrange,yrange=yrange,symsize=0.5
;              legend,[ntostr(year[0])+' '+mstr[mm[m]-1]],/top,/left,box=0
              month=month_cnv(tmon[rp[w[0]]])
              legend,[ntostr(yr[w[0]])+' '+month],/top,/left,box=0
           endfor 
           multiplot,/reset
        endif 
     endfor 
  endif  
  
  endplot
  
  erase
  print,'PLOT_HOTPIX_TRENDS: plot summary'
  begplot,name=dir+'hotpix_trends_summary.ps'
  tmoncol=tmon*1d3+yr
  mns=tmoncol[rem_dup(tmoncol)]
  nmns=n_elements(mns)
  multiplot,[1,nmns],/init
  ytitle='N'
  xrange=[-70,-50]
  for i=0,nmns-1 do begin
     if i eq 0 then title='N hotpix in central 200x200' else title=''
     if i eq nmns then xtitle='CCD Temp' else xtitle=''
     multiplot
     w=where(tmoncol eq mns[i] and detx ge 200 and detx le 400 and dety ge 200 and dety le 400)
     plothist,ccdt[w],xtitle=xtitle,ytitle=ytitle,title=title,bin=0.1,xrange=xrange
     month=month_cnv(tmon[w[0]])
     legend,[ntostr(yr[w[0]])+' '+month],/top,/left,box=0
;     legend,[ntostr(year[mns[i]-1])+' '+mstr[mns[i]-1]],/top,/left,box=0
  endfor 
  multiplot,/reset
  
;  pix=ntostr(detx)+ntostr(dety)
;  r=rem_dup(pix)
;  detx=detx[r]
;  dety=dety[r]
  
  erase
  !p.multi=[0,2,3]
  for i=0,nmns-1 do begin 
     w=where(tmoncol eq mns[i])
;     w=where(tm eq mns[i],nw) ; and detx ge 200 and detx le 400 and dety ge 200 and dety le 400)
;     map=lonarr(600,600)
;     for j=0L,nw-1 do map[detx[w[j]],dety[w[j]]]=map[detx[w[j]],dety[w[j]]]+1
;     rdis,map,xmn=000,xmx=599,ymn=000,ymx=599,title='Hot Pixel Map - '+ntostr(year[mns[i]-1])+' '+mstr[mns[i]-1],xtitle='Det x',ytitle='Det y'
     r=rem_dup(pix[w])
     r=w[r]
     help,r
     month=month_cnv(tmon[w[0]])
     str=ntostr(yr[w[0]])+' '+month
     plot,detx[r],dety[r],/iso,psym=1,title='Hot Pixel Map - '+str,xtitle='Det x',ytitle='Det y',charsize=2,xrange=[0,600],yrange=[0,600]
;     plot,x[r],y[r],/iso,psym=1,title='Hot Pixel Map - '+ntostr(year[mns[i]-1])+' '+mstr[mns[i]-1],xtitle='Det x',ytitle='Det y',charsize=2,xrange=[0,600],yrange=[0,600]
  endfor 
  !p.multi=0
  
  
  endplot
  
  return
end 
