pro plot_ccdt,yr,ddd,ahk=ahk,outfile=outfile,noplot=noplot,time=time,temp=temp
 
  if n_elements(ddd) eq 0 then begin
     jdnow=systime(/julian)
     daycnv,jdnow,yr,mn,day,hr
     ddd = ymd2dn(yr,mn,day)
     print,'NO DATE SPECIFIED, USING TODAY'
     print,'for specific date: plot_ccdt,yyyy,ddd'
  endif
  
  if n_elements(outfile) eq 0 then outfile='ccdt_'+ntostr(yr)+ntostr(ddd)+'.ps'
  
  
  dir='/bulk/yankees/xrt/pass_data/'
  
  set='pass_'+ntostr(yr)+ntostr(ddd)+'*'
  file=dir+set+'/'+set+'_ahk.0.stripchart_table.fits'
  
  files=findfile(file)
  
  set='pass_'+ntostr(yr)+ntostr(ddd-1)+'2*'
  bfile=dir+set+'/'+set+'_ahk.0.stripchart_table.fits'
  bfiles=findfile(bfile)
  set='pass_'+ntostr(yr)+ntostr(ddd+1)+'0*'
  efile=dir+set+'/'+set+'_ahk.0.stripchart_table.fits'
  efiles=findfile(efile)
  
  files=[bfiles,files,efiles]
  w=where(files ne '',nw)
  if nw gt 0 then files=files[w]
  
  nfiles=n_elements(files)
  
  for i=0,nfiles-1 do begin
     
     ahk1=mrdfits(files[i],1,/silent)

     if i gt 1 then begin
        concat_structs,ahk,ahk1,tmp
        ahk=tmp
     endif else ahk=ahk1
  endfor 
  
  stt=date2met(ntostr(yr)+'-'+ntostr(ddd)+'-0:00:00.000')
  edt=date2met(ntostr(yr)+'-'+ntostr(ddd+1)+'-0:00:00.000')
  wg=where(ahk.sctime gt stt and ahk.sctime lt edt)
  ahk=ahk[wg]
  
  temp=ahk.ccdtemp1_rtd_
  yrange=[min(temp),max(temp)]
;  plot,ahk.sctime,ahk.ccdtemp1_rtd_,xtitle='MET',ytitle='CCD Temp'
  
;  date=met2date_judy(ahk.sctime)
  if not keyword_set(noplot) then begin
     begplot,name=outfile,/land
     erase
     multiplot,[1,4],/init
     for i=0,3 do begin
        hr=i*6
        stt=date2met(ntostr(yr)+'-'+ntostr(ddd)+'-'+ntostr(hr)+':00:00.000')
        edt=date2met(ntostr(yr)+'-'+ntostr(ddd)+'-'+ntostr(hr+6)+':00:00.000')

        w=where(ahk.sctime ge stt and ahk.sctime le edt,nw)
        if i eq 3 then $
           xtitle='Hours (sub-intervals are 10 minutes)' else xtitle=''
        if i eq 2 then ytitle='Observations (see key)' else ytitle=''
        if i eq 0 then title='CCD TEMP 1 during Day '+ntostr(ddd) else title=''
        if nw gt 0 then begin 
           date=dblarr(5,nw)
           for k=0,nw-1 do begin
              date[*,k]=met2date_judy(ahk[w[k]].sctime)
           endfor 
           multiplot
           time=date[2,*]+date[3,*]/60.+date[4,*]/3600.
           
           plot,time[0,*],temp[w],xtitle=xtitle,ytitle=ytitle,title=title,charsize=1,xminor=6,xticks=6,xstyle=1,yminor=1,xticklen=.05,yticklen=0.01,yrange=yrange,xrange=[i*6,(i+1)*6],xtickname=ntostr(indgen(7)+(i*6))
        endif else begin
           multiplot
           plot,[i*6,(i+1)*6],yrange,/nodata,xtitle=xtitle,ytitle=ytitle,title=title,charsize=1,xminor=6,xticks=6,xstyle=1,yminor=1,xticklen=.05,yticklen=0.01,yrange=yrange,xrange=[i*6,(i+1)*6],xtickname=ntostr(indgen(7)+(i*6))
        endelse 
     endfor 
     multiplot,/reset
     
     endplot     
  endif 
  
  return
end 
