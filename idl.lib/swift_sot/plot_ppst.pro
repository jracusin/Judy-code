pro plot_ppst,file,outfile,ppst=ppst,sunang=sunang,noplot=noplot,angsun=angsun,uvot=uvot,xrt=xrt,date=date
  
  if n_elements(file) eq 0 then file=pickfile(filter='*ST*.txt')
  
  f=str_sep(file,'.txt')
  f=f[0]
  
  if n_elements(outfile) eq 0 then outfile=f+'.ps'
  
  if strpos(file,'AFST') eq -1 then read_ppst,file,ppst else read_afst,file,ppst,date=date
  
  if not exist(file) then return
  if n_elements(date) ne 0 then begin 
     enddate=ntostr(date*1L+1L)
     mind=date2met(strmid(date,0,4)+'-'+strmid(date,4,3)+'-00:00:00')
     maxd=date2met(strmid(enddate,0,4)+'-'+strmid(enddate,4,3)+'-00:00:00')
     stpos=strpos(f,'ST_20')

     w=where(ppst.begtime gt mind and ppst.endtime lt maxd)
     ppst=ppst[w]
     
     outfile=strmid(f,stpos-2,5)+date+'0000_'+enddate+'0000_'+strmid(f,stpos+27,2)+'.ps'
     
  endif 
     
  begtime=ppst.begtime
  endtime=ppst.endtime
  targra=ppst.ra
  targdec=ppst.dec
  targetid=ppst.targetid
  targname=ppst.targname
  targroll=ppst.roll
  xmode=ppst.xmode
  umode=ppst.umode
  segno=ppst.obsseg
  ri=rem_dup(targetid*segno)
  rtarg=targetid[ri]
  rseg=segno[ri]
  ntarg=n_elements(rtarg)

  ntime=n_elements(begtime)
  
  begday=dblarr(ntime)
  endday=begday
  
  for i=0,ntime-1 do begin 
     btime=met2date_judy(begtime[i])
     etime=met2date_judy(endtime[i])
     
     begday[i]=btime[1]+btime[2]/24.+btime[3]/24./60.+btime[4]/24./3600.
     endday[i]=etime[1]+etime[2]/24.+etime[3]/24./60.+etime[4]/24./3600.
     
     if etime[0] gt btime[0] then endday[i]=endday[i]+366.
     if i eq 0 then starttime=round(begday[0])
     if i eq ntime-1 then stoptime=round(endday[i])
  endfor 
  
  begday=begday-starttime
  endday=endday-starttime
  mday=stoptime-starttime
;  stop
  if not keyword_set(noplot) then begin 
     begplot,name=outfile,/color,/land
     simpctable
     color=[!red,!green,!blue,!magenta,!orange,!cyan,!purple,!slategrey,!hotpink,!seagreen,!sienna,!violet,!navyblue,!skyblue,!darkgreen,!salmon,!yellow,!forestgreen,!firebrick,!royalblue,!turquoise,!lightgreen,!deeppink,!darkred]
     color=[color,color,color,color,color,color,color]
     
;  !p.multi=[0,1,4]

     for k=0,mday-1 do begin
        multiplot,[1,4],/init
        for p=0,3 do begin 
           if p eq 0 then $
              title='TAKO Schedule for Day '+ntostr(k+starttime,3) else title=''
           if p eq 3 then $
              xtitle='Hours (sub-intervals are 10 minutes)' else xtitle=''
           if p eq 2 then ytitle='Observations (see key)' else ytitle=''
           multiplot
          
           plot,[p*6,(p+1)*6],[0,ntarg+1],/nodata,xtitle=xtitle,ytitle=ytitle,title=title,charsize=1,xminor=6,xticks=6,xstyle=1,yminor=1,xticklen=.05,xtickname=ntostr(indgen(7)+(p*6)),yticklen=0.01,ystyle=1
           for c=0,ntarg,5 do oplot,[p*6,(p+1)*6],[c,c],line=2,color=!grey60,thick=1
           
           for i=0,ntarg-1 do begin
              w=where((targetid eq rtarg[i]) and (segno eq rseg[i]),nw)
              
              for j=0,nw-1 do begin
                 oplot,[begday[w[j]],endday[w[j]]]*24.-k*24.,[i+1,i+1],color=color[i],thick=8      
              endfor 
              
           endfor 
        endfor 
        multiplot,/reset
        erase
        
     endfor  
     
     tname=strarr(ntarg)
     tid=tname
     sno=tid
     plot,[0,1],[0,1],/nodata,color=!white
     for i=0,ntarg-1 do begin
        nlen=strlen(targname[ri[i]])
        space='' 
        if nlen lt 15 then for j=0,15-nlen do space=space+' '
        tname[i]=targname[ri[i]] ;+space
        w=where(targetid lt 100000)
        tid[i]=ntostr(targetid[ri[i]])+' '
        sno[i]=ntostr(segno[ri[i]])+' '
     endfor 
     
;  output=ntostr(indgen(ntarg)+1)+'  '+tname+'   '+tid+'    '+ntostr(targra[ri],6)+'    '+ntostr(targdec[ri],6)+'    '+ntostr(targroll[ri],7)
;  legend,['Target Name     Target ID      RA         DEC         Roll',$
;          '',output],$
;     textcolor=[0,0,color[0:ntarg-1]],charsize=2,/left ;,box=0
     
     ntargb=ntarg
     slen=strlen(targname[ri])
     slen=max(slen)
     ssp=''
     asp=0.

     if slen gt 15 then begin
        asp=0.1
        ssp='             '
     endif
     
     if ntarg gt 26 then begin
        npages=round(ntarg/25.+0.5)
        nt=intarr(npages)
        nt[0:npages-2]=25
        nt[npages-1]=(ntarg mod 25)
     endif else begin
        npages=1
        nt=ntarg
     endelse 
     
     for p=0,npages-1 do begin
        
        ntarg=nt[p]
        xyouts,-0.1,1,'Obs #    Target Name  '+ssp+'      TargID    Seg       RA          Dec       xmode     umode'

        for k=0.,ntarg-1. do begin
           sp=k/ntarg[0]
           if p ge 1 then nk=k+total(nt[0:p-1]) else nk=k
           if ntarg lt 15 then sp=k/17.
           sr='  ' & sd='   ';sr
           nr=6
           if targdec[ri[k]] lt 0 then begin
              nd=7
              sd=' '
           endif else nd=6
           xyouts,-0.08,0.9-sp,ntostr(fix(k+(p*25))+1),color=color[nk]
           xyouts,-0.02,0.9-sp,tname[nk],color=color[nk]
           xyouts,0.26+asp,0.9-sp,tid[nk],color=color[nk]
           xyouts,0.38+asp,0.9-sp,sno[nk],color=color[nk]
           xyouts,0.45+asp,0.9-sp,ntostr(targra[ri[nk]],nr),color=color[nk]
           xyouts,0.57+asp,0.9-sp,sd+ntostr(targdec[ri[nk]],nd),color=color[nk]
           xyouts,0.69+asp,0.9-sp,'  '+ntostr(xmode[ri[nk]],nd),color=color[nk]
           xyouts,0.82+asp,0.9-sp,' '+ntostr(umode[ri[nk]],nd),color=color[nk]
;           xyouts,0.72+asp,0.85-sp,'   '+ntostr(targroll[ri[nk]],7),color=color[nk]
        endfor
        erase
     endfor  
     
     endplot   
  endif 
  
  if keyword_set(sunang) then begin
     
     day=double(strmid(begdate,5,3))
     year=double(strmid(begdate,0,4))
     
     sunangb=dblarr(n_elements(begdate))
     sunange=sunangb
     for i=0,n_elements(begdate)-1 do begin
        ydn2md,year[i],day[i],month,dy
        jdcnv,year[i],month,dy+begday[i],0.,jdb
        jdcnv,year[i],month,dy+begday[i],0.,jde
        sunpos,jdb,sunrab,sundecb
        sunpos,jde,sunrae,sundece
        gcirc,1,sunrab/360.*24D,sundecb,targra[i]/360D*24D,targdec[i],disb
        sunangb[i]=disb/60./60.
        gcirc,1,sunrae/360.*24D,sundece,targra[i]/360D*24D,targdec[i],dise
        sunange[i]=dise/60./60.
     endfor 
     angsun=[sunangb,sunange]

     if not keyword_set(noplot) then begin 
        begplot,name=f+'_sunang.ps',/color,/land
        
        for k=0,mday-1 do begin
           erase
           multiplot,[1,4],/init
           for p=0,3 do begin 
              if p eq 0 then $
                 title='Sun Angle for TAKO Schedule for Day '+$
                 ntostr(k+starttime,3) else title=''

              if p eq 3 then $
                 xtitle='Hours (sub-intervals are 10 minutes)' else xtitle=''
              if p eq 2 then ytitle='Angle between Sun and Obs (deg)' else ytitle=''
              multiplot
              plot,[p*6,(p+1)*6],[min(sunangb),max(sunange)],/nodata,xtitle=xtitle,ytitle=ytitle,title=title,charsize=1,xminor=6,xticks=6,xstyle=1,yminor=1,xticklen=.05,xtickname=ntostr(indgen(7)+(p*6)),yticklen=0.01,/ynozero
              
              for i=0,ntargb-1 do begin
                 w=where((targetid eq rtarg[i]) and (segno eq rseg[i]),nw)
                 
                 for j=0,nw-1 do oplot,[begday[w[j]],endday[w[j]]]*24.-k*24.,[sunangb[w[j]],sunange[w[j]]],color=color[i],thick=8      
                 
              endfor 
           endfor 
           multiplot,/reset 
           
        endfor  

        endplot
     endif  

  if keyword_set(rollang) then begin
     begplot,name=f+'_rollang.ps',/color,/land
     
     day=double(strmid(begdate,5,3))
     year=double(strmid(begdate,0,4))
     
     for k=0,mday-1 do begin
        erase
        multiplot,[1,4],/init
        for p=0,3 do begin 
           if p eq 0 then $
              title='Roll Angle for TAKO Schedule for Day '+$
              ntostr(k+starttime,3) else title=''

           if p eq 3 then $
              xtitle='Hours (sub-intervals are 10 minutes)' else xtitle=''
           if p eq 2 then ytitle='Roll Angle of Obs (deg)' else ytitle=''
           multiplot
           plot,[p*6,(p+1)*6],[0,360],/nodata,xtitle=xtitle,ytitle=ytitle,title=title,charsize=1,xminor=6,xticks=6,xstyle=1,yminor=1,xticklen=.05,xtickname=ntostr(indgen(7)+(p*6)),yticklen=0.01
           
           for i=0,ntargb-1 do begin
              w=where((targetid eq rtarg[i]) and (segno eq rseg[i]),nw)
              
              for j=0,nw-1 do oplot,[begday[w[j]],endday[w[j]]]*24.-k*24.,[targroll[w[j]],targroll[w[j]]],color=color[i],thick=8      
              
           endfor 
        endfor 
        multiplot,/reset 
        
     endfor  
     endplot
  endif 
endif 
;  stop
  
  
  return
end 


;date/time | commmand | begin/end | objname | target_id | obs_num | ?? | ra | dec | roll angle | BAT mode | XRT mode | UVOT mode | priority | ??
 
