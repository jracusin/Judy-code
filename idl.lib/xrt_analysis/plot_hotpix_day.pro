pro plot_hotpix_day
  
  begplot,name='~/hotpix/hotpix_day.ps'
  daymin=2005089L
  daymax=2005365L
  nday=daymax-daymin
 
  !p.multi=[0,3,4]
  for j=0,nday-1 do begin 
     day=daymin+j
     file='~/hotpix/hotpix_'+ntostr(day)+'_'+ntostr(day)+'.txt'
     if exist(file) then begin 
        pixx=''
        pixy=''
        readcol,file,d,ext,pixx,pixy,format='(A,L,A,A)',delim=','
        
;     x=[x,pixx]
;     y=[y,pixy]
        if pixx[0] ne '' then plot,pixx,pixy,psym=1,xtitle='Det x',ytitle='Det y',title='Day '+ntostr(day),/iso,xrange=[0,600],yrange=[0,600],symsize=0.5,charsize=1.5
     endif
  endfor 
  !p.multi=0
  endplot
;  rday=day[rem_dup(day)]
;  d=day*1L
;  nday=n_elements(rday)
;  !p.multi=[0,3,4]
;  for i=0,nday-1 do begin
;     w=where(d*1L eq rday[i],nw)
;     if nw gt 0 then plot,x[w],y[w],psym=1,xtitle='Det x',ytitle='Det y',title='Day '+ntostr(rday[i]),/iso,xrange=[0,600],yrange=[0,600],charsize=1
;  endfor 
;  !p.multi=0
  
  return
end 
  
