pro plot_uvot_data,ps=ps
  
  
  dir='~/Desktop/GRB080319B/data_tables/'
  cd,dir
  
  if keyword_set(ps) then begplot,name='~/Desktop/GRB080319B/GCN/uvotlc.ps',/land,/color else erase

    ;;;;uvot
  readcol,'grb080319b_UVOTphot.txt',time,tbin,rate,raterr,mag,magerr1,magerr2,format='(f,f,f,f,f,f,f)',/silent
  filters=['white','v    ','b    ','u    ','uvw1 ','uvm2 ','uvw2 ']
  colors=[!p.color,!red,!green,!blue,!cyan,!magenta,!orange]
  
  multiplot2,[1,7],/init
  n=n_elements(time)
  w=where(time-time[1:*] gt 0)
  s='     '
  for i=0,6 do begin 
     if i eq 6 then xtitle='Time since BAT trigger (s)' else xtitle=''
     if i eq 0 then start=0 else start=w[i-1]+1
     if i le 5 then stop=w[i] else stop=n-1
     nn=stop-start+1
     ind=indgen(nn)+start
     multiplot2
     if i eq 0 then begin
        k=where(time[ind] gt 1000)
        ind=ind[k]
     endif 
     if i eq 1 then begin 
        k=where(time[ind] gt 350)
        ind=ind[k]
     endif 
     plot,[1e2,3e6],[25,10],/xlog,yrange=[25,10],xtitle=xtitle,ytitle=strtrim(filters[i],2)+' mag',charsize=1.,xticklen=0.1,xrange=[1e2,3e6],/xsty,/nodata
     oplot,time[ind],mag[ind],psym=6,symsize=0.5,color=colors[i]
     for t=0,n_elements(ind)-1 do begin
        oplot,[time[ind[t]]-tbin[ind[t]],time[ind[t]]+tbin[ind[t]]],[mag[ind[t]],mag[ind[t]]],color=colors[i]
        oplot,[time[ind[t]],time[ind[t]]],[mag[ind[t]]+magerr2[ind[t]],mag[ind[t]]+magerr1[ind[t]]],color=colors[i]
     endfor 
     
;     for k=0,nn-1 do printf,lun,time[ind[k]],tbin[ind[k]]*2.,s+numdec(mag[ind[k]],2)+s+magerr[ind[k]]+s+filters[i]+s+'UVOT'
  endfor 
  
  multiplot2,/reset,/default
  
  if keyword_set(ps) then endplot
  
  stop
  
  return
end 
