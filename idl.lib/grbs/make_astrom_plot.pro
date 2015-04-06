pro make_astrom_plot,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title=title,_extra=_extra
  
  ra=[ralist,newra]
  dec=[declist,newdec]
  err=[errlist,newerr]/3600.
  name=[names,'New XRT position']
  
  minra=min(ra-err,mra1)
  maxra=max(ra+err,mra2)
  mindec=min(dec-err,mdec1)
  maxdec=max(dec+err,mdec2)
  
;  delra=round((ra[mra2]-ra[mra1])*3600.)
;  deldec=round((dec[mdec2]-dec[mdec1])*3600.)
  
  ras=ra[sort(ra)]
  decs=dec[sort(dec)]
  
  raoff=[-2,1]/3600.
  decoff=[-1.5,1.5]/3600.
  xrange=[maxra,minra]-raoff
  yrange=[mindec,maxdec]+decoff
    
  ra_names,xrange,tick_val=raval,tick_name=raname
  dec_names,yrange,tick_val=decval,tick_name=decname
  
  xticks=n_elements(raval)-1
  yticks=n_elements(decval)-1
map_set,/ait
  plot,xrange,yrange,psym=1,/yno,/nodata,/iso,$
     xtitle='RA (J2000)',ytitle='Dec (J2000)',title=title,$
     xtickv=raval,ytickv=decval,$
     xticks=xticks,yticks=yticks,xrange=xrange,$
     xminor=4,yminor=5,ytickname=decname,xtickname=raname,_extra=_extra,charsize=1
  
  
  color=[!green,!blue,!red,!magenta,!orange,!slategrey,!cyan,!forestgreen]

  for i=0,n_elements(ra)-2 do begin
     if err[i] ne 0 then tvcircle,err[i],ra[i],dec[i],color=color[i],/data else $
        plots,ra[i],dec[i],color=color[i],psym=2
     xyouts,ra[i]-err[i]/2.,dec[i]+err[i]*1.1,name[i],color=color[i],/data;,charsize=0.5
  endfor
  
  if err[i] ne 0 then tvcircle,err[i],ra[i],dec[i],color=!purple,/data else $
     plots,ra[i],dec[i],color=!purple,psym=2
  xyouts,ra[i]-err[i]/2.,dec[i]+err[i]*1.1,name[i],color=!purple,/data ;,charsize=0.5
;  stop
 
  return
end
