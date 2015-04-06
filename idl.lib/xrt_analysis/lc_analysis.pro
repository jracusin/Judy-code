pro lc_analysis,btime,tmin,lcpc,lcwt,lclr,ncounts=ncounts,nobreak=nobreak,title=title
  
  if n_params() eq 0 then begin
     print, 'syntax - lc_analysis,btime,tmin,lcpc,lcwt,lclr,ncounts=ncounts,nobreak=nobreak,title=title'
     return
  endif 
  
  simpctable
  nmodes=n_elements(tmin)
  for i=0,nmodes-1 do begin 
     if i eq 0 then lc=lcpc
     if i eq 1 then lc=lcwt
     if i eq 2 then lc=lclr
     tm=tmin[i]
     timem=lc.x+(tm-btime)
     
     bin_lc,timem,lc.density,lc.error,btm,brm,btem,bem,ncounts=ncounts
     
     if i eq 0 then begin 
        time=timem
        br=brm
        bt=btm
        bte=btem
        be=bem
        pc=indgen(n_elements(btm))
     endif else begin
        time=[time,timem]
        bt=[bt,btm]
        br=[br,brm]
        bte=[bte,btem]
        be=[be,bem]
     endelse 
     if i eq 1 then wt=indgen(n_elements(btm))+max(pc)
     if i eq 2 then lr=indgen(n_elements(btm))+max(wt)
     
  endfor 
  
  plot_lc,bt,br,bte,be,time,ind1=wt,ind2=lr,nobreak=nobreak,title=title
  
  return
end 
