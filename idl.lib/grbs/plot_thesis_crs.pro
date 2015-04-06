pro plot_thesis_crs,ps=ps,skip=skip
  

  cd,!mdata
  cr=mrdfits(!mdata+'closure_relations_total_2sig.fits',1)
  
  grbs=cr[uniq(cr.grb)].grb
  nu=n_elements(grbs)
  if keyword_set(skip) then goto,skip  
  for i=0,nu-1 do begin
     w=where(cr.grb eq grbs[i])
     grb=strtrim(grbs[i],2)
     cd,grb
;     if cr[w[0]].seg1 eq 0 then skipfirst=1 else skipfirst=0
     if keyword_set(ps) then begplot,name=grb+'_crplot.eps',/encap
     fit_crs,cr[w].alpha,cr[w].alphaerr[0],cr[w].alphaerr[1],cr[w].beta+1.,cr[w].betaerr[0],cr[w].betaerr[1],/plotlc,/nocolor;,skipfirst=skipfirst
     if keyword_set(ps) then endplot
     cd,'..'
     if not keyword_set(ps) then begin 
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 
  endfor 

  spawn,'cp GRB*/*crplot.eps ~/thesis/chapters/appendix_cr_plots/'
  skip:  
  spawn,'rm ~/thesis/chapters/appendix_cr_plots/appendix_cr_plots.tex'
  openw,lun,'~/thesis/chapters/appendix_cr_plots/appendix_cr_plots.tex',/get_lun
  printf,lun,'\chapter{CLOSURE RELATION FITS}'
  printf,lun
  
  for i=0,nu-1 do begin 
     w=where(cr.grb eq grbs[i],nw)
     grb=strtrim(grbs[i],2)
     if nw gt 1 then mo=', closure relation fits ({\it middle panels}), and model combinations ({\it bottom panel})' else mo=', closure relation fits ({\it lower panel})'
     if i gt 98 then sp='\ ' else sp=''
     printf,lun,'\clearpage'
     printf,lun,'\begin{figure}'
     printf,lun,'  \begin{center}'
     printf,lun,'    \includegraphics[scale=0.8]{chapters/appendix_cr_plots/'+grb+'_crplot.eps}'
     printf,lun,'    \caption['+sp+'GRB '+strmid(grb,3,7)+' Closure Relation Fits]{GRB '+strmid(grb,3,7)+' light curve ({\it top panel})'+mo+'.  \label{fig:'+grb+'}}'
     printf,lun,'  \end{center}'
     printf,lun,'\end{figure}'
     printf,lun
     
  endfor 
  close,lun
  free_lun,lun
  
  return
end 
     
