pro shb_figure,ps=ps
  
  if keyword_set(ps) then begplot,name='shb_figure.ps',/color
  csvfile='short_grbs.csv'
  readcol,!grbs+csvfile,grbname,tid,battime,wt,format='(a,l,d,i)',delim=','
  ngrbs=n_elements(grbname)
  
  cd,!mdata
  !p.multi=[0,2,5]
  for i=0,ngrbs-1 do begin
     if i ne 9 then begin 
        dir='GRB'+grbname[i]
        cd,dir
        if exist('lc_newout.txt') then $
           plot_like_qdp,0,0,dir,siglim=1.7,charsize=1.5,title=dir,/multi
;        replot_xrt_newlc,siglim=1.0,title=grbname[i]
        cd,'..'
     endif 
  endfor 
  !p.multi=0
  if keyword_set(ps) then endplot
stop  
  return
end 
