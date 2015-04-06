pro absorption_duration,grb,nh,nherr

  if n_elements(nh) eq 0 then begin 
     cd,'~/GRBs'
     grb=mrdfits('~/Swift/swiftgrb.fits',1)

     ngrb=n_elements(grb)
     nh=dblarr(ngrb)
     nherr=dblarr(2,ngrb)
     for i=0,ngrb-1 do begin
        g=strtrim(grb[i].name,2)
        if not exist(g) then g=g+'A'
        if exist(g) then begin
           specfile=g+'/UL_specfits.fits'
           if exist(specfile) then begin 
              spec=mrdfits(specfile,1)
              nh[i]=spec[0].nh
              nherr[*,i]=spec[0].nherr
           endif 
        endif 
     endfor 

  endif 

  ngrb=n_elements(grb)
  !p.multi=[0,1,3]
  q=where(nh lt 1e15)
  nh[q]=0.
  w=where(nh ne 0 and grb.bat_t90 ne 0.); and nh-nherr[0,*] eq 0)
  s=where(grb[w].bat_t90 le 2.,ns)
  l=where(grb[w].bat_t90 gt 2.)
  s=w[s]
  l=w[l]
  
  dir='~/stuff_for_people/Nora/'
  readcol,dir+'comblist.csv',stuff,format='(a)',delim='#'
  nstuff=n_elements(stuff)
  grbs=strarr(nstuff) & mission=grbs & ee=grbs
  for i=0,nstuff-1 do begin
     chunks=str_sep(stuff[i],',')
     grbs[i]=chunks[0]
     if strmid(grbs[i],0,1)*1. gt 1 then grbs[i]='0'+grbs[i]
     mission[i]=chunks[1]
     ee[i]=chunks[2]
  endfor 

  w2=where(nh ne 0 and grb.bat_t90 ne 0.,nw2)
  match,'GRB'+strtrim(grbs,2),strtrim(grb[w2].name,2),m1,m2
  dont_match,'GRB'+strtrim(grbs,2),strtrim(grb[w2].name,2),dm1,dm2
  s2=where(nh[w2[m2]] ne 0,ns2); and nh-nherr[0,*] eq 0,ns2)
  s2=w2[m2[s2]]
  l2=where(nh[w2[dm2]] ne 0,nl2); and nh-nherr[0,*] eq 0,nl2)
  l2=w2[dm2[l2]]
;  l2=l
;  s2=s
;  ns2=ns
  begplot,name='~/Swift/absorption_duration.eps',/color,font='helvetica'
  plotsym,0,0.5,/fill
  plot,grb[l2].bat_t90,nh[l2],psym=8,/ylog,/xlog,xrange=[1e-2,1e3],xtitle='BAT T90 (s)',ytitle='N!LH!N (cm!U-2!N)',yrange=[1e18,1e24],charsize=2,title='All GRBs'
  for i=0,nl2-1 do oplot,[grb[l2[i]].bat_t90,grb[l2[i]].bat_t90],[nh[l2[i]]-nherr[0,l2[i]]+0.01,nh[l2[i]]+nherr[1,l2[i]]]

  oplot,grb[s2].bat_t90,nh[s2],psym=8,color=!red
  for j=0,ns2-1 do begin
     i=s2[j]
     oplot,[grb[i].bat_t90,grb[i].bat_t90],[nh[i]-nherr[0,i]+0.01,nh[i]+nherr[1,i]],color=!red
  endfor 
  legend,['Long','Short'],/top,/left,box=0,textcolor=[!p.color,!red],charsize=1.5

  plothist,alog10(nh[l2]),xtitle='log N!LH!N (cm!U-2!N)',bin=0.1,charsize=2,ytitle='N'
  plothist,alog10(nh[s2]),color=!red,/over,bin=0.1
  
  kstwop,alog10(nh[s2]),alog10(nh[l2]),d,prob,/plot,charsize=2,ytitle='Prob',xtitle='log N!LH!N',xrange=[18,25],/xsty
  !p.multi=0
  endplot
  spawn,'convert ~/Swift/absorption_duration.eps ~/Swift/absorption_duration.pdf'
  
  ;;;; those with z
;  l2=l
;  s2=s
;  ns2=ns
  wl=where(grb[l2].redshift ne 0,nl2)
  l2=l2[wl]
  ws=where(grb[s2].redshift ne 0,ns2)
  s2=s2[ws]

  begplot,name='~/Swift/absorption_duration_withz.eps',/color,font='helvetica'
  !p.multi=[0,1,3]
  plotsym,0,0.5,/fill
  plot,grb[l2].bat_t90/(1.+grb[l2].redshift),nh[l2],psym=8,/ylog,/xlog,xrange=[1e-2,1e3],xtitle='BAT T90 (s)/(1+z)',ytitle='N!LH!N (cm!U-2!N)',yrange=[1e18,1e24],charsize=2,title='GRBs w/ z'
  for i=0,nl2-1 do oplot,[grb[l2[i]].bat_t90,grb[l2[i]].bat_t90]/(1.+grb[l2[i]].redshift),[nh[l2[i]]-nherr[0,l2[i]]+0.01,nh[l2[i]]+nherr[1,l2[i]]]

  oplot,grb[s2].bat_t90/(1.+grb[s2].redshift),nh[s2],psym=8,color=!red
  for j=0,ns2-1 do begin
     i=s2[j]
     oplot,[grb[i].bat_t90,grb[i].bat_t90]/(1.+grb[i].redshift),[nh[i]-nherr[0,i]+0.01,nh[i]+nherr[1,i]],color=!red
  endfor 
  legend,['Long','Short'],/top,/left,box=0,textcolor=[!p.color,!red],charsize=1.5

  plothist,alog10(nh[l2]),xtitle='log N!LH!N (cm!U-2!N)',bin=0.1,charsize=2,ytitle='N'
  plothist,alog10(nh[s2]),color=!red,/over,bin=0.1
  
  kstwop,alog10(nh[s2]),alog10(nh[l2]),d,prob,/plot,charsize=2,ytitle='Prob',xtitle='log N!LH!N',xrange=[18,25],/xsty
  !p.multi=0
  endplot
  spawn,'convert ~/Swift/absorption_duration_withz.eps ~/Swift/absorption_duration_withz.pdf'

  stop
  return
end 
        
