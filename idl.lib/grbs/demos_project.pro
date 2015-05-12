@fit_functions
@fit_functions_flares
pro plot_fit,g

  cd,'~/GRBs'
  g=mrdfits('~/stuff_for_people/Demos/bat_xrt.fits',1)
  begplot,name='~/stuff_for_people/Demos/ratio_plots.eps',/land,/color,font='helvetica'
  !p.charsize=2
  grbs=g.grb
  for i=0,n_elements(grbs)-3 do begin 
     cd,strtrim(g[i].grb,2)
     print,grbs[i]
     lcfitfile='lc_fit_out_idl_int9.dat'
;     if not exist(lcfitfile) then lcfitfile='lc_fit_out_idl_int7.dat'
     if exist(lcfitfile) and exist('bat_flux_snr5_with_cf_XRTBAND.qdp.gz') then begin 
        lc=lcout2fits(/withbat)
        b=where(lc.type eq 3 and lc.time gt 0,nb)
        spec=mrdfits('UL_specfits.fits',1)
        ff=spec[n_elements(spec)-1].unabs_cfratio

        if numlines(lcfitfile) gt 1 then begin
           read_lcfit,lcfitfile,pnames,p

           mo=fit_models(pnames,p,np,basemo=basemo)
           mo=basemo
           if np gt 3 then begin 
              if p[1] gt p[3] and p[3] lt 1. then begin 
                 tb=p[2]
                 tmp=execute('ftb='+mo+'(tb,p)')
                 plot_like_qdp,/withbat,flux=ff,/useflux,title=grbs[i],lc=lc
                 if nb gt 1 then btime=median(lc[b].time) else btime=lc[b].time
                 arrow,btime,g[i].batrate_gmean*0.5*ff,btime,g[i].xrtrate*ff,style=3,/data,thick=20,color=!orange,/solid,hthick=6
                 arrow,btime,g[i].xrtrate*1.5*ff,btime,g[i].batrate_gmean*ff,style=3,/data,thick=20,color=!orange,/solid,hthick=6

;                 arrow,btime,g[i].batrate_amean*0.5*ff,btime,g[i].xrtrate*ff,style=3,/data,thick=20,color=!orange,/solid,hthick=6
;                 arrow,btime,g[i].xrtrate*1.5*ff,btime,g[i].batrate_amean*ff,style=3,/data,thick=20,color=!orange,/solid,hthick=6


                 oplot,[1e-3,1e6],[g[i].batrate_gmean,g[i].batrate_gmean]*ff,color=!magenta,line=1
;                 oplot,[1e-3,1e6],[g[i].batrate_amean,g[i].batrate_amean]*ff,color=!cyan,line=2
                 oplot,[1e-3,1e6],[g[i].xrtrate,g[i].xrtrate]*ff,color=!orange,line=1
                 tmp=execute('yfit='+mo+'(lc.time,p)')
                 oplot,lc.time,yfit*ff,color=!green,line=2
                 print,g[i].batrate_gmean,g[i].xrtrate,g[i].batrate_gmean/g[i].xrtrate
;                 legend,['BAT/XRT geometric mean ratio =
;                 '+ntostr(round(g[i].batrate_gmean/g[i].xrtrate)),'BAT/XRT arithmatic mean ratio = '+ntostr(round(g[i].batrate_amean/g[i].xrtrate))],/top,/right,box=0,charsize=1.,textcolor=[!magenta,!cyan]
                 legend,['BAT/XRT ratio = '+ntostr(round(g[i].batrate_gmean/g[i].xrtrate))],/top,/right,box=0,charsize=1.
;                 k=get_kbrd(10)
;                 if k eq 's' then stop
              endif 
           endif 
        endif 
     endif
     cd,'~/GRBs' 
  endfor 
  endplot
  spawn,'ps2pdf ~/stuff_for_people/Demos/ratio_plots.eps ~/stuff_for_people/Demos/ratio_plots.pdf'

  return
end 

pro demos_project,go=go

  if exist('~/stuff_for_people/Demos/bat_xrt.fits') then $
     g=mrdfits('~/stuff_for_people/Demos/bat_xrt.fits',1) else begin


     cd,'~/GRBs'
     grbs=file_search('GRB*')
     ngrbs=n_elements(grbs)

     g=create_struct('grb','','batrate_gmean',0d,'batrate_amean',0d,'batrate_peak',0d,'xrtrate',0d,'ratio_gmean',0d,'ratio_amean',0d,'ratio_peak',0d,'plateau_slope',0d)
     g=replicate(g,ngrbs)


     for i=0,ngrbs-3 do begin 

        cd,grbs[i]
        print,grbs[i]
        if exist('UL_lc.fits') and exist('bat_flux_snr5_with_cf_XRTBAND.qdp.gz') then begin 
           g[i].grb=grbs[i]
           lc=lcout2fits(/withbat)
           if n_elements(lc) gt 0 then begin 
              b=where(lc.type eq 3)
;        spec=mrdfits('UL_specfits.fits',1)
;        fluxfact=spec.unabs_cfratio

;        g[i].batrate_mean=mean(lc[b].src_rate)
              g[i].batrate_peak=max(lc[b].src_rate)
              g[i].batrate_gmean=10^mean(alog10(lc[b].src_rate))
              g[i].batrate_amean=mean(lc[b].src_rate)

              lcfitfile='lc_fit_out_idl_int9.dat'
;        if not exist(lcfitfile) then lcfitfile='lc_fit_out_idl_int7.dat'
              if exist(lcfitfile) then begin 
                 if numlines(lcfitfile) gt 1 then begin
                    read_lcfit,lcfitfile,pnames,p

                    mo=fit_models(pnames,p,np,basemo=basemo)
                    mo=basemo
                    if np gt 3 then begin 
                       if p[1] gt p[3] and p[3] lt 1. then begin 
                          tb=p[2]
                          tmp=execute('ftb='+mo+'(tb,p)')
                          g[i].xrtrate=ftb
                          g[i].plateau_slope=p[3]
                          if not keyword_set(go) then begin 
                             plot_like_qdp,/withbat
                             oplot,[1e-3,1e6],[g[i].batrate_gmean,g[i].batrate_gmean],color=!magenta,line=1
                             oplot,[1e-3,1e6],[g[i].batrate_amean,g[i].batrate_amean],color=!cyan,line=1

                             oplot,[1e-3,1e6],[g[i].xrtrate,g[i].xrtrate],color=!orange,line=1
                             tmp=execute('yfit='+mo+'(lc.time,p)')
                             oplot,lc.time,yfit,color=!green,line=2
                             print,g[i].batrate_gmean,g[i].xrtrate,g[i].batrate_gmean/g[i].xrtrate
                             k=get_kbrd(10)
                             if k eq 's' then stop
                          endif 
                       endif 
                    endif 
                 endif  
              endif 
           endif 
        endif 
        cd,'..'
     endfor 
     mwrfits,g,'~/stuff_for_people/Demos/bat_xrt.fits',/create
  endelse 
  w=where(g.xrtrate ne 0 and g.batrate_gmean lt 1e8)
  g[w].ratio_amean=g[w].batrate_amean/g[w].xrtrate
  g[w].ratio_gmean=g[w].batrate_gmean/g[w].xrtrate
  g[w].ratio_peak=g[w].batrate_peak/g[w].xrtrate
  w2=where(g[w].ratio_gmean gt 1.)
  w=w[w2]
;  g=g[w]
  mwrfits,g,'~/stuff_for_people/Demos/bat_xrt.fits',/create
;
  
  begplot,name='~/stuff_for_people/Demos/bat_xrt_ratio.ps',/land,/color,font='helvetica'
  !p.charsize=2.
;  !p.multi=[0,1,2]
  plothist,alog10(g[w].ratio_gmean),bin=0.2,xtitle='log (BAT Flux/XRT Flux)',ytitle='N',yrange=[0,30],/fill,fcolor=!grey70,xrange=[0,7],/xsty
  oplot,alog10([1836.,1836.]),[0,40],line=2,color=!red,thick=5
  axis,0,xrange=[0,7],/xsty
  xyouts,3.5,27,'m!Lp!N/m!Le!N',color=!red
;  plothist,alog10(g[w].ratio_amean),bin=0.2,xtitle='log (BAT arith mean Flux/XRT Flux)',ytitle='N',yrange=[0,30]
;  oplot,alog10([1836.,1836.]),[0,40],line=2,color=!red,thick=5
;  xyouts,1.5,27,'m!Lp!N/m!Le!N',color=!red
;  !p.multi=0
  endplot

  spawn,'ps2pdf ~/stuff_for_people/Demos/bat_xrt_ratio.ps ~/stuff_for_people/Demos/bat_xrt_ratio.pdf'

  q=where(g[w].plateau_slope ge -0.5 and g[w].plateau_slope le 0.5)

;  plothist,alog10(g[w[q]].ratio_mean),bin=0.2,xtitle='log (BAT Flux/XRT Flux)',ytitle='N'
;;  plothist,alog10(g[w[q]].ratio_peak),bin=0.2,color=!orange,/over
;  oplot,alog10([1836.,1836.]),[0,30],line=2,color=!green
;  xyouts,3.5,4,'m!Lp!N/m!Le!N',color=!green

;  !p.multi=0

  stop
  
  ;; NEED TO DEMONSTRATE ARITHMATIC VERSUS GEOMETRIC (LOGARTHMIC) MEAN
  ;; FOR Mp/Me RATIO, AND MAKE CUTS ON PLATEAU <0.5 
  
  return
end 
