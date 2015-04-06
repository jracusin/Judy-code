pro canon_crap

  cd,'~/GRBs/'

  grbs=file_search('GRB*/')

  ngrbs=n_elements(grbs)
  g=create_struct('alpha',0.,'alpha_err',fltarr(2),$
                  'beta',0.,'beta_err',fltarr(2),$
                  'tb',0d,'p',fltarr(10),'perr',fltarr(2,10))
  g=replicate(g,ngrbs)
  for i=0,ngrbs-1 do begin
     cd,grbs[i]
     lcfitfile='lc_fit_out_idl_int8.dat'
     if not exist(lcfitfile) then lcfitfile='lc_fit_out_idl_int7.dat'
     if exist(lcfitfile) then begin 
        read_lcfit,lcfitfile,pnames,p,perr,np=np
        lc=lcout2fits(/phil)
        nlc=n_elements(lc)
        det=where(lc.src_rate_err ne 0,ndet)
        if ndet gt 3 and max(lc[det].tstop) gt 86400. then begin 
           maxt=max(lc[det].time)
           if pnames[0] ne 'no fit' and maxt gt 5e4 then begin 
              
              g[i].p[0:np-1]=p[0:np-1]
              g[i].alpha=p[np-1]
              g[i].alpha_err=perr[*,np-1]
              if np gt 2 then g[i].tb=p[np-2]
              if exist('UL_specfits.fits') then begin 
                 spec=mrdfits('UL_specfits.fits',1)
                 nspec=n_elements(spec)
                 if spec[nspec-1].mode eq 'PC' then begin
                    g[i].beta=spec[nspec-1].phind-1
                    g[i].beta_err=spec[nspec-1].phinderr-1.
                 endif 
              endif 
           endif 
        endif 
     endif 
     cd,'..'
  endfor 
  
  w=where(g.alpha ne 0. and g.beta ne 0 and g.alpha_err[0] lt 0.25 and g.beta_err[0] lt 0.25)
  x=g[w].alpha-g[w].beta

  begplot,name='~/stuff_for_people/canon_reproduce.ps',/color
  !p.multi=[0,1,3]
  !p.charsize=2
  plothist,g[w].alpha,bin=0.1,xtitle=!tsym.alpha+'!Lx, final segment!N',ytitle='N',xrange=[0,5]
  plothist,g[w].beta+1.,bin=0.1,xtitle=!tsym.gamma_cap+'!Lx!N='+!tsym.beta+'!Lx, PC!N+1',ytitle='N',xrange=[0,4]
;  plothist,tb[w]

  plothist,x,bin=0.1,xtitle=!tsym.alpha+'!Lx!N-'+!tsym.beta+'!Lx!N',ytitle='N',xrange=[-2,3]
  oplot,[0,0],[0,50],color=!red
  oplot,[0.5,0.5],[0,50],color=!red
  oplot,[1,1],[0,50],color=!red
  !p.multi=0
  endplot
  stop
  return
end 
