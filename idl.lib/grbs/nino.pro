@fit_functions
@fit_functions_flares
pro nino,g

  cd,'~/stuff_for_people/Nino'
  readcol,'Xray_sample.dat',grb,z,flux11,flux24,format='(a,f,f,f)'

;  200s -> 11 hr rest frame integrated Lx for sample with z's

  w=where(z ne 999,n)
  grb=grb[w]
  z=z[w]
  s=sort(grb)
  grb=grb[s]
  z=z[s]
     ;;;; using 1 sigma errors
  s1=round(1000.*(1-0.67)/2.)
  s2=round(1000.*(1.-(1-0.67)/2.))

  g=create_struct('grb','','z',0.,'lctstart',0d,'lctstop',0d,$                  
                  'model','','p',fltarr(30),'perror',fltarr(2,30),'gamma',0.,$
                  't200',0d,'t11',0d,$
                  'f11',0d,'lumint',0d,'luminterr',dblarr(2))
  g=replicate(g,n)
  g.grb=grb
  g.z=z

  cd,'~/GRBs'
  for i=0,n-1 do begin
     if exist(grb[i]) then begin 
        cd,grb[i]
        if exist('WTCURVE.qdp') or exist('PCCURVE.qdp') then begin 
              
           lcfitfile='lc_fit_out_idl_int8.dat'
           lc=lcout2fits(/phil)
           g[i].lctstart=lc[0].tstart
           g[i].lctstop=max(lc.tstop)
           int='8'
           if not exist(lcfitfile) then begin 
              lcfitfile='lc_fit_out_idl_int7.dat'
              int='7'
           endif 
           read_lcfit,lcfitfile,pnames,p,perror
           g[i].p[0:n_elements(p)-1]=p
           g[i].perror[0,0:n_elements(p)-1]=perror[0,*]
           g[i].perror[1,0:n_elements(p)-1]=perror[1,*]

           spec=mrdfits('UL_specfits.fits',1)
           unabs_cfratio=spec[n_elements(spec)-1].unabs_cfratio
           g[i].gamma=spec[n_elements(spec)-1].phind
           mo=fit_models(pnames,p)
           g[i].model=mo

           if mo ne 'nofit' then begin 
              g[i].t200=200./(1.+g[i].z)
              g[i].t11=11.*3600/(1.+g[i].z)
              g[i].f11=call_function(mo,g[i].t11,p)*unabs_cfratio

              f=qpint1d(strtrim(mo,2),g[i].t200,g[i].t11,p)*unabs_cfratio

              mcfit0=mrdfits('lc_fit_out_idl_int'+int+'_mc.fits',1)
              f0=fltarr(1000)
              for j=0,999 do begin
                 p00=0d
                 for k=0,n_tags(mcfit0)-1 do p00=[p00,mcfit0[j].(k)]
                 p00=p00[1:*]
                 f0[j]=qpint1d(strtrim(mo,2),g[i].t200,g[i].t11,p00)*unabs_cfratio
              endfor 

              s=sort(f0)
              ferr0=f0[s[500]]-f0[s[s1]]
              ferr1=f0[s[s2]]-f0[s[500]]
              
              mpc2cm=3.08568025d24 
              dist=lumdist(g[i].z)*mpc2cm
              lfact=4.*!pi*dist^2*(1.+g[i].z)^(spec[n_elements(spec)-1].phind-1.-1.)
              g[i].lumint=f*lfact
              g[i].luminterr[0]=ferr0*lfact
              g[i].luminterr[1]=ferr1*lfact
           endif 
        endif  
        cd,'~/GRBs'
     endif 
  endfor 

  w=where(g.lumint ne 0 and g.luminterr[0,*] gt 0 and g.luminterr[1,*] gt 0)
  g=g[w]
  mwrfits,g,'~/stuff_for_people/Nino/Xray_sample_lumint.fits',/create

  return
end 
