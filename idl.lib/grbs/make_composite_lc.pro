pro make_composite_lc,useflares=useflares,onlyz=onlyz,flux=flux
  
  cd,!mdata
  dir=file_search('GRB*')
  
  if keyword_set(flux) then begin 
     ytitle='Flux (0.3-10.0 keV) (erg cm!U-2!N s!U-1!N)' 
     yrange=[1e-14,1e-8]
  endif else begin
     ytitle='Count Rate (0.3-10.0 keV) (s!U-1!N)'
     yrange=[1e-4,1e4]
  endelse 
  plot,[10,1e7],yrange,/nodata,xtitle='Time since BAT trigger (s)',ytitle=ytitle,/xlog,/ylog,/ysty,/xsty
;  lcfile='lc_fit_out_idl.dat'
;  readcol,'/bulk/shadow/racusin/grbs/grb_ztable.csv',grb,tid,z,format='(a,l,f)',delim=',',/silent
  lcfile='lc_fit_out_idl_int9.dat'
  grbstr=mrdfits(!mdata+'grb_info_z_epeak.fits',1)
  grb=grbstr.grb
  z=grbstr.z
  
  for i=0,n_elements(dir)-1 do begin
     
     nwz=1
     z1=1.
     if keyword_set(onlyz) then begin 
        wz=where('GRB'+grb eq dir[i],nwz) 
        if nwz gt 0 then z1=1.+z[wz] 
     endif
     if nwz gt 0 then begin 
        cd,dir[i]
        if not keyword_set(useflares) then begin 
           file='lc_newout_noflares.txt'
           if not exist(file) then file='lc_newout.txt'
        endif else file='lc_newout.txt'
        if exist(file) and exist(lcfile) then begin 
           if numlines(lcfile) gt 1 then begin
              lc=lcout2fits(file,/silent)
              read_lcfit,lcfile,pname,p,perror,chisq,dof,breaks
              if keyword_set(flux) then begin
                 if exist('spec/seg*2s*dat') then read_specfit,spec,dir='spec'
                 nspec=n_elements(spec)
              endif 
              time=lc.time
              tstarted=lc.tstart
              tstoped=lc.tstop
              cts=lc.src_rate
              err=lc.src_rate_err
              type=lc.type
              notul=where(err gt 0,nul)
              if breaks gt 0 then br=[0,p[indgen(breaks)*2+2],1e8] else br=[0,1e8]

              for b=0,breaks do begin
                 w=where(time[notul] gt br[b] and time lt br[b+1],nw)
                 w=notul[w]
                 
                 if keyword_set(flux) then begin 
                    if nspec gt 0 then begin 
                       if nspec eq breaks+1 then flratio=spec[b].unabs_flux/spec[b].rate else begin
                          if nspec lt b+1 then flratio=spec[b-1].unabs_flux/spec[b].rate
                       endelse 
                    endif else flratio=4e-11
                 endif else flratio=1.
                 if nw gt 0 and finite(flratio)then begin 
                    oploterror,time[w]/z1[0],cts[w]*flratio,err[w]*flratio,psym=3,/nohat
                    for j=0,nw-1 do oplot,[tstarted[w[j]],tstoped[w[j]]]/z1[0],[cts[w[j]],cts[w[j]]]*flratio
                 endif 
              endfor 
           
;           stop
;           k=get_kbrd()
;           if k eq 's' then stop
           endif  
        endif 
        cd,'..'
     endif   
  endfor

  stop
  return
end 

;;;add flux normalization
;;;add z option to scale by 1+z
