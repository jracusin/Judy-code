pro click_flares,lc,fpeak,fhi,xrange=xrange
  erase
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'
  plot_like_qdp,lc=lc,_extra=_extra,flux=flux,xrange=xrange
  fpeak=0d
  fhi=0d
  !mouse.button=0
  print,'Click on flare peak'
  print,'->Right click to exit'
  while (!MOUSE.button NE 4) DO BEGIN 

     cursor,xx,yy,/wait,/change
     if !MOUSE.button NE 4 then begin      
        fpeak=[fpeak,xx]
        fhi=[fhi,yy]
        oplot,[xx,xx],[1e-20,1e20],line=2
     endif 
  endwhile
  if n_elements(fpeak) gt 1 then begin 
     fpeak=fpeak[1:*]
     fhi=fhi[1:*]
  endif 

return
end 

pro fit_flares2,lc=lc,fpar=fpar,moadd=moadd,fpnames=fpnames,fpmin=fpmin,fplog=fplog,xrange=xrange

  click_flares,lc,fpeak,fhi,xrange=xrange
  nf=n_elements(fpeak)
print,nf
  rd=rem_dup(fpeak)
  if n_elements(rd) lt nf then begin
     fpeak=fpeak[rd]
     fhi=fhi[rd]
     nf=n_elements(rd)
  endif 
print,nf
  print,'Fitting '+ntostr(nf)+' flares'
  if fpeak[0] eq 0 then nf=0
  pfact=[10.,1.,0.1]
  pminfact=[1e-2,0.05,1e-2]
  case nf of
     0: begin
        print,'You did not pick any flares'
        moadd=''
        fpar=0d
        fpmin=0d
        fpnames=''
        return
     end
     1: moadd='gauss1_'
     2: moadd='gauss2_'
     3: moadd='gauss3_'
     4: moadd='gauss4_'
     5: moadd='gauss5_'
     6: moadd='gauss6_'
     7: moadd='gauss7_'

  endcase

  fpar=0d
  fpmin=0d
  fpnames=''
  for j=0,nf-1 do begin 
     fpnames=[fpnames,['gnorm','gcenter','gwidth']+ntostr(j+1)]
     fpar=[fpar,fhi[j]*pfact[0],fpeak[j]*pfact[1],fpeak[j]*pfact[2]]
     fpmin=[fpmin,fhi[j]*pminfact[0],fpeak[j]*pminfact[1],fpeak[j]*pminfact[2]]
  endfor 
  fpar=fpar[1:*]
  fpmin=fpmin[1:*]
  fpnames=fpnames[1:*]

  fplog=replicate(1.,nf)

;;10xnorm=height,peak time, del t/t=0.1

  return
end 
