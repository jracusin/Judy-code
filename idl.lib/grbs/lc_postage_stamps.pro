pro lc_postage_stamps

  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
;  ndir=30
  !y.margin=[3,2]
  !x.margin=[10,3]
  
;  ytitle='Flux (0.3-10.0 keV) (erg cm!U-2!N s!U-1!N)'
;  yrange=[1e-14,1e-8]
  ytitle='Count rate (0.3-10.0 keV) (s!U-1!N)'
;  yrange=[1e-4,100]
  
;  plot,[10,1e7],yrange,/nodata,
  xtitle='Time since BAT trigger (s)'
  
  begplot,name='lc_postage_stamps.ps',/color,/land
  lcfile='lc_fit_out_idl.dat'
  
  file='lc_newout.txt'

  !p.multi=[0,8,7]
  for i=0,ndir-1 do begin
     
     cd,dir[i]
     if exist(file) and exist(lcfile) then begin 
        if numlines(lcfile) gt 1 then begin
           
           lc=lcout2fits(/silent)
           
           read_lcfit,lcfile,pname,p,perror,chisq,dof,breaks
           
           time=lc.time
           tstarted=lc.tstart
           tstoped=lc.tstop
           cts=lc.src_rate
           err=lc.src_rate_err
           notul=where(err gt 0,nul)
           if nul gt 0 then begin 
              yrange=[min(cts[notul]-err[notul]),max(cts[notul]+err[notul])]
              if yrange[0] lt 1e-4 then yrange[0]=1e-4

              ploterror,time[notul],cts[notul],err[notul],psym=3,/nohat,xtitle=xtitle,ytitle=ytitle,/xlog,/ylog,charsize=1.,yrange=yrange,title=dir[i];,ytickformat='(e10.0)'

              for j=0,nul-1 do oplot,[tstarted[notul[j]],tstoped[notul[j]]],[cts[notul[j]],cts[notul[j]]]
           endif 

           oplot_lcfit_results,lc,p,perror,chisq,dof,breaks,leg,pname,charsize=charsize,/noleg,noerr=noerr,name=dir[i]

;           if breaks gt 0 then begin 
;              br=p[indgen(breaks)*2+2]
;              for b=0,breaks-1 do oplot,[br[b],br[b]],[1e-6,100],line=2,color=!yellow
;           endif 
              
;           fit_lc,/justplot,title=dir[i],charsize=1.5,lc=rlc,/noleg
           
        endif 
     endif 
     cd,'..'
  endfor 

  endplot
  
  !p.multi=0  
  !y.margin=[4,2]
  !x.margin=[10,4]
  
  return
end 
