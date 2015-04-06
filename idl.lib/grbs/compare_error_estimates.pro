pro compare_error_estimates
  
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  
  pars6=fltarr(8,ndir) & parerr6m=pars6 & parerr6p=pars6
  pars7=fltarr(8,ndir) & parerr7m=pars7 & parerr7p=pars7
  ndir=120
  pnames=['Norm','pow1','tbreak1','pow2','tbreak2','pow3','tbreak3','pow4','tbreak4','pow5']
  for i=0,ndir-1 do begin
     cd,dir[i]
     print,dir[i]
     lfile='lc_fit_out_idl_int7.dat'
     if exist(lfile) then begin 
        if numlines(lfile) gt 2 then begin 
     
           read_lcfit,'lc_fit_out_idl_int7.dat',pname6,p6,perror6
           read_lcfit,'lc_fit_out_idl_int8.dat',pname7,p7,perror7
           np=n_elements(p6)
           pars6[0:np-1,i]=p6
           parerr6m[0:np-1,i]=perror6[0,*]
           parerr6p[0:np-1,i]=perror6[1,*]
           pars7[0:np-1,i]=p7
           parerr7m[0:np-1,i]=perror7[0,*]
           parerr7p[0:np-1,i]=perror7[1,*]
     
        endif 
     endif 
     
     cd,'..'
     
  endfor 
  
;  xtitle=!tsym.delta_cap+!tsym.chi+'!U2!N err '
  ytitle='monte carlo err 10000'
  xtitle='monte carlo err 1000'

  !p.multi=[0,2,4]
  for i=0,7 do begin
     xlog=0 & ylog=0
     if i mod 2 eq 0 then begin 
        xlog=1 & ylog=1
     endif 
     w=where(parerr6m[i,*] gt 0 and parerr7m[i,*] gt 0 and finite(parerr6p[i,*]) and finite(parerr7p[i,*]))
     mm=minmax([parerr6m[i,w]+parerr6p[i,w],parerr7m[i,w]+parerr7p[i,w]])
     plot,parerr6m[i,w]+parerr6p[i,w],parerr7m[i,w]+parerr7p[i,w],psym=1,/iso,xlog=xlog,ylog=ylog,/yno,charsize=1.5,xrange=mm,yrange=mm,/xsty,/ysty,title=pnames[i],xtitle=xtitle,ytitle=ytitle
     oplot,mm,mm
     print,mm
  endfor 
  !p.multi=0
  
  stop
  
  return
end 
     
