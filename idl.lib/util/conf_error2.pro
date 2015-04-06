@fit_functions
@fit_int_functions
pro conf_error2,x,y,yerr,p0,perror0,model,perror,bestfit,pvarunit,bestchisq,yfit,doplot=doplot
  
  if n_params() eq 0 then begin
     print,'syntax - conf_error,x,y,yerr,p0,perror0,model,perror'
     return
  endif 
  
  ;;default 90% confidence delchi=2.706
  delchi0=2.706
  ;;1-sigma
;  delchi0=1.
  
  npar=n_elements(p0)
  perror=dblarr(2,npar)
  
  com='yfit0='+model+'(x,p0)'
  tmp=execute(com)
  
  nvar=100.
;  pmin=0.9*p0
;  pmax=1.1*p0

  w0=where(perror0 eq 0.,nw0)
  if nw0 gt 0 then perror0[w0]=0.1*p0[w0]
  w0=where(p0 eq 0.,nw0)
  if nw0 gt 0 then perror0[w0]=[1.,1.]

;  mult=[4,replicate(2.,npar-1)]
;  pmin=p0-perror0*mult
;  pmax=p0+perror0*mult
  
  delchi=fltarr(nvar) & chisq=fltarr(nvar)
  bestfit=fltarr(npar) & pvarunit=bestfit & bestchisq=bestfit
  chisq0=total((y-yfit0)^2/yerr^2)
  bestfit=p0
  for i=0,npar-1 do begin
     p=p0
     mult=1.
     pmin=p0[i]-perror0[i]*mult
     pmax=p0[i]+perror0[i]*mult
     attempt=0
     delchi=fltarr(nvar)
     while max(delchi) le delchi0/2. and attempt lt 3 do begin 
        
        mult=mult*2.
        pmin=p0[i]-perror0[i]*mult
        pmax=p0[i]+perror0[i]*mult
;        if pmin gt 0 and p0[i] gt 0 then begin            
           pvarunit[i]=0.01*(pmax-pmin)
           pvar=indgen(nvar)/(nvar-1)*(pmax-pmin)+pmin

           for j=0,nvar-1 do begin 
              p[i]=pvar[j]
;        com='yfit='+model+'(x,p)'
;        tmp=execute(com)
              ;;need to fit free params
              parinfo = parinfo_struct(npar)
              parinfo[i].fixed=1
              newp=mpfitfun(model,x,y,yerr,p,parinfo=parinfo,yfit=yfit,/quiet)
              
              chisq[j]=total((y-yfit)^2/yerr^2)
              delchi[j]=chisq[j]-chisq0
           endfor 
;        endif 
        attempt=attempt+1
        if i eq 0 then attempt=5
     endwhile
;     wb=where(delchi lt delchi0)
;     d=delchi
;     minchi=min(delchi,m)
;     d[m:*]=-1.*d[m:*]
     
;     bestfit[i]=spline(pvar[wb],delchi[wb],0.0001)

;     com='yfit='+model+'(x,bestfit)'
;     tmp=execute(com)
;     bestchisq[i]=total((y-yfit)^2/yerr^2)
     
;     minfit=min(delchi,m)
;     if chisq[m] lt chisq0-0.01 and abs(chisq[m]-chisq0) lt 1. then $
;        bestfit[i]=pvar[m]
;     bestchisq[i]=chisq[m]

     wlow=where(pvar lt p0[i] and delchi lt 5,nwlow)
     wupp=where(pvar gt p0[i] and delchi lt 5,nwupp)

     if nwupp gt 1 then begin 
        mdcu=max(delchi[wupp],mu)
        if mu ne 0 then wupp=wupp[0:mu]
     endif else wupp=where(pvar ge p0[i])

     if nwlow gt 1 then begin 
        mdcl=max(delchi[wlow],m)
        if m ne nwlow-1 then wlow=wlow[m:*]
     endif else wlow=where(pvar le p0[i])
;stop
     lowerr=p0[i]-interpol(pvar[wlow],delchi[wlow],delchi0)
     upperr=interpol(pvar[wupp],delchi[wupp],delchi0)-p0[i]
;     lowerr=bestfit[i]-spline(pvar[wlow],delchi[wlow],delchi0)
;     upperr=spline(pvar[wupp],delchi[wupp],delchi0)-bestfit[i]

     if keyword_set(doplot) then begin 
        plot,pvar[[wlow,wupp]],delchi[[wlow,wupp]]
        oplot,[p0[i]-lowerr,p0[i]-lowerr],[min(delchi),max(delchi)*2.],line=2
        oplot,[p0[i]+upperr,p0[i]+upperr],[min(delchi),max(delchi)*2.],line=2
        oplot,[p0[i],p0[i]],[min(delchi),max(delchi)*2],line=0
;        oplot,[bestfit[i],bestfit[i]],[min(delchi),max(delchi)*2],line=1
        k=get_kbrd(10)   
     endif 
;     print,bestfit[i],lowerr,upperr,perror0[i]
     perror[0,i]=lowerr
     perror[1,i]=upperr

  endfor 
  
  com='yfit='+model+'(x,bestfit)'
  tmp=execute(com)
;;;;implement new loop for if better chisq is found then start over  
;  stop
  return
end 
