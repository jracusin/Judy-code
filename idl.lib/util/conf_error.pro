@fit_functions
pro conf_error,x,y,yerr,p0,perror0,model,perror,bestfit,pvarunit,bestchisq,yfit,doplot=doplot,pmin0=pmin0,log=log,fixed=fixed,par=par,delchi0=delchi0
  
  if n_params() eq 0 then begin
     print,'syntax - conf_error,x,y,yerr,p0,perror0,model,perror'
     return
  endif 
  
  ;;default 90% confidence delchi=2.706
  if n_elements(delchi0) eq 0 then delchi0=2.706
  ;;1-sigma
;  delchi0=1.
  
  npar=n_elements(p0)
;  if n_elements(fixed) gt 0 then par=where(fixed eq 0) else par=indgen(npar)
  if n_elements(par) eq 0 then par=indgen(npar)
  if keyword_set(doplot) then !p.multi=[0,round(npar/2),2]

  perror=dblarr(2,npar)
  
  com='yfit0='+model+'(x,p0)'
  tmp=execute(com)
  
  nvar=100.
;  pmin=0.9*p0
;  pmax=1.1*p0

  w0=where(perror0 eq 0.,nw0)
  if nw0 gt 0 then perror0[w0]=0.1*p0[w0]
  w0=where(p0 eq 0.,nw0)
  if nw0 gt 0 then perror0[w0]=1.

  mult=[replicate(2.,npar)]
  pmin=p0-perror0*mult
  if n_elements(pmin0) gt 0 then begin
     wmin=where(pmin-pmin0 lt 0.,nwmin)
     if nwmin gt 0 then pmin[wmin]=pmin0[wmin]
  endif 
  pmax=p0+perror0*mult
;doplot=1
  delchi=fltarr(nvar) & chisq=fltarr(nvar)
  bestfit=fltarr(npar) & pvarunit=bestfit & bestchisq=bestfit
  chisq0=total((y-yfit0)^2/yerr^2)
  bestfit=p0
  doag=0
  multlow=mult
  multup=mult     
  if n_elements(log) eq 0 then log=intarr(npar)
  once=0
  for i=0,npar-1 do begin
     winpar=where(par eq i,nwin)
     if nwin eq 1 then begin 
        
        p=p0
;     pvarunit[i]=0.01*(pmax[i]-pmin[i])
        pvar=fltarr(nvar)
        pvar[nvar/2.:*]=(findgen(nvar/2.)+1.5)/(nvar/2.+1)*(pmax[i]-p0[i])+p0[i]
        pvar[0:nvar/2.-1]=(findgen(nvar/2.)+1.5)/(nvar/2.+1)*(p0[i]-pmin[i])+pmin[i]
        olddelchi=0
        doagain:
        for j=0,nvar-1 do begin 
           p[i]=pvar[j]
           if npar eq 1 then begin 
              com='yfit='+model+'(x,p)'
              tmp=execute(com)
           endif else begin 
              ;;need to fit free params
              parinfo = parinfo_struct(npar)
              if n_elements(fixed) gt 0 then parinfo[fixed].fixed=1
              parinfo[i].fixed=1
              newp=mpfitfun(model,x,y,yerr,p,parinfo=parinfo,yfit=yfit,/quiet)
           endelse 
           chisq[j]=total((y-yfit)^2/yerr^2)
           delchi[j]=chisq[j]-chisq0

        endfor 

        wlow=where(pvar le p0[i],nlow)
        wupp=where(pvar ge p0[i],nupp)
        if max(delchi[wlow]) lt delchi0 then begin 
           print,'wlow delchi ',max(delchi[wlow])
           multlow[i]=multlow[i]*2.
           pmin[i]=p0[i]-perror0[i]*multlow[i] ;pmin[i]
           if n_elements(pmin0) gt 0 then begin 
              if pmin[i] le pmin0[i] then begin 
                 pmin[i]=pmin0[i]
                 if not once then begin 
                    once=1 
                    doag=1
                 endif else begin
                    doag=0
                    once=0
                 endelse 
              endif else doag=1
           endif else doag=1
        endif 

        if max(delchi[wupp]) lt delchi0 then begin
           print,'wupp delchi ',max(delchi[wupp])
           multup[i]=multup[i]*2.
           pmax[i]=p0[i]+perror0[i]*multup[i]
;        pvar=findgen(nvar+1)/(nvar)*(pmax[i]-pmin[i])+pmin[i]
;        pvar=[pvar[wlow],indgen(nupp)/(nupp-1.)*(pmax[i]-p0[i])+p0[i]]
;        pvar=indgen(nvar)/(nvar-1)*(pmax[i]*2.-pmin[i])+pmin[i]
;        pvar=[pvar[wlow],pvar[wupp]*2.]
           if max(delchi[wupp]) eq olddelchi then doag=0 else doag=1
           olddelchi=max(delchi[wupp])

        endif 
        if doag then begin 
           pvar=fltarr(nvar)
           pvar[nvar/2.:*]=(findgen(nvar/2.)+1.5)/(nvar/2.+1)*(pmax[i]-p0[i])+p0[i]
           pvar[0:nvar/2.-1]=(findgen(nvar/2.)+1.5)/(nvar/2.+1)*(p0[i]-pmin[i])+pmin[i]
           doag=0
           goto,doagain
        endif
;     polint,delchi[wlow],pvar[wlow],delchi0,intlow
;     intlow=base_interp(pvar[wlow],delchi[wlow],delchi0,order=2,minsig=100.)

;     polint,delchi[wupp],pvar[wupp],delchi0,intupp
;     intupp=base_interp(pvar[wupp],delchi[wupp],delchi0,order=2,minsig=100.)

;     intlow=interpol(pvar[wlow],delchi[wlow],delchi0)
;     intupp=interpol(pvar[wupp],delchi[wupp],delchi0)
        fin=where(finite(delchi) eq 0,nfin)
        if nfin gt 0 then delchi[fin]=0.
        if nlow gt 0 and nupp gt 0 then begin 
           intlow=findval(delchi[wlow],pvar[wlow],delchi0)
           intupp=findval(delchi[wupp],pvar[wupp],delchi0)
           if n_elements(intlow) gt 1 then intlow=min(intlow)
           if n_elements(intupp) gt 1 then intupp=max(intupp)
           if intlow eq -1 then intlow=interpol(pvar[wlow],delchi[wlow],delchi0)
           if intupp eq -1 then intupp=interpol(pvar[wupp],delchi[wupp],delchi0)
           
           lowerr=p0[i]-intlow     
           upperr=intupp-p0[i]     
           if max(delchi[wlow]) lt delchi0 and lowerr lt 0. then lowerr=p0[i]
           
        endif 
        if keyword_set(doplot) then begin 
;        w=where(delchi lt 5)
           plot,pvar,delchi,xlog=log[i],yrange=[min(delchi)-2,10]
           oplot,[p0[i]-lowerr,p0[i]-lowerr],[min(delchi),max(delchi)*2.],line=2
           oplot,[p0[i]+upperr,p0[i]+upperr],[min(delchi),max(delchi)*2.],line=2
           oplot,[p0[i],p0[i]],[min(delchi),max(delchi)*2],line=0
           oplot,[min(pvar),max(pvar)],[delchi0,delchi0],line=1
;        oplot,[bestfit[i],bestfit[i]],[min(delchi),max(delchi)*2],line=1
;        k=get_kbrd(10)   
;        if k eq 's' then stop
        endif 
;     print,bestfit[i],lowerr,upperr,perror0[i]
        perror[0,i]=lowerr
        perror[1,i]=upperr
        print,i,p0[i],lowerr,upperr[0]
     endif 
  endfor 

  com='yfit='+model+'(x,bestfit)'
  tmp=execute(com)
  if keyword_set(doplot) then !p.multi=0
;;;;implement new loop for if better chisq is found then start over  
;  stop
  return
end 
