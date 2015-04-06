@fit_functions
pro conf_error2d,x,y,z,zerr,p0,perror0,model,perror,bestfit,pvarunit,bestchisq,zfit,psave,doplot=doplot,pmin0=pmin0,log=log,d2=d2,parinfo=parinfo,weights=weights,wpar=wpar,delchi0=delchi0
  
  if n_params() eq 0 then begin
     print,'syntax - conf_error,x,y,z,zerr,p0,perror0,model,perror'
     return
  endif 
  
  ;;default 90% confidence delchi=2.706
  if n_elements(delchi0) eq 0 then delchi0=2.706
  ;;1-sigma
;  delchi0=1.
  
  npar=n_elements(p0)
;  !p.multi=[0,round(npar/2),2]

  perror=dblarr(2,npar)
  
  com='zfit0='+model+'(x,y,p0)' 
  
  tmp=execute(com)
  
  nvar=20.
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
  doplot=1
  delchi=dblarr(nvar) & chisq=dblarr(nvar)
  bestfit=dblarr(npar) & pvarunit=bestfit & bestchisq=bestfit
  psave=dblarr(n_elements(p0),nvar)
  wno0=where(z ne 0 and zfit0 ne 0 and zerr ne 0,nwno0)
  chisq0=total((z[wno0]-zfit0[wno0])^2/zerr[wno0]^2)*n_elements(zerr)/nwno0
  bestfit=p0
  doag=0
  multlow=mult
  multup=mult     
  if n_elements(log) eq 0 then log=intarr(npar)
  ppix=intarr(35)
  ppix[[2,3,4,5,9,13,17,21]]=1
  
  once=0
  go=1
  if n_elements(wpar) eq 0 then wpar=indgen(npar) else npar=n_elements(wpar)
  for i=0,npar-1 do begin
     if n_elements(parinfo) gt 0 then begin
        if parinfo[wpar[i]].fixed eq 1 then go=0 else go=1
     endif 
     if go then begin 
        p=p0
;     pvarunit[i]=0.01*(pmax[i]-pmin[i])
        pvar=dblarr(nvar)
        pvar[nvar/2.:*]=(findgen(nvar/2.)+1.5)/(nvar/2.+1)*(pmax[wpar[i]]-p0[wpar[i]])+p0[wpar[i]]
        pvar[0:nvar/2.-1]=(findgen(nvar/2.)+1.5)/(nvar/2.+1)*(p0[wpar[i]]-pmin[wpar[i]])+pmin[wpar[i]]
        olddelchi=0
        doagain:
        nvar=n_elements(pvar)
        for j=0,nvar-1 do begin 
           if delchi[j] eq 0. then begin 
              p[wpar[i]]=pvar[j]
;           if npar eq 1 then begin 
;              if not d2 then com='yfit='+model+'(x,p)' else com='yfit='+model+'(x,x,p)'
;              tmp=execute(com)
;           endif else begin 
              ;;need to fit free params
              if n_elements(parinfo) gt 0 then nparinfo=parinfo else $
                 nparinfo = parinfo_struct(npar)
              nparinfo[wpar[i]].fixed=1
              nparinfo[wpar[i]].value=pvar[j]
              newp=mpfit2dfun(model,x,y,z,zerr,p,parinfo=nparinfo,yfit=zfit,/quiet)
;           endelse 
              wno0=where(z ne 0 and zfit ne 0 and zerr ne 0,nwno0)
              psave[*,j]=newp
              chisq[j]=total((z[wno0]-zfit[wno0])^2/zerr[wno0]^2)*n_elements(zerr)/nwno0
              delchi[j]=chisq[j]-chisq0
           endif 
           print,j

        endfor 

        wlow=where(pvar le p0[wpar[i]],nlow)
        wupp=where(pvar ge p0[wpar[i]],nupp)
        if max(delchi[wlow]) lt delchi0 then begin 
           print,'wlow delchi ',max(delchi[wlow])
           multlow[wpar[i]]=multlow[wpar[i]]*2.
           pmin[wpar[i]]=p0[wpar[i]]-perror0[wpar[i]]*multlow[wpar[i]] ;pmin[wpar[i]]
           if n_elements(pmin0) gt 0 then begin 
              if pmin[wpar[i]] le pmin0[wpar[i]] then begin 
                 pmin[wpar[i]]=pmin0[wpar[i]]
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
           multup[wpar[i]]=multup[wpar[i]]*2.
           pmax[wpar[i]]=p0[wpar[i]]+perror0[wpar[i]]*multup[wpar[i]]
;        pvar=findgen(nvar+1)/(nvar)*(pmax[wpar[i]]-pmin[wpar[i]])+pmin[wpar[i]]
;        pvar=[pvar[wlow],indgen(nupp)/(nupp-1.)*(pmax[wpar[i]]-p0[wpar[i]])+p0[wpar[i]]]
;        pvar=indgen(nvar)/(nvar-1)*(pmax[wpar[i]]*2.-pmin[wpar[i]])+pmin[wpar[i]]
;        pvar=[pvar[wlow],pvar[wupp]*2.]
           if max(delchi[wupp]) eq olddelchi then doag=0 else doag=1
           olddelchi=max(delchi[wupp])

        endif 
        if doag then begin 
;           pvar=dblarr(nvar)
;           pvar[nvar/2.+0.5:*]=(findgen(nvar/2.)+1.5)/(nvar/2.+1)*(pmax[wpar[i]]-p0[wpar[i]])+p0[wpar[i]]
;           pvar[0:nvar/2.-1]=(findgen(nvar/2.)+1.5)/(nvar/2.+1)*(p0[wpar[i]]-pmin[wpar[i]])+pmin[wpar[i]]
           nolow=0 & nohigh=0
           if parinfo[wpar[i]].limited[0] eq 1 then begin
              if pmin[wpar[i]] lt parinfo[wpar[i]].limits[0] or max(delchi[wlow]) gt delchi0 then nolow=1
           endif 
           if parinfo[wpar[i]].limited[1] eq 1 then begin
              if pmax[wpar[i]] gt parinfo[wpar[i]].limits[1] or max(delchi[wupp]) gt delchi0 then nohigh=1
           endif 
           nnew=nvar/4
           ppmin=min(pvar)-perror0[wpar[i]]*multlow[wpar[i]]
           ppmax=max(pvar)+perror0[wpar[i]]*multup[wpar[i]]
           if ppmin ne parinfo[wpar[i]].limits[0] and ppmax ne parinfo[wpar[i]].limits[1] then begin 
              if ppmin lt parinfo[wpar[i]].limits[0] then ppmin=parinfo[wpar[i]].limits[0]
              if ppmax gt parinfo[wpar[i]].limits[1] and parinfo[wpar[i]].limits[1] ne 0 then ppmax=parinfo[wpar[i]].limits[1]
              plow=(findgen(nvar/4.)+1.5)/(nvar/4.+1)*(min(pvar)-ppmin)+ppmin
              phigh=(findgen(nvar/4.)+1.5)/(nvar/4.+1)*(ppmax-max(pvar))+max(pvar)

              if parinfo[wpar[i]].limited[0] eq 1 then begin
;              winlim=where(pvar ge parinfo[wpar[i]].limits[0] and pvar le parinfo[wpar[i]].limits[1])
;              pvar=pvar[winlim]
                 woutlim=where(pvar gt parinfo[wpar[i]].limits[1] or pvar lt parinfo[wpar[i]].limits[0],nwout)
                 nps=n_elements(psave[0,*])
                 if nwout eq 0 then begin 
                    doag=0
                    if not nolow then begin
                       pvar=[plow,pvar]
                       delchi=[dblarr(nnew),delchi]
                       chisq=[dblarr(nnew),chisq]
                       pstmp=dblarr(35,nps+nnew)
                       pstmp[*,nnew:*]=psave
                       psave=pstmp
;                       psave=[dblarr(35,nnew),psave]
                    endif 
                    if not nohigh then begin
                       nps=n_elements(psave[0,*])
                       pvar=[pvar,phigh]
                       delchi=[delchi,dblarr(nnew)]
                       chisq=[chisq,dblarr(nnew)]
                       pstmp=dblarr(35,nps+nnew)
                       pstmp[*,0:nps-1]=psave
                       psave=pstmp
;                       psave=[psave,dblarr(35,nnew)]
                    endif 
;                    pvar=[plow,pvar,phigh]
;                    delchi=[dblarr(nnew),delchi,dblarr(nnew)]
;                    chisq=[dblarr(nnew),chisq,dblarr(nnew)]
;                    stop
                    if not nohigh or not nolow then goto,doagain
                 endif 
              endif else begin 
                 doag=0
                 nps=n_elements(psave[0,*])
                 pvar=[plow,pvar,phigh]
                 delchi=[dblarr(nnew),delchi,dblarr(nnew)]
                 chisq=[dblarr(nnew),chisq,dblarr(nnew)]
                 pstmp=dblarr(35,nps+nnew*2)
                 pstmp[*,nnew:nps+nnew-1]=psave
;                 pstmp[*,nnew:nps-1]=psave
                 psave=pstmp
;                 psave=[dblarr(35,nnew),psave,dblarr(35,nnew)]
                 goto,doagain
              endelse 
           endif 
           
        endif
;     polint,delchi[wlow],pvar[wlow],delchi0,intlow
;     intlow=base_interp(pvar[wlow],delchi[wlow],delchi0,order=2,minsig=100.)

;     polint,delchi[wupp],pvar[wupp],delchi0,intupp
;     intupp=base_interp(pvar[wupp],delchi[wupp],delchi0,order=2,minsig=100.)

;     intlow=interpol(pvar[wlow],delchi[wlow],delchi0)
;     intupp=interpol(pvar[wupp],delchi[wupp],delchi0)

        intlow=findval(delchi[wlow],pvar[wlow],delchi0)
        intupp=findval(delchi[wupp],pvar[wupp],delchi0)
        if n_elements(intlow) gt 1 then intlow=min(intlow)
        if n_elements(intupp) gt 1 then intupp=max(intupp)
        if intlow eq -1 then intlow=interpol(pvar[wlow],delchi[wlow],delchi0)
        if intupp eq -1 then intupp=interpol(pvar[wupp],delchi[wupp],delchi0)
        
        lowerr=p0[wpar[i]]-intlow     
        upperr=intupp-p0[wpar[i]]     
        if max(delchi[wlow]) lt delchi0 and lowerr lt 0. then lowerr=p0[wpar[i]]

        if keyword_set(doplot) then begin 
;        w=where(delchi lt 5)
           plot,pvar,delchi,xlog=log[wpar[i]],yrange=[min(delchi)-2,10]
           oplot,[p0[wpar[i]]-lowerr,p0[wpar[i]]-lowerr],[min(delchi),max(delchi)*2.],line=2
           oplot,[p0[wpar[i]]+upperr,p0[wpar[i]]+upperr],[min(delchi),max(delchi)*2.],line=2
           oplot,[p0[wpar[i]],p0[wpar[i]]],[min(delchi),max(delchi)*2],line=0
           oplot,[min(pvar),max(pvar)],[delchi0,delchi0],line=1
;        oplot,[bestfit[wpar[i]],bestfit[wpar[i]]],[min(delchi),max(delchi)*2],line=1
;        k=get_kbrd(10)   
;        if k eq 's' then stop
        endif 
;     print,bestfit[wpar[i]],lowerr,upperr,perror0[wpar[i]]
        
        ;;;interpolate other parameters to get effect on systematics
        whpar=0
        ns=n_elements(psave[0,*])
        range=fltarr(3,n_elements(psave[*,0]))
        for k=0,n_elements(psave[*,0])-1 do begin
           ps=psave[k,*]
           s=indgen(n_elements(ps))
           if ps[0] ne min(ps) then s=reverse(s)
           ps=ps[s]
           mpar=min(delchi[s],m)
           if m lt ns-1 and m gt 0 then begin 
              lowpar=where(ps[0:m-1] lt ps[m],nlow) ; or ps[0:m-1] gt ps[m],nlow)
              highpar=where(ps[m+1:*] gt ps[m],nhigh) ; or ps[m+1:*] lt ps[m],nhigh)
              if abs(nlow-m) lt 5 and abs(nhigh-m) lt 5 then begin
                 whpar=[whpar,k]
              
                 intlow=interpol(ps[0:m],delchi[s[0:m]],delchi0)
                 intupp=interpol(ps[m:*],delchi[s[m:*]],delchi0)
                 range[*,k]=[intlow,p0[k],intupp]
                 
              endif 
           endif
        endfor 
        if n_elements(whpar) gt 1 then begin 
           whpar=whpar[1:*]
           print,whpar
           r=where(range[0,*] ne 0)
;           range=range[*,r]
;           wr=where(range[0,*] lt 10 and ppix eq 1)
           wr=[4,5]
           syserrlow=sqrt(total((range[0,wr]-range[1,wr])^2))
           syserrupp=sqrt(total((range[2,wr]-range[1,wr])^2))
           w5=where(r eq 5,nw5)
           if nw5 eq 0 then begin
              syserrlow=sqrt(syserrlow^2+lowerr^2)
              syserrupp=sqrt(syserrupp^2+upperr^2)
           endif 
        endif else begin
           syserrlow=lowerr
           syserrupp=upperr
        endelse 
        print,syserrlow,syserrupp
        
;        stop
        perror[0,wpar[i]]=syserrlow;lowerr
        perror[1,wpar[i]]=syserrupp;upperr
     endif 
  endfor

  com='zfit='+model+'(x,y,bestfit)'
  tmp=execute(com)
;  !p.multi=0
;;;;implement new loop for if better chisq is found then start over  
;  stop
  return
end 
