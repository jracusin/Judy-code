pro fit_expan,ages,r0,r0err,color,_extra=_extra,input=input
  
  sym=!vsym
  g0=[0,1]
  g1=[2,3]
  g2=[0,1,2,3]

  a=[-0.1d,2.5e-4,6000,2e-5]
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                       limits:[0.D,0],$
                       parname:''}, n_elements(a))
  
  parinfo[2].limited=[1,1]
  parinfo[2].limits=[min(ages),max(ages)]
  g3=[indgen(10),indgen(7)+11]
  f=mpfitfun('bknlin',ages[g3],r0[g3],r0err[g3],a,yfit=yfit,parinfo=parinfo,dof=dof,perror=sigma)
  t=findgen(500)/500.*(max(ages)-min(ages))+min(ages)
  waf=where(t ge f[2]-50)
  wbe=where(t le f[2])
  oplot,t[waf],bknlin(t[waf],f),color=color
  oplot,t[wbe],bknlin(t[wbe],f),color=color
;  print,f
  d=1.54285e18                  ;km to LMC
  expr2=d*(f[[1,3]]*!dtor/3600.)/86400D 
  chisq2=total((r0[g3]-yfit)^2./(r0err[g3])^2)
  wbe=where(ages[g3] lt f[2],nwbe)
  waf=where(ages[g3] ge f[2],nwaf)
  dofa=nwbe-2
  dofb=nwaf-2
  chisq2a=total((r0[g3[wbe]]-yfit[wbe])^2/(r0err[g3[wbe]])^2)
  chisq2b=total((r0[g3[waf]]-yfit[waf])^2/(r0err[g3[waf]])^2)

  conf_error,ages[g3],r0[g3],r0err[g3],f,sigma,'bknlin',perror ;,/doplot
  exprerr2=d*(perror[*,[1,3]]*!dtor/3600.)/86400D 
  
;  a=[1,1e-5]
;  f1=mpfitfun('linfunc',ages[g3],r0[g3],r0err[g3]/1.645,a,yfit=yfit,dof=dof3,perror=sigma)
;  oplot,t,linfunc(t,f1),color=color,line=2
;  expr3=d*(f1[1]*!dtor/3600.)/86400D 
;  chisq3=total((r0[g3]-yfit)^2./(r0err[g3]/1.645)^2)
;  conf_error,ages[g3],r0[g3],r0err[g3]/1.645,f1,sigma,'linfunc',perror1 ;,/doplot
;  exprerr3=d*(perror1[*,1]*!dtor/3600.)/86400D 
  
;  expr=[expr2[0],0,expr2[1],0,expr3]
  expr=[expr2[0],expr2[1]];,expr3]
;  exprerr=[exprerr2[0,0],0,exprerr2[0,1],0,exprerr3[0,0]]
  exprerr=[min(exprerr2[*,0]),min(exprerr2[*,1])];,exprerr3[0,0]]
  dofc=[dof,0,0,0];,dof3]
  chisq=[chisq2,0,0,0];,chisq3]
  cdf=chisq/dofc
  cdfs=sym.chi+'!U2!N/dof = '+numdec(cdf,2)+' (dof='+[ntostr(fix(dofc))]+')]'
  cdfs[1:3]='                                        '
  
  abc=['a','','b','','c']       ;,'','c','','d','','d','','e']
;  abc=''
  txt=['v!L'+abc+'!N = '+[ntostr(round(expr))]+' '+sym.plusminus+' '+[ntostr(round(exprerr))]+' km s!U-1!N   ']+cdfs
  txt[[1,3]]=''
  
  ind=[0,2,4]
  
  txt='v!L'+abc
  txt[[1,3]]=''
;        legend,[txt],box=0,/bottom,/right,charsize=1.,line=[0,-1,0,-1,2],color=[!blue,0,!red,0,!green]
  print,f,perror
;  print,f1,perror1
  print
  print,expr2,exprerr2
;  print,expr3,exprerr3
  center=0 & right=0
  if keyword_set(input) then center=1 else right=1
  legend,[ntostr(round(expr[[0,1]]))+sym.plusminus+ntostr(round(exprerr[[0,1]]))],_extra=_extra,right=right,center=center,box=0,textcolor=[color,color],charsize=1.

  return
end 

pro sn1987a_sim_results,append=append,ps=ps
  
  dir='/Volumes/Firewire1/racusin/sn1987a/simulations/'
  simmodels=['0deg_torus','45deg_torus','45deg_torus_4spot']
  if n_elements(append) eq 0 then append=''
  mymodels=['ring','ptsrc','bilat','lobes']+append
  cd,dir
  
  ages=[4600,5200,6200,7600]
  counts=[890,6000,12800,12900.]
  scale=0.01475                 ;*25.
  if keyword_set(ps) then begplot,name='sn1987a_sim_results'+append+'.ps',/color else erase
  jcol=[!magenta,!cyan,!orange]
  multiplot2,[1,4],/init
  plotsym,0,/fill
  for i=0,3 do begin  ;;; loop over each of my models
     title2=mymodels[i]
     if i eq 3 then xtitle='Time since SN (days)' else xtitle=''
     multiplot2
     if i eq 0 then title='SNR 1987A Expansion Measure'
     plot,[4000,8000],[0,1.0],/nodata,xtitle=xtitle,ytitle='r!L0!N (arcsec)',_extra=_extra,yrange=[0.4,0.8],charsize=1.5
     legend,title2,box=0,/top,/left
     if i eq 0 then begin
        legend,['input','0 deg','45 deg','45 deg + spots'],/bottom,/left,box=0,textcolor=[!p.color,jcol],charsize=1.
     endif 
     r=[0.6,0.67,0.74,0.77]
     rerr=r*0.01
     oplot,ages,r,psym=2
     fit_expan,ages,r,rerr,!p.color,/input
     for j=0,2 do begin   ;;;loop over each of Svet's models
        mdir=simmodels[j]+'/'+mymodels[i]+'/'
        fparfile=mdir+'new_sn1987a_fpars.fits'
        if exist(fparfile) then begin 
           fpars=mrdfits(fparfile,1,/silent)       
                                ;       print,fpars.sigr,fpars.sigrerr
           
           r0=fpars.r0*scale
           ctswe=dblarr(2,n_elements(counts))
           ctswe[0,*]=sqrt(counts)/counts*fpars.r0
           ctswe[1,*]=ctswe[0,*]
           r0err=sqrt(fpars.r0err^2+ctswe^2)
           r0err=r0err*scale
           ind=indgen(4)
           r0=r0[ind]           ;^0.75
           g=where(r0 gt 0)
           r0err=r0err[*,ind]
           rerr=r0err
           rerr=dblarr(n_elements(ind))
           for k=0,n_elements(ind)-1 do rerr[i]=min(r0err[*,k])
           oplot,ages,r0,psym=8,color=jcol[j]
           for k=0,3 do oplot,[ages[k],ages[k]],[r0[k]-r0err[0,k],r0[k]+r0err[1,k]],color=jcol[j]
           
;        print,r0,r0err
           
           if j eq 0 then fit_expan,ages,r0,r0err,jcol[j],/top
           if j eq 1 then fit_expan,ages,r0,r0err,jcol[j],/center
           if j eq 2 then fit_expan,ages,r0,r0err,jcol[j],/bottom

;        f0=linfit(ages[g0],r0[g0],measure_errors=rerr[g0]/1.645,yfit=yfit,sigma=sigma0,chisq=chisq0)
;        chisq0=total((r0[g0]-yfit)^2./r0err[g0]^2)  
;        f1=linfit(ages[g1],r0[g1],measure_errors=rerr[g1]/1.645,yfit=yfit,sigma=sigma1,chisq=chisq1)
;        chisq1=total((r0[g1]-yfit)^2./r0err[g1]^2)  
;        d=1.54285e18            ;km to LMC
;        f2=linfit(ages[g2],r0[g2],measure=rerr[g2]/1.645,yfit=yfit,sigma=sigma2,chisq=chisq2)
;        chisq2=total((r0[g2]-yfit)^2./r0err[g2]^2)  
;        b=[f0[1],0,f1[1],0,f2[1]] 
;        expr=d*(b*!dtor/3600.)/86400D
;        exprerr=d*([sigma0[1],0,sigma1[1],0,sigma2[1]]*1.645*!dtor/3600.)/86400D
;        print,expr,exprerr
;        dofc=[(n_elements(g0)-2),0,(n_elements(g1)-2),0,(n_elements(g2)-2)]*1d
;        ticpt=(f0[0]-f1[0])/(f1[1]-f0[1])
;        icpt=f1[0]+f1[1]*ticpt
;        p0=sqrt(chisq0/(total(dofc[g0])-n_elements(g0)))
;        p1=sqrt(chisq1/(total(dofc[g1])-n_elements(g1)))
;        p2=sqrt(chisq2/(total(dofc[g])-n_elements(g2)))
;        a=f0[0]*p0 & b=f0[1]*p0 & c=f1[0]*p1 & d=f1[1]*p1
;        siga=sigma0[0]*p0 & sigb=sigma0[1]*p0 & sigc=sigma1[0]*p1 & sigd=sigma1[1]*p1
;        ticpterr=sqrt((1/(d-b))^2*siga^2+(2*(a-c)/(d-b)^2)^2*sigb^2+(-1/(d-b))^2*sigc^2+(-2*(a-c)/(d-b)^2)^2*sigd^2)*1.645
;        print,icpt,ticpt,ticpterr
           
           
;        goto,skip
           
;        skip:
        endif 
     endfor 
  endfor
  multiplot2,/reset,/default
  if keyword_set(ps) then endplot
  stop  
  return
end 
