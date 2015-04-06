pro new_sn1987a_results,exclude=exclude,show=show,ps=ps,_extra=_extra,title2=title2,ring=ring,ptsrc=ptsrc,lobes=lobes,skip=skip,inclination=inclination,bilat=bilat,append=append,all=all,noleg=noleg,hardsoft=hardsoft,noave=noave
  
  if not keyword_set(ring) and not keyword_set(ptsrc) and not keyword_set(lobes) and not keyword_set(bilat) and not keyword_set(all) then begin
     print,'Must set ring, bilat, ptsrc, lobes, or all'
     return
  endif 
  
  if not keyword_set(all) then begin 
     if keyword_set(ring) then begin
        dof0=6
        model='ring'
     endif 
     if keyword_set(ptsrc) then begin
        dof0=18
        model='ptsrc'
     endif 
     if keyword_set(lobes) then begin
        dof0=18
        model='lobes'
     endif 
     if keyword_set(bilat) then begin
        dof0=16
        model='bilat'
     endif 
     nm=1
  endif else begin
     multiplot2,[1,4],/init
     model=['ring','bilat','ptsrc','lobes']
     dof0=[6,18,18,16]
     nm=4
  endelse   
  
  if n_elements(append) eq 0 then append=''
  if keyword_set(ps) then begplot,name='~/SNR87A/new_sn1987a_expansion_results'+append+'.eps',font='helvetica',/encap,/color,/land else erase
  !x.margin=[13,3]
  !y.margin=[4,2]
  if append ne '_free' then dof0=dof0-1
  
  for m=0,nm-1 do begin 
     mdir=model[m]+append+'/'
     add0='_'+model[m]+append
     
;     title2=' - '+model[m]
     if nm eq 1 or m eq 3 then xtitle='Time since SN (days)' else xtitle=''
     if nm eq 1 or m eq 2 then ytitle='r!L0!N (arcsec)' else ytitle=''
     if m eq 0 then title='SN1987a expansion measure' else title='' ;+title2
     if keyword_set(ps) then title=''
;  if n_elements(title2) eq 0 then title2=''
     if not keyword_set(exclude) then exclude=0
     if not keyword_set(hardsoft) then begin 
        n=ntostr([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21])
        n=[n,'22a','22b','23a','23b','23c','24']
      
      ;  dir=!data+'sn1987a/image_analysis/'
        dir='~/SNR87A/image_analysis/'
;     dir='/Volumes/Firewire1/racusin/sn1987a/image_analysis/'
        cd,dir
        files=dir+['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a11_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits','sn1987a16_300-8000_cr.fits','sn1987a17_300-8000_cr.fits','sn1987a19_300-8000_cr.fits','sn1987a20A_300-8000_cr.fits','sn1987a21A_300-8000_cr.fits','sn1987a22a_300-8000_cr.fits','sn1987a22b_300-8000_cr.fits','sn1987a23a_300-8000_cr.fits','sn1987a23b_300-8000_cr.fits','sn1987a23c_300-8000_cr.fits','sn1987a24_300-8000_cr.fits']
;     files=dir+['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a11_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits','sn1987a16_300-8000_cr.fits','sn1987a17_300-8000_cr.fits','sn1987a19_300-8000_cr.fits','sn1987a20A_300-8000_cr.fits','sn1987a20A_300-8000_cr_psu.fits','sn1987a20H_300-8000_cr.fits','sn1987a20H_300-8000_cr_psu.fits','sn1987a21A_300-8000_cr.fits','sn1987a21A_300-8000_cr_psu.fits','sn1987a21H_300-8000_cr.fits','sn1987a21H_300-8000_cr_psu.fits']
     endif else begin 
        n=['16m','16h','16s','17m','17h','17s','19m','19h','19s','20m','20h','20s']
        dir='/Volumes/Firewire1/racusin/sn1987a/hardsoft_images/'
        cd,dir
        files=dir+['sn1987a16_1500-8000.fits','sn1987a16_2000-8000.fits','sn1987a16_300-800.fits','sn1987a17_1500-8000.fits','sn1987a17_2000-8000.fits','sn1987a17_300-800.fits','sn1987a19_1500-8000.fits','sn1987a19_2000-8000.fits','sn1987a19_300-800.fits','sn1987a20A_1500-8000_psu.fits','sn1987a20A_2000-8000_psu.fits','sn1987a20A_300-800_psu.fits']
     endelse 

     nn=n_elements(files)
     fpars=mrdfits(mdir+'new_sn1987a_fpars.fits',1,/silent)
     dof1=lonarr(nn)
     if keyword_set(show) then begin 
        !p.multi=[0,2,2]
        i=0
                                ;    for i=0,nn-1 do begin
        while i lt nn do begin 
           im=mrdfits(files[i],/silent)
           ofile=mdir+'new_sn1987a'+ntostr(n[i])+'_fitim.fits'
           newim=mrdfits(ofile,0,/silent)
           fitim=mrdfits(ofile,1,/silent)
           diff=mrdfits(ofile,2,/silent)
;        w=where(im gt 5.*mean(im),nw)
;        w=where(abs(diff) gt 3.*stddev(diff),nw)
;        dof1[i]=nw
           rdis,im,title='Image '+ntostr(n[i]),/silent
           rdis,newim,title='Deprojected image',/silent
           tvcircle,fpars[i].r0,fpars[i].x0+100.,fpars[i].y0+100,!green,/data
           rdis,fitim,title='Fit model image',/silent
           tvcircle,fpars[i].r0,fpars[i].x0+100.,fpars[i].y0+100,!green,/data
           rdis,diff,title='Residuals',/silent
           if not keyword_set(ps) and not keyword_set(skip) then k=get_kbrd(10) else k='n'
           if k eq 'n' then i=i+1
           if k eq 'p' then i=i-1
           if k eq 'q' then i=nn
        endwhile
;     endfor 

        !p.multi=0
     endif 
     
     sn1987a_age,ages
     if keyword_set(hardsoft) then $
        ages=ages[[15,15,15,16,16,16,17,17,17,18,18,18,19,19,19]]

     scale=0.01475              ;*25.
     if keyword_set(inclination) then begin
        cos43=cos(43.*!dtor) 
        yrange=[0.3,0.7]
     endif else begin
        cos43=1.
        yrange=[0.5,0.85]
     endelse 
;  dof=dof-22.
;  w=where(dof lt 0,nw)
;  if nw gt 0 then dof[w]=22.
     if not keyword_set(hardsoft) then begin 
        counts=[690,607,9030,1800,6226,6427,9277,9668,11856,17979,16557,24939,27048,30940,30870,32798,27945,12008,12119,9204,3537,4970,2663,3103,3566,8361]*1d
     endif else counts=[5841,2410,8148,5258,2170,6429,2110,828,2739,2247,916,2721]*1d
;     counts=[690,607,9030,1800,6226,6427,9227,9668,11856,17979,16557,24939,27048,30940,30870,32798,27945,9256,12123,12123,5174,5174,9193,9204,9447,9447]*1d
;  dof=counts-dof0
     dof=200.^2-dof0[m]
;  dof=dof1-dof0
     
     r0=fpars.r0*scale*cos43    ;reprojects v to compare to radio/opt/ir
     
     ctswe=dblarr(2,n_elements(counts))
     ctswe[0,*]=sqrt(counts)/counts*fpars.r0
     ctswe[1,*]=ctswe[0,*]
;  r0err=sqrt(fpars.r0err^2)*scale;*sqrt(fpars.chisq/dof)
     r0err=sqrt(fpars.r0err^2+ctswe^2)
     r0err=r0err*scale*cos43

     ctswe=dblarr(2,n_elements(counts))
     ctswe[0,*]=sqrt(counts)/counts*fpars.sigr
     ctswe[1,*]=ctswe[0,*]
     sigr=fpars.sigr*scale*cos43
     sigrerr=sqrt(fpars.sigrerr^2+ctswe^2)*scale*cos43
;  r0err=fpars.sigr*scale
;  r0err=r0err*10.
;  r0err=sqrt(r0err^2+0.004^2)  ;;add systematic error
     
     ind=indgen(n_elements(files))
     
;  ind=[ind[0:4],ind[6:*]]
     r0=r0[ind]
     r0err=r0err[*,ind]
     rerr=r0err
     rerr=dblarr(n_elements(ind))
     for i=0,n_elements(ind)-1 do rerr[i]=min(r0err[*,i])
;  r0err[0,*]=rerr
;  r0err[1,*]=rerr
     
     dof=dof[ind]
     ages=ages[ind]
     
     plotsym,0,/fill
     
     g=where(r0 GT 0)
     g=[g[0:9],g[11:*]] ;;skip LETG obs 11
;     g=[g[0:9],g[11:21],g[23]]
;     if not exclude then g0=g[0:8] else g0=g[2:8]
;     if not exclude then g1=g[8:*] else g1=g[8:*]
     if keyword_set(exclude) then g=g[2:*]
;     if keyword_set(hardsoft) then stop
     if keyword_set(all) then multiplot2

     ;;; average 23abc
     if not keyword_set(noave) then begin 
        g=[g[0:19],g[21],g[24]]
        r0_22=weighted_mean(r0[19:20],r0err[0,19:20],r0err_22)
        r0_23=weighted_mean(r0[21:23],r0err[0,21:23],r0err_23)
        r0[20]=r0_22
        r0err[*,20]=r0err_22
        r0[22]=r0_23
        r0err[*,22]=r0err_23
        n[20]='22ab'
        n[22]='23abc'
        sigr_22=weighted_mean(sigr[20:21],sigrerr[0,20:21],sigrerr_22)
        sigr_23=weighted_mean(sigr[22:24],sigrerr[0,22:24],sigrerr_23)
        sigr[20]=sigr_22
        sigrerr[*,20]=sigrerr_22
        sigr[22]=sigr_23
        sigrerr[*,22]=sigrerr_23
     endif 
     s='   '
colprint,n[g],ages[g],s+numdec(r0[g],3),s+numdec(r0err[0,g],3),s+numdec(r0err[1,g],3),s+numdec(sigr[g],3),s+numdec(sigrerr[0,g],3)
stop

     if not keyword_set(hardsoft) then begin 

        plot,ages[g],r0[g],psym=8,xtitle=xtitle,ytitle=ytitle,title=title,_extra=_extra,yrange=yrange,xrange=[4000,9000],/xsty,/ysty
;  oploterror,ages[g],r0[g],r0err[g]
        for i=0,n_elements(g)-1 do oplot,[ages[g[i]],ages[g[i]]],[r0[g[i]]-r0err[0,g[i]],r0[g[i]]+r0err[1,g[i]]]
;     f0=linfit(ages[g0],r0[g0],measure_errors=rerr[g0],yfit=yfit,sigma=sigma0,chisq=chisq0)
;     chisq0=total((r0[g0]-yfit)^2./r0err[g0]^2)  
;  oplot,[ages[g0]],[ages[g0]]*f0[1]+f0[0],color=!blue
;     f1=linfit(ages[g1],r0[g1],measure_errors=rerr[g1],yfit=yfit,sigma=sigma1,chisq=chisq1)
;     chisq1=total((r0[g1]-yfit)^2./r0err[g1]^2)  
;  oplot,ages[g1],ages[g1]*f1[1]+f1[0],color=!red
;     d=1.54285e18               ;km to LMC
;     f2=linfit(ages[g],r0[g],measure=rerr[g],yfit=yfit,sigma=sigma2,chisq=chisq2)
;     chisq2=total((r0[g]-yfit)^2./r0err[g]^2)  
;     b=[f0[1],0,f1[1],0,f2[1]] 
;     expr=d*(b*!dtor/3600.)/86400D
;     exprerr=d*([sigma0[1],0,sigma1[1],0,sigma2[1]]*!dtor/3600.)/86400D
;  oplot,ages[g],yfit,color=!green,line=2
;     print,expr,exprerr
        
;  chisq=[chisq0,0,chisq1,0,chisq2]
;  dofc=[(n_elements(g0)-2),0,(n_elements(g1)-2),0,(n_elements(g)-2)]*1d
;  cdf=chisq/dofc
;  print,'chisq = ',chisq0,chisq1,chisq2
;  print,'dof = ',(n_elements(g0)-2),(n_elements(g1)-2),(n_elements(g)-2)
;  print,'chisq/dof = ',chisq0/(n_elements(g0)-2),chisq1/(n_elements(g1)-2),chisq2/(n_elements(g)-2)
        
;  ticpt=(f0[0]-f1[0])/(f1[1]-f0[1])
;  icpt=f1[0]+f1[1]*ticpt
;  p0=sqrt(chisq0/(total(dofc[g0])-n_elements(g0)))
;  p1=sqrt(chisq1/(total(dofc[g1])-n_elements(g1)))
;  p2=sqrt(chisq2/(total(dofc[g])-n_elements(g)))
;  a=f0[0]*p0 & b=f0[1]*p0 & c=f1[0]*p1 & d=f1[1]*p1
;  siga=sigma0[0]*p0 & sigb=sigma0[1]*p0 & sigc=sigma1[0]*p1 & sigd=sigma1[1]*p1
;  ticpterr=sqrt((1/(d-b))^2*siga^2+(2*(a-c)/(d-b)^2)^2*sigb^2+(-1/(d-b))^2*sigc^2+(-2*(a-c)/(d-b)^2)^2*sigd^2)
;  print,icpt,ticpt,ticpterr
        
        a=[-0.1d,1.5e-4,6000,2e-5];,7500,1e-6]
        parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                             limits:[0.D,0],$
                             parname:''}, n_elements(a))
        
        parinfo[2].limited=[1,1]
        parinfo[2].limits=[min(ages),max(ages)]

        f=mpfitfun('bknlin',ages[g],r0[g],rerr[g],a,yfit=yfit,parinfo=parinfo,dof=dof,perror=sigma)
        t=findgen(500)/500.*(max(ages)-min(ages))+min(ages)
        waf=where(t ge f[2]-50); and t le f[4])
;        waff=where(t ge f[4])
        wbe=where(t le f[2])
        oplot,t[waf],bknlin(t[waf],f),line=1 ;,color=!red
        oplot,t[wbe],bknlin(t[wbe],f)        ;,color=!blue
;        oplot,t[waff],bkn2lin(t[waff],f),line=2

        print,f
;     d=1.54285e18               ;km to LMC
        d=3.08568025d16*51.4 ;;; distance to 87a (Panagia et al. 2003)
        expr2=d*tan(f[[1,3]]*!dtor/3600.)/86400D 
        chisq2=total((r0[g]-yfit)^2./(rerr[g])^2)
        wbe=where(ages[g] lt f[2],nwbe)
        waf=where(ages[g] ge f[2],nwaf)
;        waf=where(ages[g] ge f[2] and ages[g] lt f[4],nwaf)
;        waff=where(ages[g] ge f[4],nwaff)
        dofa=nwbe-2
        dofb=nwaf-2
        chisq2a=total((r0[g[wbe]]-yfit[wbe])^2/(rerr[g[wbe]])^2)
        chisq2b=total((r0[g[waf]]-yfit[waf])^2/(rerr[g[waf]])^2)
;        chisq2c=total((r0[g[waff]]-yfit[waff])^2/(rerr[g[waff]])^2)
        
;     wset,1
        conf_error,ages[g],r0[g],rerr[g],f,sigma,'bknlin',perror ;,/doplot
        exprerr2=d*tan(perror[*,[1,3]]*!dtor/3600.)/86400D 
;     k=get_kbrd(10)
        a=[1,1e-5]
        dof3=n_elements(g)+2
        f1=mpfitfun('linfunc',ages[g],r0[g],rerr[g],a,yfit=yfit,dof=dof3,perror=sigma)
;     oplot,t,linfunc(t,f1),line=2;color=!green
        expr3=d*tan(f1[1]*!dtor/3600.)/86400D 
        chisq3=total((r0[g]-yfit)^2./(rerr[g])^2)
        conf_error,ages[g],r0[g],rerr[g],f1,sigma,'linfunc',perror1 ; ,/doplot
        exprerr3=d*tan(perror1[*,1]*!dtor/3600.)/86400D 
;     wset,0
        expr=[f[2],0,expr2[0],0,expr2[1],0,expr3]
        exprerr=[perror[0,2],0,exprerr2[0,0],0,exprerr2[0,1],0,exprerr3[0,0]]
        exprerrp=[perror[1,2],0,exprerr2[1,0],0,exprerr2[1,1],0,exprerr3[1,0]]
        dofc=[0,0,dof,0,0,0,dof3]
        chisq=[0,0,chisq2,0,0,0,chisq3]
        cdf=chisq/dofc
        cdfs='['+!tsym.chi+'!U2!N/dof = '+numdec(cdf,2)+' (dof='+[ntostr(fix(dofc))]+')]'
        cdfs[[0,1,3,4,5]]=''
        
        abc=['','a','','b']     ;,'','c']    ;,'','c','','d','','d','','e']
;  abc=''
        txt=['t = '+ntostr(round(expr[0]))+' '+!tsym.plusminus+' [-'+ntostr(round(exprerr[0]))+',+'+ntostr(round(exprerrp[0]))+'] days   ','v!L'+abc+'!N = '+[ntostr(round(expr[1:*]))]+' [-'+ntostr(round(exprerr[1:*]))+',+'+ntostr(round(exprerrp[1:*]))+'] km s!U-1!N   ']+cdfs
        
        tlen=strlen(txt)
        for tx=0,n_elements(txt)-1 do begin 
           sp=''
           for xx=0,max(tlen)-tlen[tx]-1 do sp=sp+' '
           txt[tx]=txt[tx]+sp
        endfor 
        txt[[1,3]]=''           ;,5]]=''
        ind=[0,2,4]             ;,6]
        
        if not keyword_set(noleg) then legend,[txt[ind]],box=0,/bottom,/right,charsize=1 $ ;,textcolor=[!p.color,!blue,!red,!green] 
        else begin 
           
           abc=['a','','b','']  ;,'c']
           txt='v!L'+abc
           txt[[1,3]]=''
           legend,[txt],box=0,/top,/left,charsize=1.5,line=[0,-1,1,-1] ;,2];,color=[!blue,0,!red,0,!green]
        endelse 
        
        print,f,perror
        print,f1,perror1
        print
        print,expr2,exprerr2
        print,expr3,exprerr3
        
        if not keyword_set(noleg) then legend,model[m]+append,box=0,/bottom,/left
;  txt='v!Ld!N = '+ntostr(round(expr2))+' '+!tsym.plusminus+' '+ntostr(round(exprerr2))+' km s!U-1!N   ['+!tsym.chi+'!U2!N/dof = '+sigfig(cdf,4)+' (dof='+ntostr(fix(dof))+')]'
;  legend,[txt],/top,/right,textcolor=!orange,charsize=2,box=0,/bottom
     endif else begin 
        plot,ages,r0,psym=8,xtitle=xtitle,ytitle=ytitle,title=title,_extra=_extra,yrange=[0.6,0.8],xrange=[7000,8000],/xsty
        
        colors=[!red,!blue,!green]
        for k=0,2 do begin 
;           j=indgen(3)+(k*3)
           j=indgen((n_elements(ages)/3.))*3+k
           oplot,ages[j],r0[j],psym=8,color=colors[k]
           for i=0,3 do oplot,[ages[j[i]],ages[j[i]]],[r0[j[i]]-r0err[0,j[i]],r0[j[i]]+r0err[1,j[i]]],color=colors[k]
        endfor 
        legend,['0.3-0.8','1.5-8.0','2.0-8.0']+'keV',textcolor=[!green,!red,!blue],/top,/left,box=0
        if keyword_set(ps) then begin 
           endplot
           spawn,'mv new_sn1987a_expansion_results.eps new_sn1987a_expansion_results_'+model+'.eps'
        endif 
        stop
     endelse         
  endfor
  if keyword_set(ps) then endplot
  multiplot2,/reset,/default
  
  ng=n_elements(g)
  aa=replicate(' & ',n_elements(g))
;  colprint,indgen(n_elements(g))+1,aa,ntostr(ages[g]),aa,'$'+numdec(fpars[g].r0,1)+'\pm ',numdec(fpars[g].r0err,1)+'$',aa,'$'+numdec(r0[g],4)+'\pm ',numdec(r0err[g],4)+'$ \\'
;  colprint,indgen(n_elements(g))+1,aa,ntostr(ages[g]),aa,'$'+numdec(r0[g],4)+'_{-'+numdec(r0err[0,g],4)+'}^{+'+numdec(r0err[1,g],4)+'}$ \\'
;  colprint,indgen(n_elements(g))+1,aa,ntostr(ages[g]),aa,'$'+numdec(r0[g],3)+'\pm '+numdec(or0err,3)+'$ \\'
;    colprint,aa,'$'+numdec(fpars[g].sigr*scale,3)+'\pm '+numdec(fpars[g].sigrerr[0,*]*scale,3)+'$ &','$'+numdec(r0[g],3)+'\pm '+numdec(or0err,3)+'$ '
  wlt=where(r0err lt 0.01)
;  r0err[wlt]=0.01
  w=where(abs(r0err[0,*]-r0err[1,*]) lt 0.005)
  sr0err='^{+'+numdec(r0err[1,*],4)+'}_{-'+numdec(r0err[0,*],4)+'}'
  sr0err[w]=' \pm '+numdec(r0err[0,w],4)
  
  sigrerr=fpars.sigrerr*scale
  wlt=where(sigrerr lt 0.01)
  sigrerr[wlt]=0.01
  w=where(abs(sigrerr[0,*]-sigrerr[1,*]) lt 0.005)
  ssigerr='^{+'+numdec(sigrerr[1,*],2)+'}_{-'+numdec(sigrerr[0,*],2)+'}'
  ssigerr[w]=' \pm '+numdec(sigrerr[0,w],2)
  
;  colprint,aa,'$'+numdec(fpars[g].sigr*scale,3)+'^{+'+numdec(fpars[g].sigrerr[1,*]*scale,3)+'}_{+'+numdec(fpars[g].sigrerr[0,*]*scale,3)+'}$ &','$'+numdec(r0[g],3)+'^{+'+numdec(r0err[1,g],3)+'}_{-'+numdec(r0err[0,g],3)+'}$'
  
;  writecol,mdir+'table.tex','$'+numdec(fpars[g].sigr*scale,2)+ssigerr[g]+'$ & $'+numdec(r0[g],2)+sr0err[g]+'$',delimiter=' '
  writecol,mdir+'table.tex','$'+numdec(r0[g],4)+sr0err[g]+'$',delimiter=' '
  
  print,'Results'
  colprint,'$'+ntostr(round(expr2[0]))+'_{-'+ntostr(round(exprerr2[0,0]))+'}^{+'+ntostr(round(exprerr2[1,0]))+'}$ & $'+ntostr(round(f[2]))+'_{-'+ntostr(round(perror[0,2]))+'}^{+'+ntostr(round(perror[1,2]))+'}$ & $'+ntostr(round(expr2[1]))+'_{-'+ntostr(round(exprerr2[0,1]))+'}^{+'+ntostr(round(exprerr2[1,1]))+'}$ & $'+numdec(cdf[2],2)+'$ \\'
  
  stop
  return
end 
