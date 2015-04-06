PRO sn1987a_results,suffix,ps=ps,exclude=exclude

  IF n_params() EQ 0 THEN suffix=''
  if not keyword_set(exclude) then exclude=0
  
  begplot,name='sn1987a_image_fits'+suffix+'.ps',/land
  fpars=mrdfits('sn1987a_fpars'+suffix+'.fits',1,/silent)
  if not exclude then n=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] else $
     n=[3,4,5,6,7,8,9,10,12,13,14,15]
  fpars=fpars[n-1]
  nn=n_elements(n)

  scale=25.;50.
  half=100.;200.
  offset=-0.02

  dof=lonarr(nn)
  FOR i=0,nn-1 DO BEGIN 
      j=n[i]
      ii=ntostr(j)

      imname='sn1987a'+ii+'_fitim'+suffix+'.fits'
      IF exist(imname) THEN BEGIN 
          oim=mrdfits('sn1987a'+ii+'_300-8000_cr.fits',0,/silent)
          im=mrdfits('sn1987a'+ii+'_fitim'+suffix+'.fits',0,/silent)
          fim=mrdfits('sn1987a'+ii+'_fitim'+suffix+'.fits',1,/silent)
          diff=mrdfits('sn1987a'+ii+'_fitim'+suffix+'.fits',2,/silent)
          
          tmp=where(oim GT 3.*mean(oim),ntmp)
          dof[i]=ntmp
          diff=abs(im-fim)
          !p.multi=[0,2,2]
          x0=round((fpars[i].x0-offset)*scale+half)
          y0=round((fpars[i].y0-offset)*scale+half)
          sigx=(fpars[i].sigx-offset)*scale;*0.01475
          sigy=(fpars[i].sigy-offset)*scale;*0.01475

          print,n[i],(fpars[i].x0-offset)*scale+half+1,(fpars[i].y0-offset)*scale+half+1,fpars[i].r0*scale

          im[x0-34.:x0+34.,30]=max(im)
          fim[x0-34.:x0+34.,30]=max(fim)
          diff[x0-34.:x0+34.,30]=max(diff)
          im[x0-1:x0+1,y0-1:y0+1]=max(im)
          fim[x0-1:x0+1,y0-1:y0+1]=max(fim)
          diff[x0-1:x0+1,y0-1:y0+1]=max(diff)

          rdis,im,low=min(oim),high=max(oim),title='Original image: SN1987a'+ii,charsize=1,/silent;,xmn=100,xmx=300,ymn=100,ymx=300
          xyouts,5200,11000,'1 arcsec',/device,charsize=1
          tvcircle,fpars[i].r0*scale,x0,y0,max(im),/data
          tvellipse,sigx,sigy,x0,y0,0,max(im),/data
          rdis,fim,low=min(oim),high=max(oim),title='Model image: SN1987a'+ii,charsize=1,/silent;,xmn=100,xmx=300,ymn=100,ymx=300
          xyouts,17200,11000,'1 arcsec',/device,charsize=1
          tvcircle,fpars[i].r0*scale,x0,y0,max(fim),/data
          tvellipse,sigx,sigy,x0,y0,0,max(im),/data
          rdis,diff,low=min(diff),high=max(diff),title='Subtracted image: SN1987a'+ii,charsize=1,/silent;,xmn=100,xmx=300,ymn=100,ymx=300
          xyouts,5200,2000,'1 arcsec',/device,charsize=1
          tvcircle,fpars[i].r0*scale,x0,y0,max(diff),/data
          tvellipse,sigx,sigy,x0,y0,0,max(im),/data

          xyouts,14000,7000,'a0 = '+ntostr(fpars[i].a0),/device,charsize=1
          xyouts,14000,6500,'a1 = '+ntostr(fpars[i].a1),/device,charsize=1
          xyouts,14000,6000,'x0 = '+ntostr(fpars[i].x0),/device,charsize=1
          xyouts,14000,5500,'y0 = '+ntostr(fpars[i].y0),/device,charsize=1
          xyouts,14000,5000,'sigx = '+ntostr(fpars[i].sigx),/device,charsize=1
          xyouts,14000,4500,'sigy = '+ntostr(fpars[i].sigy),/device,charsize=1
          xyouts,14000,4000,'b1 = '+ntostr(fpars[i].b1),/device,charsize=1
          xyouts,14000,3500,'r0 = '+ntostr(fpars[i].r0),/device,charsize=1
          xyouts,14000,3000,'theta0 = '+ntostr(fpars[i].theta0),/device,charsize=1
          xyouts,14000,2500,'sigr = '+ntostr(fpars[i].sigr),/device,charsize=1
          xyouts,18000,7000,'sigt = '+ntostr(fpars[i].sigt),/device,charsize=1
          xyouts,18000,6500,'c0 = '+ntostr(fpars[i].c0),/device,charsize=1
          xyouts,18000,6000,'c1 = '+ntostr(fpars[i].c1),/device,charsize=1
          xyouts,18000,5500,'phi0 = '+ntostr(fpars[i].phi0),/device,charsize=1
          xyouts,18000,5000,'counts = '+ntostr(fpars[i].counts),/device,charsize=1
          xyouts,18000,4500,'mu0 = '+ntostr(fpars[i].mu0),/device,charsize=1
          xyouts,18000,4000,'sigt2 = '+ntostr(fpars[i].sigt2),/device,charsize=1
          xyouts,18000,3500,'b2 = '+ntostr(fpars[i].b2),/device,charsize=1
          xyouts,18000,3000,'chisq = '+ntostr(fpars[i].chisq),/device,charsize=1
          xyouts,18000,2500,'iter = '+ntostr(fpars[i].iter),/device,charsize=1

 ;     k=get_kbrd(10)
          w=where(im GT 0.0005,nw)
      endiF 
  ENDFOR 
  
  !p.multi=0
  
  endplot
;print,dof
  defsymbols
  IF keyword_set(ps) THEN begplot,name='sn1987a_expansion_results.ps',/land,/color
  sn1987a_age,ages
  ages=ages[n-1]
;  sigx=(fpars.sigx)*scale*0.01475
;  sigy=(fpars.sigy)*scale*0.01475
;  r=(sqrt(fpars.sigx^2+fpars.sigy^2)-offset)*scale*0.01475
  r=sqrt(sigx^2+sigy^2)
  r0=(fpars.r0)*scale*0.01475
  cerr=(fpars.x0err/fpars.x0)^2+(fpars.y0err/fpars.y0)^2
;  r0err=sqrt((fpars.r0err/fpars.r0)^2)*sqrt(fpars.chisq/(dof-13))*r0;*scale*0.01475;*5.

  ;;;;;;;;;;;;;NOT SURE IF RIGHT THING TO DO;;;;;;;;;;;;;;;
  ctsinim=[690,607,9030,1800,6226,6427,9277,9668,11856,17979,16557,24939,27048,31048]
  dof=ctsinim[n]
  
  r0err=fpars.r0err[1]*scale*0.01475*sqrt(fpars.chisq/(dof-13))
  r0err=r0err*4.
  ;;probably not right
  
  
;  sigr=sqrt((fpars.sigxerr/fpars.sigx)^2+(fpars.sigyerr/fpars.sigy)^2)*r
  g=where(r0 GT 0); AND r0err LT 100.)
;  !p.multi=[0,1,2]
  plotsym,0,/fill
  ploterror,ages,r0,r0err,xtitle='Time since SN explosion(days)',ytitle='r!L0!N (arcsec)',psym=8,xrange=[4500,7500],xstyle=1,title='SN1987A expansion measure'
  print,r0,r0err

  if exclude then g0=g[0:6] else g0=g[2:8]
  f0=linfit(ages[g0],r0[g0],measure_errors=r0err[g0],yfit=yfit,sigma=sigma0,chisq=chisq0)
;  print,chisq0
;  oplot,ages[g0],yfit,color=!blue
  oplot,[ages[0:6],6266],[ages[0:6],6266]*f0[1]+f0[0],color=!blue
  if exclude then g1=g[6:*] else g1=g[[8,9,11,12,13]]
  f1=linfit(ages[g1],r0[g1],measure_errors=r0err[g1],yfit=yfit,sigma=sigma1,chisq=chisq1)
;  print,chisq1
;  oplot,ages[g1],yfit,color=!red
  oplot,ages[6:*],ages[6:*]*f1[1]+f1[0],color=!red
;  b=[f0[1],0,f1[1]]
  d=1.54285e18 ;km to LMC
;  expr=d*(b*!dtor/3600.)/86400D
;  exprerr=d*([sigma0[1],0,sigma1[1]]*!dtor/3600.)/86400D
;  legend,['v!Lexpansion!N = '+sigfig(expr,4)+' '+!tsym.plusminus+' '+sigfig(exprerr,3)+' km/s'],box=0,/bottom,/right,charsize=2,textcolor=[!blue,!white,!red]
  
  f2=linfit(ages[g],r0[g],measure=r0err[g],yfit=yfit,sigma=sigma2,chisq=chisq2)
  b=[f0[1],0,f1[1],0,f2[1]] 
  expr=d*(b*!dtor/3600.)/86400D 
  exprerr=d*([sigma0[1],0,sigma1[1],0,sigma2[1]]*!dtor/3600.)/86400D
  oplot,ages[g],yfit,color=!green,line=2
  print,expr,exprerr
  
  print,'chisq = ',chisq0,chisq1,chisq2
  print,'chisq/dof = ',chisq0/(7-2),chisq1/(5-2),chisq2/(11-2)
  txt=['v!Lexpansion!N = '+ntostr(round(expr))+' '+!tsym.plusminus+' '+ntostr(round(exprerr))+' km s!U-1!N']
  txt[[1,3]]=''
  
  legend,[txt],box=0,/bottom,/right,charsize=2,textcolor=[!blue,!white,!red,!white,!green]

  
  ticpt=(f0[0]-f1[0])/(f1[1]-f0[1])
  icpt=f1[0]+f1[1]*ticpt
  p0=sqrt(chisq0/(total(dof[g0])-n_elements(g0)))
  p1=sqrt(chisq1/(total(dof[g1])-n_elements(g1)))
  p2=sqrt(chisq2/(total(dof[g])-n_elements(g2)))
  a=f0[0]*p0 & b=f0[1]*p0 & c=f1[0]*p1 & d=f1[1]*p1
  siga=sigma0[0]*p0 & sigb=sigma0[1]*p0 & sigc=sigma1[0]*p1 & sigd=sigma1[1]*p1
  ticpterr=sqrt((1/(d-b))^2*siga^2+(2*(a-c)/(d-b)^2)^2*sigb^2+(-1/(d-b))^2*sigc^2+(-2*(a-c)/(d-b)^2)^2*sigd^2)
  ;;;PRETTY SURE THIS IS INCORRECT
  
  
  
  print,icpt,ticpt,ticpterr
  
  !p.multi=0
  IF keyword_set(ps) THEN endplot
  
  ;;;make output table of fit params
  openw,lun,'fpars_table.tex',/get_lun
  d='$ & $'
  pm=' \pm '
  s=scale*0.01475
  for i=0,n_elements(fpars)-1 do begin
     printf,lun,'$'+ntostr(n[i])+d+sigfig(fpars[i].r0*s,3)+pm+$
        sigfig(fpars[i].r0err[0]*s,2)+d+ $
        sigfig(fpars[i].theta0,3)+d+sigfig(fpars[i].phi0,4)+d+$
        sigfig(fpars[i].mu0,4)+d+$
        sigfig(fpars[i].sigr*s,2)+d+sigfig(fpars[i].sigt*s,3)+d+$
        sigfig(fpars[i].sigt2*s,3)+d+$
;        sigfig(fpars[i].sigx,3)+d+sigfig(fpars[i].sigy,2)+d+$
        sigfig(fpars[i].chisq,3)+'$ \\'
  endfor 
  close,lun,/file
  
  
  
stop
  return
END 
