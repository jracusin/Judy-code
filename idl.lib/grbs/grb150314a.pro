@fit_functions
@fit_functions_flares
pro xy2rd,x,y,cra,cdec,ra,dec,s=s
  if n_elements(s) eq 0 then s=1.
  cd=cdec*!dtor
  cr=cra*!dtor
  theta=tan(-x/y)
  phi=tan(s*sqrt(x^2+y^2))
  ra=cra+asin(sin(phi)*sin(theta)/(cos(cd)*cos(theta)-sin(cd)*sin(phi)*cos(theta)))*!radeg
  dec=asin(sin(cd)*cos(theta)+cos(cd)*sin(phi)*cos(theta))*!radeg

return
end 

pro rd2xy,ra,dec,raerr,decerr,cra,cdec,x,y,xerr,yerr,s=s
  if n_elements(s) eq 0 then s=1.
  d=dec*!dtor
  cd=cdec*!dtor
  r=ra*!dtor
  cr=cra*!dtor

  x=-1./s*cos(d)*sin(r-cr)/(sin(d)*sin(cd)+cos(d)*cos(cd)*cos(r-cr))
  y=1./s*(sin(d)*cos(cd)-cos(d)*sin(cd)*cos(r-cr))/(sin(d)*sin(cd)+cos(d)*cos(cd)*cos(r-cr))

  d=d+decerr*!dtor
  r=r+raerr*!dtor
  x2=-1./s*cos(d)*sin(r-cr)/(sin(d)*sin(cd)+cos(d)*cos(cd)*cos(r-cr))
  y2=1./s*(sin(d)*cos(cd)-cos(d)*sin(cd)*cos(r-cr))/(sin(d)*sin(cd)+cos(d)*cos(cd)*cos(r-cr))

  xerr=x2-x
  yerr=y2-y

return
end 

function ast_str,ra,dec,raerr,decerr

  g=create_struct('ra',0.,'dec',0.,'raerr',0.,'decerr',0.)
  g=replicate(g,n_elements(ra))
  g.ra=ra
  g.dec=dec
  g.raerr=raerr
  g.decerr=decerr

  return,g
end 


pro rel_ast,ra1,dec1,ra1err,dec1err,ra2,dec2,ra2err,dec2err,newra2,newdec2,newra2err,newdec2err

  ;;; correct g2 relative to g1, cra & cdec are center
;; should setup fits structures maybe or arrays, too many parameters

  cra=weighted_mean(ra1,ra1err)
  cdec=weighted_mean(dec1,dec1err)

  rd2xy,ra1,dec1,ra1err,dec1err,cra,cdec,x1,y1,x1err,y1err
  rd2xy,ra2,dec2,ra2err,dec2err,cra,cdec,x2,y2,x2err,y2err

  xerr12=sqrt(x1err^2+x2err^2)
  yerr12=sqrt(y1err^2+y2err^2)

  dx=weighted_mean(x1-x2,xerr12,dxerr)
  dy=weighted_mean(y1-y2,yerr12,dyerr)

  newx2=x2-dx
  newy2=y2-dy

  newx2err=sqrt(dxerr^2+xerr12^2)
  newy2err=sqrt(dyerr^2+yerr12^2)

  xy2rd,newx2,newy2,cra,cdec,newra2,newdec2
  xy2rd,newx2+newx2err,newy2+newy2err,cra,cdec,newra2err,newdec2err
  newra2err=newra2err-newra2
  newdec2err=newdec2err-newdec2

  return
end 

pro relative_astrometry
  
  cd,'~/Chandra/GRB150314A'
  dirs=['16959','16960','16961']
  
  ra=dblarr(3) & dec=dblarr(3)
  for i=0,n_elements(dirs)-1 do begin 
     cd,dirs[i]
     if not exist("dmstat.out") then spawn,'/Users/jracusin/CIAO/ciao-4.6/bin/dmstat "acis_evt2_clean.fits[sky=region(src_3.reg)][bin sky=1]" centroid=yes > dmstat.out'
     readcol,'dmstat.out',lines,format='a',delim='$'
     vals=strsplit(lines[4],'() ',/ex)
     x=vals[1]
     y=vals[2]
     pcad=file_search('primary/pcad*asol*fits')
     if not exist("dmcoords.out") then spawn,'/Users/jracusin/CIAO/ciao-4.6/bin/dmcoords acis_evt2_clean.fits asol='+pcad+' option=sky x='+x+' y='+y+' verbose=1 > dmcoords.out'
     readcol,'dmcoords.out',lines,format='a',delim='$'
;     colprint,indgen(n_elements(lines)),lines
     vals=strsplit(lines[28],' ',/ex)
     ra[i]=vals[1]*1d
     dec[i]=vals[2]*1d
     cd,'..'
  endfor 

;  colprint,ra,dec

  print,separation(ra[0],dec[0],ra[1],dec[1])
  print,separation(ra[1],dec[1],ra[2],dec[2])

  d=60*60.
  rar=(ra-ra[0])*d
  decr=(dec-dec[0])*d
;  plot,rar,decr,psym=2,/yno,xrange=[-1,1],yrange=[-1,1],xtitle=!tsym.delta_cap+' RA (arcsec)',ytitle=!tsym.delta_cap+' Dec (arcsec)'
;  xyouts,rar-0.01,decr+0.05,ntostr(indgen(3)+1),/data

;;; 2 methods to detect sources - celldetect is way faster and finds
;                                 fewer sources
;celldetect "acis_evt2_clean.fits[events]" cell_output.fits regfile=cellregfile.reg clobber=yes

;mkpsfmap acis_evt2_clean.fits mypsfmap.fits energy=1.49 ecf=0.393
;wavdetect acis_evt2_clean.fits source_list.fits source_cell.fits image.fits background.fits psffile=mypsfmap.fits regfile=wavregfile.reg scales=1 clobber=yes

  cell1=mrdfits(dirs[0]+'/cell_output.fits',1)
  cell2=mrdfits(dirs[1]+'/cell_output.fits',1)
  cell3=mrdfits(dirs[2]+'/cell_output.fits',1)

;  cell1=mrdfits(dirs[0]+'/source_list.fits',1)
;  cell2=mrdfits(dirs[1]+'/source_list.fits',1)
;  cell3=mrdfits(dirs[2]+'/source_list.fits',1)

  w=where(cell2.ra_err gt 0)
  cell2=cell2[w]
  w=where(cell3.ra_err gt 0)
  cell3=cell3[w]
;  plot,cell3.ra,cell3.dec,psym=1,/yno,/iso
  
;  color=[!green,!orange,!blue]
;  for j=0,2 do begin 
;     cell=mrdfits(dirs[j]+'/cell_output.fits',1)
;     for i=0,n_elements(cell)-1 do tvcircle,0.01,cell[i].ra,cell[i].dec,/data,color=color[j]
;  endfor 

  ;; sources in both 1 & 2
  m21=match_2d(cell1.ra,cell1.dec,cell2.ra,cell2.dec,2./3600.)
  wm12=where(m21 ne -1)
  wm21=m21[wm12]
  dist12a=separation(cell1[wm12].ra,cell1[wm12].dec,cell2[wm21].ra,cell2[wm21].dec)

  ;; sources in both 1 & 2 to see if in 3
  m123=match_2d(cell1[wm12].ra,cell1[wm12].dec,cell3.ra,cell3.dec,2./3600.)
  wm123=where(m123 ne -1)
  wm312=m123[wm123]
  wm213=wm21[wm123]
  wm123=wm12[wm123]

  n=n_elements(wm123)

  ra1=cell1[wm123].ra
  dec1=cell1[wm123].dec
  ra2=cell2[wm213].ra
  dec2=cell2[wm213].dec
  ra3=cell3[wm312].ra
  dec3=cell3[wm312].dec

  ra_err1=cell1[wm123].ra_err
  dec_err1=cell1[wm123].dec_err
  ra_err2=cell2[wm213].ra_err
  dec_err2=cell2[wm213].dec_err
  ra_err3=cell3[wm312].ra_err
  dec_err3=cell3[wm312].dec_err
  
  dist12=separation(ra1,dec1,ra2,dec2)
  dist23=separation(ra2,dec2,ra3,dec3)
  dist13=separation(ra1,dec1,ra3,dec3)

  colprint,dist12,dist23,dist13

  begplot,name='~/Chandra/GRB150314A/relative_astrometry.ps',/land,/color

  ;;; which one is the grb
  gdist=separation(ra1,dec1,ra[0],dec[0])*3600.
  mdist=min(gdist,g)
  ind=intarr(n)
  ind[g]=1
  wnotg=where(ind eq 0,nsrc)
  plotsym,0,0.8,/fill

  raoff2=(ra2-ra1)*3600.;*cos(dec1*!dtor)
  raoff3=(ra3-ra1)*3600.;*cos(dec1*!dtor)
  decoff2=(dec2-dec1)*3600.
  decoff3=(dec3-dec1)*3600.

  sigra2=ra_err2*3600.;*cos(dec1*!dtor)
  sigra3=ra_err3*3600.;*cos(dec1*!dtor)
  sigdec2=dec_err2*3600.
  sigdec3=dec_err3*3600.

  plot,[0,1],/nodata,/yno,xrange=[-1,1],yrange=[-2,1],xtitle=!tsym.delta_cap+' RA (arcsec)',ytitle=!tsym.delta_cap+' Dec (arcsec)'
;  oplot,raoff,decoff,psym=1,color=!green
  plots,[0,0],[0,0],psym=8,color=!green
  oploterror2,raoff2[wnotg],decoff2[wnotg],sigra2[wnotg],sigdec2[wnotg],psym=8,color=!pink,errcolor=!pink,/nohat
  oploterror2,raoff3[wnotg],decoff3[wnotg],sigra3[wnotg],sigdec3[wnotg],psym=8,color=!lightblue,errcolor=!lightblue,/nohat

;  tvcircle,0.05,(ra2[g]-ra1[g])*3600.,(dec2[g]-dec1[g])*3600.,/data,!red
;  tvcircle,0.05,(ra3[g]-ra1[g])*3600.,(dec3[g]-dec1[g])*3600.,/data,!blue
  oploterror2,raoff2[g],decoff2[g],sigra2[g],sigdec2[g],psym=4,color=!red,symsize=1.5,/nohat,errcolor=!red
  oploterror2,raoff3[g],decoff3[g],sigra3[g],sigdec3[g],psym=4,color=!blue,symsize=1.5,/nohat,errcolor=!blue

  rel_ast,ra1[wnotg],dec1[wnotg],ra_err1[wnotg],dec_err1[wnotg],ra2[wnotg],dec2[wnotg],ra_err2[wnotg],dec_err2[wnotg],newra2,newdec2,newra_err2,newdec_err2

  rel_ast,ra1[wnotg],dec1[wnotg],ra_err1[wnotg],dec_err1[wnotg],ra3[wnotg],dec3[wnotg],ra_err3[wnotg],dec_err3[wnotg],newra3,newdec3,newra_err3,newdec_err3


  ;;; find weighted mean offset
  mra2=weighted_mean(raoff2[wnotg],sigra2[wnotg],mraerr2)
  mdec2=weighted_mean(decoff2[wnotg],sigdec2[wnotg],mdecerr2)

  mra3=weighted_mean(raoff3[wnotg],sigra3[wnotg],mraerr3)
  mdec3=weighted_mean(decoff3[wnotg],sigdec3[wnotg],mdecerr3)

;  newra2=ra2[g]-mra2/3600./cos(dec3[g]*!dtor)
;  newdec2=dec2[g]-mdec2/3600.
;  newra_err2=sqrt((mraerr2/3600.)^2+ra_err2[g]^2)
;  newdec_err2=sqrt((mdecerr2/3600.)^2+dec_err2[g]^2)

;  newra3=ra3[g]-mra3/3600./cos(dec3[g]*!dtor)
;  newdec3=dec3[g]-mdec3/3600.
;  newra_err3=sqrt((mraerr3/3600.)^2+ra_err3[g]^2)
;  newdec_err3=sqrt((mdecerr3/3600.)^2+dec_err3[g]^2)

  newraoff2=(newra2-ra1[g])*3600.;*cos(newdec2*!dtor)
  newraoff3=(newra3-ra1[g])*3600.;*cos(newdec2*!dtor)
  newdecoff2=(newdec2-dec1[g])*3600.
  newdecoff3=(newdec3-dec1[g])*3600.

  newsigra2=newra_err2*3600.;*cos(newdec2*!dtor)
  newsigra3=newra_err3*3600.;*cos(newdec2*!dtor)
  newsigdec2=newdec_err2*3600.
  newsigdec3=newdec_err3*3600.


;  print,newra2,newdec2,(newra2-ra1[g])*3600.,(newdec2-dec1[g])*3600.,mra2*3600.,mdec2*3600.
;  print,newra3,newdec3,(newra3-ra1[g])*3600.,(newdec3-dec1[g])*3600.,mra3*3600.,mdec3*3600.

  oploterror2,newraoff2,newdecoff2,newsigra2,newsigdec2,psym=6,color=!red
  oploterror2,newraoff3,newdecoff3,newsigra3,newsigdec3,psym=6,color=!blue

  plots,mra2,mdec2,psym=2,color=!pink,symsize=2
  plots,mra3,mdec3,psym=2,color=!lightblue,symsize=2

  legend,dirs,/top,/left,textcolor=[!green,!red,!blue],box=0
  legend,['Background Sources','Weighted Means','GRB Start','GRB Corrected'],/top,/right,psym=[8,2,4,6],box=0

  endplot
  ps2pdf,'~/Chandra/GRB150314A/relative_astrometry.ps'

stop
return
end 

function int_con_gauss2_bknpow,t,p

  yfit=p[0]+intgauss(t,p[5:7])+intgauss(t,p[8:10])+intbknpow(t,p[1:4])
return,yfit
end

function con_gauss2_bknpow,t,p

  yfit=p[0]+gauss(t,p[5:7])+gauss(t,p[8:10])+bknpow(t,p[1:4])
return,yfit
end
  
pro grb150314a

  ;;; LAT GRB w/ chandra
  ;;; want to fit weird LC model

  cd,'~/Chandra/GRB150314A'
  lc=lcout2fits(/chandra)

  read_lcfit,'lc_fit_comb.dat',pnames,p0
  mo=fit_models(pnames,p0,basemo=basemo)
  intmo='int_con_gauss2_bknpow'
  mo='con_gauss2_bknpow'
  p=[2e-4,p0]
  tt=dblarr(2,n_elements(lc))
  tt[0,*]=lc.tstart
  tt[1,*]=lc.tstop
  print,p
  quiet=1
  newp=mpfitfun(intmo,tt,lc.src_rate,lc.src_rate_err,p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=1000,errmsg=errmsg,$
                perror=perror,yfit=yfit,status=status,nprint=nprint,$
                ftol=1e-25,xtol=1e-25,gtol=1e-25,quiet=quiet)
  chisq=total(((yfit-lc.src_rate)/lc.src_rate_err[0])^2)

  print,newp

  xrt=mrdfits('~/Chandra/GRB150314A/UL_specfits.fits',1)

  r=xrt[2].unabs_cfratio
  begplot,name='~/Chandra/GRB150314A/GRB150314A_combined_lc_wcons.ps',/land,/color
  plot_like_qdp,lc=lc,/nohard,yrange=[1e-15,1e-7],/ysty,flux=r,/useflux

  t=logarr(lc[0].tstart,1e7,bin=0.1)
  oplot,t,call_function(mo,t,newp)*r
  oplot,t,call_function(basemo,t,newp[1:*])*r,line=2,color=!green
  oplot,t,gauss(t,newp[5:7])*r,color=!green,line=2
  oplot,t,gauss(t,newp[8:10])*r,color=!green,line=2
  oplot,minmax(t),[newp[0],newp[0]]*r,line=2,color=!magenta
  endplot
  ps2pdf,'~/Chandra/GRB150314A/GRB150314A_combined_lc_wcons.ps'

  stop

return
end 
