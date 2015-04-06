pro grb060801_astrometry,ps=ps,wavdet=wavdet,skip=skip
  
  grbra=213.006666667d          ;14 12 01.6 +16 58 54.8
  grbdec=16.9818888889d
  grberr=5.5
  
  natsra=213.005416667d         ;14 12 01.3 +16 58 54.4
  natsdec=16.9817777778d
  natserr=1.1
  
  ara=213.0075d                 ;14 12 01.8
  adec=16.9834722222d           ;16 59 00.5
  
  bra=213.005416667d            ;14 12 01.3 
  bdec=16.9816666667d           ;16 58 54.0
  
  cra=213.007833333d
  cdec=16.9815833333d
  
  dra=213.005833333d
  ddec=16.9818611111d
  
  grbbsra=213.006062007d
  grbbsdec=16.9815738769d
  grbbserr=3.68
  
  ralist=[grbra,grbbsra,natsra,ara,bra,cra,dra]
  declist=[grbdec,grbbsdec,natsdec,adec,bdec,cdec,ddec]
  errlist=[grberr,grbbserr,natserr,0,0,0,0]
  
  names=['XRT refined','XRT boresight corr','Butler','A','B','C','D']
  
;  optfile='usno.txt'
  optfile='sdss.csv'
  
  xrt_astrometry,'','',grbra,grbdec,newra,newdec,newerr,ralist,declist,errlist,names,/filt,iter=5,phamin=40,snr=3,/sdss,suffix='_f100',skip=skip;,dir='00*-xrt/'
  
;  xrt_astrometry,optfile,'','',grbs[i].xra,grbs[i].xdec,newra,newdec,merr,ralist,declist,errlist,names,mra,mdec,mraerr,nsrc,expotime,sdra,sddec,sdss=sdss,dposs=dposs,dss=dss,twomass=twomass,dir='00*-xrt/',/filt,iter=iter,skipgv=skipgv,nospawn=nospawn,suffix=suffix,grb=gb
  
;  return
;end 
  
  window,1
  if keyword_set(ps) then begplot,name='grb060801_astrometry.ps',/color
  !p.multi=0
  
  title='GRB 060801'
  compare_plots,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title=title,/donames,/iso,charsize=1
  
;  make_astrom_plot,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title=title,donames=donames,_extra=_extra,charsize=1
  
;;  dras=(grbra-ralist)/cos(grbdec*!dtor)*3600.
;;  ddecs=(grbdec-declist)*3600.
;   raref=(14.*15d)+12./60d*15d ;14 12 00
;   decref=16.+58./60d ;16 58 00
;   dras=(ralist-raref)*3600./cos(decref*!dtor)
;   ddecs=(declist-decref)*3600.

;   s=' '  

;   n=[1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0]*15.
;   n2=[n[0],0,n[2],0,n[4],0,n[6],s,n[8]]
  
;   w=where(n2 eq 0)
;   xtickname=n
  
;   xtickname2='14h 12m '+[sigfig(n2/15.,2)]+'s'
;   xtickname2[w]=s

;   n=ntostr(indgen(7)*2+58)

;   ytickname=['16'+!tsym.degrees+' 58'+"' "+['50','52','54','56','58']+'"','16'+!tsym.degrees+' 59'+"' "+['00','02']+'"']
  
;   plot,[min(dras)-2,max(dras)+2],[min(ddecs)-4,max(ddecs)+2],psym=1,/yno,/nodata,/xstyle,/ystyle,/iso,xtitle='RA(J2000)',ytitle='Dec(J2000)',title='GRB 060801',ytickname=ytickname,charsize=1,xtickv=xtickname,xticks=8,xtickname=xtickname2 ;,xtickname=xtickname,xticks=9,
  
;                                 ;,xticks=10,xtick_get=xtick;xtickname=xtickname,
  
  
;   cs=1.0
;   tvcircle,3.7,dras[0],ddecs[0],color=!green,/data
;   xyouts,dras[0]-1.5,ddecs[0]+4,'XRT refined position',color=!green,/data,charsize=cs
  
;   tvcircle,1.1,dras[1],ddecs[1],color=!red,/data
;   xyouts,dras[1]-0.7,ddecs[1]+1.3,'Butler et al.',color=!red,/data,charsize=cs
  
;   tvcircle,3.7,dras[6],ddecs[6],color=!blue,/data
;   xyouts,dras[6]-1.5,ddecs[6]+4,'XRT boresight corr',color=!blue,/data,charsize=cs
  
;                                 ;  tvcircle,3.7,dras[2],ddecs[2],color=!yellow,/data
;                                 ;  xyouts,dras[2]-1,ddecs[2]+3,'XRT refined no rounding',color=!yellow,/data,charsize=cs
  
;   plots,dras[2],ddecs[2],color=!violet,psym=2
;   xyouts,dras[2]-0.05,ddecs[2]+0.2,'A',color=!violet,/data,charsize=cs
  
;   plots,dras[3],ddecs[3],color=!orange,psym=2
;   xyouts,dras[3]-0.05,ddecs[3]+0.2,'B',color=!orange,/data,charsize=cs
  
;   plots,dras[4],ddecs[4],color=!magenta,psym=2
;   xyouts,dras[4]-0.05,ddecs[4]+0.2,'C',color=!magenta,/data,charsize=cs
  
;   plots,dras[5],ddecs[5],color=!cyan,psym=2
;   xyouts,dras[5]-0.05,ddecs[5]+0.2,'D',color=!cyan,/data,charsize=cs
  
;   mra=(newra-raref)*3600./cos(decref*!dtor)
;   mdec=(newdec-decref)*3600.
  
;   tvcircle,merr,mra,mdec,color=!purple,/data
;   xyouts,mra-1,mdec+merr+0.08,'New XRT position',color=!purple,/data,charsize=cs
;   print,'offset between New & Butler: '+ntostr(separation(newra,newdec,natsra,natsdec))
;   print,'offset between New and XRT refined: '+ntostr(separation(newra,newdec,grbra,grbdec))
  
  if keyword_set(ps) then endplot
;  stop
  return
end 

  
