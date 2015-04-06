pro compare_plots,newra,newdec,merr
  
  mygcnra=213.006666667d  ;14 12 01.6 +16 58 54.8
  mygcndec=16.9818888889d
  write_regfile,'mygcn.reg',mygcnra,mygcndec,3.7
  
  natsra=213.005416667d  ;14 12 01.3 +16 58 54.4
  natsdec=16.9817777778d
  
  xrtctdra=213.00686406d  ;14 12 01.65 +16 58 54.85
  xrtctddec=16.9819027561d
  
  ara=213.0075d ;14 12 01.8
  adec=16.9834722222d ;16 59 00.5
  
  bra=213.005416667d ;14 12 01.3 
  bdec=16.9816666667d ;16 58 54.0
  
  cra=213.007833333d
  cdec=16.9815833333d
  
  dra=213.005833333d
  ddec=16.9818611111d
  
  ras=[mygcnra,natsra,xrtctdra,ara,bra,cra,dra]
  decs=[mygcndec,natsdec,xrtctddec,adec,bdec,cdec,ddec]
  
  raref=(14.*15d)+12./60d*15d ;14 12 00
  decref=16.+58./60d ;16 58 00
  
  dras=(ras-raref)*3600.;/cos(decs*!dtor)
  ddecs=(decs-decref)*3600.
  
  
;  dras=(ras-mygcnra)*3600./cos(decs*!dtor)
;  ddecs=(decs-mygcndec)*3600.

;  plot,[min(dras)-2,max(dras)+2],[min(ddecs)-4,max(ddecs)+2],psym=1,/yno,/nodata,/xstyle,/ystyle,/iso,xtitle='RA offset (arcseconds)',ytitle='Dec offset (arcseconds)',title='GRB 060801'
  
  s=' '  
;  n=[18,20,22,24,26,28,30]
;  n=[17.500001, 19.087501, 20.675001, 22.262500, 23.850000, 25.437500, 27.024999, 28.612499, 30.199999]
  n=[1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0]*15.
  n2=[n[0],0,n[2],0,n[4],0,n[6],s,n[8]]
;  n=[1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0]*15.
  w=where(n2 eq 0)
  xtickname=n
  
  xtickname2='14h 12m '+[sigfig(n2/15.,2)]+'s'
  xtickname2[w]=s
;  xtickname=['14h 12m 20s',s,'14h 12m 24s',s,'14h 12m 28s',s]
  n=ntostr(indgen(7)*2+58)
;  ytickname='16'+!tsym.degrees+' 58'+"'"+n+'"'
  ytickname=['16'+!tsym.degrees+' 58'+"' "+['50','52','54','56','58']+'"','16'+!tsym.degrees+' 59'+"' "+['00','02']+'"']
  
  plot,[min(dras)-2,max(dras)+2],[min(ddecs)-4,max(ddecs)+2],psym=1,/yno,/nodata,/xstyle,/ystyle,/iso,xtitle='RA(J2000)',ytitle='Dec(J2000)',title='GRB 060801',ytickname=ytickname,charsize=1,xtickv=xtickname,xticks=8,xtickname=xtickname2;,xtickname=xtickname,xticks=9,
  
;,xticks=10,xtick_get=xtick;xtickname=xtickname,
  
  
  cs=1.0
  tvcircle,3.7,dras[0],ddecs[0],color=!green,/data
  xyouts,dras[0]-1.5,ddecs[0]+4,'XRT refined position',color=!green,/data,charsize=cs
  
  tvcircle,1.1,dras[1],ddecs[1],color=!red,/data
  xyouts,dras[1]-0.7,ddecs[1]+1.3,'Butler et al.',color=!red,/data,charsize=cs
  
;  tvcircle,3.7,dras[2],ddecs[2],color=!yellow,/data
;  xyouts,dras[2]-1,ddecs[2]+3,'XRT refined no rounding',color=!yellow,/data,charsize=cs
  
  plots,dras[3],ddecs[3],color=!blue,psym=2
  xyouts,dras[3]-0.05,ddecs[3]+0.2,'A',color=!blue,/data,charsize=cs
  
  plots,dras[4],ddecs[4],color=!orange,psym=2
  xyouts,dras[4]-0.05,ddecs[4]+0.2,'B',color=!orange,/data,charsize=cs
  
  plots,dras[5],ddecs[5],color=!magenta,psym=2
  xyouts,dras[5]-0.05,ddecs[5]+0.2,'C',color=!magenta,/data,charsize=cs
  
  plots,dras[6],ddecs[6],color=!cyan,psym=2
  xyouts,dras[6]-0.05,ddecs[6]+0.2,'D',color=!cyan,/data,charsize=cs
  
  if n_params() gt 0 then begin
;     mra=(newra-mygcnra)*3600./cos(newdec*!dtor)
;     mdec=(newdec-mygcndec)*3600.
     mra=(newra-raref)*3600.;/cos(newdec*!dtor)
     mdec=(newdec-decref)*3600.
     
     tvcircle,merr,mra,mdec,color=!purple,/data
     xyouts,mra-1,mdec+merr+0.2,'New XRT position',color=!purple,/data,charsize=cs
     print,'offset between New & Butler: '+ntostr(separation(newra,newdec,natsra,natsdec))
     print,'offset between New and XRT refined: '+ntostr(separation(newra,newdec,mygcnra,mygcndec))
  endif 
 
end 
pro grb060801_astrometry,ps=ps,wavdet=wavdet
  
  if keyword_set(ps) then begin 
     begplot,name='grb_err_pos.ps',/color
     !p.charsize=1
  endif 
  !p.multi=[0,1,2]
  mygcnra=213.006666667d         ;14 12 01.6 +16 58 54.8
  mygcndec=16.9818888889d
  
  hpd=22.63
  p=-0.48
;  grbcts=477
  searchdist=10.
  
  readcol,'sdss.csv',objid,run,rerun,camcol,field,obj,type,ra,dec,u,g,r,i,z,Err_u,Err_g,Err_r,Err_i,Err_z,format='(a,l,i,i,L,i,i,d,d,d,d,d,d,d,d,d,d,d,d)',/silent
  
;  src=mrdfits('source_list.fits',1,/silent) ;wavdet srclist

  if not keyword_set(wavdet) then src=mrdfits('src_xrtcentroid.fits',1,/silent) else $  
     src=mrdfits('src_wavdetect.fits',1,/silent)
  wsrc=where(src.src_significance gt 3.,ns)
  src=src[wsrc]
  
  
;  get_centroid_pos,'combined_image.fits','sw00222154004xpcw2po_cl_pha40.det',/ximage,src
;  ns=n_elements(src)
  
  grbsep=separation(src.ra,src.dec,mygcnra,mygcndec)
  tmin=min(grbsep,grb)
  n=indgen(ns)
  n=[n[0:grb-1],n[grb+1:*]]
  ssrc=src[n]
  
  raoff=0d & decoff=0d
  sigra=0d & sigdec=0d
  sep=0d
  color=0
  nmatch=intarr(ns)
  for i=0,ns-2 do begin 
     s=separation(ssrc[i].ra,ssrc[i].dec,ra,dec)
     w=where(s lt searchdist,nw)
;     print,nw
     if nw gt 0 then begin 
;        raoff=[raoff,separation(ssrc[i].ra,0,ra[w],0)]
;        decoff=[decoff,separation(0,ssrc[i].dec,0,dec[w])]
        raoff=[raoff,(ssrc[i].ra-ra[w])/cos(ssrc[i].dec*!dtor)*3600.]
        decoff=[decoff,(ssrc[i].dec-dec[w])*3600.]
;        decoff=[decoff,(ssrc[i].dec-dec[w])*cos(ssrc[i].dec*!dtor)*3600.]
        
;        sigra=[sigra,replicate(ssrc[i].ra_err,nw)*3600.]
;        sigdec=[sigdec,replicate(ssrc[i].dec_err,nw)*3600.]
        ;;DO WE INCLUDE THE XRT SYSTEMATIC ERROR ON POSITIONS?
        sigra=[sigra,replicate(hpd*(ssrc[i].counts)^p,nw)]
        sigdec=[sigdec,replicate(hpd*(ssrc[i].counts)^p,nw)]
        
;        decoff=[decoff,(ssrc[i].dec-dec[w])*cos(ssrc[i].dec*!dtor)*3600.]
        sep=[sep,s[w]]
        color=[color,replicate(i,nw)]
        nmatch[n[i]]=nw
     endif 
  endfor 
  
  raoff=raoff[1:*]
  decoff=decoff[1:*]
  sigra=sigra[1:*]
  sigdec=sigdec[1:*]
  sep=sep[1:*]
  color=color[1:*]
  
  ploterror,raoff,decoff,sigra,sigdec,/yno,psym=3,/iso,xtitle='RA offset (")',ytitle='Dec offset (")',/nohat
  uclr=color[uniq(color)]
  
  clr=[!red,!blue,!green,!orange,!yellow,!cyan,!magenta,!deeppink,!purple,!darkred,!royalblue,!salmon,!hotpink,!violet,!grey,!navyblue,!seagreen,!sienna,!forestgreen,!lightgreen,!midnightblue,!darkslategrey,!darkblue,!skyblue,!darkgreen]
  for i=0,n_elements(uclr)-1 do begin
     t=where(color eq uclr[i])
     oploterror,raoff[t],decoff[t],sigra[t],sigdec[t],psym=3,/nohat,errcolor=clr[i]
  endfor 
  
   
  mra=weighted_mean(raoff,sigra,mraerr)
  mdec=weighted_mean(decoff,sigdec,mdecerr)
  
  
  
  
  plots,mra,mdec,psym=2,color=!green
  tvellipse,mraerr,mdecerr,mra,mdec,/data,color=!green
  
;  merr=sqrt(mraerr^2+mdecerr^2+3.7^2)
  print,'Frame shift: '+ntostr(mra)+','+ntostr(mdec);,merr
  sep2=sqrt((raoff-mra)^2+(decoff-mdec)^2)
  msep2=weighted_mean(sep2,sqrt(sigra^2+sigdec^2),unc)
  msep2sig=stddev(sep2)
  
;  merr=sqrt(mraerr^2+mdecerr^2+msep2sig^2+(18/sqrt(477))^2)
  merr=sqrt(mraerr^2+(hpd*(src[grb].counts)^p)^2)
  print,'New err: '+ntostr(merr)
  
  newdec=src[grb].dec-mdec/3600.
  newra=src[grb].ra-mra/3600.*cos(newdec*!dtor)


  compare_plots,newra,newdec,merr
  
  
;  msep=median(sep)
;  std=stddev(sep)
;  g=where(sep lt msep+std); and sep gt msep-std)
;  oplot,raoff[g],decoff[g],psym=1,color=!red
;  tmp=min(abs(sep-msep),ws)
;  plots,raoff[ws],decoff[ws],psym=2,color=!green
;  tvellipse,msep+std,msep+std,raoff[ws],decoff[ws],0,!green,/data
  
;  ploth,raoff,decoff,hist,nxbin=20,nybin=20
;  cntrd,hist.map,13,12,xcen,ycen,4
  
  ;;find some sorta centroid in raoff/decoff distribution
  ;;then get sep from that centroid & stddev for error?
  
  
;  wdra=213.00681d
;  wddec=16.981861d
  
;  find_sources,xrtctdra,xrtctddec,newra,newdec,compra=natsra,compdec=natsdec,suffix='_pha40'
;  find_sources,xrtctdra,xrtctddec,newra,newdec,compra=natsra,compdec=natsdec
  
;  find_sources,wdra,wddec,newra,newdec,compra=natsra,compdec=natsdec,suffix='_nat';'_pha40'
  
  !p.multi=0
  
  if keyword_set(ps) then endplot
  
  stop
  return
end 
