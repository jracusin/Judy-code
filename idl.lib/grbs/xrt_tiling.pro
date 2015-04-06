@fit_functions
@fit_functions_flares
function scale_cts,icts,tiledur,zfact,cts=cts,bcts=bcts

  ;; figure out if wt or pc based on rate and rate*zfact
  ;; determine cts from rate*tiledur & bcts from relations
  ;; calc sig based on cts & bcts
  rate=icts/tiledur*zfact
  cts=rate*tiledur
  bcts=fltarr(n_elements(cts))

  wt=where(rate gt 5.,nwt)
  if nwt gt 0 then $
     bcts[wt]=10^(alog10(rate[wt])*0.61756-1.30226)*tiledur[wt]
  pc=where(rate le 5.,npc)
  if npc gt 0 then $
     bcts[pc]=10^(alog10(rate[pc])*0.56125-2.41456)*tiledur[pc]
  
  sig=cts/sqrt(cts+bcts)

  return,sig
end 
pro avg_brate

  cd,'~/GRBs/'
  grbs=file_search('GRB*')
  ngrbs=n_elements(grbs)

  brate=0.
  exptime=0.
  crate=0.
  type=0
  for i=0,ngrbs-1 do begin
     cd,grbs[i]
     print,grbs[i]
     if exist('UL_lc.fits') then begin
        lc=lcout2fits()
        w=where(lc.type eq 1 or lc.type eq 0)
        crate=[crate,lc[w].src_rate]
        brate=[brate,lc[w].back_ctrate]
;        exptime=[exptime,lc[w].exptime]
        type=[type,lc[w].type]
     endif 
     cd,'~/GRBs/'
  endfor 
  crate=crate[1:*]
  brate=brate[1:*]
;  exptime=exptime[1:*]

  plot,crate,brate,psym=3,/xlog,/ylog,xrange=[1e-5,1e5],yrange=[1e-5,1e2],/iso,/xsty,/ysty
  pc=where(type eq 1 and brate gt 0 and crate gt 0)
  wt=where(type eq 0 and brate gt 0 and crate gt 0)
  rwt=linfit(alog10(crate[wt]),alog10(brate[wt]))
  oplot,crate[wt],10^(alog10(crate[wt])*rwt[1]+rwt[0]),color=!green
  rpc=linfit(alog10(crate[pc]),alog10(brate[pc]))
  oplot,crate[pc],10^(alog10(crate[pc])*rpc[1]+rpc[0]),color=!green
  oplot,[1e-5,1e5],[1e-5,1e5],color=!red
  print,rwt,rpc

;  plothist,brate,bin=0.0001
  stop


  return
end

pro draw_from_contour,ntiles,cra,cdec,wtile
  
  xrtrad=23.6/2./60.
  nc=n_elements(cra)
  
  locprob=file_search('~/Fermi/gbmtrig/glg_locprob_all*fit')
;  locprob=locprob[36] ;; GRB141011282 - typical burst
  locprob=locprob[42] ;; GRB141022087 - best case burst
  probmap=mrdfits(locprob,1,hdr)
;  cra=sxpar(hdr,'CRVAL1')
;  cdec=sxpar(hdr,'CRVAL2')
;  cra=center[0]
;  cdec=center[1]
  pix=sxpar(hdr,'CDELT1')
  xmap=fltarr(512,512)
  ymap=fltarr(512,512)

  if exist('~/Fermi/gbmtrig/glg_locprob_all_bn141022087_v01.arr.txt') then begin
     yesarr=1
     restore,'~/Fermi/gbmtrig/glg_locprob_all_bn141022087_v01.arr.txt'
  endif else yesarr=0
  r=random_pdist(probmap,nc,xrand=xrand,yrand=yrand,xarr=xarr,yarr=yarr,arr=arr)

  wtile=intarr(nc)
  for i=0,nc-1 do begin 

     xmap[0,0]=cra[i]-pix*512/2.
     ymap[0,0]=cdec[i]-pix*512/2.
     for j=0,511 do begin
        xmap[j,*]=xmap[0,0]+pix*j
        ymap[*,j]=ymap[0,0]+pix*j
     endfor 

     xrt_tiling,ntiles[i],x,y,rad,center=[cra[i],cdec[i]],/noplot
     dist=separation(xmap[xrand[i],yrand[i]],ymap[xrand[i],yrand[i]],x,y)/3600.
     w=where(dist le xrtrad)
     wtile[i]=w[0]
  endfor 
  
  if not yesarr then save,arr,filename='~/Fermi/gbmtrig/glg_locprob_all_bn141011282_v01.arr.txt'

  return
end

pro plot_gw

  dir='~/Swift/Tiling/'
  cd,'~/GRBs/'  
  g=mrdfits('~/Swift/swift_grb_properties.fits',1)
  ng=n_elements(g)

  sgrb=mrdfits('~/Swift/jochen_z_list.fits',1)
  match,strtrim(sgrb.grb,2),strtrim(g.grb,2),m1,m2
  match,strtrim(sgrb.grb,2)+'A',strtrim(g.grb,2),m1a,m2a
  m1=[m1,m1a]
  m2=[m2,m2a]
  sm=sort(m1)
  m1=m1[sm]
  m2=m2[sm]

  s=where(sgrb[m1].shb eq 1)

  s=m2[s]
  g=g[s]
  ng=n_elements(g)

  begplot,name=dir+'scaled_gw_xrt_lc_rate.ps',/land,font='helvetica',/color
  yrange=[1e-15,1e-5];[1e-4,1e6]
  xrange=[10,1e7]
  ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)'
  xtitle='Time since trigger (s)'

  yrange2=[-5,-1]
  bin=0.2
  
  z=g.z
  w0=where(z le 0)
  z[w0]=0.8 ;;; median value
  dist400=200.
  dist=lumdist(z)
  zfact=dist^2/dist400^2/kcorr(z,[1.,g.phind],/pow)

;  !x.margin=[2,2]
;  !y.margin=[3,1]
  plot,xrange,yrange,/xlog,/ylog,xtitle=xtitle,ytitle=ytitle,/nodata,/ysty,/xsty,xtickformat='loglabels',ytickformat='loglabels',charsize=2,xminor=9,xrange=xrange,yrange=yrange
  for i=0,ng-1 do begin 
     grb=strtrim(g[i].grb,2)
     bdir='~/GRBs/'+grb+'/'
     lc=lcout2fits(dir=bdir)
     wdet=where(lc.src_rate_err gt 0 and lc.time gt 0)
     lc=lc[wdet]
     time=lc.time
     time=[time,time*2]
     wt=where(time le max(lc.tstop))
     time=time[wt]
     time=time[sort(time)]
     f=call_function(strtrim(g[i].model,2),time,g[i].p)*g[i].unabs_cfratio
     oplot,time,f,color=!grey70
     oplot,time,f*zfact[i];,color=!grey50
  endfor 

  legend,['Observed','Scaled to 200 Mpc'],box=0,/top,/right,textcolor=[!grey70,!p.color]
  endplot
  spawn,'ps2pdf '+dir+'scaled_gw_xrt_lc_rate.ps '+dir+'scaled_gw_xrt_lc_rate.pdf'
  spawn,'convert '+dir+'scaled_gw_xrt_lc_rate.pdf '+dir+'scaled_gw_xrt_lc_rate.png'


  begplot,name=dir+'scaled_gw_xrt_lc_rate_fast_slow.ps',/land,font='helvetica',/color
  yrange=[1e-15,1e-7]
  plot,xrange,yrange,/xlog,/ylog,xtitle=xtitle,ytitle=ytitle,/nodata,/ysty,/xsty,xtickformat='loglabels',ytickformat='loglabels',charsize=2,xminor=9,xrange=xrange,yrange=yrange
  for i=0,ng-1 do begin 
     grb=strtrim(g[i].grb,2)
     bdir='~/GRBs/'+grb+'/'
     lc=lcout2fits(dir=bdir)
     wdet=where(lc.src_rate_err gt 0 and lc.time gt 0)
     lc=lc[wdet]
     time=lc.time
     time=[time,time*2]
     wt=where(time le max(lc.tstop))
     time=time[wt]
     time=time[sort(time)]
     f=call_function(strtrim(g[i].model,2),time,g[i].p)*g[i].unabs_cfratio
     speed=call_function(strtrim(g[i].model,2),1e5,g[i].p)
     if speed lt 1e-5 then color=!red
     if speed ge 1e-5 then color=!blue
     oplot,time,f,color=color
  endfor 
  legend,['Fast Decaying','Slow Decaying'],box=0,/top,/right,textcolor=[!red,!blue]
  endplot
  spawn,'ps2pdf '+dir+'scaled_gw_xrt_lc_rate_fast_slow.ps '+dir+'scaled_gw_xrt_lc_rate_fast_slow.pdf'
  spawn,'convert '+dir+'scaled_gw_xrt_lc_rate_fast_slow.pdf '+dir+'scaled_gw_xrt_lc_rate_fast_slow.png'
  
  f=dblarr(ng)
  for i=0,ng-1 do f[i]=call_function(strtrim(g[i].model,2),1e5,g[i].p)
  fast=where(f lt 1e-5)
  slow=where(f ge 1e-5)

stop
  return
end

pro plot_example

  g=mrdfits('~/Swift/swift_grb_properties.fits',1)
  w=where(strmatch(g.model,'gauss*pow') eq 1,nw)
  sig=[0,1,2,3]
  conf=[0,sigprob(1.),sigprob(2.),sigprob(3.)]
  conflow=(1.-conf)/2.
  confhigh=(1.-conf)/2.+conf

  sim_observations,sim,tiles=1,upload=100

  for j=0,nw-1 do begin
     i=w[j]
;  i=w[2]
     lc=mrdfits('~/GRBs/'+strtrim(g[i].grb,2)+'/UL_lc.fits',1)
     read_lcfit,'~/GRBs/'+strtrim(g[i].grb,2)+'/lc_fit_out_idl_int9.dat',pnames,p
     mo=fit_models(pnames,p,breaks=breaks)
     p1=errorplot(lc.time,lc.src_rate,lc.time-lc.tstart,lc.src_rate_err,errorbar_capsize=0,/xlog,/ylog,xtitle='Time Since Trigger (s)',ytitle='0.3-10 keV Count Rate (cts/s)',title=g[i].grb,linestyle='none',layout=[1,2,1])
;     for q=0,n_elements(lc)-1 do oplot,[lc[q].tstart,lc[q].tstop],[lc[q].src_rate,lc[q].src_rate],color=!black
     t=[lc.time,220,240,260,280,400,500,1000,1200,p[breaks]]
     t=t[sort(t)]
     p2=plot(t,call_function(strtrim(mo,2),t,p),color='green',/overplot,/current)
     jj=2
     wobs=where(sim.noobs eq 0 and sim.tiledur[jj] gt 0,nw)

     p2a=plot([min(sim[wobs].btobs[0,jj]),max(sim[wobs].btobs[1,jj])],[1e-1,1e-1],/overplot,/current,color='red',thick=5)
     med=(sim[wobs].btobs[0,jj]+sim[wobs].btobs[1,jj])/2.
     wm=where(med eq median(med))
     p2b=plot([sim[wobs[wm[0]]].btobs[0,jj],sim[wobs[wm[0]]].btobs[1,jj]],[1e-2,1e-2],/overplot,/current,color='red',thick=5)

     rate=call_function('int'+strtrim(mo,2),sim[wobs].btobs[*,jj],p)
     cts=rate*sim[wobs].tiledur[jj]
     s=sort(cts)
     ctslow=cts[s[round(nw*conflow)]]
     ctshigh=cts[s[round(nw*confhigh)]]

     plothist,cts,bin=1,x,y,/noplot
     yrange=[0,round((max(y)+50)/100.)*100.]
     p3=plot(x,y,layout=[1,2,2],/histogram,/current,xtitle='Counts in Tile 1',yrange=yrange)
     p3a=plot([ctslow[0],ctslow[0]],[0,5000],/current,/overplot,linestyle='--',color='green',yrange=yrange)
     p3b=plot([ctslow[1],ctslow[1]],[0,5000],/current,/overplot,linestyle=':',color='red',yrange=yrange)
     p3c=plot([ctslow[2],ctslow[2]],[0,5000],/current,/overplot,linestyle='-.',color='orange',yrange=yrange)
     p3d=plot([ctslow[3],ctslow[3]],[0,5000],/current,/overplot,linestyle='-:',color='purple',yrange=yrange)
     p3e=plot([ctshigh[1],ctshigh[1]],[0,5000],/current,/overplot,linestyle=':',color='red',yrange=yrange)
     p3f=plot([ctshigh[2],ctshigh[2]],[0,5000],/current,/overplot,linestyle='-.',color='orange',yrange=yrange)
     p3g=plot([ctshigh[3],ctshigh[3]],[0,5000],/current,/overplot,linestyle='-:',color='purple',yrange=yrange)
     print,j

     k=get_kbrd(10)
     if k eq 's' then stop
     p1.refresh
     p1.close
  endfor 
;  w=where(sim.tiledur[0] gt 0,nw)
;  for q=0,nw-1 do oplot,[sim[w[q]].btobs[0,0],sim[w[q]].btobs[1,0]],10^([(q+1.),(q+1)]*2.9e-3)/1e4,color=!red

  stop
  return
end

pro plot_simresults,lat=lat,highflu=highflu,gbm=gbm,short=short,gw1=gw1,gw2=gw2,add2=add2

  tiless=[1,4,7,19,37,61,91]
;  begplot,name='~/Swift/Tiling/tiling_sim.ps',/land,/color,font='helvetica'
;  !x.margin=[4,3]
  det2=100.
  add=''
  if n_elements(add2) eq 0 then add2=''
;  gbmcat=mrdfits('~/Swift/GBM_burstcat.fits',1)
  gbmcat=mrdfits('~/Fermi/GBM_grbcat_4yr.fits',1)

;  xrange=[[1e2,1e5],[1e3,1e5],[1e4,2e5],[2e4,3e5],[7e4,3e5]]
  bin=[100,10,10,5,2,1,0.5]
  for l=0,n_elements(tiless)-1 do begin
;  for l=0,0 do begin
     if l eq 0 then st='' else st='s'
     tiles=tiless[l]
     restore,'~/Swift/Tiling/tiling_sim'+ntostr(fix(tiles))+add2+'.sav'
     g=mrdfits('~/Swift/swift_grb_properties.fits',1)
     ng=n_elements(g)
     wg=indgen(ng)
     add='_all'

     if keyword_set(lat) then begin
        latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090531B','GRB090902B','GRB090926A','GRB091003','GRB091208B','GRB100414A','GRB100728A','GRB110625A','GRB110709A','GRB110731A','GRB120624B','GRB121011A','GRB130305A','GRB130427A','GRB130504C','GRB130518A','GRB130606B','GRB130702A','GRB130907A','GRB131014A','GRB131108A','GRB131231A','GRB140102A','GRB140323A']
        match,strtrim(g.grb,2),latgrbs,m1,m2
        wg=m1
        ng=n_elements(wg)
        add='_lat'
     endif
     if keyword_set(highflu) then begin
        wg=where(g.fluence gt 4.45e-6,ng) ;; top 20% - only selects BAT bursts
        add='_highflu'
     endif 
     if keyword_set(gbm) then begin
        w=where(g.gbmname ne '')
        match,strtrim(g[w].gbmname,2),strtrim(gbmcat.name,2),m1,m2 ;;; have GBM name
        wg=w[m1]
        ng=n_elements(wg)
        add='_gbm'
     endif 
     if keyword_set(short) then begin
        sgrb=mrdfits('~/Swift/jochen_z_list.fits',1)

        match,strtrim(sgrb.grb,2),strtrim(g.grb,2),m1,m2
        w=where(sgrb[m1].shb eq 1,ng)
        wg=m2[w]
        add='_short'
     endif 
     if keyword_set(gw1) or keyword_set(gw2) then begin
        sgrb=mrdfits('~/Swift/jochen_z_list.fits',1)

        match,strtrim(sgrb.grb,2),strtrim(g.grb,2),m1,m2
        if keyword_set(gw1) then begin
           w=where(sgrb[m1].shb eq 1 and sgrb[m1].z gt 0,ng)
           wg=m2[w]
           z=g[wg].z
           add='_gw1'
        
        endif
        if keyword_set(gw2) then begin  ;; biases results way brighter, hmm ....
           w=where(sgrb[m1].shb eq 1,ng)
           wg=m2[w]
           z=g[wg].z
           w0=where(z le 0)
           z[w0]=0.7 ;;; median value
           add='_gw2'
        endif
        dist400=200.
        dist=lumdist(z)
        zfact=dist^2/dist400^2
     endif else zfact=replicate(1.,ng) 

     add=add+add2

     ntimes=n_elements(times)
     med=fltarr(norb)
     totcts=fltarr(ntimes,ng) & medtotcts=fltarr(ntimes)
     sigctslow=fltarr(norb,4) & sigctshigh=fltarr(norb,4) & medt=med & startt=med & stopt=med
     clow=fltarr(norb,4) & chigh=clow
     timenames=['100 s','1 hr','6 hrs','12 hrs','1 day']
     ccts=fltarr(norb,ng) & cmed=fltarr(norb)
     sigdet2=5.

     for k=0,ntimes-1 do begin 
        for j=0,norb-1 do begin
           
;           w=where(medcts[k,j,wg]*zfact gt det,nw)
           w=where(scale_cts(medcts[k,j,wg],mtiledur[k,j],zfact) gt sigdet,nw)
           med[j]=nw*1./ng      ;numuse[5,j,0]

           for sn=0,3 do begin 
;              w=where(ctslow[k,j,wg,sn]/sqrt(ctslow[k,j,wg,sn]+bctslow[k,j,wg,sn])*zfact gt sigdet,nw)
;              w=where(ctslow[k,j,wg,sn]*zfact gt det,nw)
              w=where(scale_cts(ctslow[k,j,wg,sn],mtiledur[k,j],zfact) gt sigdet,nw)
              sigctslow[j,sn]=nw*1./ng
;              w=where(ctshigh[k,j,wg,sn]/sqrt(ctshigh[k,j,wg,sn]+bctshigh[k,j,wg,sn])*zfact gt sigdet,nw)
              w=where(scale_cts(ctshigh[k,j,wg,sn],mtiledur[k,j],zfact) gt sigdet,nw)
;              w=where(ctshigh[k,j,wg,sn]*zfact gt det,nw)
              sigctshigh[j,sn]=nw*1./ng

              nlow=0 & nhigh=0
              for i=0,ng-1 do begin 
;                 if sn eq 0 then ccts[j,i]=total(medcts[k,0:j,wg[i]]*zfact[i])
;                 if total(ctslow[k,0:j,wg[i],sn]*zfact[i]) gt det2 then nlow=nlow+1
;                 if total(ctshigh[k,0:j,wg[i],sn]*zfact[i]) gt det2
;                 then nhigh=nhigh+1
                 if sn eq 0 then ccts[j,i]=scale_cts(total(medcts[k,0:j,wg[i]]),total(mtiledur[k,0:j]),zfact[i])
                 if scale_cts(total(ctslow[k,0:j,wg[i],sn]),total(mtiledur[k,0:j]),zfact[i]) gt sigdet2 then nlow=nlow+1
                 if scale_cts(total(ctshigh[k,0:j,wg[i],sn]),total(mtiledur[k,0:j]),zfact[i]) gt sigdet2 then nhigh=nhigh+1

              endfor 
              clow[j,sn]=nlow*1./ng
              chigh[j,sn]=nhigh*1./ng
           endfor 
          
;           w=where(ccts[j,*] gt det2,nw)
           w=where(ccts[j,*] gt sigdet2,nw)
           cmed[j]=nw*1./ng

           medt[j]=median(medtime[k,j,wg])
           startt[j]=median(starttime[k,j,wg])
           stopt[j]=medt[j]*2.-startt[j]

        endfor 
;        for i=0,ng-1 do totcts[k,i]=total(medcts[k,wg,i]*zfact)
;        medtotcts[k]=median(totcts[k,*])
        if k eq 0 then current=0 else current=1
        p=plot([1e2,2e5],[0,1],/nodata,/xlog,xtitle='T-T0 (s)',ytitle='Fraction Detectable',font_size=10,title=ntostr(fix(tiles))+' tile'+st+', Tupload='+timenames[k],xrange=[1e3,3e5],yrange=[0,1],layout=[2,3,k+1],current=current,margin=[0.2,0.2,0.1,0.17])

;        color=[!purple,!green,!blue,!orange]
;        gcolor=[0,!grey40,!grey60,!grey80]
        gcolor=['black','dark gray','gray','dim gray']
;        ccolor=[!navyblue,!blue,!dodgerblue,!lightsteelblue]
        ccolor=['navy','blue','dodger blue','sky blue']
        for sn=3,1,-1 do begin
           tt=[startt,medt,stopt]
           s=sort(tt)
           tt=tt[s]

           clo=[clow[*,sn],clow[*,sn],clow[*,sn]]
           chig=[chigh[*,sn],chigh[*,sn],chigh[*,sn]]
           clo=clo[s]
           chig=chig[s]

           x=[tt,reverse(tt)]
           y=[clo,reverse(chig)]
           pg1=polygon(x,y,fill_color=ccolor[sn],color=ccolor[sn],layout=[2,3,k+1],/current,/data,target=p,transparency=60)

        endfor 

        for sn=3,1,-1 do begin
           tt=[startt,medt,stopt]
           s=sort(tt)
           tt=tt[s]

           low=[sigctslow[*,sn],sigctslow[*,sn],sigctslow[*,sn]]
           high=[sigctshigh[*,sn],sigctshigh[*,sn],sigctshigh[*,sn]]
           low=low[s]
           high=high[s]
           x=[tt,reverse(tt)]
           y=[low,reverse(high)]
;           polyfill,x,y,color=gcolor[sn]
           pg2=polygon(x,y,fill_color=gcolor[sn],color=gcolor[sn],layout=[2,3,k+1],/current,/data,target=p,transparency=40)

        endfor 

;        oplot,medt,sighigh,line=2
        p1=plot(medt,med,symbol='*',sym_filled=1,sym_size=0.5,layout=[2,3,k+1],/current,/overplot)
        p2=plot(medt,med,layout=[2,3,k+1],/current,/overplot)

        p3=plot(medt,cmed,color=ccolor[0],layout=[2,3,k+1],/current,/overplot)
        p4=plot(medt,cmed,color=ccolor[0],symbol='circle',sym_filled=1,sym_size=0.5,layout=[2,3,k+1],/current,/overplot)
        p5=plot([times[k],times[k]],[0,1],color='red',linestyle='--',/current,/overplot)
        right=1 & top=1 & bottom=0 & left=0
        if l eq 0 and k le 2 then begin 
           top=0 & right=0 & bottom=1 & left=1
        endif 
        if k eq 2 then begin 
;           t1=text(0.12,0.55,'individual tile ('+ntostr(fix(det))+' cts)',/overplot,/current,color='gray',font_size=10)
;           t2=text(0.12,0.52,'cumulative ('+ntostr(fix(det2))+'
;           cts)',/overplot,/current,color='dodger blue',font_size=10)
           t1=text(0.12,0.55,'individual tile ('+ntostr(fix(sigdet))+'$\sigma$)',/overplot,/current,color='gray',font_size=10)
           t2=text(0.12,0.52,'cumulative ('+ntostr(fix(sigdet2))+'$\sigma$)',/overplot,/current,color='dodger blue',font_size=10)
 
        endif 
;        legend,['individual tile ('+ntostr(fix(det))+' cts)','cumulative ('+ntostr(fix(det2))+' cts)'],box=0,top=top,right=right,bottom=bottom,left=left,textcolor=[!p.color,!red],charsize=1
;     legend,[ntostr(fix(tiles))+' tiles','Tstart='+ntostr(round(times[k]))],box=0,/bottom,/left,charsize=1.5
;     key=get_kbrd(10)

     endfor 

     ;;; plot histogram of tile durations - only need first 2 orbits

     w=where(tiledur(1,1,*) gt 0)
;     h0=histogram(tiledur[0,0,w],binsize=bin[l],locations=l0)
;     h1=histogram(tiledur[0,1,w],binsize=bin[l],locations=l1)
;     plothist,tiledur[0,0,w],bin=bin[l],l0,h0,/noplot
     plothist,tiledur[1,1,w],bin=bin[l],l1,h1,/noplot

     p=plot([l1[0],l1,max(l1)+bin[l]],[0,h1,0],/histogram,xtitle='Tile Duration (s)',ytitle='N',title=ntostr(fix(tiles))+' tile'+st,/current,layout=[2,3,6],margin=[0.2,0.2,0.1,0.17],font_size=10)
;     p=plot([l1[0],l1,max(l1)+bin[l]],[0,h1,0],/histogram,/current,/overplot,color='red')
     

;     !p.multi=0
;  contour,probmap,timesarr,timemap,/xlog,/ylog,xtitle='Tstart (s)',ytitle='T-T0 (s)',levels=[0.2,0.4,0.6,0.8],xrange=[1e2,1e5],yrange=[1e2,1e5],c_annotation=['0.2','0.4','0.6','0.8'],c_charsize=2.
;  oplot,[1e2,1e6],[1e2,1e6]

;     stop
     t=ntostr(fix(tiles))
     if tiles lt 10 then t='0'+t
     p.save,'~/Swift/Tiling/tiling_sim'+t+add+'.png'
     p.refresh
     p.close
     
  endfor 


  stop

  return
end 

pro sim_grbobs,diagnostic=diagnostic,gbmprob=gbmprob ;,lat=lat

  ;;; given all GRB LCs, what is probability of all bursts being detected
  ;;; over N counts during the first day of tiles on random positions
  ;;; accross the sky

  ;; ~7-9% never observable due to pole constraint

  ;; right now ignoring background, PSF correction
  ;; right now only works for XRT
  ;;  - need sensitivities and conversion fluxes for Lobster

  pt=systime(1)
  g=mrdfits('~/Swift/swift_grb_properties.fits',1)
  ng=n_elements(g)
  add=''
  if keyword_set(gbmprob) then begin
     print,'Using GBM probability contour'
     add2='_gbmprob' 
  endif else begin
     add2=''
     print,'Using equal tile probability'
  endelse 

  det=10. ;; cts to a detection
  sigdet=3. ;; sigma for a detection (to replace det)
  sigdet2=5.
  tiles0=[1,4,7,19,37,61,91]
  threshold=5e-4

  norb=16. ;; 1 day
  day=86400.
  f=findgen(9)+1
;  times=[f*1e2,f*1e3,f[0:4]*1e4]
  times=[100,3600.,6*3600.,12.*3600.,24.*3600.]
  ntimes=n_elements(times)

  sig=[0,1,2,3]
  conf=[0,sigprob(1.),sigprob(2.),sigprob(3.)]
  conflow=(1.-conf)/2.
  confhigh=(1.-conf)/2.+conf
  nsim=2592.

;  brate=1e-4  ;;; not sure about this????

  for t=0,n_elements(tiles0)-1 do begin
;  for t=0,1 do begin 
     tiles=tiles0[t]
     mfrac=fltarr(ntimes) & mfracsiglow=mfrac & mfracsighigh=mfrac
     frachist=fltarr(ntimes,ng) & frachist2=fltarr(ntimes,norb,ng)
     medcts=fltarr(ntimes,norb,ng) & ctslow=fltarr(ntimes,norb,ng,4) & ctshigh=ctslow
     medtime=medcts & starttime=medtime & num=medtime & tiledur=fltarr(ntimes,norb,nsim)
;     bmedcts=medcts & bctslow=ctslow & bctshigh=ctshigh & 
     mtiledur=fltarr(ntimes,norb)
;  alltime=fltarr(ntimes,norb,ng,nsim) & allcts=fltarr(ntimes,norb,ng,nsim)
     for k=0,ntimes-1 do begin 
;  dok=5
;  for k=dok,dok do begin
        upload=times[k]         ;300.
        print,'Tiles = ',tiles
        print,'Upload time = ',upload/60.,' min'

        sim_observations,sim,tiles=tiles,upload=upload,gbmprob=gbmprob ;,nsim=nsim
        
        frac=fltarr(ng) & frac2=fltarr(norb,ng)
        for i=0,ng-1 do begin
           wp=where(g[i].p ne 0,np)
           if np gt 0 then begin 
              if keyword_set(diagnostic) then begin
                 lc=mrdfits('~/GRBs/'+strtrim(g[i].grb,2)+'/UL_lc.fits',1)
                 ploterror,lc.time,lc.src_rate,lc.src_rate_err,/xlog,/ylog,/nohat,psym=3,xrange=[10,1e5],yrange=[1e-3,1e3]
                 for j=0,n_elements(lc)-1 do oplot,[lc[j].tstart,lc[j].tstop],[lc[j].src_rate,lc[j].src_rate]
              endif 
;        print,g[i].grb
              wobs=where(sim.noobs eq 0,nwobs)

              tcts=fltarr(nsim)
              for j=0,norb-1 do begin 
                 wobs=where(sim.noobs eq 0 and sim.tiledur[j] gt 0,nwobs1) ; and sim.btobs[0,j] lt day,nwobs1)
                 cts=fltarr(nsim) & rate=fltarr(nsim) & bcts=fltarr(nsim)
                 if nwobs1 gt 0 then begin 
                    rate[wobs]=call_function('int'+strtrim(g[i].model,2),sim[wobs].btobs[*,j],g[i].p*1d)
                    wnsig=where(rate le threshold,nwnsig)
                    if nwnsig gt 0 then rate[wnsig]=0
                    cts[wobs]=rate[wobs]*sim[wobs].tiledur[j]
;                   bcts[wobs]=brate*sim[wobs].tiledur[j]
;;                    using simple PL fit to src vs background for
;;                    large sample of bursts
                    wt=where(rate[wobs] gt 5.,nwt)
                    if nwt gt 0 then $
                       bcts[wobs[wt]]=10^(alog10(rate[wobs[wt]])*0.61756-1.30226)*sim[wobs[wt]].tiledur[j]
                    pc=where(rate[wobs] le 5.,npc)
                    if npc gt 0 then $
                       bcts[wobs[pc]]=10^(alog10(rate[wobs[pc]])*0.56125-2.41456)*sim[wobs[pc]].tiledur[j]
;                    cts[wobs]=cts[wobs]-bcts[wobs]  ;; subtract off background cts - wrong!
                    tcts[wobs]=tcts[wobs]+cts[wobs]
                    sim[wobs].bcts[j]=reform(bcts[wobs],nwobs1)
                    sim[wobs].cts[j]=reform(cts[wobs],nwobs1)
                    sim[wobs].tcts=reform(tcts[wobs],nwobs1)
                    sim[wobs].rate[j]=reform(rate[wobs],nwobs1)

                    w0=where(sim.cts[j] ge 0 and sim.tiledur[j] gt 0,nw0)
                    medcts[k,j,i]=median(sim[w0].cts[j])
                    s=sort(sim[w0].cts[j])
                    ctslow[k,j,i,*]=sim[w0[s[round(nw0*conflow)]]].cts[j]
                    ctshigh[k,j,i,*]=sim[w0[s[round(nw0*confhigh)]]].cts[j]

;                    bmedcts[k,j,i]=median(sim[w0].bcts[j])
;                    s=sort(sim[w0].bcts[j])
;                    bctslow[k,j,i,*]=sim[w0[s[round(nw0*conflow)]]].bcts[j]
;                    bctshigh[k,j,i,*]=sim[w0[s[round(nw0*confhigh)]]].bcts[j]

                    medtime[k,j,i]=median((sim[w0].btobs[0,j]+sim[w0].btobs[1,j])/2.)
                    starttime[k,j,i]=median(sim[w0].btobs[0,j])
                    mtiledur[k,j]=median(sim[w0].tiledur[j])
                    if i eq 0 then tiledur[k,j,w0]=sim[w0].tiledur[j]
                    num[k,j,i]=nw0
                 endif
              endfor 
;              if keyword_set(diagnostic) then begin
;                 key=get_kbrd(10)
;                 if key eq 's' then stop
;              endif 
           endif 
        endfor
     endfor 
     

     ptime,systime(1)-pt
     save,filename='~/Swift/Tiling/tiling_sim'+ntostr(fix(tiles))+add+add2+'.sav'
  endfor 
;  stop

  ;;; diagnostics 
  ;; tstart vs probability of detecting N counts
  ;; cts in tiles vs time

  ;; questions
  ;; given specific tiling config, probability of detection vs start time
  ;; given specific tiling config, how long worth observing

  return
end

pro sim_observations,sim,tiles=tiles,upload=upload,gbmprob=gbmprob
  
  ;;; generate nsim positions on sky, and determine their tile viewing windows


  common seed,seed1,seed2,r3
  nsim=2592.  ;;; 5 deg grid
;  if n_elements(nsim) eq 0 then nsim=1000
  norb=16. ;; 1 day
  sim=create_struct('lon',0.,'lat',0.,'tupload',0.,'obswindow',0.,'tview',fltarr(2),$
                    'tobs',0.,'twindow',fltarr(2),'del',fltarr(16),$
                    'ntiles',0,'tile_overhead',0.,'noobs',0,$
                    'btile',0,'tiledur',fltarr(16),'btobs',fltarr(2,norb),$,
                    'rate',fltarr(16),'cts',fltarr(16),'bcts',fltarr(16),'tcts',0.)
  sim=replicate(sim,nsim)
  
  ;;; observation window

  if n_elements(upload) eq 0 then upload=300.
  sim.tupload=upload        ;;; minimum time to upload burst position in seconds - set at 5 min for now
  torbit=96.*60. ;;; orbital period
  if n_elements(tiles) eq 0 then tiles=7.
  sim.ntiles=tiles  ;;; # tile sequence
  sim.tile_overhead=20. ;;; slew time, what else?

  sim_grid,lon,lat
  sim.lon=lon[*]
  sim.lat=lat[*]
  
  sim.tobs=sim.tupload ;;; add in other delays here later
  for i=0,nsim-1 do begin 
     sim_orbit,lon[i],lat[i],tview
     sim[i].tview=tview
     sim[i].obswindow=tview[1]-tview[0]

     if sim[i].tobs lt sim[i].tview[1] then $
        sim[i].twindow=[max([sim[i].tobs,sim[i].tview[0]]),sim[i].tview[1]] else begin
        if sim[i].tview[1] ne -1 then begin 
           orbvis=round(sim[i].tobs/torbit-0.5)+1
           sim[i].twindow=sim[i].tview+torbit*orbvis
;           if sim[i].tobs gt 5400 then stop
        endif 
     endelse 
  endfor 
  sim.del[0]=sim.twindow[1]-sim.twindow[0]
  
  for i=0,norb-1 do begin 
     if i gt 0 then sim.del[i]=sim.tview[1]-sim.tview[0]
     sim.tiledur[i]=sim.del[i]/sim.ntiles-sim.tile_overhead
  endfor 

  w=where(sim.tiledur[0] lt 0)
  sim[w].tiledur[0]=0
;  print,'viewable: ',tview
;  print,'obs starts: ',tobs
;  print,'resultant window: ',twindow
;  print,'window del: ',del

  ;; need to do:
  ;; place burst randomly within contour (use typical GBM contour)
  ;; create tiling pattern with ntiles
  ;; which tile is the burst in?  that's r3
  ;; if it's not in any then need to return that and make it a
  ;; non-detection

  if keyword_set(gbmprob) then begin 
     draw_from_contour,sim.ntiles,sim.lon,sim.lat,r3
     sim.btile=r3
  ;;; doesn't include overlap between tiles
  endif else begin 
     r3=randomu(seed3,nsim) ;;;randomness of which tile contains burst
     sim.btile=fix(sim.ntiles*r3)
  endelse 
;  print,'burst tile: ',btile
;  print,'tile dur: ',tiledur
     sim.btobs[0,0]=sim.twindow[0]+(sim.tiledur[0]+sim.tile_overhead)*sim.btile+sim.tile_overhead
     sim.btobs[1,0]=sim.twindow[0]+(sim.tiledur[0]+sim.tile_overhead)*(sim.btile+1)
     w=where(sim.tview[1] lt sim.tobs and sim.tview[1] ne -1,nw)
     addorb=intarr(nsim)
     addorb[w]=round(sim[w].tobs/torbit-0.5)+1
     for i=1,norb-1 do begin ;;; wrong for tobs>tview[0]
        sim.btobs[0,i]=sim.tview[0]+sim.tiledur[i]*sim.btile+torbit*(i+addorb)+sim.tile_overhead
        sim.btobs[1,i]=sim.tview[0]+sim.tiledur[i]*(sim.btile+1)+torbit*(i+addorb)
     endfor 
     w=where(r3 eq -1 or (sim.tview[0] eq -1 and sim.tview[1] eq -1 and sim.obswindow le 20))
     sim[w].noobs=1 

;  print,'burst obs: ',btobs
  return
end 

pro sim_grid,lon,lat,doplot=doplot

  ;; no longer a grid, but randomly distributed on sky with equal area
  lon=0.
  lat=0.
  np=2592.
  npgen=round(randomu(seed,np)*np*1.)*1.;findgen(np)
  lat=reverse(acos((npgen/(np+1.)*2.-((np-1.)/(np+1.))))*!radeg-90.)
  lon=randomu(seed,np)*360.
  ;; for i=0,n_elements(lat0)-1 do begin
  ;;    n=round(sin(!pi/2.-lat0[i]*!dtor)/sin(!pi/2.)*72.)
  ;;    if n eq 0 then begin
  ;;       lon=[lon,0.] 
  ;;       lat=[lat,lat0[i]]
  ;;    endif else begin
  ;;       ngen=round(randomu(seed,n)*n*1.)*1.;findgen(n)
  ;;       lon=[lon,ngen/n*360.]
  ;;       lat=[lat,replicate(lat0[i],n)]
  ;;    endelse
  ;; endfor 
  ;; lat=lat[1:*]
  ;; lon=lon[1:*]

;     lat[i,*]=reverse(acos((findgen(36)/35.)*2.-1.)*!radeg-90.)
;     n=round(sin(!pi/2.-lat[i,0]*!dtor)/sin(!pi/2.)*72.)
;     print,n
;     if n gt 0 then lon[i,*]=[findgen(n)/(n-1)*360.,replicate(0.,36-n)]
;  endfor
;  w=where(lon ne 0)
;  lat=lat[w]
;  lon=lon[w]

;  for i=0,71 do begin
;     for j=0,35 do begin
;        lon[i,j]=i*5.
;        lat[i,j]=j*5-90.
;     endfor
;  endfor 

;;   rlon=randomu(seed,100)*360.
;;   rlat=randomu(seed,100)*180.-90.
;;   b=fltarr(100)
;;   for i=0,99 do begin
;;      dist=separation(rlon[i],rlat[i],lon,lat)/3600.
;;      w=where(dist lt 20,nw)
;;      area=(sin((rlat[i]+20)*!dtor)-sin((rlat[i]-20)*!dtor))*((rlon[i]+20)-(rlon[i]-20))*!dtor
;;      b[i]=nw/area
;; ;     print,rlon[i],rlat[i],nw/area
;;   endfor 
;;   plothist,alog10(b),bin=0.1

  if keyword_set(doplot) then begin
;     map_set,/aitoff
;     oplot,lon,lat,psym=1
;     euler,lon,lat,lon2,lat2,1
     lat2=lat & lon2=lon
     m=map('Orthographic',label_position=0)
     m.mapgrid.label_show=0
     m.mapgrid.linestyle='none'

     p=plot(lon2,lat2,symbol='*',/overplot,linestyle='none',sym_size=0.5,/current)

     m.save,'~/Swift/Tiling/map_simgrid.png'
     m.refresh
     m.close

  endif 

end

pro sim_orbit,orblon,orblat,tview,doplot=doplot

  ;;; generate random positions on the sky, and see when they are
  ;;; viewable by follow-up telescope

  common seed,seed1,seed2,r3

;  r1=randomu(seed1,1)
;  r2=randomu(seed2,1)
;  orblat=r1*180.-90.
;  orblon=r2*360.

  ;;; random place on sky for burst
  torbit=90.*60. ;;; orbital period

  earthcon=67.+28.
  earthgood=180.-earthcon

  time=findgen(torbit)
  ang=time/max(torbit)*360.
  morblon=orblon[0]+ang
  w=where(morblon gt 360)
  morblon[w]=morblon[w]-360.
  morblat=replicate(orblat,n_elements(morblon))

  dist=separation(morblon,morblat,0.,0.)/3600.
  good=where(dist lt earthgood,ngood)
  bad=where(dist ge earthgood)

  frac=ngood*1./n_elements(time)

  tview=minmax(time[good])

  if tview[0] eq 0 and ngood gt 0 then begin 
     w=where(time gt torbit/2.)
     time2=time
     time2[w]=time[w]-torbit
     tview=minmax(time2[good])
  endif
  if ngood eq 0 then tview=[-1,-1]

  if keyword_set(doplot) then begin 
     print,tview,frac*torbit/60.
;     map_set,/aitoff,/grid

     m=map('Hammer',label_show=0,limit=[-90,-180,90,180],grid_longitude=30,grid_latitude=30)
;     m.mapgrid.label_show=0
;     m.mapgrid.linestyle='none'

     if ngood gt 0 then $
        p=plot(morblon[good],morblat[good],symbol='*',/overplot,linestyle='none',sym_size=0.5,color='green',/current)

     p2=plot(morblon[bad],morblat[bad],symbol='*',color='red',/overplot,sym_size=0.5,linestyle='none')
     p3=symbol(orblon,orblat,'Star',sym_size=2,/overplot,/data,/current,/sym_filled,sym_color='orange')

     elon=fltarr(91) & elat=elon
     for ct=0,90 do begin
        temp=ll_arc_distance([0,0]*!dtor,earthgood*!dtor,ct*4.*!dtor)
        elon[ct]=temp[0]*!radeg
        elat[ct]=temp[1]*!radeg
     endfor
     w=where(elon lt 0)
     elon[w]=elon[w]+360.

     p4=plot(elon,elat,color='blue',/overplot,/current,thick=5)

     t=text(0.3,0.8,'Lon, Lat = '+ntostr(round(orblon))+', '+ntostr(round(orblat))+'        Tview = '+ntostr(round(tview[0]))+' - '+ntostr(round(tview[1]))+' s')

;     if ngood gt 0 then oplot,morblon[good],morblat[good],psym=1,color=!green
;     oplot,morblon[bad],morblat[bad],psym=1,color=!red
;     plots,orblon,orblat,psym=2
     m.save,'~/Swift/Tiling/map_simview_'+ntostr(round(orblon))+'_'+ntostr(round(orblat))+'.png'
     m.refresh
     m.close
  endif 
;stop
;  endfor 

  return
end 

pro simulation0
  g=mrdfits('~/Swift/swift_grb_properties.fits',1)
  ng=n_elements(g)
;  x=dindgen(9)+1
  x2=(dindgen(99)+1)/10.
  time=[x2*10,x2*1d3,x2*1d5,x2*1d7];,x2*1d5,x2*1d6,x2*1d7]
  det=10. ;; cts to a detection
  exposure=dblarr([n_elements(time),ng])

  for i=0,ng-1 do begin
     wp=where(g[i].p ne 0,np)
     if np gt 0 then begin 
        f=call_function(strtrim(g[i].model,2),time,g[i].p[wp])
        w=where(time ge g[i].tstart and time le g[i].tstop and f lt 1e4,nw)

        t=10/f ;;; not instantaneous, need to integrate
        plot,time[w],t[w],/xlog,/ylog
        exposure[w,i]=t[w]
;        k=get_kbrd(10)
;        if k eq 's' then stop
        ;;; give a tstart, functional form, how long take to get detection

     endif 
  endfor 

  stop


  return
end 
pro feasibility

  restore,'~/Swift/Tiling/idlsave.dat'
  w=where(v ne 0)
  v=v[w]
  s=sort(v)
  v=v[s]
  nv=n_elements(v)

  t=[580,322.9,106.3,44.9,19.3,6.4]
  nt=n_elements(t)
  n=[4,7,19,37,61,91]
  m=median(v)
  p=sigprob(1.)
  sig1=round([(1-p)/2.*nv,((1.-p)/2.+p)*nv])
  p=sigprob(2.)
  sig2=round([(1-p)/2.*nv,((1.-p)/2.+p)*nv])
  p=sigprob(3.)
  sig3=round([(1-p)/2.*nv,((1.-p)/2.+p)*nv])

  begplot,name='~/Swift/Tiling/ratehist_1hr.ps',/land,/color,font='helvetica'
  plothist,alog10(v),bin=0.1,xtitle='log XRT Count Rate (cts/s)',ytitle='N'
  oplot,alog10([m,m]),[0,100]
  oplot,alog10(v[sig1[[0,0]]]),[0,100],line=1
  oplot,alog10(v[sig1[[1,1]]]),[0,100],line=1
  oplot,alog10(v[sig2[[0,0]]]),[0,100],line=2
  oplot,alog10(v[sig2[[1,1]]]),[0,100],line=2
  oplot,alog10(v[sig3[[0,0]]]),[0,100],line=3
  oplot,alog10(v[sig3[[1,1]]]),[0,100],line=3
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/ratehist_1hr.ps ~/Swift/Tiling/ratehist_1hr.pdf'


  begplot,name='~/Swift/Tiling/counts_1storb.ps',/land,/color,font='helvetica'

  plot,n,t*m,xtitle='# Tiles',ytitle='Counts is 1st orbit',yrange=[1,1000],/ylog
  oplot,n,t*v[sig1[0]],line=1
  oplot,n,t*v[sig1[1]],line=1
  oplot,n,t*v[sig2[0]],line=2
  oplot,n,t*v[sig2[1]],line=2
  oplot,n,t*v[sig3[0]],line=3
  oplot,n,t*v[sig3[1]],line=3

  oplot,[0,100],[20,20],color=!red,line=2

  legend,['median','1 sigma','2 sigma','3 sigma'],line=[0,1,2,3],box=0,/top,/right

  endplot
  spawn,'ps2pdf ~/Swift/Tiling/counts_1storb.ps ~/Swift/Tiling/counts_1storb.pdf'

  ;; pick start time, color code detectability of each LC with tile duration

  g=mrdfits('~/Swift/swift_grb_properties.fits',1)
  ng=n_elements(g)
  start=10.
  mincts=20.
  begplot,name='~/Swift/Tiling/tile_lcs.ps',/color,/land,font='helvetica'
  x=dindgen(9)+1
  x2=(dindgen(99)+1)/10.
  time=[x2*10,x2*1e2,x2*1e3,x2*1e4,x2*1e5,x2*1e6,x2*1e7]
  color=[!red,!orange,!purple,!magenta,!green,!blue,!cyan]

  plot,[10,1e7],[1e-4,1e4],/nodata,/xlog,/ylog,xtitle='Time',ytitle='Count Rate (cts/s)',yminor=9,ytickv=[1e-4,1e-3,1e-2,1e-1,1,1e1,1e2,1e3,1e4],yticks=8
  for i=0,ng-1 do begin
     wp=where(g[i].p ne 0,nwp)
     mo=strtrim(g[i].model)
     tmp=execute('f='+mo+'(time,g[i].p[wp])')
     w=where(time ge g[i].tstart and time le g[i].tstop,nw)
     if nw gt 1 then oplot,time[w],f[w],color=!grey50
     for j=0,nt-1 do begin 
        minrate=mincts/t[j]
        w=where(f ge minrate and time ge g[i].tstart and time le g[i].tstop,nw)
        if nw gt 1 then oplot,time[w],f[w],color=color[j]
     endfor 
    
  endfor 

  legend,ntostr(n)+' tiles ('+numdec(t,1)+' s)',box=0,/top,/right,textcolor=color
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/tile_lcs.ps ~/Swift/Tiling/tile_lcs.pdf'

  begplot,name='~/Swift/Tiling/lcs_plot.ps',/color,/land,font='helvetica'

  plot,[10,1e7],[1e-4,1e4],/nodata,/xlog,/ylog,xtitle='Time',ytitle='Count Rate (cts/s)',yminor=9,ytickv=[1e-4,1e-3,1e-2,1e-1,1,1e1,1e2,1e3,1e4],yticks=8
  for i=0,ng-1 do begin
     wp=where(g[i].p ne 0,nwp)
     mo=strtrim(g[i].model)
     tmp=execute('f='+mo+'(time,g[i].p[wp])')
     w=where(time ge g[i].tstart and time le g[i].tstop and f lt 1e4,nw)
     if nw gt 1 then oplot,time[w],f[w],color=!grey50
  endfor 
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/lcs_plot.ps ~/Swift/Tiling/lcs_plot.pdf'



  return
end 

pro gbm_plot,zoom=zoom
  xrtrad=23.6/2./60.
  r=700/60./60.
  g=r*sqrt(3.)
  g2=r*sqrt(2.)

  readcol,'~/proposals/swift_gi_10/Valerie/glg_loclist_all_bn120624933_v02.txt',gra,gdec

  ra=gra[0]
  dec=gdec[0]

  xrange=[157,187]
  yrange=[-8,20]
  add=''
  if keyword_set(zoom) then begin
     xrange=[167,177]
     yrange=[1,11]
     add='_zoom'
  endif 

  begplot,name='~/proposals/swift_gi_10/Valerie/tiling_loc'+add+'.ps',/color,font='helvetica'
  plot,xrange,yrange,/iso,/nodata,xrange=xrange,yrange=yrange,/xsty,/ysty,xtitle='RA (degrees)',ytitle='Dec (degrees)'
  ras=ra+[0,-g,g,g*0.5,-g*0.5,-g*0.5,g*0.5,g*2.,-g*2.,g*1.5,-g*1.5,-g*1.5,g*1.5,g,g,-g,-g,0,0,g*0.5,-g*0.5,g*1.5,-g*1.5,g*0.5,-g*0.5,g*1.5,-g*1.5,g*3.,-g*3.,g*2.5,-g*2.5,-g*2.5,g*2.5,g*2.,g*2,-g*2.,-g*2.]
  decs=dec+[0,0,0,g2,-g2,g2,-g2,0,0,g2,-g2,g2,-g2,-g2*2.,g2*2.,-g2*2.,g2*2.,-g2*2.,g2*2.,g2*3.,g2*3.,g2*3.,g2*3.,-g2*3.,-g2*3.,-g2*3.,-g2*3.,0,0,g2,-g2,g2,-g2,-g2*2.,g2*2.,-g2*2.,g2*2.]

  for i=0,36 do tvcircle,xrtrad,ras[i],decs[i],/data

  w1=indgen(71)+1
  w2=indgen(119)+71+1
  w3=indgen(231)+119+71+1
  plots,ra,dec,psym=2,color=!green,symsize=0.5
  oplot,gra[w1],gdec[w1],color=!red
  xyouts,171.5,10.5,'1'+!tsym.sigma,color=!red
  if not keyword_set(zoom) then begin
     oplot,gra[w2],gdec[w2],color=!red
     oplot,gra[w3],gdec[w3],color=!red
     xyouts,171.5,13.5,'2'+!tsym.sigma,color=!red
     xyouts,171.5,18.5,'3'+!tsym.sigma,color=!red
  endif 
  rr=g*3.+xrtrad
;  tvcircle,rr,ra,dec,color=!red
  endplot
;  spawn,'convert ~/proposals/swift_gi_10/Valerie/tiling_loc.ps'
  
  

stop
  return
end 

pro xrt_tiling_plot

  xrtrad=23.6/2.
;  r=xrtrad*0.9
  r=700/60.
  g=r*sqrt(3.)
  g2=r*sqrt(2.)
  yrange=[-2,3]
  xrange=[-2,2]
  begplot,name='~/Swift/Tiling/tiling_plot.ps',/color,font='helvetica'
  !p.multi=[0,3,3]
;  !x.margin=[2,2]
;  !y.margin=[2,2]
;  plot,range,range,/iso,/nodata,title='4 tiles',xrange=range,yrange=range,/xsty,/ysty,charsize=2.0

  ntiles=[1,4,7,19,37,61,91]
  for j=0,n_elements(ntiles)-1 do begin
     plot,xrange,yrange,/iso,/nodata,title=ntostr(ntiles[j])+' tiles',xrange=xrange,yrange=yrange,/xsty,/ysty,charsize=2.0,xmargin=0.15,ymargin=0.2
     xrt_tiling,ntiles[j],x,y,rad,rad2,/noplot,center=[10,0]
     for i=0,ntiles[j]-1 do begin
        skycircle,x[i],y[i],xrtrad/60.,xx,yy
        oplot,xx-10,yy
     endfor 
     skycircle,10,0,rad,xx,yy
     oplot,xx-10,yy,color=!red
     skycircle,10,0,rad2,xx,yy
     oplot,xx-10,yy,color=!green
     legend,['r='+numdec(rad,2)+'deg','r='+numdec(rad2,2)+'deg'],box=0,/top,/left,charsize=1,textcolor=[!red,!green]
     if j eq 4 then plot,indgen(10),color=!white
     
  endfor 

  plot,xrange,yrange,/iso,/nodata,title='Arbitrary tiles',xrange=xrange,yrange=yrange,/xsty,/ysty,charsize=2.0,xmargin=0.15,ymargin=0.2
  xrt_tiling,91,x,y,rad,rad2,/noplot,center=[10,0]
;  i=[85,56,33,16,5,0,2,10,24,44,70,71,45,25,11,3,4,15,32,55,84] ;;
;  long line
  i=[19,32,0,23,24,45,46,47,27,28] ;; smiley face
  i=[63,64,65,66,67,68,42,23,10,25,46,26,12,28,50,77,78,79,80,81,82,31,15,16,34,35,18,7,38,20,54,37,55,17,39,6,5,53,40,21,8,1,0,4,14,30,52,41,22,9,2,3,13,29,51,11] ;; batman
  x=x[i]
  y=y[i]
;  xrt_tiling,7,x,y,rad,rad2,/noplot,center=[10,0]
;  xrt_tiling,7,x2,y2,rad,rad2,/noplot,center=[x[5],y[5]]
;  xrt_tiling,7,x3,y3,rad,rad2,/noplot,center=[x[2],y[2]]
;  xrt_tiling,7,x3a,y3a,rad,rad2,/noplot,center=[x3[2],y3[2]]
;  xrt_tiling,7,x4,y4,rad,rad2,/noplot,center=[x2[5],y2[5]]
;  xrt_tiling,7,x4a,y4a,rad,rad2,/noplot,center=[x4[5],y4[5]]

;  x=[x,x2,x3,x4,x3a,x4a]
;  y=[y,y2,y3,y4,y3a,y4a]
  for i=0,n_elements(x)-1 do begin
     skycircle,x[i],y[i],xrtrad/60.,xx,yy
     oplot,xx-10,yy
  endfor 
  

  
  !p.multi=0
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/tiling_plot.ps ~/Swift/Tiling/tiling_plot.pdf'
stop
  return
end 
  
pro xrt_tiling,ntiles,x,y,rad,rad2,center=center,noplot=noplot

  if n_elements(center) eq 0 then center=[0.,0.]
  xrtrad=23.6/2.
  r=xrtrad                      ;/cos(center[1]*!dtor);700/60.
  g=r*sqrt(3.)
  g2=r*sqrt(2.)


  case ntiles of 
     1: begin
        x=[0.,0.]
        y=[0.,0.]
        rad=r*2.-g/2.
        rad2=rad
     end 
     4: begin 
        x=[-r,0,r,0]
        y=[0,r,0,-r]
        rad=r*2
     end 
     7: begin 
        x=[0,-g,-g*0.5,g*0.5,g,g*0.5,-g*0.5]
        y=[0,0,-g2,-g2,0,g2,g2]
        rad=g+r
     end 
     19: begin
        x=[0,-g,-g*0.5,g*0.5,g,g*0.5,-g*0.5,-g*1.5,-g*2,-g*1.5,-g,0,g,g*1.5,g*2.,g*1.5,g,0,-g]
        y=[0,0,-g2,-g2,0,g2,g2,g2,0,-g2,-g2*2.,-g2*2,-g2*2,-g2,0,g2,g2*2,g2*2,g2*2]
        rad=g*2+xrtrad
     end 
     37: begin
        x=[0,-g,-g*0.5,g*0.5,g,g*0.5,-g*0.5,-g*1.5,-g*2,-g*1.5,-g,0,g,g*1.5,g*2.,g*1.5,g,0,-g,$
           -g*2,-g*2.5,-g*3.,-g*2.5,-g*2,-g*1.5,-g*0.5,g*0.5,g*1.5,g*2,g*2.5,g*3,g*2.5,$
           g*2,g*1.5,g*0.5,-g*0.5,-g*1.5]
        y=[0,0,-g2,-g2,0,g2,g2,g2,0,-g2,-g2*2.,-g2*2,-g2*2,-g2,0,g2,g2*2,g2*2,g2*2.,$
           g2*2,g2,0,-g2,-g2*2,-g2*3,-g2*3,-g2*3,-g2*3,-g2*2,-g2,0,g2,g2*2,g2*3,g2*3,g2*3,g2*3,g2*3]
        rad=g*3.+xrtrad
     end 
     61: begin
        x=[0,-g,-g*0.5,g*0.5,g,g*0.5,-g*0.5,-g*1.5,-g*2,-g*1.5,-g,0,g,g*1.5,g*2.,g*1.5,g,0,-g,$
           -g*2,-g*2.5,-g*3.,-g*2.5,-g*2,-g*1.5,-g*0.5,g*0.5,g*1.5,g*2,g*2.5,g*3,g*2.5,$
           g*2,g*1.5,g*0.5,-g*0.5,-g*1.5,-g*2.5,$
           -g*3.,-g*3.5,-g*4,-g*3.5,-g*3,-g*2.5,-g*2,-g,0,g,g*2,g*2.5,g*3.,g*3.5,g*4.,$
           g*3.5,g*3,g*2.5,g*2.,g,0,-g,-g*2]
        y=[0,0,-g2,-g2,0,g2,g2,g2,0,-g2,-g2*2.,-g2*2,-g2*2,-g2,0,g2,g2*2,g2*2,g2*2.,$
           g2*2,g2,0,-g2,-g2*2,-g2*3,-g2*3,-g2*3,-g2*3,-g2*2,-g2,0,g2,g2*2,g2*3,g2*3,g2*3,g2*3,g2*3,$
           g2*2,g2,0,-g2,-g2*2,-g2*3,-g2*4,-g2*4,-g2*4,-g2*4,-g2*4,-g2*3,-g2*2,-g2,0,$
           g2,g2*2,g2*3,g2*4,g2*4,g2*4,g2*4,g2*4]
        rad=g*4.+xrtrad
     end 
     91: begin
        x=[0,-g,-g*0.5,g*0.5,g,g*0.5,-g*0.5,-g*1.5,-g*2,-g*1.5,-g,0,g,g*1.5,g*2.,g*1.5,g,0,-g,$
           -g*2,-g*2.5,-g*3.,-g*2.5,-g*2,-g*1.5,-g*0.5,g*0.5,g*1.5,g*2,g*2.5,g*3,g*2.5,$
           g*2,g*1.5,g*0.5,-g*0.5,-g*1.5,-g*2.5,$
           -g*3.,-g*3.5,-g*4,-g*3.5,-g*3,-g*2.5,-g*2,-g,0,g,g*2,g*2.5,g*3.,g*3.5,g*4.,$
           g*3.5,g*3,g*2.5,g*2.,g,0,-g,-g*2,$
           -g*3,-g*3.5,-g*4.,-g*4.5,-g*5.,-g*4.5,-g*4,-g*3.5,-g*3,-g*2.5,$
           -g*1.5,-g*0.5,g*0.5,g*1.5,g*2.5,g*3,g*3.5,g*4,g*4.5,g*5,g*4.5,g*4,g*3.5,g*3,g*2.5,$
           g*1.5,g*0.5,-g*0.5,-g*1.5,-g*2.5,-g*3.5]
        y=[0,0,-g2,-g2,0,g2,g2,g2,0,-g2,-g2*2.,-g2*2,-g2*2,-g2,0,g2,g2*2,g2*2,g2*2.,$
           g2*2,g2,0,-g2,-g2*2,-g2*3,-g2*3,-g2*3,-g2*3,-g2*2,-g2,0,g2,g2*2,g2*3,g2*3,g2*3,g2*3,g2*3,$
           g2*2,g2,0,-g2,-g2*2,-g2*3,-g2*4,-g2*4,-g2*4,-g2*4,-g2*4,-g2*3,-g2*2,-g2,0,$
           g2,g2*2,g2*3,g2*4,g2*4,g2*4,g2*4,g2*4,$
           g2*4,g2*3,g2*2,g2,0,-g2,-g2*2,-g2*3,-g2*4,-g2*5,$
           -g2*5,-g2*5,-g2*5,-g2*5,-g2*5,-g2*4,-g2*3,-g2*2,-g2,0,g2,g2*2,g2*3,g2*4,g2*5,$
           g2*5,g2*5,g2*5,g2*5,g2*5,g2*5]
        rad=g*5.+xrtrad

     end 
     
  endcase 

  if ntiles ne 1 then rad2=rad-g/2.
  x=x/60. ;;; deg
  y=y/60.
  y=y+center[1]
  x=x/cos(y*!dtor)+center[0]
  xrtrad=xrtrad/60.
  rad=rad/60.
  g=g/60.
  rad2=rad2/60.

;  help,x,y
  r=20
;  if ntiles le 19 then r=50 else r=120
;  r=r/6.
  if not keyword_set(noplot) then begin 
     map_set,/aitoff,/grid,label=1,limit=[-r+center[1],-r+center[0],r+center[1],r+center[0]]
;  plot,[-r,r],[-r,r],/iso,/nodata,xrange=[-r,r]+center[0],yrange=[-r,r]+center[1],/xsty,/ysty
     for i=0,ntiles-1 do begin
;     tvcircle,xrtrad,x[i]*cos(y[i]*!dtor),y[i]
;     xyouts,x[i],y[i],ntostr(fix(i+1))
        skycircle,x[i],y[i],xrtrad,xx,yy
        oplot,xx,yy
     endfor 
     skycircle,center[0],center[1],rad,cx,cy
     oplot,cx,cy,color=!red
     skycircle,center[0],center[1],rad2,cx,cy
     oplot,cx,cy,color=!green
  endif 
;  tvcircle,rad,center[0],center[1],color=!red
;  tvcircle,rad-g/2.,center[0],center[1],color=!green
  return
end 
