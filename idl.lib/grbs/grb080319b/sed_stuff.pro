@fit_functions
function sed_model,x,p,f1,f2
  
  n=n_elements(x[*,0])
  nx=n_elements(x[0,*])
;;p=[sed1,norm1,beta_o1,ebk1,beta_o2,norm2,beta_x1,ebk2,beta_x2,ebk3,beta_x3]
  sed=p[0,*]
  yfit=dblarr(n,nx)
  for i=0,nx-1 do begin
     j=i*nx
     p1=p[j+indgen(4)+1]
     p2=p[j+indgen(6)+5]

     xx=x[*,i]
     w=where(xx ne 0)
     xx=xx[w]
     f1=bknpow(xx,p1)
     if i le 4 then f2=bkn2pow(xx,p2) else f2=bknpow(xx,p2[0:3])
;     if i gt 0 then p1[2]=p[2,i-1]*(sed[i-1]/sed[i])^(-3./2.)
     f=f1+f2
     yfit[w,i]=f

  endfor 
  
  return,yfit
end 

pro model_simult_sed
  
  cd,'~/Desktop/GRB080319B/SEDs/'
  seds=['150','250','350','600','720','1500','5856','1.2e4','8e4']
  sed=seds*1.
  n=n_elements(sed)
  x=dblarr(460,n) & flux=x & fluxerr=x
  fluxerr[*,*]=-1.
  np=11.*n
  parinfo=parinfo_struct(np)
  beta_o1=0.74
  p150=[9.61706,1.74066,0.33225,1.85208,7.00598,2.24317]
  p250=[4.63675,1.68298,0.335878,1.82675,5.62279,2.13487]
  p350=[3.67538,1.57078,0.12865,1.77998,4.45690,2.12979]
  p600=[11.5796,1.24392,1.62345e-2,1.77079,4.51592,2.13109]
  p720=[9.85899,1.21473,1.38630e-2,1.76155,4.21768,2.12715]
  p1500=[4.83316,1.10034,2.55191e-2,1.83919,0,0]
  p5856=[0.891351,1.06107,9.14984e-3,1.79703,0,0]
  p1e4=[0.37947,1.0891,6.42223e-3,1.79679,0,0]
  p8e4=[4.5881e-2,0.984799,1.22858e-2,1.83878,0,0]
  
  for i=0,n-1 do begin
     case i of
        0: p=p150
        1: p=p250
        2: p=p350
        3: p=p600
        4: p=p720
        5: p=p1500
        6: p=p5856
        7: p=p1e4
        8: p=p8e4
     endcase 
     p[1]=p[1]-1
     p[3]=p[3]-1
     if i le 4 then p[5]=p[5]-1     
     
     readcol,'sed'+seds[i]+'.txt',xt,ftmp,ftmperr,format='(f,f,f)'
     nx=n_elements(xt)
     x[0:nx-1,i]=xt
     flux[0:nx-1,i]=ftmp
     fluxerr[0:nx-1,i]=ftmperr
     print,nx
     if i eq 0 then ebk_opt=0.01 else ebk_opt=pg[3]*(sed[i]/sed[i-1])^(1/2.)
     if i eq 0 then ebk_x1=1. else ebk_x1=pg[7]*(sed[i]/sed[i-1])^(-3./2.)
     pg=[sed[i],p[0],beta_o1,ebk_opt,beta_o1+0.5,p[0],-1./3.,ebk_x1,p[3],p[4],p[5]]
     pname=['sed','Norm1','beta_opt1','ebk_opt','beta_opt2','Norm2','beta_x1','ebk_x1','beta_x2','ebk_x2','beta_x3']
     j=i*11
     parinfo[j:j+10].value=pg
     parinfo[j:j+10].parname=pname

     parinfo[j+0].fixed=1 ;;sed time
     parinfo[j+1].limits=[0,0] ;;norm1
     parinfo[j+1].limited=[1,0]
     parinfo[j+2].fixed=1 ;;beta_opt1
     parinfo[j+3].limits=[1e-3,0.5] ;;ebk_opt
     parinfo[j+3].limited=[1,1]
     j03=ntostr(fix((i-1)*n+3)) 
     j00=ntostr(fix((i-1)*n))
     j0=ntostr(j)
     ;;; nu_c ~ t^(1/2)
;     if i gt 0 then parinfo[j+3].tied='p['+j03+']*(p['+j0+']/p['+j00+'])^(1./2)'
;     parinfo[j+4].limits=[0,0] ;;beta_opt2
;     parinfo[j+4].limited=[1,0]
     parinfo[j+4].fixed=1
     parinfo[j+5].limits=[0,0] ;;norm2
     parinfo[j+5].limited=[1,0]
     parinfo[j+6].fixed=1 ;;beta_x1
     parinfo[j+7].limits=[1e-5,100] ;;ebk_x1
     parinfo[j+7].limited=[1,1]
     j07=ntostr(fix((i-1)*n+7)) 
     ;;; nu_m ~ t^(-3/2)
;     if i gt 0 then parinfo[j+7].tied='p['+j07+']*(p['+j0+']/p['+j00+'])^(-3./2)'
     parinfo[j+8].limits=[0,0] ;;beta_x2
     parinfo[j+8].limited=[1,0]
     if i le 4 then begin 
        parinfo[j+9].limits=[0.1,100] ;;ebk_x2
        parinfo[j+9].limited=[1,1]
        parinfo[j+10].limits=[0,0] ;;beta_x3
        parinfo[j+10].limited=[1,0]
     endif 
        
;;p=[sed1,norm1,beta_o1,ebk1,beta_o2,norm2,beta_x1,ebk2,beta_x2,ebk3,beta_x3]
  endfor 
  p=parinfo.value
  weights=1./fluxerr^2
  w=where(weights eq 1)
  weights[w]=0
  
  newp=mpfitfun('sed_model',x,flux,fluxerr,p,weights=weights,yfit=yfit,nprint=100,parinfo=parinfo)   
  
  plot,[1e-4,1e3],[1e-4,2e3],/xlog,/ylog,/nodata,/xsty,/iso,xtitle='Energy (keV)',ytitle='keV (Photons cm!U-2!N s!U-1!N kev!U-1!N)',xminor=9,yminor=9,yticks=8,xtickf='loglabels',ytickf='loglabels'
  colors=[!red,!green,!blue,!cyan,!magenta,!yellow,!orange,!purple,!salmon]
  for i=0,8 do begin
     w=where(x[*,i] ne 0)
     oplot,x[w,i],sed_model(x[w,i],newp[0:10],f1,f2),color=colors[i]
     oplot,x[w,i],f1,color=colors[i],line=1
     oplot,x[w,i],f2,color=colors[i],line=2
  endfor 
  
  
  stop
  return
end 

pro model_flat_sed
  
  cd,'~/Desktop/GRB080319B/SEDs/'
  sed=['150','250','350','600','720','1500','5856','1.2e4','8e4']
  popt=[3.7454157e+16,7.4656517, 96264933.,2.4870896,26702.779,1.2458044]
  px=[1.,0.85]
  
  p150=[9.61706,1.74066,0.33225,1.85208,7.00598,2.24317]
  p250=[4.63675,1.68298,0.335878,1.82675,5.62279,2.13487]
  p350=[3.67538,1.57078,0.12865,1.77998,4.45690,2.12979]
  p600=[11.5796,1.24392,1.62345e-2,1.77079,4.51592,2.13109]
  p720=[9.85899,1.21473,1.38630e-2,1.76155,4.21768,2.12715]
  p1500=[4.83316,1.10034,2.55191e-2,1.83919,4,2]
  p5856=[0.891351,1.06107,9.14984e-3,1.79703,4,2]
  p1e4=[0.37947,1.0891,6.42223e-3,1.79679,4,2]
  p8e4=[4.5881e-2,0.984799,1.22858e-2,1.83878,4,2]
  n=9
  
  spec=create_struct('sed','','norm1',0d,'beta_opt1',0d,$
                     'ebk_opt',0d,'beta_opt2',0d,$
                     'norm2',0d,'beta_x1',0d,'ebk_x1',0d,'beta_x2',0d,$
                     'ebk_x2',0d,'beta_x3',0d,'chisq',0d,'dof',0L)
  spec=replicate(spec,n)
  lc=create_struct('time',0d,'vflux',0d,'xflux',0d,'vflux1',0d,'vflux2',0d,'xflux1',0d,'xflux2',0d)
  lc=replicate(lc,n)
  lc.time=sed*1.
  
  begplot,name='sed_fits_double_bknpow.ps',/color,font='helvetica'
  eng=[(indgen(9)+1.)/10000.,0.0015,(indgen(9)+1.)/1000.,(indgen(9)+1.)/100.,(indgen(10)+1.)/10.,1.5,indgen(9)+2.,indgen(100)*3.+11]
  colors=[!red,!green,!blue,!cyan,!magenta,!yellow,!orange,!purple,!salmon]

;  w=where((eng gt 5e-4 and eng lt 0.01) or (eng gt 0.3 and eng lt 10.))
;  x=eng[w]
  simpctable
  plot,[1e-4,1e3],[1e-4,2e3],/xlog,/ylog,/nodata,/xsty,/iso,xtitle='Energy (keV)',ytitle='keV (Photons cm!U-2!N s!U-1!N kev!U-1!N)',xminor=9,yminor=9,yticks=8,xtickf='loglabels',ytickf='loglabels'

;  multiplot2,[1,n],/init
  for i=0,n-1 do begin
     case i of
        0: p=p150
        1: p=p250
        2: p=p350
        3: p=p600
        4: p=p720
        5: p=p1500
        6: p=p5856
        7: p=p1e4
        8: p=p8e4
     endcase 
     readcol,'sed'+sed[i]+'.txt',x,flux,fluxerr

     p[1]=p[1]-1
     p[3]=p[3]-1
     if n_elements(p) gt 4 then p[5]=p[5]-1
;     if i le 4 then yfit=bkn2pow(x,p) else yfit=bknpow(x,p)
;     oplot,x,yfit,color=colors[i],psym=1
;     multiplot2
;     plot,[1e-4,1e3],[1e-4,2e3],/xlog,/ylog,/nodata,/xsty
;     plot,[1e-4,1e3],minmax(flux),/xlog,/ylog,/nodata,/xsty,charsize=1.
     oploterror,x,flux,fluxerr,color=colors[i],errcolor=colors[i],psym=4,/nohat,symsize=0.5
     
     
;     pg=[p[0],p350[1]-1,0.001,1.,p[0],-1.5,0.01,p[3]]
     pname=['Norm1','beta_opt1','ebk_opt','beta_opt2','Norm2','beta_x1','ebk_x','beta_x2']
;     if i le 2 then norm=1e4 else 
;     pg=[p[0],p150[1]-1,p[0],-1./3.,0.01,p[3]]
     pg=[p[0],p150[1]-1,1e-2,1.,p[0],-1./3.,0.01d,p[3]]
;     pname=['Norm1','beta_opt1','Norm2','beta_x1','ebk_x','beta_x2']
     if i le 4 then begin 
        pg=[pg,p[4],p[5]]
       pname=[pname,'ebk_x2','beta_x3']
     endif 
     np=n_elements(pg)
     parinfo = parinfo_struct(np)
     parinfo.value=pg
     parinfo.parname=pname
     print,p[0],min(flux)
;     norm=bknpow(0.001,p[0:5])/pow(0.001,p[0:1])
;     print,norm
     
     parinfo[0].limits=[0,0] ;;norm1
     parinfo[0].limited=[1,0]
;     parinfo[0].fixed=1 
     parinfo[1].fixed=1      ;;beta_opt1
     parinfo[2].limits=[1e-3,0.1] ;;ebk_opt
     parinfo[2].limited=[1,1]
     parinfo[3].limits=[0,0] ;;beta_opt2
     parinfo[3].limited=[1,0]
;     parinfo[3].fixed=1
     parinfo[4].limits=[0,0] ;;norm2
     parinfo[4].limited=[1,0]
;     parinfo[3].limits=[-10,-0.3] ;;beta_x1 < 0
;     parinfo[3].limited=[1,1]
     parinfo[5].fixed=1
     parinfo[6].limits=[5e-3,10] ;;ebk_x1
     parinfo[6].limited=[1,1]
;     parinfo[7].fixed=1      ;;beta_x2
     parinfo[7].limits=[0,0]
     parinfo[7].limited=[1,0]
     
     if i le 4 then begin
        parinfo[8].limits=[1,100] ;;ebk_x2
        parinfo[8].limited=[1,1]
        parinfo[9].limits=[0,0] ;;ebk_x2
        parinfo[9].limited=[1,0]

        newp=mpfitfun('bknpow_bkn2pow',x,flux,fluxerr,pg,yfit=yfit2,nprint=100,parinfo=parinfo) 
        oplot,eng,bknpow_bkn2pow(eng,newp,sf1,sf2),color=colors[i]
;        oplot,eng,bkn2pow(eng,newp[4:9]),line=2,color=colors[i]
        lc[i].vflux=bknpow_bkn2pow(2.3e-3,newp,f1,f2)
        lc[i].vflux1=f1
        lc[i].vflux2=f2
        lc[i].xflux=bknpow_bkn2pow(1.,newp,f1,f2)
        lc[i].xflux1=f1
        lc[i].xflux2=f2
     endif else begin
        newp=mpfitfun('double_bknpow',x,flux,fluxerr,pg,yfit=yfit2,nprint=100,parinfo=parinfo)
        oplot,eng,double_bknpow(eng,newp,sf1,sf2),color=colors[i]
;        oplot,eng,bknpow(eng,newp[4:7]),line=2,color=colors[i]

        lc[i].vflux=double_bknpow(2.3e-3,newp,f1,f2)
        lc[i].vflux1=f1
        lc[i].vflux2=f2
        lc[i].xflux=double_bknpow(1.,newp,f1,f2)
        lc[i].xflux1=f1
        lc[i].xflux2=f2
     endelse 
     oplot,eng,sf2,line=2,color=colors[i]
     oplot,eng,sf1,line=1,color=colors[i]
     
;     oplot,eng,bknpow(eng,newp[0:3]),line=1,color=colors[i]
     chisq=total(((yfit2-flux)/fluxerr)^2.)
     dof=n_elements(x)-6
     print,chisq,dof,chisq/dof
     
     spec[i].sed=sed[i]
     spec[i].norm1=newp[0]
     spec[i].beta_opt1=newp[1]
     spec[i].ebk_opt=newp[2]
     spec[i].beta_opt2=newp[3]
     spec[i].norm2=newp[4]
     spec[i].beta_x1=newp[5]
     spec[i].ebk_x1=newp[6]
     spec[i].beta_x2=newp[7]
     if i le 4 then begin 
        spec[i].ebk_x2=newp[8]
        spec[i].beta_x3=newp[9]
     endif 
     spec[i].chisq=chisq
     spec[i].dof=dof
;     k=get_kbrd(10)
;     if k eq 's' then stop
  endfor 
  
  endplot
  
  s='       '
  colprint,spec.sed+s,numdec(spec.beta_opt1,2)+s,numdec(spec.ebk_opt,3)+s,numdec(spec.beta_opt2,2)+s,numdec(spec.beta_x1,2)+s,numdec(spec.ebk_x1,3)+s,numdec(spec.beta_x2,2)+s,numdec(spec.ebk_x2,3)+s,numdec(spec.beta_x3,2)+s,numdec(spec.chisq,2)+'/'+ntostr(spec.dof)+'='+numdec(spec.chisq/spec.dof,2)
  
  plot,[100,1e5],[1e-4,1e4],/nodata,/xlog,/ylog,xtitle='Time (s)',ytitle='Flux'
  oplot,lc.time,lc.vflux
  oplot,lc.time,lc.vflux1,line=1
  oplot,lc.time,lc.vflux2,line=2
  oplot,lc.time,lc.xflux,color=!red
  oplot,lc.time,lc.xflux1,line=1,color=!red
  oplot,lc.time,lc.xflux2,line=2,color=!red
  
    
;  multiplot2,/reset,/default
;  endplot
  stop
  
  return
end 

pro sed_stuff
  
  sed=[150,250,350,600,720,1500,5856,1.2e4,8e4]
  popt=[3.7454157e+16,7.4656517, 96264933.,2.4870896,26702.779,1.2458044]
  
  v=2.3e-3
  k=6e-4
  uv=6e-3
  
  beta2=0.85
  beta3=0.05
  norm=1.
  tbreak=500.
  
  time=sed
;  time=[sed[0:2],tbreak,sed[3:*]]
;  flux_v2=bknpow(time,[popt[2],popt[3]],tbreak,7+popt[3]])
  flux_v2=pow(time,[popt[2],popt[3]])
  flux_v3=pow(time,[popt[4]*norm,popt[5]])
  
  flux_k2=(k/v)^(-beta2)*flux_v2
  flux_k3=(k/v)^(-beta3)*flux_v3
  
  flux_uv2=(uv/v)^(-beta2)*flux_v2
  flux_uv3=(uv/v)^(-beta3)*flux_v3
  
;  !p.multi=[0,1,2]
;  plot,[1e-4,1e-2],[1e-6,1e4],/nodata,/xlog,/ylog,xtitle='Energy (keV)',ytitle='Flux'
;  oplot,replicate(v,9),flux_v2,color=!red,psym=1
;  oplot,replicate(k,9),flux_k2,color=!blue,psym=1
;  oplot,replicate(uv,9),flux_uv2,color=!green,psym=1
   
;  plot,[1e-4,1e-2],[1e-6,1e4],/nodata,/xlog,/ylog,xtitle='Energy (keV)',ytitle='Flux'
;  oplot,replicate(v,9),flux_v3,color=!red,psym=1
;  oplot,replicate(k,9),flux_k3,color=!blue,psym=1
;  oplot,replicate(uv,9),flux_uv3,color=!green,psym=1
  
;  !p.multi=0
  
  !p.multi=0
  wset,0
  plot,[100,1e5],[1e-4,1e4],/nodata,/xlog,/ylog,xtitle='Time (s)',ytitle='Flux'
  
  oplot,time,pow_pow_pow(time,popt),thick=3

  oplot,time,flux_v2,color=!red,line=3
  oplot,time,flux_k2,color=!blue,line=3
  oplot,time,flux_uv2,color=!green,line=3
  
  oplot,time,flux_v3,color=!red,line=2
  oplot,time,flux_k3,color=!blue,line=2
  oplot,time,flux_uv3,color=!green,line=2
  
  flux_v=flux_v2+flux_v3
  flux_k=flux_k2+flux_k3
  flux_uv=flux_uv2+flux_uv3
  
  oplot,time,flux_v,color=!red,line=0
  oplot,time,flux_k,color=!blue,line=0
  oplot,time,flux_uv,color=!green,line=0
  wset,1
  plot,[6e-4,10],[1e-2,2e3],/nodata,/xlog,/ylog,xtitle='Energy (keV)',ytitle='Flux',/xsty,/ysty
  
  eng=[uv,v,k]
  colors=[!red,!green,!blue,!cyan,!magenta,!yellow,!orange,!purple,!turquoise]
;  ind=[0,1,2,4,5,6,7,8,9]
  fit=dblarr(2,n_elements(time))
  for j=0,n_elements(time)-1 do begin
;     i=ind[j]
     i=j
     s=[flux_uv[i],flux_v[i],flux_k[i]]
     oplot,eng,s,color=colors[j],psym=1
     fit[*,j]=linfit(alog10(eng),alog10(s))
     oplot,eng,10.^fit[0,j]*eng^fit[1,j],color=colors[j]
     print,time[i],fit[*,j]
  endfor 
  
  stop
  return
end 
