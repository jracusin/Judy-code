pro plot_dist,r

  !p.multi=[0,1,2]
  plothist,alog10(g[w].flux_1024),x,y,bin=0.1,/noplot
  plothist,alog10(g[w].flux_batse_1024),xb,yb,bin=0.1,/noplot
  plot,xb,(n-cumulative(yb))/nyears,/ylog,yrange=[1,1e3],psym=10,ytitle='log photon flux',xtitle='N'
  oplot,x,(n-cumulative(y))/nyears,color=!red,psym=10
  v=interpol(n-cumulative(yb),xb,alog10(r))/6
  for i=0,n_elements(r)-1 do oplot,alog10([r[i],r[i]]),[1,1e4]
  legend,['1.024 s'],/top,/right,box=0
  print,'1.024 s: ',v
  v1024=v

  plothist,alog10(g[w].flux_64),x,y,bin=0.1,/noplot
  plothist,alog10(g[w].flux_batse_64),xb,yb,bin=0.1,/noplot
  plot,xb,(n-cumulative(yb))/nyears,/ylog,yrange=[1,1e3],psym=10,ytitle='log photon flux',xtitle='N'
  oplot,x,(n-cumulative(y))/nyears,color=!red,psym=10
  v=interpol(n-cumulative(yb),xb,alog10(r))/6
  for i=0,n_elements(r)-1 do oplot,alog10([r[i],r[i]]),[1,1e4]
  legend,['64 ms'],/top,/right,box=0
  print,'64 ms: ',v
  v64=v
  !p.multi=0
  return
end 

pro grb_rates

  g=mrdfits('~/Fermi/GBM_GRB_Catalog.fits',1)
;  trig=mrdfits('~/Burstcube/gbm_trigcat.fits',1)  
  
;  match,g.trigger_name,trig.trigger_name,m1,m2
;  trig_ts=trig[m2].trigger_timescale

  nyears=6.

  ;;; GBM NaI 12.7/1.27 cm
  ;;; BurstCube CsI 9.4/1.27 cm
  rad=[12.7,9.4]
  area=[!pi*(rad[0]/2.)^2,rad[1]^2]
  eff=[0.452,0.488] ;; from John K
  inst=['GBM','BurstCube']
  back=400. ;; cts/s

  t=[1.024,0.256,0.064]
  sig=[4.5,4.5,5]

  !p.multi=[0,1,3]
  for j=0,1 do begin ;; GBM + BurstCube
     print,inst[j]
     id=intarr(n_elements(g)) & id2=intarr(n_elements(g))
     for i=0,n_elements(t)-1 do begin ;; timescales

        ratelim=sig[i]*sqrt(back*t[i]*eff[0]/eff[j])/area[j]/t[i]

        case i of 
           0: flux=g.flux_batse_1024
           1: flux=g.flux_batse_256
           2: flux=g.flux_batse_64
        endcase
        
        w=where(flux ne 0 and g.t90 lt 2.,n) ; and trig_ts ge t[i]*1000.,n)  ;; only sGRBs
        print,n/nyears,' total'
        
        plothist,alog10(flux[w]),xb,yb,bin=0.1,charsize=2. ;,/noplot
        
        v=interpol(n-cumulative(yb),xb,alog10(ratelim))/nyears
        v2=interpol(n-cumulative(yb),xb,alog10(ratelim/sqrt(2.)))/nyears
        
        q=where(flux ge ratelim)
        id[q]=1
        q2=where(flux ge ratelim/sqrt(2.))
        id2[q2]=1
        
        print,t[i],' timescale'
        print,sig[i],' sigma threshold'
        print,ratelim,' rate limit'
        print,v,' short grbs'
        print,v2,' short grbs with 2 BurstCubes'
        print
     endfor 
     w=where(g.t90 lt 2. and id eq 1,nw)
     print,nw/nyears

     w=where(g.t90 lt 2. and id2 eq 1,nw)
     print,nw/nyears


  endfor 
  !p.multi=0


stop
  return
end 



return
end 
