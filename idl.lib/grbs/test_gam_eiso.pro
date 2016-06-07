pro test_gam_eiso

  ;;; for refereeing of Yi et al. for ApJ

  z=[1.61,2.615,2.198,3.91,1.49,1.314,0.84,2.95,0.98,0.947,2.692,1.688,1.408,1.727,2.22,1.728,2.876,3.97,0.703,1.262,0.97,4.394,1.95,1.51,0.845,3.35,2.1,3.375,0.544,2.452,1.092,2.752,0.542,2.106,1.46,2.358]

  tp=[19.16,108.17,42,97,60.73,250,162.09,113.83,185.92,67,275.88,595,523,37,311,364,643,120,272,35,298,2225,117,248,1192,27,118,242,0,17.4,1912,88,3443,16,81,147]

  eiso=[22900,9700,370,4150,1000,2610,145,1000,13,255,390,150,630,3340,5600,3600,900,900,72,10465,24,700d,2255,41,80,3000,1700,320,400,4030,2800,740,437,300,640,212]*1d50

  eta=[30.,64.4,7.2,3.1,57,56,3,10.7,1.8,26,71.5,2.8,3.6,58.4,64.2,90,54,9.5,.1,77.8,0.7,19.4,23.3,1.9,23.2,41.8,60.1,1.1,7,21.3,42.9,15.5,3.8,3,19.9,23]

  w=where(tp ne 0.,n)
  z=z[w]
  tp=tp[w]
  eiso=eiso[w]
  eta=eta[w]
  eta=0.1
;stop
  vslope=0.21
  vrank=0.59

  nsim=1e4
  sim=create_struct('ind',0,'norm',0.,'pow',0.,'rank',0.,'p',0.)
  sim=replicate(sim,nsim)
  pfrac=fltarr(3) & rfrac=fltarr(3)
  
  for j=0,2 do begin 
     for i=0,nsim-1 do begin
        r1=indgen(n) & r2=indgen(n) & r3=indgen(n)

        if j eq 0 then r1=round(randomu(seed,n)*n)
        if j eq 1 then r2=round(randomu(seed,n)*n)
        if j eq 0 then r3=round(randomu(seed,n)*n)
        if j eq 2 then begin
 ;          r2=round(randomu(seed,n)*n)
           reiso=10^(randomu(seed,n)*4.+50d)
           rz=randomu(seed,n)*6.
           rtp=randomu(seed,n)*1990.+10.
        endif else begin
           reiso=eiso[r2]
           rz=z[r1]
           rtp=tp[r3]
        endelse 

        num=3*eiso[r2]*(1.+rz)^3
        denom=32*!pi*0.1*1.67e-24*3d10^5*eta*rtp^3
        gam=1.4*(num/denom)^(1./8)

;     plot,eiso,gam,psym=1,/yno,/xlog,/ylog
        ab=linfit(alog10(eiso[r2]),alog10(gam))
        r=correlate(alog10(eiso[r2]),alog10(gam))

;     oplot,eiso,10^ab[0]*eiso^(ab[1])

        sim[i].ind=i
        sim[i].norm=10^ab[0]
        sim[i].pow=ab[1]
        sim[i].rank=r[0]

;     sim[i].p=r[1]
;        if i mod 100 eq 0 then print,i
     endfor
     if j eq 0 then begin 
        mwrfits,sim,'~/referee/Lei_sim_rand_z_tp.fits',/create
        sim1=sim
     endif 
     if j eq 1 then begin
        mwrfits,sim,'~/referee/Lei_sim_rand_eiso.fits',/create
        sim2=sim
     endif 
     if j eq 2 then begin
        mwrfits,sim,'~/referee/Lei_sim_rand_uniform_z_tp.fits',/create
        sim3=sim
     endif 
        w=where(sim.pow ge vslope,nw)
        print,'sim '+ntostr(j+1)+', pow: ',nw*1./1e4
        pfrac[j]=nw*1./nsim
        w=where(sim.rank ge vrank,nw)
        print,'sim '+ntostr(j+1)+', rank: ',nw*1./1e4
        rfrac[j]=nw*1./nsim
  endfor 

  begplot,name='~/referee/Yi_etal_simulation.ps',/color,/land
  !p.multi=[0,3,3]
  !p.charsize=2
  plothist,sim1.pow,bin=0.01,xtitle='Correlation Slope',title='Sim 1: Random z, Tp',ytitle='N'
  oplot,[vslope,vslope],[0,1e4],color=!red,line=2
  legend,['>'+numdec(vslope,2),numdec(pfrac[0]*100.,1)+'%'],box=0,/top,/left,charsize=1
  plothist,sim1.rank,bin=0.01,xtitle='Pearson Rank',title='Sim 1: Random z, Tp',ytitle='N'
  oplot,[vrank,vrank],[0,1e4],color=!red,line=2 
  legend,['>'+numdec(vrank,2),numdec(rfrac[0]*100.,1)+'%'],box=0,/top,/left,charsize=1
  plot,sim1.pow,sim1.rank,psym=3,xtitle='Correlation Slope',ytitle='Pearson Rank',title='Sim 1: Random z, Tp'
  plots,vslope,vrank,psym=2,color=!red

  plothist,sim2.pow,bin=0.01,xtitle='Correlation Slope',title='Sim 2: Random Eiso',ytitle='N'
  oplot,[vslope,vslope],[0,1e4],color=!red,line=2
  legend,['>'+numdec(vslope,2),numdec(pfrac[1]*100.,1)+'%'],box=0,/top,/left,charsize=1
  plothist,sim2.rank,bin=0.01,xtitle='Pearson Rank',title='Sim 2: Random Eiso',ytitle='N'
  oplot,[vrank,vrank],[0,1e4],color=!red,line=2 
  legend,['>'+numdec(vrank,2),numdec(rfrac[1]*100.,1)+'%'],box=0,/top,/left,charsize=1
  plot,sim2.pow,sim2.rank,psym=3,xtitle='Correlation Slope',ytitle='Pearson Rank',title='Sim 2: Random Eiso'
  plots,vslope,vrank,psym=2,color=!red

  plothist,sim3.pow,bin=0.01,xtitle='Correlation Slope',title='Sim 3: Random uniform Eiso, z, Tp',ytitle='N'
  oplot,[vslope,vslope],[0,1e4],color=!red,line=2
  legend,['>'+numdec(vslope,2),numdec(pfrac[2]*100.,1)+'%'],box=0,/top,/left,charsize=1
  plothist,sim3.rank,bin=0.01,xtitle='Pearson Rank',title='Sim 3: Random uniform Eiso, z, Tp',ytitle='N'
  oplot,[vrank,vrank],[0,1e4],color=!red,line=2 
  legend,['>'+numdec(vrank,2),numdec(rfrac[2]*100.,1)+'%'],box=0,/top,/left,charsize=1
  plot,sim3.pow,sim3.rank,psym=3,xtitle='Correlation Slope',ytitle='Pearson Rank',title='Sim 3: Random uniform Eiso, z, Tp'
  plots,vslope,vrank,psym=2,color=!red

  !p.multi=0
  endplot
  ps2pdf,'~/referee/Yi_etal_simulation.ps'

  stop
  return
end 
