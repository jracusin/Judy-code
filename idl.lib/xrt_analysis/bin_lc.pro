pro bin_lc,timei,ratei,erri,bt,br,bte,be,ncounts=ncounts;nbins=nbins
  
;  time=lc.time
;  rate=lc.rate
;  err=lc.error
  
  
  w=where(timei gt 0 and ratei gt 0,n)
  
  time=timei[w]
  rate=ratei[w]
  err=erri[w]
  
  mrate=total(rate)
  
  if n_elements(ncounts) eq 0 then nbins=round(mrate/20.) else nbins=round(mrate/ncounts)
  print,nbins
  
  binval=mrate/nbins
  
  bt=fltarr(nbins)
  br=fltarr(nbins)
  be=fltarr(nbins)
  bte=fltarr(nbins)
  
  j=0
  nobj=0
  rates=0.
  times=0.
  errs=0.
  delt=0.
  for i=0L,n-2 do begin
;     if i lt n-2 then begin 
     if (total(rates)+rate[i]) le binval and i lt n-2 then begin 
        nobj=nobj+1L
        rates=[rates,rate[i]]
        times=[times,time[i]]
        errs=[errs,err[i]]
        dt=(time[i+1]-time[i])
        if dt gt 50 then dt=0.
        delt=[delt,dt]
;        br[j]=br[j]+rate[i] 
;        bt[j]=bt[j]+time[i]
;        be[j]=be[j]+(err[i]/br[j])^2
     endif else begin 
        br[j]=total(rates)
        bt[j]=total(times*rates)/total(rates)
        bte[j]=total(delt[1:nobj]);(max(times)-min(times[1:*]))/2.
;        be[j]=sqrt(total((errs[1:*]/rates[1:*])^2))*br[j]/sqrt(nobj)
        be[j]=sqrt(total(errs[1:*]^2))
;        print,total(rates),i,j
        rates=0.
        times=0.
        errs=0.
        delt=0.
        j=j+1
        nobj=0
        
     endelse 
;  endif 
;     print,total(rates),i,j
  endfor 
  
  
;  ploterror,bt,br/bte,bte/2.,be/bte/2.,psym=2,$
;     xtitle='Time (s)',ytitle='Count Rate (cts/s)',$
;     xrange=[1,max(time)],yrange=[0.01,max(br/bte+be/bte)],type=3
  
;  if n_elements(modes) gt 0 then begin 
;     wwt=where(modes eq 6,nwt)
;     wpc=where(modes eq 7,npc)
;     wlr=where(modes eq 5,nlr)
;     if nwt gt 0 then oploterror,bt[wwt],br[wwt]/bte[wwt]/2.,be[wwt]/bte[wwt]/2.,psym=2,color=!green
;     if npc gt 0 then oploterror,bt[wpc],br[wpc]/bte[wpc]/2.,be[wpc]/bte[wpc]/2.,psym=2,color=!purple
;     if nlr gt 0 then oploterror,bt[wlr],br[wlr]/bte[wlr]/2.,be[wlr]/bte[wlr]/2.,psym=2,color=!orange
;  endif 
  
;   t1=alog10(bt[1])
;   t2=alog10(bt[n_elements(bt)-1])
;   nbt=20
;   a=[0D,0D] & b=a & chisq=a & breaktime=dblarr(nbt) & j=0
;   for j=0,nbt-2 do begin 
     
;      i=(j+1)*(t2-t1)/nbt+t1
;      breaktime[j]=10.^i
     
;      w1=where(bt gt 0 and bt lt breaktime[j])
;      w2=where(bt ge breaktime[j])
  
;      fitexy,alog10(bt[w1]),alog10(br[w1]/bte[w1]),a1,b1,x_sig=(alog10(bte[w1]/2.+bt[w1])-alog10(bt[w1]-bte[w1]/2.)),y_sig=((alog10(be[w1]/bte[w1]/2.)+bt[w1])-(alog10(bt[w1]-be[w1]/bte[w1]/2.))),sigma_A_B1, chi_sq1, q
     
;      fitexy,alog10(bt[w2]),alog10(br[w2]/bte[w2]),a2,b2,x_sig=(alog10(bte[w2]/2.+bt[w2])-alog10(bt[w2]-bte[w2]/2.)),y_sig=((alog10(be[w2]/bte[w2]/2.)+bt[w2])-alog10(bt[w2]-be[w2]/bte[w2]/2.)),sigma_A_B2, chi_sq2, q
;      a=[[a],[a1,a2]]
;      b=[[b],[b1,b2]]
;      chisq=[[chisq],[chi_sq1,chi_sq2]]
     
;   endfor 

;   mchisq=min(sqrt(chisq[0,1:nbt-1]^2),m);+chisq[1,1:nbt-1]^2),m)
  
;   a1=a[0,m+1]
;   a2=a[1,m+1]
;   b1=b[0,m+1]
;   b2=b[1,m+1]
  
;   simpctable
;   oplot,bt,10^(alog10(bt)*b1+a1),color=!red
;   oplot,bt,10^(alog10(bt)*b2+a2),color=!blue
;   legend,['slope1 = '+ntostr(b1)+' +/- '+ntostr(sigma_a_b1[1]),'slope2 = '+ntostr(b2)+' +/- '+ntostr(sigma_a_b2[1]),'t!Lbreak!N = '+ntostr(round(breaktime[m]))],/top,/right,box=0
;stop
  x=bt
  y=br/bte
  yerr=be/bte
;  stop
                                ;print,bt[w1],bt[w2]
;  print,br
  return
end 
