pro peirce,x,xerr,xm,xmerr,keep
  
  ;;;Peirce criterion for rejecting spurious outliers
  ;;1 - determine sd and mean
  ;;2 - obtain R  value for nobs w/ 1 doubtful obs
  ;;3 - calculate max allowable deviation |xi-xm|max = sigma*R
  ;;4 - obtain actual deviations for suspicious measurements
  ;;5 - check for elimination of suspicious data where |xi-xm| > |xi-xm|max
  ;;6 - 
  
  ;xmean=weighted_mean(x,xerr,xmeanerr)
  xmean=total(x/xerr^2)/total(1./xerr^2)
  sigma=sqrt(1./total(1./xerr^2))
  nobs=n_elements(x)
  
  readcol,'/home/jracusin/idl.lib/grbs/peirce.table',n,d1,d2,d3,d4,d5,d6,d7,d8,d9,skip=1
  
  wn=where(n eq nobs)
  r=d1[wn]
  xmax=sigma*r[0]
  
  xim=abs(x-xmean)
  elim=where(xim gt xmax,nelim)
  keep=where(xim lt xmax,nkeep)
;  xim=xim[keep]
  
;  m=max(xim,wm)
  
  
;     stop
  
  
  
  
  
  
  
  
  
;  xm=total(x[w]/xerr[w]^2)/total(1./xerr[w]^2)
;  xmeanerr=sqrt(1./total(1./xerr[w]^2))
  
  return
end 
