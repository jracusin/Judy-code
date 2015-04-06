function factor,x
; Computes log of factorial of x
  
  fct=1d
  
  if x gt 1 and x lt 20 then begin
     if x ne 2 then for i=2d,x do fct=fct*i
     fct=alog(fct)
  endif else begin
; Use Stirlings Approx. if number of observed counts is large
     if x ge 20 then begin
        alpha=x
        fct=alpha*alog(alpha)-alpha
     endif
  endelse
  
  return,fct
end 
  
function poiprob,mean,num
; This function computes the Poisson probability for a 
; mean of 'mean' and number of counts 'num'
  
  if num eq 0 then p=exp(-mean) else begin
     alpha=-mean+num*alog(mean)-factor(num)
     p=exp(alpha)
  endelse
  
  return,p
end

     
pro confidlev,bkgrt,ncount,confid,smin,smax
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;+    NAME:
;          CONFIDLEV
;
;     PURPOSE:
;          This program determines upper and lower confidence limits
;     for counting experiments with low numbers of counts.  The
;     distribution is assumed to be a Poisson distribution.  A
;     mean background and number of counts in the source box are
;     input.  The upper and lower limits on the source are then
;     determined to any desired confidence level.  A full description
;     of the statistical basis of this program is given in
;     Kraft, Burrows, and Nousek 1991, Ap. J., 374, 344.
;
;     Author: Ralph Kraft
;     Date:   2/12/91
;     Version: 1.2
;
;     CALLING SEQUENCE:
;          confidlev,backrate,counts,conlevel,smin,smax
;
;     INPUTS:
;          backrate = background count rate
;          counts =  counts observed
;          conlevel = confidence level(0.0-1.0)
;
;     OUTPUTS:
;          smin = minimum expected counts
;          smax = maximum expected counts
;
;     REVISION HISTORY: 
;        07/16/91 by RPK to allow confidence intervals
;               to be computed for N>20 and B>20.
;        01/24/05 by DNB: increased max Ncounts to 1,000,000 to handle cases
;               with large number of counts (up to 1000 counts from source).
;        09/21/05 by JAK: Converted to python
;        08/02/06 by JLR: Converted to IDL  
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  
  if n_params() eq 0 then begin
     print,'syntax - confidlev,backrate,counts,conlevel,smin,smax'
     print,'This program computes upper and lower'
     print,'Bayesian confidence intervals for'
     print,'counting experiments with low numbers of counts'
     print,'in the presence of non-zero background'
     return
  endif 
  
  if confid[0] gt 1 then confid=confid/100.
  
  srcdim=100000
  srcstp=0.001
  ncount=ncount*1d
  bkgrt=bkgrt*1d
  
  psrc=dblarr(srcdim+1)
  
  pmax=0.0
  nmax=0d

  for i=0L,srcdim-1 do begin
     mnrt=bkgrt+i*srcstp
     psrc[i]=poiprob(mnrt,ncount)

     if psrc[i] gt pmax then begin
        pmax=psrc[i]
        nmax=i*1d
     endif 
  endfor 
  t=0.0
  for i=1L,srcdim-1 do t=t+psrc[i]

  area=(psrc[0]+psrc[srcdim-1])/2.+t
  t=0.0

  for i=0L,srcdim-1 do psrc[i]=psrc[i]/area

  nupper=nmax
  nlower=nmax
  area=0.0
  
;      At least three points are required
;      for use of the extended trapezoidal
;      rule.  The points are always chosen
;      so that Smax-Smin is minimized.
;      (See note below)

  for i=1,2 do begin
      if psrc[nlower] eq 0 then begin
          nupper=nupper+1 
      endif else begin
          if nlower eq 0 then pnlower=0. else pnlower=psrc[nlower-1]
          if pnlower ge psrc[nupper+1] then begin 
              nlower=nlower-1 
          endif else begin 
              nupper=nupper+1
          endelse 
      endelse 
  endfor 
  
;      By starting at the most probable
;      value and integrating in both directions,
;      always choosing to sum the side with the higher
;      probability, we can guarantee that the
;      confidence interval chosen is the smallest
;      possible one.
  
  area=(psrc[nupper]+psrc[nlower])/2.+psrc[nlower+1]
  k=0
  
  for i=0L,srcdim-3 do begin
     if nlower eq 0 then begin
        nupper=nupper+1
        area=area+(psrc[nupper-1]+psrc[nupper])/2.
     endif else begin
        if psrc[nlower-1] ge psrc[nupper+1] then begin
           nlower=nlower-1
           area=area+(psrc[nlower+1]+psrc[nlower])/2.
        endif else begin
           nupper=nupper+1
           area=area+(psrc[nupper-1]+psrc[nupper])/2.
        endelse 
     endelse
     if k eq 0 and area ge confid then begin
        smax=nupper*srcstp
        smin=nlower*srcstp
        k=1
     endif 
  endfor 
  print,smin,smax

;  print,smin,smax
  
;      Upper and lower limits are converted
;      into source rates.

  return
end
  
     
     

     
