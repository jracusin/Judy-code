function factor,x
; Computes log of factorial of x
  
  fct=1d
  
  if x gt 1 and x lt 170 then begin
     if x ne 2 then for i=2d,x do fct=fct*i
     fct=alog(fct)
  endif else begin
; Use Stirlings Approx. if number of observed counts is large
     if x ge 170 then begin
        alpha=x
        fct=alpha*alog(alpha)-alpha+0.5*alog(2.*!pi*alpha)
;        fct=alog(sqrt(2.*!pi*x)*(x/exp(1d))^x)
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
