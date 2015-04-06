function sigprob,input

  if n_params() eq 0 then begin
     print,'syntax - sig=sigprob(prob) or prob=sigprob(sig)'
     return,0
  end

  sig=0 & prob=0
  if input ge 1. then begin 
     output=1.-(1.-gauss_pdf(input))*2d
     sig=1
  endif else begin
     output=-gauss_cvf((1.+input)/2d)
     prob=1
  endelse 

;  if prob then print,'Significance (P='+ntostr(input)+') = '+ntostr(output)
;  if sig then print,'Prob (sig='+ntostr(input)+') = '+ntostr(output)

  return,output
end 
