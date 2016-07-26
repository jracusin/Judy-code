function random_distfunc,x,p,num
  ;;; x is variable
  ;;; p is probability as a function of x
  ;;; num is number of random numbers

  ;renorm p to 1
  p2=p/total(p)

  cdf=total(p2,/cumulative) ;;; cumulative distribution function
  
  y=randomu(seed,num)
  
  r=interpol(x[0:n_elements(x)-2],cdf,y)

  return,r
end 
