pro GRB090715B_cr_fits,ps=ps

  cd,'~/Desktop/GRB090715B/'
  read_lcfit,'lc_fit_out_idl_int7.dat',pname,p,perror

  gamma=[1.99,2.15,2.10,2.33]

  gammaerr1=[0.13,0.16,0.24,0.37]
  gammaerr2=[0.15,0.20,0.20,0.31]

;  i=[1,3,5,7]
  i=[3,5,7]
  alpha=p[i]
  alphaerr1=perror[0,i]
  alphaerr2=perror[1,i]
  
  j=[1,2,3]
  fit_crs,alpha,alphaerr1,alphaerr2,gamma[j],gammaerr1[j],gammaerr2[j],/plotcompat,ps=ps;,/twocomp

return
end 
