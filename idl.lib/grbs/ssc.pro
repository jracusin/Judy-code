function ssc,nu,nuc,num,p

  f=dblarr(n_elements(nu))
  w1=where(nu le nuc)
  f[w1]=(nuc/num)^0.5*(nu[w1]/nuc)^(4./3.)
  w2=where(nu gt nuc and nu le num)
  f[w2]=(nu[w2]/num)^0.5
  w3=where(nu gt num)
  f[w3]=(nu[w3]/num)^((2.-p)/2.)

return,f
end 
