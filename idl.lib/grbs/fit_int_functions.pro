function intbkn3pow,t,p,seg=seg,perror=perror,yerror=yerror
  norm=p[0]
  pow1=p[1]
  break1=p[2]
  pow2=p[3]
  break2=p[4]
  pow3=p[5]
  break3=p[6]
  pow4=p[7]
  t1=t[0,*]
  t2=t[1,*]
  tm=(t2-t1)/2.+t1
  td=t2-t1
  w1=where(tm lt break1,nw1)
  w2=where(tm ge break1 and tm lt break2,nw2)
  w3=where(tm ge break2 and tm lt break3,nw3)
  w4=where(tm ge break3,nw4)
  seg=dblarr(n_elements(tm))
  if nw1 gt 0 then seg[w1]=1
  if nw2 gt 0 then seg[w2]=2
  if nw3 gt 0 then seg[w3]=3
  if nw4 gt 0 then seg[w4]=4
  yfit=dblarr(n_elements(tm))
  if nw1 gt 0 then $
     yfit[w1]=norm/(1.-pow1)*(t2[w1]^(1.-pow1)-t1[w1]^(1.-pow1))
  if nw2 gt 0 then $
     yfit[w2]=norm*break1^(pow2-pow1)/(1.-pow2)*(t2[w2]^(1.-pow2)-t1[w2]^(1.-pow2))
  if nw3 gt 0 then $
     yfit[w3]=norm*break1^(pow2-pow1)*break2^(pow3-pow2)/(1.-pow3)*(t2[w3]^(1.-pow3)-t1[w3]^(1.-pow3))
  if nw4 gt 0 then $
     yfit[w4]=norm*break1^(pow2-pow1)*break2^(pow3-pow2)/(1.-pow4)*break3^(pow4-pow3)*(t2[w4]^(1.-pow4)-t1[w4]^(1.-pow4))
  yfit=yfit/td
  
  return,yfit
end 

function intbkn2pow,t,p,seg=seg,perror=perror,yerror=yerror
  norm=p[0]
  pow1=p[1]
  break1=p[2]
  pow2=p[3]
  break2=p[4]
  pow3=p[5]
  t1=t[0,*]
  t2=t[1,*]
  tm=(t2-t1)/2.+t1
  td=t2-t1
  w1=where(tm lt break1,nw1)
  w2=where(tm ge break1 and tm lt break2,nw2)
  w3=where(tm ge break2,nw3)
  seg=dblarr(n_elements(tm))
  if nw1 gt 0 then seg[w1]=1
  if nw2 gt 0 then seg[w2]=2
  if nw3 gt 0 then seg[w3]=3
  yfit=dblarr(n_elements(tm))
  if nw1 gt 0 then $
     yfit[w1]=norm/(1.-pow1)*(t2[w1]^(1.-pow1)-t1[w1]^(1.-pow1))
  if nw2 gt 0 then $
     yfit[w2]=norm*break1^(pow2-pow1)/(1.-pow2)*(t2[w2]^(1.-pow2)-t1[w2]^(1.-pow2))
  if nw3 gt 0 then $
     yfit[w3]=norm*break1^(pow2-pow1)*break2^(pow3-pow2)/(1.-pow3)*(t2[w3]^(1.-pow3)-t1[w3]^(1.-pow3))
  yfit=yfit/td
  
  return,yfit
end 


function intbknpow,t,p,seg=seg
  norm=p[0]
  pow1=p[1]
  break1=p[2]
  pow2=p[3]
  t1=t[0,*]
  t2=t[1,*]
  tm=(t2-t1)/2.+t1
  td=t2-t1
  w1=where(tm lt break1,nw1)
  w2=where(tm ge break1,nw2)
  seg=dblarr(n_elements(tm))
  if nw1 gt 0 then seg[w1]=1
  if nw2 gt 0 then seg[w2]=2
  yfit=dblarr(n_elements(t))
  if nw1 gt 0 then $
     yfit[w1]=norm/(1.-pow1)*(t2[w1]^(1.-pow1)-t1[w1]^(1.-pow1))
  if nw2 gt 0 then $
     yfit[w2]=norm*break1^(pow2-pow1)/(1.-pow2)*(t2[w2]^(1.-pow2)-t1[w2]^(1.-pow2))
  yfit=yfit/td
  
  return,yfit
end 

function intpow,t,p,terror=terror
  norm=p[0]
  pow1=p[1]
  t1=t[0,*]
  t2=t[1,*]
  tm=(t2-t1)/2.+t1
  td=t2-t1
  yfit=norm/(1.-pow1)*(t2^(1.-pow1)-t1^(1.-pow1))
  yfit=yfit/td
  
  return,yfit
end 
