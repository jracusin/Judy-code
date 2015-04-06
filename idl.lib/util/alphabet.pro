pro alphabet,az,cap=cap
  
  az=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
  az2=''
  for i=0,25 do az2=[az2,az[i]+az]
  az2=az2[1:*]

  az=[az,az2]
  if keyword_set(cap) then az=strupcase(az)
  
  return
end 
