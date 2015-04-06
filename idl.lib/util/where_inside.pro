function where_inside,objra,objdec,ralist,declist,nw

  r=rotate([[objra],[objdec]],3)
  o = obj_new('idlanroi', r, type = 0)
  inside=o->ContainsPoints(ralist,declist)
  w=where(inside eq 1,nw)

  return,w
end 
