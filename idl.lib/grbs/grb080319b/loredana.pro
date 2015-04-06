pro loredana
  
  trigtime=227599969.0      
  tmin=[67.6899,101.696,131.201,169.192,219.202,259.234,313.191,389.695,533.193,906.221]+trigtime
  tmax=[101.690,131.696,169.201,219.192,259.202,313.234,391.191,539.695,913.193,1726.22]+trigtime
  
  evfiles='../00306757000-xrt/sw00306757000xwtw2po_cl.evt' 
  for i=0,9 do begin
     make_xselect_file,i,evfiles,0,tmin[i],tmax[i],'wt',/noreg,/src  
     spawn,'xselect @xsel'+ntostr(i+1)+'.xco > xselect'+ntostr(i+1)+'.out'
  endfor 
  
  return
end 
