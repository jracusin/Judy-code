pro find_sip
  
  BAT_time=date2met('2005-234-03:49:29')
  files=['00151486000-xrt/sw00151486000xpcw4po_cl_ff.evt',$
         '00151486001-xrt/sw00151486001xpcw4po_cl.evt',$
         '00151486002-xrt/sw00151486002xpcw4po_cl.evt',$
         '00151486003-xrt/sw00151486003xpcw4po_cl.evt',$
         '00151486004-xrt/sw00151486004xpcw4po_cl.evt',$
         '00151486005-xrt/sw00151486005xpcw4po_cl.evt',$
         '00151486006-xrt/sw00151486006xpcw4po_cl.evt',$
         '00151486007-xrt/sw00151486007xpcw4po_cl.evt',$
         '00151486008-xrt/sw00151486008xpcw4po_cl.evt',$
         '00151486009-xrt/sw00151486009xpcw4po_cl.evt',$
         '00151486010-xrt/sw00151486010xpcw4po_cl.evt',$
         '00151486011-xrt/sw00151486011xpcw4po_cl.evt',$
         '00151486012-xrt/sw00151486012xpcw4po_cl.evt',$
         '00151486013-xrt/sw00151486013xpcw4po_cl.evt',$
         '00151486014-xrt/sw00151486014xpcw4po_cl.evt',$
         '00151486015-xrt/sw00151486015xpcw4po_cl.evt',$
         '00151486016-xrt/sw00151486016xpcw4po_cl.evt',$
         '00151486018-xrt/sw00151486018xpcw4po_cl.evt',$
         '00151486020-xrt/sw00151486020xpcw4po_cl.evt',$
         '00151486021-xrt/sw00151486021xpcw4po_cl.evt',$
         '00151486022-xrt/sw00151486022xpcw4po_cl.evt']
  
  plot,[100,5e6],[10,10000],/nodata;,/xlog,/ylog
  colors=[!red,!blue,!green,!orange,!purple,!magenta,!cyan,!hotpink,!lightgreen,!sienna,!violet,!darkred]
  colors=[colors,colors]
   for i=0,n_elements(files)-1 do begin
      s=mrdfits(files[i],1)
      oplot,s.time-bat_time,s.pha,color=colors[i],psym=1
   endfor 
   
   return
end 
