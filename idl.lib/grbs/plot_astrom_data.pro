pro plot_astrom_data,grbs
  
  ngrb=n_elements(grbs)
  n=sqrt(ngrb)
  !p.multi=[0,round(n+0.5),round(n)]
  for i=0,n_elements(grbs)-1 do begin
     ralist=[grbs[i].xra,grbs[i].ora]
     declist=[grbs[i].xdec,grbs[i].odec]
     errlist=[grbs[i].xerr,grbs[i].oerr]
     names=['XRT refined position','Optical position']
     compare_plots,grbs[i].xra,grbs[i].xdec,ralist,declist,errlist,names,grbs[i].new_xra,grbs[i].new_xdec,grbs[i].new_xerr,title=grbs[i].grb
     
  endfor
  !p.multi=0



;  plot,[-5,5],[-5,5],/nodata,xtitle='dRA',ytitle='dDec'

;  clr=[!red,!blue,!green,!orange,!yellow,!cyan,!magenta,!deeppink,!purple,!darkred,!royalblue,!salmon,!hotpink,!violet,!grey,!navyblue,!seagreen,!sienna,!forestgreen,!lightgreen,!midnightblue,!darkslategrey,!darkblue,!skyblue,!darkgreen]
;  clr=[clr,clr,clr,clr]

;  for i=0,n_elements(grbs)-1 do begin
;     tvcircle,grbs[i].xerr,0,0,/data,color=clr[i]
;     xra=(grbs[i].xra-grbs[i].new_xra)*3600./cos(grbs[i].xdec*!dtor)
;     xdec=(grbs[i].xdec-grbs[i].new_xdec)*3600.
;     ora=(grbs[i].xra-grbs[i].ora)*3600./cos(grbs[i].xdec*!dtor)
;     odec=(grbs[i].xdec-grbs[i].odec)*3600.

;     tvcircle,grbs[i].new_xerr,xra,xdec,/data,color=clr[i]
;     plots,ora,odec,psym=2,color=clr[i]
;  endfor

  return
end
