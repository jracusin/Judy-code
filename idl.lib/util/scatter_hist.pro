pro scatter_hist,x,y,xerrs,yerrs,bin1=bin1,bin2=bin2,xrange=xrange,yrange=yrange,psyms=psyms,colors=colors,xtitle=xtitle,ytitle=ytitle,w0=w0,w1=w1,w2=w2,w3=w3,w4=w4,w5=w5,leg=leg,pw0=pw0,pw1=pw1,pw2=pw2,pcolors=pcolors,fill=fill,forient=forient,fline=fline,plines=plines,xlog=xlog,ylog=ylog,right=right,top=top,bottom=bottom,left=left,center=center,com=com,charsize=charsize,extralines=extralines,wex0=wex0,wex1=wex1,wex2=wex2

  multiplot2,[2,2],/init,/square
  multiplot2
  sx=size(xerrs)
  if n_elements(sx) eq 5 then sx=sx[1] else sx=sx[0]
  sy=size(yerrs)
  if n_elements(sy) eq 5 then sy=sy[1] else sy=sy[0]
  if sy eq 1 and sx eq 1 then begin 
     xerr=dblarr(2,n_elements(xerrs))
     xerr[0,*]=xerrs
     xerr[1,*]=xerrs
     yerr=dblarr(2,n_elements(yerrs))
     yerr[0,*]=yerrs
     yerr[1,*]=yerrs
  endif else begin 
     xerr=xerrs
     yerr=yerrs
  endelse 
  if n_elements(w1) eq 0 then w1=-1
  if n_elements(w2) eq 0 then w1=-1
  if n_elements(w3) eq 0 then w1=-1
  if n_elements(w4) eq 0 then w1=-1
  if n_elements(w5) eq 0 then w1=-1
  
  if n_elements(xrange) eq 0 then xrange=prange(x,xerr[0,*])
  if n_elements(yrange) eq 0 then yrange=prange(y,yerr[0,*])
;  g=where(x ne 0 and y ne 0)
  g=[pw0,pw1,pw2]
  
  plot,xrange,yrange,/nodata,ytitle=ytitle,yrange=yrange,xrange=xrange,/ysty,xlog=xlog,ylog=ylog,xtick_get=xtickv,/xsty,charsize=charsize
  if n_elements(w0) eq 0 and n_elements(w1) eq 0 and n_elements(w2) eq 0 and n_elements(w3) eq 0 and n_elements(w4) eq 0 and n_elements(w5) eq 0 then w1=indgen(n_elements(x))
  oploterror2,x[w0],y[w0],xerr[*,w0],yerr[*,w0],psym=psyms[0],color=colors[0]
  if w1[0] ne -1 then oploterror2,x[w1],y[w1],xerr[*,w1],yerr[*,w1],psym=psyms[1],color=colors[1]
  if w2[0] ne -1 then oploterror2,x[w2],y[w2],xerr[*,w2],yerr[*,w2],psym=psyms[2],color=colors[2]
  if w3[0] ne -1 then oploterror2,x[w3],y[w3],xerr[*,w3],yerr[*,w3],psym=psyms[3],color=colors[3]
  if w4[0] ne -1 then oploterror2,x[w4],y[w4],xerr[*,w4],yerr[*,w4],psym=psyms[4],color=colors[4]
  if w5[0] ne -1 then oploterror2,x[w5],y[w5],xerr[*,w5],yerr[*,w5],psym=psyms[5],color=colors[5]

  if n_elements(extralines) gt 0 then begin 
     for i=0,n_elements(extralines)-1 do begin 
        tmp=execute(extralines[i])
     endfor 
  endif 

  legend,leg,box=0,textcolor=pcolors,right=right,top=top,bottom=bottom,left=left,center=center,charsize=1.
  if n_elements(com) gt 0 then tmp=execute(com)

  ;;; right hist plot
  multiplot2,xrightgap=0.2
  
  if n_elements(bin1) eq 0 then bin1=1
  if n_elements(bin2) eq 0 then bin2=1

  if keyword_set(ylog) then begin 
     logy=alog10(y) 
     lyrange=alog10(yrange)
  endif else begin 
     logy=y
     lyrange=yrange
  endelse 

  plothist,logy[g],xhist,yhist,bin=bin2,/noplot
  my=max(yhist)
  if my gt 20 then b=10
  if my le 20 and max(yhist) ge 15 then b=5
  if my lt 15 then b=2
  my=my+(my mod b)
  ytickv=my/b+1 
  ytickname=[' ',ntostr((indgen(ytickv)+1)*b)]
  hrange=[0,my]

  plot,hrange,lyrange,/nodata,yrange=lyrange,xrange=hrange,/ysty,xtitle='N',xtickname=ytickname,/xsty,charsize=charsize

  plothist,logy[pw0],color=pcolors[0],bin=bin2,lines=plines[0],fill=fill[0],forient=forient[0],fline=fline[0],fcolor=pcolors[0],/over,/rotate,xrange=hrange,/xsty
  if n_elements(pw1) ne 0 then plothist,logy[pw1],bin=bin2,color=pcolors[1],line=plines[1],fill=fill[1],forient=forient[1],fline=fline[1],fcolor=pcolors[1],/over,/rotate,xrange=hrange,/xsty
  if n_elements(pw2) ne 0 then plothist,logy[pw2],bin=bin2,color=pcolors[2],line=plines[2],fill=fill[2],forient=forient[2],fline=fline[2],fcolor=pcolors[2],/over,/rotate,xrange=hrange,/xsty
;  xyouts,24,46,'log L!Lx, 1 day!N (erg s!U-1!N)',orient=-90,/data
  oplot,[0,0],lyrange

  ;;; bottom hist plot
  multiplot2,yupgap=0.17,xrightgap=0

  nrange=[0,20]
  arange=[48,56]

  if keyword_set(xlog) then begin 
     logx=alog10(x) 
     w=where(x eq 0,nw)
     if nw gt 0 then logx[w]=xrange[0]*1d-5
     lxrange=alog10(xrange)
     xtickname='10!U'+ntostr(fix(alog10(xtickv)))+'!N'
     xticks=n_elements(xtickname)-1
  endif else begin
     logx=x
     lxrange=xrange
  endelse 

  plothist,logx[g],xhist,yhist,bin=bin1,/noplot
  hrange=[0,max(yhist)]
  plot,hrange,lxrange,/nodata,yrange=hrange,xrange=lxrange,/xsty,xtitle=xtitle,xtickname=xtickname,ytitle='N',charsize=charsize,xticks=xticks
  plothist,logx[pw0],color=pcolors[0],bin=bin1,lines=plines[0],fill=fill[0],forient=forient[0],fline=fline[0],fcolor=pcolors[0],/over
  if n_elements(pw1) ne 0 then plothist,logx[pw1],bin=bin1,color=pcolors[1],line=plines[1],fill=fill[1],forient=forient[1],fline=fline[1],fcolor=pcolors[1],/over
  if n_elements(pw2) ne 0 then plothist,logx[pw2],bin=bin1,color=pcolors[2],line=plines[2],fill=fill[2],forient=forient[2],fline=fline[2],fcolor=pcolors[2],/over
  oplot,lxrange,[0,0]
  multiplot2,/reset,/default

  return
end 
