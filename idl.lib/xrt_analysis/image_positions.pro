pro image_positions,ldps,xguess,yguess,theta_roll,alpha_SC,delta_SC

  files=''
  for i=0,n_elements(ldps)-1 do begin
     file=findfile('*LDP'+ntostr(ldps[i])+'*im*image*')
     if file[0] ne '' then files=[files,file]
  endfor 
  lista=files[1:*]
  
  
  
;  readcol,'HD131156off3.cat',lista,format='(a)'
  nl=n_elements(lista)
  xout=dblarr(nl)
  yout=dblarr(nl)
  scra=dblarr(nl)
  scdec=dblarr(nl)

  for j=0,n_elements(lista)-1 do begin
     tab=mrdfits(lista[j],0,hdr,/silent)
     target=sxpar(hdr,'TARGETID')
     rdis,tab,low=min(tab),high=max(tab)
     xyouts,100,0,'100',/device
     xyouts,200,0,'200',/device
     xyouts,300,0,'300',/device
     xyouts,400,0,'400',/device
     xyouts,500,0,'500',/device
     xyouts,600,0,'600',/device
     xyouts,0,100,'100',/device
     xyouts,0,200,'200',/device
     xyouts,0,300,'300',/device
     xyouts,0,400,'400',/device
     xyouts,0,500,'500',/device
     xyouts,0,600,'600',/device
     read,'enter the xguess and yguess: ',xguess,yguess
;     cntrd,tab,241.42,524.32,xo,yo,5
	cntrd,tab,xguess,yguess,xo,yo,5
     print,lista[j],xo,yo,' target='+strtrim(string(target),2)
     xout[j]=xo
     yout[j]=yo
     scra[j]=sxpar(hdr,'RA')
     scdec[j]=sxpar(hdr,'DEC')
  endfor
  stop
  print,'Mean X-centr: ',mean(xout)
  print,'St dev X-centr: ',stddev(xout)
  print,'Mean Y-centr: ',mean(yout)
  print,'St dev Y-centr: ',stddev(yout)
  print,'Mean SC RA: ',mean(scra)
  print,'Mean SC DEC: ',mean(scdec)

  return
end 
