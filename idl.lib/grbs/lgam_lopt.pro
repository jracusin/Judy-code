pro lgam_lopt

  ;;; exploring request of Demos for L_gamma vs L_opt

  readcol,'~/GRBs/grb_table_1259015432.txt',grb,t90,xtime,ra,dec,utime,v,other,format='(a,d,d,a,a,d,a,a)'  

  w=where(t90-xtime gt 0 and t90-utime gt 0 and utime ne 0 and xtime ne 0 and ra ne 'n/a')  
  
  eqsign1=strpos(v,'=')
  w1=where(eqsign1 ne -1)
  eqsign2=strpos(other,'=')
  w2=where(eqsign2 ne -1)
  w3=where(eqsign1[w] ne -1 and eqsign2[w] ne -1,nw3)
  
  help,w1,w2,w3
  w3=w[w3]

  plothist,t90[w3]-utime[w3],bin=10
  later=fltarr(nw3) & mag=later
  for i=0,nw3-1 do begin
     later[i]=max([utime[w3[i]],xtime[w3[i]]])
     pos=strpos(other[w3[i]],'White')
     mag[i]=strmid(other[w3[i]],pos+7,4)     
     if pos eq -1 then begin 
        pos=strpos(other[w3[i]],'V')
        mag[i]=strmid(other[w3[i]],pos+4,4)*1.
     endif 
    if pos eq -1 then begin 
        pos=strpos(other[w3[i]],'B')
        mag[i]=strmid(other[w3[i]],pos+4,4)*1.
     endif 
    print,strmid(other[w3[i]],pos,4)
  endfor 
;  colprint,grb[w3],t90[w3],utime[w3],xtime[w3],t90[w3]-later,other[w3]

  readcol,'~/GRBs/BAT_CAT.dat',tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,grbs,t90s,flux,tstart,tstop,format='(f,f,f,f,f,f,a,f,f,f,f)'
  match,grb,grbs,m1,m2
  help,m1,m2
  match,grbs[m2],grb[w3],m1,m2
  help,m1,m2
  print,mag
;  plothist,flux[m2]


  stop
return
end 
