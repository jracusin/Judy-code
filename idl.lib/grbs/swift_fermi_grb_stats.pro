pro swift_fermi_grb_stats,out

  cd,'~/Fermi/Swift_Fermi'

  readcol,'GBM_triggers_111021.txt',trigger_name,gname,gtrigtime,loc,format='(a,a,a,a)',delim='|',/silent
  
  g=where(strtrim(loc,2) eq 'GRB',ng)
  gname=gname[g]
  gtrigtime=gtrigtime[g]
  loc=loc[g]
  ggrb=strmid(gname,3,6)
  gmet=dblarr(ng)
  s=sort(ggrb)
  gname=gname[s] & gtrigtime=gtrigtime[s] & loc=loc[s] & ggrb=ggrb[s]
  for i=0,ng-1 do gmet[i]=date2met(gtrigtime[i],/fermi)
  print,'GBM GRBs - ',ng

  readcol,'Swift_GRBs_111021.txt',name,dur,btrigtime,bat,xrt,uvot,z,ot1,ot2,ot3,ot4,format='(a,a,a,a,a,a,f,a,a,a,a)',delim='|',/silent
  name=name[390:*] & dur=dur[390:*] & btrigtime=btrigtime[390:*] & bat=bat[390:*] & xrt=xrt[390:*] 
  uvot=uvot[390:*] & z=z[390:*] & ot1=ot1[390:*] & ot2=ot2[390:*] & ot3=ot3[390:*] & ot4=ot4[390:*]
  n=n_elements(name)
  bat=strtrim(bat,2)
  xrt=strtrim(xrt,2)
  uvot=strtrim(uvot,2)
  ot1=strtrim(ot1,2)
  ot2=strtrim(ot2,2)
  ot3=strtrim(ot3,2)
  ot4=strtrim(ot4,2)
  bmet=dblarr(n)
  for i=0,n-1 do bmet[i]=date2met(btrigtime[i])
  sgrb=strmid(name,4,6)

  m1=0
  m2=0
  for i=0,n-1 do begin
     w=where(ggrb eq sgrb[i],nw)
     if nw gt 0 then begin 
        a=approx(gmet[w],bmet[i],m)
;        print,a,bmet[i],abs(a-bmet[i]),gtrigtime[w],btrigtime[i]
        if abs(a-bmet[i]) lt 20. then begin 
           m1=[m1,w[m]]
           m2=[m2,i]
        endif 
     endif 
  endfor 
  m1=m1[1:*]
  m2=m2[1:*]

;; need to match better from trigcat to catch those bursts not having
;; circulars
;; by trigger time - within 20 s?


;  w=where((ot1 eq 'Fermi-GBM' or ot2 eq 'Fermi-GBM' or ot3 eq
;  'Fermi-GBM' or ot4 eq 'Fermi-GBM') and bat eq 'Y',nw)
  w=where(bat[m2] eq 'Y',nw)
  print,'Swift/GBM - ',nw
  out=strcompress(name[m2[w]],/rem)

;  wz=where((ot1 eq 'Fermi-GBM' or ot2 eq 'Fermi-GBM' or ot3 eq
;  'Fermi-GBM' or ot4 eq 'Fermi-GBM') and bat eq 'Y' and z ne 0.,nwz)
  wz=where(bat[m2] eq 'Y' and z[m2] ne 0.,nwz)
  print,'Swift/GBM + redshift - ',nwz

;  x=where((ot1 eq 'Fermi-GBM' or ot2 eq 'Fermi-GBM' or ot3 eq
;  'Fermi-GBM' or ot4 eq 'Fermi-GBM') and bat eq 'Y' and xrt eq
;  'Y',nx)
  x=where(bat[m2] eq 'Y' and xrt[m2] eq 'Y',nx)
  print,'Swift/GBM/XRT det - ',nx
  
;  nx=where((ot1 eq 'Fermi-GBM' or ot2 eq 'Fermi-GBM' or ot3 eq
;  'Fermi-GBM' or ot4 eq 'Fermi-GBM') and bat eq 'Y' and xrt ne
;  'Y',nnx)
  nx=where(bat[m2] eq 'Y' and xrt[m2] ne 'Y',nnx)
  print,'Swift/GBM/ no XRT det - ',nnx

;  u=where((ot1 eq 'Fermi-GBM' or ot2 eq 'Fermi-GBM' or ot3 eq
;  'Fermi-GBM' or ot4 eq 'Fermi-GBM') and bat eq 'Y' and uvot eq
;  'Y',nu)
  u=where(bat[m2] eq 'Y' and uvot[m2] eq 'Y',nu)
  print,'Swift/GBM / UVOT det - ',nu


  l=where((ot1 eq 'Fermi-LAT' or ot2 eq 'Fermi-LAT' or ot3 eq 'Fermi-LAT' or ot4 eq 'Fermi-LAT' or strtrim(name,2) eq 'GRB 100728A') and bat eq 'Y',nl)
  print,'Swift/LAT - ',nl

  lz=where((ot1 eq 'Fermi-LAT' or ot2 eq 'Fermi-LAT' or ot3 eq 'Fermi-LAT' or ot4 eq 'Fermi-LAT' or strtrim(name,2) eq 'GRB 100728A') and bat eq 'Y' and z ne 0,nlz)
  print,'Swift/LAT + redshift - ',nlz

  f=where((ot1 eq 'Fermi-LAT' or ot2 eq 'Fermi-LAT' or ot3 eq 'Fermi-LAT' or ot4 eq 'Fermi-LAT') and bat eq 'U',nf)
  print,'Swift follow-up - ',nf
  
  lx=where((ot1 eq 'Fermi-LAT' or ot2 eq 'Fermi-LAT' or ot3 eq 'Fermi-LAT' or ot4 eq 'Fermi-LAT') and bat eq 'U' and xrt eq 'Y',nlx)
  print,'Swift follow-up / XRT det - ',nlx
  
  lu=where((ot1 eq 'Fermi-LAT' or ot2 eq 'Fermi-LAT' or ot3 eq 'Fermi-LAT' or ot4 eq 'Fermi-LAT') and bat eq 'U' and uvot eq 'Y',nlu)
  print,'Swift follow-up / UVOT det - ',nlu

  lzm=where((ot1 eq 'Fermi-LAT' or ot2 eq 'Fermi-LAT' or ot3 eq 'Fermi-LAT' or ot4 eq 'Fermi-LAT') and bat eq 'U' and z ne 0.,nlz)
  print,'Swift follow-up + redshift - ',nlz

stop
  return
end 
