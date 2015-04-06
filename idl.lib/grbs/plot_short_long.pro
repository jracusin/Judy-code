pro plot_short_long
  
  cd,!mdata
  restore,'cr_files.sav'
  
  z=where(cr[last].z gt 0 and cr[last].eiso gt 0 and cr[last].theta ne 0)
  z=last[z]
  
  s=where(cr[z].who_eiso eq 'shb')
  s=z[s]
  
  jb=[gold,silver];,pewter,iron]
  match,jb,z,m1,m2
  zjb=jb[m1]
  
  match,zjb,s,m1,m2
  sjb=s[m2]
  
  dont_match,zjb,s,m1,m2
  ljb=zjb[m1]
  help,sjb,ljb
  
  segam=alog10(cr[sjb].eiso*1d52*(1.-cos(cr[sjb].theta*!dtor)))
  legam=alog10(cr[ljb].eiso*1d52*(1.-cos(cr[ljb].theta*!dtor)))
  
;  begplot,name='thetaj_egam_short_long1.eps',/encap
  plotsym,0,/fill
  aplot,1,[47,52],[0,30],/nodata,ytitle='Opening Angle (deg)',xtitle='log E!L'+!tsym.gamma+'!N (erg)'
  oplot,segam,cr[sjb].theta,psym=1
  oplot,legam,cr[ljb].theta,psym=8
;  endplot
  
  stop
  return
end 
