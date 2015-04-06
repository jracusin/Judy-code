pro peakflux

  readcol,'bat_grb_fluence_pflux_bat2.txt',GRB1,Trig,Mo_S,Sband1,Sband1_e,Sband2,Sband2_e,Sband3,Sband3_e,Sband4,Sband4_e,Stot,Stot_e,St_st,St_end,Mo_F,Fband1,Fband1_e,Fband2,Fband2_e,Fband3,Fband3_e,Fband4,Fband4_e,Ftot1,Ftot_e,Ft_st,Ft_end,format='(a,l,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)',/silent

  readcol,'bat_grb_flux.txt',GRB2,Trig,Mo_S,Sband1,Sband1_e,Sband2,Sband2_e,Sband3,Sband3_e,Sband4,Sband4_e,Stot,Stot_e,St_st,St_end,Mo_F,Fband1,Fband1_e,Fband2,Fband2_e,Fband3,Fband3_e,Fband4,Fband4_e,Ftot2,Ftot_e,Ft_st,Ft_end,format='(a,l,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)',/silent

  flux1=ftot1*1.
  flux2=ftot2*1.

  s1=sort(flux1)
  s2=sort(flux2)

  n1=n_elements(flux1)
  n2=n_elements(flux2)

;  colprint,grb1[s1[n1-10:*]],flux1[s1[n1-10:*]]
  
;  print
;  colprint,grb2[s2[n2-10:*]],flux2[s2[n2-10:*]]


  flux=[flux1,flux2]
  grb=[grb1,grb2]
  s=sort(flux)
  n=n1+n2
  x=10
  colprint,grb[s[n-x:*]],replicate('   ',x),ntostr(flux[s[n-x:*]],4)

stop
return
end 
