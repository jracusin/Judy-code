PRO SCI_NOT, a

; read in the energy flux array and reformat the scientific notation to optimal form
FOR i=0, N_ELEMENTS(a[0,*])-1 DO BEGIN
  b=STRMID(a[0,i], 0, 5)
  c1=STRMID(a[0,i], STRPOS(a[0,i], 'E')+2)
  c2=STRMID(a[1,i], STRPOS(a[1,i], 'E')+2)
  d=STRMID(a[1,i], 0, STRPOS(a[1,i], 'E'))
  e=FLOAT(d)/10^(FIX(c2)-FIX(c1))
  e=decimals(e, 3)
  a[0,i]='('+STRING(b)
  a[1,i]=' +/- '+STRING(e)+')E-'+STRING(c1)
ENDFOR

END