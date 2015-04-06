function month,mon

  t=type(mon)
  n=n_elements(mon)
  if t ne 7 then put=strarr(n) else put=intarr(n)

  for i=0,n-1 do begin 
     if t eq 7 then begin 
        case mon[i] of
           'Jan': out=1
           'Feb': out=2
           'Mar': out=3
           'Apr': out=4
           'May': out=5
           'Jun': out=6
           'Jul': out=7
           'Aug': out=8
           'Sep': out=9
           'Oct': out=10
           'Nov': out=11
           'Dec': out=12
           else: out=0
        endcase 
     endif else begin 
        case mon[i] of
           1: out='Jan'
           2: out='Feb'
           3: out='Mar'
           4: out='Apr'
           5: out='May'
           6: out='Jun'
           7: out='Jul'
           8: out='Aug'
           9: out='Sep'
           10: out='Oct'
           11: out='Nov'
           12: out='Dec'
           else: out=''
        endcase
     endelse

     put[i]=out
  endfor 

return,put
end 
