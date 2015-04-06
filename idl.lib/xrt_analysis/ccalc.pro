readcol,'HD131156off3.cat',lista,format='(a)'
xout=fltarr(n_elements(lista))
yout=fltarr(n_elements(lista))
for j=0,n_elements(lista)-1 do begin
tab=mrdfits(lista[j],0,hdr,/silent)
cntrd,tab,241.42,524.32,xo,yo,5
print,lista[j],xo,yo
xout[j]=xo
yout[j]=yo
endfor
print,'Mean X-centr: ',mean(xout)
print,'St dev X-centr: ',stdev(xout)
print,'Mean Y-centr: ',mean(yout)
print,'St dev Y-centr: ',stdev(yout)
end
