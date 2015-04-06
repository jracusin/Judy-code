pro find_errors, fname ,bfit,err90,err68

;fname is a file which contains the output
;of an XSPEC single dimension steppar run.

a=rdfile(fname,3,lun=lun)

N=(size(a))[2]

;fit a 3rd order spline to the data

y2=spl_init(a[2,*],a[1,*])
del=(a[2,N-1]-a[2,0])/float(N)
x=findgen(N*1000)*(del/1000.0)+a[2,0]
f = SPL_INTERP(a[2,*],a[1,*],y2,x)

lowf=min(abs(f),minf)

f[*]=f[*]-lowf

a[1,*]=a[1,*]-lowf

print, '*********************************************************'
;print, 'Best fit: ', x[minf], ' +/- ',

plot, a[2,*],a[1,*]

oplot, x,f

IF bfit EQ 0. THEN bfit=x[minf]

yrange=[min(a[1,*])-100.,max(a[1,*])*2.]
oplot,[bfit,bfit],yrange

foo=where(abs(f-2.706) lt 0.01)
err90=fltarr(2)
if (size(foo))[0] gt 0 then BEGIN
   tmp1=x[where(abs(f-2.706) lt 0.01)]
   N90=(size(tmp1))[1]
;stop
   err90[0]=tmp1[0]-bfit
   err90[1]=tmp1[N90-1]-bfit
   print, 'Best fit: ', bfit, err90[0],err90[1]
   print, '90 percent confidence: ',tmp1[0],tmp1[N90-1]
   oplot,[tmp1[0],tmp1[0]],yrange,line=1
   oplot,[tmp1[N90-1],tmp1[N90-1]],yrange,line=1
endif else BEGIN 
    print, 'Best fit: ', x[minf]
    print, '90 percent confidence: larger than input range!'
ENDELSE 

foo=where(abs(f-1.0) lt 0.01)
err68=fltarr(2)
if (size(foo))[0] gt 0 then begin
   tmp2=x[where(abs(f-1.0) lt 0.01)]
   N68=(size(tmp2))[1]
   print, '68 percent confidence: ',tmp2[0],tmp2[N68-1]
   err68[0]=tmp2[0]-bfit
   err68[1]=tmp2[N68-1]-bfit
   oplot,[tmp2[0],tmp2[0]],yrange,line=2
   oplot,[tmp2[N68-1],tmp2[N68-1]],yrange,line=2
endif else print, '68 percent confidence: larger than input range!'
print, '*********************************************************'

close,lun
free_lun,lun
return
end


