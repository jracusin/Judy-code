@fit_functions
@calc_eiso2
pro toy_seds

eng=[indgen(99)*10.,indgen(100)*300.+1000.]

;alpha=-0.32
;epeak=228.4
;beta=-2.65
;alpha=-0.86
;epeak=207.6
;beta=-2.43
;alpha=-0.73
;epeak=139.1
;beta=-2.27
;e0=epeak/(2.+alpha)

;band=band2(eng,alpha,e0,beta)

;alpha=-1.46
;alpha=-1.56
;alpha=-1.617
alpha=-2.09
po=pow(eng,[1.,-alpha])
;n1=band[10]/po[10]
;po=pow(eng,[n1,-alpha])

;alpha=-0.42
;ep=275.0
;alpha=-0.94
;ep=243.2
;alpha=-0.94
;ep=192.5
alpha=-1.375
ep=45.59
comp=cutoff_pow(eng,[1.,-alpha,ep])
;n2=band[10]/comp[10]
n2=po[10]/comp[10]
comp=cutoff_pow(eng,[n2,-alpha,ep])

nu=!tsym.nu
plot,[1,1e5],[1,100e3],/nodata,/xlog,/ylog,xrange=[1.,1e5],xtitle='Energy (keV)',ytitle=nu+'F!L'+nu+'!N'
oplot,eng,po*eng^2*1e3,line=0
oplot,eng,comp*eng^2*1e3,line=1
;oplot,eng,band*eng^2,line=2

;legend,['PL','Comp','Band'],box=0,/top,/left,line=[0,1,2]
legend,['PL','Comp'],box=0,/top,/left,line=[0,1]
stop

return
end 
