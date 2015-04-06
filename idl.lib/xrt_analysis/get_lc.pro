pro get_lc,lc1,lc2,lc3,lc4,mint,btime,modes,time,density,err


    time=[mint[0]+lc1.x,mint[1]+lc2.x,mint[2]+lc3.x,mint[3]+lc4.x]-btime
    density=[lc1.density,lc2.density,lc3.density,lc4.density]
    err=[lc1.error,lc2.error,lc3.error,lc4.error]

;    s=sort(time)
;    time=time[s]
;    density=density[s]
;    err=err[s]
;    modes=modes[s]

return
end 
    
