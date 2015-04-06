pro hist_tam,f_tam

;set_plot,'ps'
;device,filename='hist_tam.ps',/landscape

openr,lun_tamcent,f_tam,/get_lun
nl_tamcent=get_lines(f_tam)
data_tam=dblarr(nl_tamcent,11)

for n=0,nl_tamcent-1 do begin
        line=parse_str(lun_tamcent)
        data_tam(n,*)=line
endfor

xtit=['frame1_xpos (pix)','frame1_ypos (pix)','frame2_xpos (pix)','frame2_ypos (pix)']
title='TAM centroid postions'

!p.multi=[0,2,2]

plothist,data_tam[*,5],xhist,yhist,bin=.01,xtitle=xtit[0],title=title
a=[max(yhist),median(xhist),stddev(data_tam[*,5])]
g=gaussfit(xhist,yhist,est=a,a1,nterms=3)
oplot,xhist,g
a1=strcompress(string(float(a1)),/remove_all)
legend,['bin size = 0.01','mean = '+a1[1],'sigma = '+a1[2]],/top,/right,box=0

plothist,data_tam[*,6],xhist,yhist,bin=.01,xtitle=xtit[1],title=title
a=[max(yhist),median(xhist),stddev(data_tam[*,6])]
g=gaussfit(xhist,yhist,est=a,a2,nterms=3)
oplot,xhist,g
a2=strcompress(string(float(a2)),/remove_all)
legend,['bin size = 0.01','mean = '+a2[1],'sigma = '+a2[2]],/top,/right,box=0

plothist,data_tam[*,8],xhist,yhist,bin=.01,xtitle=xtit[2],title=title
a=[max(yhist),median(xhist),stddev(data_tam[*,8])]
g=gaussfit(xhist,yhist,est=a,a3,nterms=3)
oplot,xhist,g
a3=strcompress(string(float(a3)),/remove_all)
legend,['bin size = 0.01','mean = '+a3[1],'sigma = '+a3[2]],/top,/right,box=0

plothist,data_tam[*,9],xhist,yhist,bin=.01,xtitle=xtit[3],title=title
a=[max(yhist),median(xhist),stddev(data_tam[*,9])]
g=gaussfit(xhist,yhist,est=a,a4,nterms=3)
oplot,xhist,g
a4=strcompress(string(float(a4)),/remove_all)
legend,['bin size = 0.01','mean = '+a4[1],'sigma = '+a4[2]],/top,/right,box=0
!p.multi=0
;device,/close

wy1a=where(data_tam[*,1] lt 1070)
wy1b=where(data_tam[*,1] gt 1100 and data_tam[*,1] lt 1120)
wy1c=where(data_tam[*,1] gt 1135 and data_tam[*,1] lt 1155)
print,'  frames         mean(y1)      median(y1)'
print,'    <1070',mean(data_tam[wy1a,6]),median(data_tam[wy1a,6])
print,'1100-1120',mean(data_tam[wy1b,6]),median(data_tam[wy1b,6])
print,'1135-1155',mean(data_tam[wy1c,6]),median(data_tam[wy1c,6])

print,''
wy2a=where(data_tam[*,1] lt 1070)
wy2b=where(data_tam[*,1] gt 1100 and data_tam[*,1] lt 1120)
wy2c=where(data_tam[*,1] gt 1135 and data_tam[*,1] lt 1155)
print,'  frames         mean(y2)      median(y2)'
print,'    <1070',mean(data_tam[wy2a,9]),median(data_tam[wy2a,9])
print,'1100-1120',mean(data_tam[wy2b,9]),median(data_tam[wy2b,9])
print,'1135-1155',mean(data_tam[wy2c,9]),median(data_tam[wy2c,9])


stop

return
end 