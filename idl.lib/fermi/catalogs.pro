pro catalogs

  c=create_struct('catalog','','n',0,'years',0.,'type','','comment','',$
                  'xoffset',0.,'yoffset',0.)
  c=replicate(c,30)

  c.xoffset=0.8
  c.yoffset=1.1


  i=0
  n=4
  c[i+0:i+n-1].type='FGL'
  c[i+0:i+n-1].catalog=ntostr(indgen(n))+'FGL'
  c[i+0:i+n-1].n=[205,1451,1873,3034]
  c[i+0:i+n-1].years=[3./12.,11./12.,2,4]

  i=n+i
  n=2
  c[i+0:i+n-1].type='FHL'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[514,360]
  c[i+0:i+n-1].years=[3,80/12.]
  c[i].comment='>10 GeV'
  c[i+1].comment='>50 GeV'
  c[i].xoffset=0.9
;  c[i+n-1].xoffset=0.65
  c[i+n-1].yoffset=0.65

  i=n+i
  n=3
  c[i+0:i+n-1].type='LAC'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[709,1017,1591]
  c[i+0:i+n-1].years=[11./12.,2,4]
  
  i=n+i
  n=2
  c[i+0:i+n-1].type='FAVA'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[215,395]
  c[i+0:i+n-1].years=[47./12.,7]

  i=n+i
  n=2
  c[i+0:i+n-1].type='LGRB'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[35,130]
  c[i+0:i+n-1].years=[3.,7.]
  c[i+n-1].yoffset=1.2

  i=n+i
  n=2
  c[i+0:i+n-1].type='PC'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[46,117]
  c[i+0:i+n-1].years=[0.5,3.]

  i=n+i
  n=2
  c[i+0:i+n-1].type='PWN'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[10.,58]
  c[i+0:i+n-1].years=[16/12.,45./12.]
    
  i=n+i
  n=1
  c[i+0:i+n-1].type='SNR'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[30]
  c[i+0:i+n-1].years=[3.]
  c[i+0:i+n-1].yoffset=0.65

  i=n+i
  n=3
  c[i+0:i+n-1].type='GGRB'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[491,953,1403]
  c[i+0:i+n-1].years=[2.,4.,6]
  
  i=n+i
  n=1
  c[i+0:i+n-1].type='Mag'
  c[i+0:i+n-1].catalog=c[i].type
  c[i+0:i+n-1].n=[440]
  c[i+0:i+n-1].years=[5.]

  i=n+1
  n=1
  c[i+0:i+n-1].type='TGF'
  c[i+0:i+n-1].catalog=ntostr(indgen(n)+1)+c[i].type
  c[i+0:i+n-1].n=[2700]
  c[i+0:i+n-1].years=[7]

  w=where(c.n gt 0)
  c=c[w]

  types=c[rem_dup(c.type)].type
  nt=n_elements(types)
  colors=['red','orange','blue','dark orchid','green','light sky blue','dark red','hot pink','indigo','salmon','aqua']

  p=plot([0.1,10],[1,1e4],/nodata,xrange=[0.1,10],yrange=[1,1e4],/xlog,/ylog,xtitle='Integration Time (Years)',ytitle='Number of Sources',font_size=14.,title='        Catalogs')
  t=text(0.423,0.912,'Fermi',font_style='it',font_size=14.)

  for i=0,nt-1 do begin
     w=where(strtrim(c.type,2) eq strtrim(types[i],2),nw)
     p1=scatterplot(c[w].years,c[w].n,symbol='dot',sym_color=colors[i],/sym_filled,sym_size=10.,/overplot)
     if nw gt 1 then p2=plot(c[w].years,c[w].n,color=colors[i],/overplot)
     for j=0,nw-1 do t=text(c[w[j]].years*c[w[j]].xoffset,c[w[j]].n*c[w[j]].yoffset,c[w[j]].catalog,/data,color=colors[i])

  endfor 

;  k=get_kbrd(10)
;  if k eq 's' then stop
  
  p.save,'~/Fermi/Senior_Review/SR2016/catalogs.png'
  p.close
stop
return
end 
     
