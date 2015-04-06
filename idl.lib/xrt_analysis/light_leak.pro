pro light_leak
  
;  data from light_leak_test file in home dir
;  set_plot,'ps'
;  device,filename='light_leak.ps';,/landscape
  dir='~/light_leak_test/'
  begplot,name=dir+'light_leak.ps'
  !p.multi=[0,1,3]
;test 1  
  bmean=[-.23,-.24,-.19,-.19]
  meanf=[0.25,0.26,0.27,0.08]
  volt=[30,60,120,120]
  
  plot,volt,bmean,psym=2,xrange=[-20,150],yrange=[-.3,.3],xtitle='Lamp (VAC)',title='Light Leak Test 1 - xrt_20041610343',ytitle='DN',charsize=1.5
  oplot,volt,meanf,psym=4
  legend,['bias map','mean frame'],psym=[2,4],box=0,/top,/left,charsize=1
  
;test 2
  bmean=[-.16,-.13,-.24,-.16]
  meanf=[0.09,0.10,0.07,0.06]
  volt=[0,120,60,30]
  
  plot,volt,bmean,psym=2,xrange=[-20,150],yrange=[-.3,.3],xtitle='Chamber Light (VAC)',title='Light Leak Test 2 - xrt_20041610908',ytitle='DN',charsize=1.5
  oplot,volt,meanf,psym=4
  legend,['bias map','mean frame'],psym=[2,4],box=0,/top,/left,charsize=1
  
;test 3
  bmean=[-0.17,-0.24,-0.21,-0.23]
  meanf=[0.04,0.08,0.11,0.06]
  volt=[0,120,240,0]
  
  plot,volt,bmean,psym=2,xrange=[-20,300],yrange=[-.3,.3],xtitle='Lamp + Chamber Light (VAC)',title='Light Leak Test 3 - xrt_20041611810',ytitle='DN',charsize=1.5
  oplot,volt,meanf,psym=4
  legend,['bias map','mean frame'],psym=[2,4],box=0,/top,/left,charsize=1
  
  !p.multi=0
  endplot
;  device,/close
  return
end 
