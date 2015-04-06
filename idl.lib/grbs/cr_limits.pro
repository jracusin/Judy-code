pro cr_limits
  
  ;;purpose to plug in p=1.5 into all Jet CRs to get minimum limit for alpha
  
  
  cr=['ISMs2a','ISMs2b',$ ;'ISMs2ai','ISMs2bi',$
      'ISMs3a','ISMs3b',$ ;'ISMs3ai','ISMs3bi',$
      'WINDs2a','WINDs2b',$ ;'WINDs2ai','WINDs2bi',$
      'WINDs3a','WINDs3b',$ ;'WINDs3ai','WINDs3bi',$
;      'ISMf2a','ISMf2b',$ ;'ISMf2ai','ISMf2bi',$
;      'ISMf3a','ISMf3b',$ ;'ISMf3ai','ISMf3bi',$
;      'WINDf2a','WINDf2b',$ ;'WINDf2ai','WINDf2bi',$
;      'WINDf3a','WINDf3b',$ ;'WINDf3ai','WINDf3bi',$
      'JETs2a','JETs2b',$
      'JETs3a','JETs3b']
;      'JETsISM2a','JETsISM2b',$ ;;uniform jet (non-spreading)
;      'JETsISM3a','JETsISM3b',$
;      'JETsWIND2a','JETsWIND2b',$
;      'JETsWIND3a','JETsWIND3b',$
;      'JETsoism2','JETsoism3',$ ;;structured outflow
;      'JETsowind2','JETsowind3']
  
  n=n_elements(cr)
  
  p=[2.,1.5,$ ;2.,1.5,$  ;;ISMs2
     2.,1.5,$ ;2.,1.5,$  ;;ISMs3
     2.,1.5,$ ;2.,1.5,$  ;;WINDs2
     2.,1.5,$ ;2.,1.5,$  ;;WINDs3
;     2.,1.5,$ ;2.,1.5,$  ;;ISMf2
;     2.,1.5,$ ;2.,1.5,$  ;;ISMf3
;     2.,1.5,$ ;2.,1.5,$  ;;WINDf2
;     2.,1.5,$ ;2.,1.5,$  ;;WINDf3
     2.,1.5,$  ;;JET2
     2.,1.5]  ;;JET3
  
  b1=(p-1.)/2.
  b2=p/2.
  
  bb=[1,1,$ ;1,1,$  ;;ISMs2
     2,2,$ ;2,2,$  ;;ISMs3
     1,1,$ ;1,1,$  ;;WINDs2
     2,2,$ ;2,2,$  ;;WINDs3
;     1,1,$ ;1,1,$  ;;ISMf2
;     2,2,$ ;2,2,$  ;;ISMf3
;     1,1,$ ;1,1,$  ;;WINDf2
;     2,2,$ ;2,2,$  ;;WINDf3
     1,1,$  ;;JET2
     2,2]                       ;,$  ;;JET3
  
  w1=where(bb eq 1)
  w2=where(bb eq 2)
  b=fltarr(n)
  b[w1]=b1[w1]
  b[w2]=b2[w2]
    
  j=[0.75,0.75,$ ;0.75,0.75,$  ;;ISM
     0.75,0.75,$ ;0.75,0.75,$  ;;ISM
     0.5,0.5,$ ;0.5,0.5,$      ;;WIND
     0.5,0.5,$ ;0.5,0.5,$      ;;WIND
;     0.75,0.75,$ ;0.75,0.75,$  ;;ISM
;     0.75,0.75,$ ;0.75,0.75,$  ;;ISM
;     0.5,0.5,$ ;0.5,0.5,$      ;;WIND
;     0.5,0.5,$ ;0.5,0.5,$      ;;WIND
     0.,0.,$             ;;JET
     0.,0.]             ;;JET
  
  q=1.
  
  a=dblarr(n)
  for i=0,n-1 do begin 
     case i of 
        0: a[i]=3.*b[i]/2.
        1: a[i]=3.*(2.*b[i]+3)/16.
;        2: a[i]=(q-1.)+(2.+q)*b[i]/2.
;        3: a[i]=(q-7./16.)-(1.-4.*q)*b[i]/8.
        2: a[i]=(3.*b[i]-1.)/2.
        3: a[i]=(3.*b[i]+5.)/8.
;        6: a[i]=(q-2.)/2.+(2.+q)*b[i]/2.
;        7: a[i]=(4.*q+1.)/8.-(1.-4.*q)*b[i]/8.
        4: a[i]=b[i]/2.
        5: a[i]=b[i]/2.
;        10: a[i]=(q-1.)+(2.+q)*b[i]/2.
;        11: a[i]=(q-1.)+(2.+q)*b[i]/2.
        6: a[i]=(3.*b[i]-1.)/2.
        7: a[i]=(3.*b[i]+5.)/8.
;        14: a[i]=(q-2.)/2.+(2.+q)*b[i]/2.
;        15: a[i]=(4.*q+1.)/8.-(1.-4.*q)*b[i]/8.
;        8: a[i]=(3.*b[i]+1.)/2.
;        9: a[i]=(2.*b[i]+9.)/8.
;        18: a[i]=q/2.+(2.+q)*b[i]/2.
;        19: a[i]=(4.*q+5.)/8.-(1.-2.*q)*b[i]/4.
;        10: a[i]=(3.*b[i]-1.)/2.
;        11: a[i]=(b[i]+3.)/4.
;        22: a[i]=(q-2.)/2.+(2.+q)*b[i]/2.
;        23: a[i]=(2.*q+1.)/4.-(1.-2.*q)*b[i]/4.
;        12: a[i]=(1.-b[i])/2.
;        13: a[i]=(1.-b[i])/2.
;        26: a[i]=q/2.-(2.-q)*b[i]/2.
;        27: a[i]=q/2.-(2.-q)*b[i]/2.
;        14: a[i]=(3.*b[i]-1.)/2.
;        15: a[i]=(b[i]+3.)/4.
;        30: a[i]=(q-2.)/2.+(2.+q)*b[i]/2.
;        31: a[i]=(2.*q+1.)/4.-(1.-2.*q)*b[i]/4.
        8: a[i]=2.*b[i]+1.
        9: a[i]=(2.*b[i]+7.)/4.
        10: a[i]=2.*b[i]
        11: a[i]=(b[i]+3.)/2.
     endcase 
     if i lt 8 then a[i]=a[i]+j[i]
     
  endfor 

  colprint,cr,sigfig(a,4)
  plothist,a,bin=0.1
  
  stop
  return
end 
