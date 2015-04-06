PRO read_fit_results,gfit,nH,kT,abund,Tau_u,norm

  nfiles=6;7

  temp=mrdfits('/home/whitebirch/racusin/fall_project/data_products/result_template.fit',1)

  fit=read_ascii('combined_fit_result.txt',temp=temp)

  gfit=create_struct('model_par',0L,'fit_par',0L,'parameter','','value',0D)

  n=n_elements(fit.field01)
  
  gfit=replicate(gfit,n-5)

  gfit.model_par=fit.field01[0:n-6]
  gfit.fit_par=fit.field03[0:n-6]
  gfit.parameter=fit.field05[0:n-6]
  gfit.value=fit.field07[0:n-6]

  nH=gfit[0].value
  w=where(gfit.parameter EQ 'kT')
  kT=gfit[w].value
  w=where(gfit.parameter EQ 'Tau_u')
  Tau_u=gfit[w].value
  w=where(gfit.parameter EQ 'norm')
  norm=gfit[w].value
  abund=gfit[indgen(13)+2].value

  print,'nH =',nH
  FOR i=0,nfiles-1 DO print,'kT('+ntostr(i)+') = ',kT[i]
  FOR i=0,12 DO print,gfit[i+2].parameter+' = ',abund[i]
  FOR i=0,nfiles-1 DO print,'Tau_u ('+ntostr(i)+') = ',Tau_u[i]
  FOR i=0,nfiles-1 DO print,'Norm ('+ntostr(i)+') = ',norm[i]

 

  return
END 
