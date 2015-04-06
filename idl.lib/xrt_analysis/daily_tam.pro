pro daily_tam,yesterday,outfile
  
  webdird='~/webdir/daily/'
  ddir='~/daily/'
  newfile='pass_'+yesterday+'_tam_two_win.0' 
  
  filestr='pass_'+yesterday+'*/pass_'+yesterday+'*tam_two_win.0                '
  files=findfile(filestr)
  
  list=''
  for i=0,n_elements(files)-1 do list=list+files[i]+' '
  
  spawn,'cat '+list+'> '+newfile
  spawn,'mv '+newfile+' '+ddir
  
  tam,ddir+newfile,/noprint,/quicklook,/nodtas
  outfile=newfile+'.trends.pdf'
  spawn,'ps2pdf '+ddir+newfile+'.trends.ps '+webdird+outfile
  spawn,'rm '+ddir+'*fits*'
  spawn,'rm '+ddir+'*err'
  spawn,'rm '+ddir+'*log'
  spawn,'rm '+ddir+'*centroids'
  spawn,'rm '+ddir+'*timeline'
  
  return
end 
