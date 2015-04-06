pro daily_stripchart,yesterday,outfile
  
  passdata='/bulk/yankees2/xrt/pass_data/'
  cd,passdata
  webdird='~/webdir/daily/'
  ddir='~/daily/'
  newfile='pass_'+yesterday+'_ahk.0' 
  
  filestr='pass_'+yesterday+'*/pass_'+yesterday+'*ahk.0                '
  files=findfile(filestr)
  
  list=''
  for i=0,n_elements(files)-1 do list=list+files[i]+' '
  
  spawn,'cat '+list+'> '+newfile
  spawn,'mv '+newfile+' '+ddir
  
  stripchart,ddir+newfile,/noprint,/quicklook
  outfile=newfile+'.stripchart_plots.pdf'
  spawn,'ps2pdf '+ddir+newfile+'.stripchart_plots.ps '+webdird+outfile
  
  return
end 
