pro sn1987a_results_table
  
  cd,'/Volumes/Firewire1/racusin/sn1987a/image_analysis'
  dirs=['ring','bilat','lobes']
  files=dirs+'/table.tex'
  
  readcol,files[0],ring,format='a',delim='#'
  readcol,files[1],bilat,format='a',delim='#'
  readcol,files[2],lobes,format='a',delim='#'
  
  aa=' & '
  
  sn1987a_age,ages
  ages=[ages[0:9],ages[11:*]]
  help,ages,ring,bilat,lobes
;  colprint,ntostr(indgen(19)+1)+aa+ntostr(ages)+aa+ring+aa+bilat+aa+lobes+' \\'
  colprint,ntostr(ages)+aa+ring+aa+bilat+aa+lobes+' \\'
  
  return
end 
