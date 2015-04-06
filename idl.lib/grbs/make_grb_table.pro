pro make_grb_table
  
  file='~/jetbreaks/grb_z_epeak_band.csv'
  
  readcol,'grb_z_epeak_band.csv',lines,format='(a)',delim='$',skip=1
  
  n=n_elements(lines)
  a=' & '
  d='$'
  for i=0,n-1 do begin
     stuff=str_sep(lines[i],',')
     alphab='' & alphacpl='' & beta=''
     if stuff[2] ne 0. or stuff[5] ne 0. or stuff[8] ne 0. then begin
        if stuff[5] ne 0. then alpha=d+stuff[5]+'^{+'+stuff[6]+'}_{-'+stuff[7]+'}'+d else alpha=''
        if stuff[8] ne 0. then beta=d+stuff[8]+'^{+'+stuff[9]+'}_{-'+stuff[10]+'}'+d else beta=''
        if beta ne '' then alphab=alpha else alphacpl=alpha
        if stuff[2] ne 0. then ep=d+stuff[2]+'^{+'+stuff[3]+'}_{-'+stuff[4]+'}'+d else ep=''
        if stuff[11] eq '' then foot='' else foot='\footnotemark[1]'
        print,stuff[0]+foot+a+alphab+a+beta+a+alphacpl+a+ep+a+stuff[12]+'\\'
        
     endif 
  endfor 
  return
end 
