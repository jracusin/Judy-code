pro add_other_hk,totalsize,filesize

get_file_begin,filebegin

files=filebegin+['_startup.0','_memorystatus.0','_taskstatus.0','_errors.0','_pp.0','_modechange.0','_memdump.0','_comandstats.0','_comandecho.0','_thcstatus.0','_ccdinterface.0','_tmtest.0','_tecpsstatus.0','_bhcstatus.0','_tam_sw_hk.0']

totalsize=0.
filesize=fltarr(n_elements(files))
for i=0,n_elements(files)-1 do begin
   if exist(files[i]) then begin 
      size=file_size(files[i])/1000.
      if size le 0. then size=0 else size=round(size+0.5)
      filesize[i]=size
      totalsize=totalsize+size
   endif 
endfor

;print,totalsize

return
end 
