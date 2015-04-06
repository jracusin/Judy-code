pro rate_calcs,scitime,tamfulltime,tamtwowintime,ahktime,scisize,scirate,tamfullsize,tamfullrate,tamtwowinsize,tamtwowinrate,ahksize,ahkrate,otherhksize,otherhkrate,silent=silent
  
  ;;;given user inputs of times, rate_calcs will calculate data rates to write on analysis checklist
  
  get_file_begin,filebegin
  
  files=filebegin+['_science.0','_science.all','_tam_full_frame.0','_tam_two_win.0','_ahk.0']
  
  if exist(files[1]) then scisize=file_size(files[1])/1000. else scisize=file_size(files[0])/1000.
  
  tamfullsize=file_size(files[2])/1000. > 0
  tamtwowinsize=file_size(files[3])/1000. >0
  ahksize=file_size(files[4])/1000. >0
  
  add_other_hk,otherhksize
  
  if n_elements(scitime) eq 0 then $
     read,scitime,prompt='Total Time(s) - Science: '
  if n_elements(tamfulltime) eq 0 then $
     read,tamfulltime,prompt='Total Time(s) - Tam Full Frame: '
  if n_elements(tamtwowintime) eq 0 then $
     read,tamtwowintime,prompt='Total Time(s) - Tam Two Win: '
  if n_elements(ahktime) eq 0 then $
     read,ahktime,prompt='Total Time(s) - HK time: '
  
  if scitime gt 1.e4 then scitime=0.
  if tamfulltime gt 1.e4 then tamfulltime=0.
  if tamtwowintime gt 1.e4 then tamtwowintime=0.
  if ahktime gt 1.e4 then ahktime=0.
  
  
  scirate=ntostr(scisize/scitime >0)
  tamfullrate=ntostr(tamfullsize/tamfulltime > 0)
  tamtwowinrate=ntostr(tamtwowinsize/tamtwowintime > 0)
  ahkrate=ntostr(ahksize/ahktime > 0)
  otherhkrate=ntostr(otherhksize/ahktime > 0)
  scisize=ntostr(scisize > 0)
  tamfullsize=ntostr(tamfullsize > 0)
  tamtwowinsize=ntostr(tamtwowinsize > 0)
  ahksize=ntostr(ahksize > 0)
  otherhksize=ntostr(otherhksize > 0)
  
  if scirate eq !values.f_infinity then scirate=0.
  if tamfullrate eq !values.f_infinity then tamfullrate=0.
  if tamtwowinrate eq !values.f_infinity then tamtwowinrate=0.
  if ahkrate eq !values.f_infinity then ahkrate=0.
  if otherhkrate eq !values.f_infinity then otherhkrate=0.

  
  if not keyword_set(silent) then begin 
     print
     print,'Science:          '+scisize+' kb'
     print,'Tam Full size:    '+tamfullsize+' kb'
     print,'Tam Two Win size: '+tamtwowinsize+' kb'
     print,'AHK size:         '+ahksize+' kb'
     print,'Other HK size:    '+otherhksize+' kb'
     
     print
     print,'Science rate:     '+scirate+' kb/s'
     print,'Tam Full rate:    '+tamfullrate+' kb/s'
     print,'Tam Two Win rate: '+tamtwowinrate+' kb/s'
     print,'AHK rate:         '+ahkrate+' kb/s'
     print,'Other HK rate:    '+otherhkrate+' kb/s'
  endif 
     
  return
end 
