#!/bin/sh

idl<<EOF
wait,1
print,'**********************CHECK_POSTA***************************'
get_file_begin,filebegin
file=filebegin+'_postagestamp.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
    check_posta & $
    run='  Y   ' & endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'check_posta      ',file+'         ',run,' N/A'
close,unit
free_lun,unit
EOF
