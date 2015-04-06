pro set_idl_path
  
  spawn,'uname -s > htype'
  readcol,'htype',htype,/silent,format='(a)'
  spawn,'rm htype'
  if htype eq 'Linux' then begin
     path='/usr/local/rsi/idl/lib/hook:/usr/local/rsi/idl/lib/macros:/usr/local/rsi/idl/lib/obsolete:/usr/local/rsi/idl/lib/utilities:/usr/local/rsi/idl/lib:/usr/local/rsi/idl/examples/HP_TIFF:/usr/local/rsi/idl/examples/analysis:/usr/local/rsi/idl/examples/data:/usr/local/rsi/idl/examples/data_access/sdf:/usr/local/rsi/idl/examples/data_access:/usr/local/rsi/idl/examples/demo/demodata:/usr/local/rsi/idl/examples/demo/demosrc:/usr/local/rsi/idl/examples/demo:/usr/local/rsi/idl/examples/doc:/usr/local/rsi/idl/examples/external:/usr/local/rsi/idl/examples/imsl:/usr/local/rsi/idl/examples/misc:/usr/local/rsi/idl/examples/project:/usr/local/rsi/idl/examples/visual/utility:/usr/local/rsi/idl/examples/visual:/usr/local/rsi/idl/examples/widgets/wexmast:/usr/local/rsi/idl/examples/widgets:/usr/local/rsi/idl/examples:/home/jracusin/xray/idl_lib/planning:/home/jracusin/xray/idl_lib/xrt_pass1:/home/jracusin/xray/idl_lib/swift_sot:/home/jracusin/xray/idl_lib/praxis:/home/jracusin/xray/idl_lib/xrt_caldb:/home/jracusin/xray/idl_lib/acis_mit:/home/jracusin/xray/idl_lib/acis_psu:/home/jracusin/xray/idl_lib'
     !path=path
  endif 
  
  return
end 
