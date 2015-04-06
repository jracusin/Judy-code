FUNCTION display_exists
  
  spawn,'if test -n "${DISPLAY}" ; then echo ok; fi;',ans,/SH
   IF ans[0] EQ 'ok' THEN return,1 ELSE return,0


;  spawn,'if ( $?DISPLAY != 0 ) echo ok',ans

;  IF ans[0] EQ 'ok' THEN return,1 ELSE return,0

END 
