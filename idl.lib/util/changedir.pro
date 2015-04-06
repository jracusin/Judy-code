; Copyright(c) 1992, CreaSo Creative Software Systems GmbH. All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CHANGEDIR
;
; PURPOSE:
;    This routine changes the current directory.
;
; CALLING SEQUENCE:
;	changedir, dir, error=error, /message
;
; INPUTS:
;       None 
;
; KEYWORDS:
;       error       - if an error occured, error is -1
;       message     - if an error occured, a message wil be send
;
; OUTPUT: 
;       None
;
; COMMON BLOCKS:
;	None
;
; SIDE EFFECTS:
;       No known side effects.
;
; RESTRICTIONS:
;	For VAX/VMS only.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	July 1992, AH,	CreaSo		Created.
;-
pro changedir, directory, error=error, message=message

   ;Inialize variables

   error = 0

   if n_params() eq 0 then return

   ;a: Initialize path value

   if (strpos(directory, '[]') ne -1) then cd, current=dire $
                                      else dire = strcompress(directory) 

   ;a: Scan full path

   filescan, dire, path=path, node=node, dir=dir, dev=dev, $
                                /full, /current, /exclude

   if ((byte(dir(0)))(0) eq (byte('.'))(0)) or $ 
      ((byte(dir(0)))(0) eq (byte('-'))(0)) then begin
      error = -1 
      if keyword_set(message)then begin
         print, "% changedir.pro: Path not found - "+ path
      endif
      return
   endif

   if (dir ne '') then begin
      junk = loadarray(dir, '.')
      if (n_elements(junk) eq 1 and junk(0) eq '000000') then begin
         stat = trnlog (dev, ans)
         if (stat eq 1) then if (strpos(ans(0), '.]') eq -1) then dirs = junk
      endif else dirs=junk
   endif

   case n_elements(dirs) of

      0 : begin
          stat = trnlog (dev, ans)
          if (stat eq 1) then begin
             pos = strpos(ans(0), '.]')
             if (pos ne -1) then begin
                filescan, strmid(ans(0), 0, pos)+']', $
                          node=cnode,dev=cdev,dir=cdir, /exclude
                d = loadarray (cdir, '.')
                if (n_elements(d) gt 1) then $
                     cdir = '['+restorearray (d, '.', 0, n_elements(d)-2)+']'  $
                else cdir = '[000000]'
                destdir = d(n_elements(d)-1)
             endif else begin 
                cnode = node
                cdev = dev
                cdir = '[000000]'
                destdir = '000000'
             endelse
          endif else begin 
             cnode = node
             cdev = dev
             cdir = '[000000]'
             destdir = '000000'
          endelse
      end

      1 : begin
          stat = trnlog (dev, ans)
          if (stat eq 1) then begin
             pos = strpos(ans(0), '.]')
             if (pos ne -1) then begin
                cdir = strmid(ans(0), 0, pos)+']'
                filescan,cdir,dir=cdir,dev=cdev,/exclude
                if (cdir eq '') then cdir = '000000'
                cdir = '['+cdir+']'
             endif else begin 
                cdir = '[000000]'
             endelse
          endif else begin 
             cdir = '[000000]'
          endelse
          cnode = node
          cdev = dev
          destdir = dirs(0)
      end
      else : begin
         cnode = node
         cdev = dev
         cdir = '['+restorearray(dirs,'.',0,n_elements(dirs)-2)+']'
         destdir = dirs(n_elements(dirs)-1)
      end
   endcase

   dest = ''
   if (cnode ne '') then dest = cnode+'::'
   if (cdev  ne '') then dest = dest+cdev+':'
   if (cdir  ne '') then dest = dest+cdir $
                    else dest = dest+'[000000]'
   dest = dest+destdir+'.dir'

   ans = findfile (dest)
   if (ans(0) ne '') then cd, path $
   else begin       
      error = -1 
      if keyword_set(message)then begin
         print, "% changedir.pro: Path not found - "+ path
      endif
   endelse

   return
end
