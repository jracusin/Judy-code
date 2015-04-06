; Copyright(c) 1992, CreaSo Creative Software Systems GmbH. All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	FILESCAN
;
; PURPOSE:
;	This functions returns the required parts of the filename in a string
;       array.
;
; CALLING SEQUENCE:
;	FILESCAN
;
; INPUTS:
;       filename  - Filename to be scanned
;       CURRENT   - if set and necessary the path value will be completed with
;                   current path elements.
;
; KEYWORDS:
;       EXCLUDE   - Return the parts of filename without '::', ':', []...
;       FULL      - If not set, the logicals in filename will be translated all
;                   except a concealed device.
;
; OUTPUTS:
;       NODE      - Node of filename (includes '::')
;       DEV       - Device of filename (includes ':')
;       DIR       - Directory of filename (includes '[]')
;	NAME      - Filename to be scanned 
;       TYPE      - Extension of the filename (includes '.')
;       VER       - Version number of the filename (includes ';' or '.')
;       PATH      - Node+device+directory
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
;	FileScan ("disk4:[ah]filescan.pro", TYPE = MyExtension, $
;                                           DIR  = MyDirectory)
;       MyDirectory eq "[ah]"
;       MyExtension eq ".pro"
;
; MODIFICATION HISTORY:
;	October 1992, AH,	CreaSo		Created.
;-
;
;
; FSC_LL: Low level filescan routine.
;
pro fsc_ll, filename,    $ 
            node      = node,    $
            dev       = dev,     $
            dir       = dir,     $
            name      = name,    $
            type      = type,    $
            ver       = ver,     $
            path      = path,    $
            exclude   = exclude

common fsccmn       , $  
         node_sep   , $
         dev_sep    , $
         dir_sep    , $
         dir1_sep   , $
         dir1_ssep  , $
         dir2_sep   , $
         dir2_ssep  , $
         name_sep   , $
         type_sep   , $
         type1_sep  , $
         type2_sep

   ; Initialize return elements

   node = ''
   dev  = ''
   dir  = ''
   name = ''
   type = ''
   ver  = ''
   path = ''

   ; Copy and compress filename

   file = strupcase(strcompress(filename, /remove_all))

   ; Determine Node

   spos = 0
   if ((strlen(node_sep) ne 0)) then begin
      pos = strpos (file, node_sep, spos)
      if (pos(0) ne -1) then begin
         node = strmid (file, spos, pos(0)-spos)
         spos = pos(0)+strlen(node_sep)
      endif
   endif

   ; Determine Device

   if (strlen (dev_sep) ne 0) then begin
      pos = strpos (file, dev_sep, spos)
      if (pos(0) ne -1) then begin
         dev = strmid (file, spos, pos(0)-spos)
         spos = pos(0)+strlen(dev_sep)
      endif
   endif

   ; Determine Directory

   pos = strpos (file, dir1_ssep, spos)
   if (pos(0) ne -1) then begin
      spos = pos(0)+strlen(dir1_ssep) 
      dir_sep = dir1_sep
   endif else begin
      pos = strpos (file, dir2_ssep, spos)
      if (pos(0) ne -1) then begin
         spos = pos(0)+strlen(dir2_ssep) 
         dir_sep = dir2_sep
      endif
   endelse

   if (pos(0) ne -1) then begin
      pos(0) = strpos (file, dir_sep, spos)
      if (pos(0) ne -1) then begin
         dir = strmid (file, spos, pos(0) - spos)
         spos = pos(0) + strlen(dir_sep)
      endif
   endif

   ;a: Determine name.

   if (strlen (name_sep) ne 0) then begin

      pos = strpos (file, name_sep, spos)
      if (pos(0) ne -1) then begin
         name = strmid (file, spos, pos(0)-spos)
         spos = pos(0)+strlen(name_sep)

         ; Determine Extension
 
         if (strlen (type1_sep)) then begin
            pos = strpos (file, type1_sep, spos)
            if (pos(0) ne -1) then begin
               type = strmid (file, spos, pos(0)-spos)
               spos = pos(0)+strlen(type1_sep)
               type_sep = 1
            endif else begin
                pos(0) = strpos (file, type2_sep, spos)
               if (pos(0) ne -1) then begin
                  type = strmid (file, spos, pos(0)-spos)
                  spos = pos(0)+strlen(type2_sep)
                  type_sep = 2
               endif else begin
                  type = strmid (file, spos, strlen(file))
                  spos = strlen(file)
               endelse
           endelse
         endif

         ; Determine version number
   
         ver = strmid (file, spos, strlen(file))

      endif else begin
         if (spos eq 0) then name = file $
         else name = strmid (file, spos, strlen(file)-1)
      endelse
   endif else begin
      if (spos eq 0) then name = file $
      else name = strmid (file, spos, strlen(file)-1)
   endelse

   ;a: Calculate path

   if node ne '' then path = node+'::'
   if dev ne '' then path = path + dev+':'
   if dir ne '' then $
      if (dir_sep eq dir1_sep) then path = path + dir1_ssep + dir + dir1_sep $
                              else path = path + dir2_ssep + dir + dir2_sep

   ;a: Concatenate seperators, if nesseccary.

   if (not keyword_set(exclude)) then begin

      if (node ne '') then node = node+node_sep
      if (dev ne '')  then dev =  dev + dev_sep

      if (dir ne '') then $
         if (dir_sep eq dir1_sep) then dir = dir1_ssep+dir+dir1_sep $
                                  else dir = dir2_ssep+dir+dir2_sep
      if (type ne '') then type = name_sep+type
      if (ver ne '') then if (type_sep eq 1) then ver = type1_sep+ver $
                                             else ver = type2_sep+ver 
   endif

end
;
;
; FSC_PARSE_DIR: Determine curent path.
;
pro fsc_parse_dir, dir
   if (strpos(dir,'-') ne -1) then begin
      da = loadarray (dir, '.')
      idx = where (da eq '-')
      for i=n_elements(idx)-1,0,-1 do begin
         if (idx(i) ne 0) then begin
            dx = where (da(0:idx(i)) ne '-' and da(0:idx(i)) ne '')
            if (dx(0) ne -1) then begin
               da(dx(n_elements(dx)-1)) = ''
               da(idx(i)) = ''
            endif
         endif
      endfor
      if (strpos (dir, '.') eq 0) then dir = '.' else dir = ''
      for i=0, n_elements(da)-1 do begin
         if (da(i) ne '') then dir = dir+da(i)+'.'
      endfor
      if (strpos (dir, '.') eq 1 and strlen(dir) eq 1) then dir = '' $
      else dir = strmid (dir, 0, strlen(dir)-1)
   endif
end
;
;
; FSC_VMS_TRNL: Full logical translation, without concealed devices.
;
function fsc_vms_trnl, logi
   stat = trnlog (logi, ans)
   if (stat eq 1) then begin
      if (strpos (ans(0), '.]') ne -1) then stat=0 $
      else begin
         fsc_ll, ans(0), node=node, dev=dev, dir=dir, /exclude
         dev = fsc_vms_trnl (dev+':')
         if (node eq '') then logi = dev+'['+dir+']'    $
                         else logi = node+"::"+dev+"["+dir+']'
      endelse
   endif
   return, logi
end
;
;
; FILESCAN: Main routine.
;
pro filescan, filename,    $ 
                node      = node,    $
                dev       = dev,     $
                dir       = dir,     $
                name      = name,    $
                type      = type,    $
                ver       = ver,     $
                path      = path,    $
                error     = error,   $
                current   = current, $
                full      = full,    $
                exclude   = exclude

common fsccmn       , $  
         node_sep   , $
         dev_sep    , $
         dir_sep    , $
         dir1_sep   , $
         dir1_ssep  , $
         dir2_sep   , $
         dir2_ssep  , $
         name_sep   , $
         type_sep   , $
         type1_sep  , $
         type2_sep

   ; Initialize return elements

   node = ''
   dev  = ''
   dir  = ''
   name = ''
   type = ''
   ver  = ''
   path = ''

   ; Load separators according to current operating system

   error = 0
   case !version.os of

   "vms": begin
         node_sep  = '::'     ; node end separator
         dev_sep   = ':'      ; device end separator
         dir1_sep  = ']'      ; directory end seperator
         dir1_ssep = '['      ; directory start separator
         dir2_sep  = '>'      ; directory end seperator
         dir2_ssep = '<'      ; directory start separator
         name_sep  = '.'      ; name end separator
         type1_sep = '.'      ; version start separator - type one
         type2_sep = ';'      ; version start separator - type two
         dir_sep   = '['
      end
   endcase

   if (not keyword_set(current) and not keyword_set(full)) then begin

      fsc_ll, filename, path=path, node=node, dev=dev, dir=dir, $
              name = name, type=type, ver=ver, exclude=exclude

      return
   endif

   if (keyword_set(current) or keyword_set(full)) then begin
      cd, current=cpath
      fsc_ll, cpath, node=cnode, dev=cdev, dir=cdir, /exclude
   endif

   fsc_ll, filename, node=node, dev=dev,   dir=dir, $
                     name=name, type=type, ver=ver, /exclude

   fsc_parse_dir, dir
   if (keyword_set(current)) then begin
      if (node eq '') then node = cnode
      if (dev  eq '') then dev  = cdev
      if (strpos (dir,'.') eq 0) then begin
         dir = cdir+dir
         fsc_parse_dir, dir
      endif
   endif

   if (not keyword_set(full) and dir eq '' or strpos(dir,'.') eq 0) then begin
      full = 1
      cd, current=cpath
      fsc_ll, cpath, node=cnode, dev=cdev, dir=cdir, /exclude
   endif
   if (keyword_set(full)) then begin
      if (node eq '') then node = cnode
      if (dev  eq '') then dev  = cdev
      d = dev

;****************************************************************************
;
;   ERROR: 
;      --->>> Definition of recursiv procedures.
;
;   Change the following line (fsc_ll, fsc_vms_trnl...) with the next.
;
;      1. Start idl and compile filescan
;
;      2. Test filescan with the /full keyword and a defined logical path.
;         --->>> % Variable is undefined: FSC_VMS_TRNL
;
;      3. Compile filescan again.
;         --->>> Procedure works.
;
;      fsc_ll, fsc_vms_trnl(d), node=lnode, dir=ldir, dev=ldev, /exclude
;
;****************************************************************************

      fsc_ll, vms_trnl(d), node=lnode, dir=ldir, dev=ldev, /exclude
      if (ldir ne '') then begin
         if (dir eq '' or strpos(dir,'.') eq 0) then dir = ldir+dir $
                                                 else dir = ldir+'.'+dir

         fsc_parse_dir, dir
         node = lnode
         dev=ldev
      endif
   endif

   if ((dev ne '' or node ne '') and dir eq '') then $
      dir = '000000'

   ;a: Calculate path

   path = ''
   if node ne '' then path = path + node+'::'
   if dev ne '' then path = path + dev+':'
   if dir ne '' then $
      if (dir_sep eq dir1_sep) then path = path + dir1_ssep + dir + dir1_sep $
                               else path = path + dir2_ssep + dir + dir2_sep

   ;a: Concatenate seperators, if nesseccary.

   if (not keyword_set(exclude)) then begin

      if (node ne '') then node = node+node_sep
      if (dev ne '')  then dev =  dev + dev_sep

      if (dir ne '' and dir_sep eq dir1_sep) then dir = dir1_ssep+dir+dir1_sep $
                                             else dir = dir2_ssep+dir+dir2_sep
      if (type ne '') then type = name_sep+type
      if (ver ne '') then if (type_sep eq 1) then ver = type1_sep+ver $
                                             else ver = type2_sep+ver 
   endif

end
