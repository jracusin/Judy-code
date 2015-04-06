;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;       GET_ATLAS
; PURPOSE:
;	Grab the atlas images for a list of objects.
;
; CALLING SEQUENCE:
;   get_atlas, struct, indices, clr=clr, dir=dir, 
;              maxsize=maxsize, noprompt=noprompt, nodisplay=nodisplay,
;              imtot=imtot, imu=imu, img=img, imr=imr, imi=imi, imz=imz,
;              row0=row0, col0=col0, silent=silent, hideradec=hideradec,
;              _extra=extra
;
; INPUTS: struct: photo structure.  Must contain run,field,camcol (rerun)
;         indices:  array of indices (or just an integer for one object)
;                   containing the index for each object to be retrieved.
; 
; OPTIONAL INPUTS: 
;         clr: indices of colors to use.  Can be an array.
;
;         Follows photo convention: 0,1,2,3,4 -> u,g,r,i,z
;
;         dir: directory of atlas images. Only needed if the images 
;              are not in cwd.  THIS IS NOW OPTIONAL, since this
;              information is stored in a system variable.
;
;         maxsize: maximum size for images.  [500,500] is default.
;         noprompt: If set there is no prompting.
;         nodisplay: If set there is no display.
;         silent: will run silently unless bad images or missing
;                 images are found
;	  hideradec: will not include ra dec in titles 
;         _extra=extra: extra keywords for plotting.
;
; OPTIONAL OUTPUTS: 
;         imtot:  An image containing Atlas images for index 
;                 in all 5 colors.  Only returns 
;                 the image of the last index that was retrieved.
;         imu, img, imr, imi, imz:  The images for individual
;                 colors.  Only returns 
;                 the image of the last index that was retrieved.
;         row0, col0:  objc_rowc, objc_colc position of
;                 bottom left corner of atlas image.  Only
;                 returns last one found (z for regular use)
; 
; PROCEDURE: call external c routine (originally written by R. Lupton) which
;      has been modified to link with IDL.  This c program gets an atlas image
;      for a given color, run, camcol, field, id.
;
;
; EXAMPLES:
;   Look at the atlas images in each bandpass for first 10 objects in frame 35
;   of run 109
;
; IDL> run=745 & rerun=20 & camcol=3
; IDL> read_tsobj, [run,rerun,camcol], struct, start=35
; IDL> index = indgen(10)
; IDL> get_atlas, struct, index
;
;   Save the r' band atlas image with no display or prompting.  If clr is 
;   specified, then only that image is retrieved and the program runs faster.
;
; IDL> get_atlas, struct, 33, clr=2, /noprompt, /nodisplay, imr=red_image
; 
;
; REVISION HISTORY:
;       Author:
;       Erin Scott Sheldon    UM      2/5/99  Modified Tim Mckay's 
;                                       get_atlas_images which used spawn
;                                       to call Lupton's stand alone code.
;                                       Linked directly to IDL with 
;                                       call_external.  
;                                     2/8/99  Let index be an array, added
;                                       prompting.  Added subroutine
;                                       missing_atlas for when there is 
;                                       no atlas image.  Added prompt keyword
;                                       Added displaying.
;	David Johnston			added hideradec
;       Erin Scott Sheldon            27-Sep-2002: 
;                                       -dir no longer required, since
;                                        can figure it out from the system
;                                        variables in sdssidl_setup.
;                                       -check for photo version
;                                       -rerun now required
;       Erin Sheldon                  16-Oct-2005:
;                                       -found bug checking for children
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO get_atlas_checktags, struct, mrun, mrerun, mcamcol, mfield, mid, nchild_exist, maguse=maguse, silent=silent

  tags = tag_names(struct)
  
  ;; check for required tags
  match, tags, 'RUN', mtmp, mrun
  match, tags, 'RERUN', mtmp, mrerun
  match, tags, 'CAMCOL', mtmp, mcamcol
  match, tags, 'FIELD', mtmp, mfield
  match, tags, 'ID', mtmp, mid

  IF mrun[0] EQ -1 THEN print,'Structure must have RUN tag'
  IF mrerun[0] EQ -1 THEN print,'Structure must have RERUN tag'
  IF mcamcol[0] EQ -1 THEN print,'Structure must have CAMCOL tag'
  IF mfield[0] EQ -1 THEN print,'Structure must have FIELD tag'
  IF mid[0] EQ -1 THEN print,'Structure must have ID tag'
  IF ( (mrun[0] EQ -1) OR $
       (mrerun[0] EQ -1) OR $
       (mcamcol[0] EQ -1) OR $
       (mfield[0] EQ -1) OR $
       (mid[0] EQ -1) ) THEN BEGIN 
      return
  ENDIF 

  wt = where(tags EQ 'NCHILD', nwt)
  IF nwt NE 0 THEN nchild_exist = 1 ELSE nchild_exist=0

  wmag = sdss_maguse(struct, maguse=maguse, silent=silent)

  IF (wmag NE -1) AND (NOT keyword_set(silent)) THEN BEGIN  
      print,'GET_ATLAS: Using '+tags[wmag]
  ENDIF 
  
  ;; check for ra/dec
  wra = where(tags EQ 'RA')
  wdec = where(tags EQ 'DEC')

  IF (wra[0] EQ -1 OR wdec[0] EQ -1) AND (NOT keyword_set(silent)) THEN BEGIN
      print
      print,'No RA/DEC found. Not displaying position'
  ENDIF

END 

PRO get_atlas_combine, docolor, size, imu, img, imr, imi, imz, imtot

  w=where(docolor EQ 1, nw)

  ;; Side-by-Side
  total_size_x = size[0]*nw
  total_size_y = size[1]

  imtot = lonarr(total_size_x, total_size_y)

  beg = 0L
  FOR i=0L, 4 DO BEGIN 

      IF docolor[i] THEN BEGIN 
          CASE i OF
              0: imtot[beg:beg+size[0]-1, *] = imu
              1: imtot[beg:beg+size[0]-1, *] = img
              2: imtot[beg:beg+size[0]-1, *] = imr
              3: imtot[beg:beg+size[0]-1, *] = imi
              4: imtot[beg:beg+size[0]-1, *] = imz
              ELSE: 
          ENDCASE 
          beg = beg+size[0]
      ENDIF 

  ENDFOR 


END 

pro get_atlas_combine_old, xs, ys, cnum, color,imtot, $
                       imu=imu,img=img,imr=imr,imi=imi,imz=imz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; NAME: 
;    GET_ATLAS_COMBINE
;
; PURPOSE: 
;    Subroutine of get_atlas.  Combines images
;
; CALLING SEQUENCE:
;   get_atlas_combine_atlas,xs, ys, cnum, color,imtot, 
;         imu=imu,img=img,imr=imr,imi=imi,imz=imz
;
; INPUTS:  
;          xs: xsize array
;          ys: ysize array
;          cnum: color numbers
;          color: ['u','g'.....
;          imtot: output array with all subimages in it.
;          imu, img, ....
;
; OUTPUTS: 
;          imtot
;
; REVISION HISTORY:
;    Erin Scott Sheldon  3/14/99  Just a modularization.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Very rarely, the atlas images are of different size 
;;; We must allow for that possibility.

 xsize = fix( total(xs) )
 ysize = max(ys)
 imtot=intarr(xsize,ysize)

 nn = n_elements(cnum)
 xstart = 0
 xend = xs[cnum[0]]
 for kk=0, nn-1 do begin
   n=cnum[kk]

   if (kk ne 0) then begin
      xstart = xstart + xs[ cnum[kk-1] ]
      xend = xend + xs[n]
   endif
   yend = ys[n]
   col = color[n]

   CASE col OF
      'u': imtmp=imu
      'g': imtmp=img
      'r': imtmp=imr
      'i': imtmp=imi
      'z': imtmp=imz
   ENDCASE
   imtot[xstart:xend-1, 0:yend-1] = imtmp
 endfor
 imtmp=0

return
end

pro get_atlas_copy, col,im, xs,ys,size_im, $
                imu=imu, img=img, imr=imr, imi=imi, imz=imz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; NAME: 
;    GET_ATLAS_COPY
;
; PURPOSE: 
;    Subroutine of get_atlas. 
;
; CALLING SEQUENCE:
;  get_atlas_copy, col,im, xs,ys,size_im, 
;              imu=imu, img=img, imr=imr, imi=imi, imz=imz
;
; INPUTS:  col: color index
;          im:  input image
;          xs: xsize array
;          ys: ysize array
;          imu: img: imr: imi: imz:
;
; OUTPUTS: one of the subimages listed above.
;
; REVISION HISTORY:
;    Erin Scott Sheldon  3/14/99  Just a modularization.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  im = temporary( im[ 0:size_im[0]-1, 0:size_im[1]-1 ] )
  
  CASE col OF

      'u': BEGIN
              imu = im
              usize=size(imu)
              xs[0]=usize(1)
              ys[0]=usize(2)
           END
      'g': BEGIN
              img = im
              gsize=size(img)
              xs[1]=gsize(1)
              ys[1]=gsize(2)
           END
      'r': BEGIN
              imr = im
              rsize=size(imr)
              xs[2]=rsize(1)
              ys[2]=rsize(2)
           END
      'i': BEGIN
              imi = im
              isize=size(imi)
              xs[3]=isize(1)
              ys[3]=isize(2)
           END
      'z': BEGIN
              imz = im
              zsize=size(imz)
              xs[4]=zsize(1)
              ys[4]=zsize(2)
           END
   ENDCASE

   return
end

pro get_atlas_display, objstruct, imtot, i, ret, zoom, run, camcol, $
                       maguse=maguse,$
                       field, obj_id, cnum, rerun=rerun, sep=sep, pa=pa, $
                       noprompt=noprompt,silent=silent,$
                       hideradec=hideradec,_extra=extra

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; NAME: 
;    GET_ATLAS_DISPLAY
;
; PURPOSE: 
;    Subroutine of get_atlas.  Displays image.
;
; CALLING SEQUENCE
;    get_atlas_display, objstruct, imtot, i, ret, zoom, run, camcol, 
;                   field, obj_id, cnum, rerun=rerun, sep=sep, pa=pa, 
;                   noprompt=noprompt,silent=silent,
;                   hideradec=hideradec,_extra=extra
;
; Notes:  1. If plot titles don't fit on view screen, just make screen bigger.
;         2. Might want to alter the !p.charsize commands in CASE statement
;         to fit your tastes.  Same with titles, etc.
;
;
; INPUTS:  imtot:  image containing some atlas images
;          i:      index of index of atlas image.  May change in this program.
;          ph_id:  index of atlas image
;          zoom:   zoom or not?
;          run:    sdss run
;          camcol: camera column
;          field:  field of the object
;          obj_id: id of object
;          cnum: the requested color indexes
;          color_str: color strings ['u','g',...
;          struct: a photo structure
;	   hideradec: if set it will not include ra dec in title	
;          _extra: extra keywords for plotting.
;
; OUTPUTS: i may have changed
;          ret: returns 'next' or 'q' for quit
;
; REVISION HISTORY:
;    Erin Scott Sheldon  3/14/99  Just a modularization.
;    Dave Johnston shortened plot titles to fit more info 
;    Dave Johnston 5/18/99 added seperation and position angle
;	(when nchild eq 2) to title
;	added hideradec keyword
;    Changed title to run-rerun-camcol-field-id (from run-camcol-rerun-field-id)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   IF n_params() LT 3 THEN BEGIN
       print,'-Syntax: get_atlas_display, imtot,i, ph_id,ret, zoom, run, camcol, field, obj_id,cnum,noprompt=noprompt,struct=struct,silent=silent,hideradec=hideradec,_extra=extra'
       print,'Use doc_library, "get_atlas_display" for more help'
       return 
   ENDIF 

   IF NOT keyword_set(silent) THEN silent=0

   tags = tag_names(objstruct)

   wmag = sdss_maguse(objstruct, maguse=maguse,/silent)
   IF wmag EQ -1 THEN domag=0b ELSE domag=1b

   ;; check for ra/dec
   wra = where(tags EQ 'RA')
   wdec = where(tags EQ 'DEC')

   IF wra[0] EQ -1 OR wdec[0] EQ -1 THEN BEGIN
       ;;print
       ;;print,'No RA/DEC found. Not displaying position'
       dora=0b 
   ENDIF ELSE dora=1b

   IF n_elements(rerun) NE 0 THEN rerstr = '-'+ntostr(rerun) ELSE rerstr=''
   IF n_elements(sep) EQ 0 THEN sep = -1
   IF n_elements(pa) EQ 0 THEN pa = -1
   
   ;; We have had problems fitting all the info on the screen! 
   ;; Need an adaptive character size.
   char_old = !p.charsize

   px = !p.multi[1]
   py = !p.multi[2]

   CASE px OF 
       0.: !p.charsize= 1.
       1.: !p.charsize= 1.
       2.: !p.charsize= 1.
       ELSE: !p.charsize = .7
   ENDCASE 

   CASE py OF
       3: !p.charsize=1.75*!p.charsize
       4: !p.charsize=1.4*!p.charsize
       5: !p.charsize=1.4*!p.charsize
       ELSE: 
   ENDCASE

   xtitle = ''
   subtitle = ''
   IF domag THEN BEGIN 
       c = objstruct.(wmag)
       mag=['','','','','']
       color = ['u','g','r','i','z']

       FOR jj=0,n_elements(cnum)-1 DO BEGIN 
           mag[cnum[jj]]=strmid( strtrim(string(c[cnum[jj]]),2), 0, 5)
       ENDFOR 

       FOR jj=0,4 DO BEGIN 
           IF (mag[jj] NE '') THEN BEGIN 
               IF (xtitle NE '') THEN xtitle=xtitle+'  '
               xtitle=xtitle + color[jj]+'='+mag[jj]
           ENDIF 
       ENDFOR 
   
       FOR jj=0,3 DO BEGIN 
           IF (mag[jj] NE '' AND mag[jj+1] NE '') THEN BEGIN 
               diff=strmid(strtrim(string(c[jj] - c[jj+1]),2), 0, 5)
               subtitle=subtitle+'  '+color[jj]+'-'+color[jj+1]+'='+diff
           ENDIF 
       ENDFOR 
       xtitle=xtitle + '  ' + subtitle

   ENDIF 

   ;; changed order to run-rerun-camcol-field-id
   ;;title = run2string(run)+'-'+ntostr(camcol)+rerstr
   title = run2string(run)+rerstr+'-'+ntostr(camcol)
   title = title+ '-'+field2string(field)+$
     '-'+strn(obj_id,length=5,padchar='0')
   
   if (not keyword_set(hideradec)) AND dora then begin
       radecstr, objstruct.ra, objstruct.dec, ra, dec
       title=title+ '  '+ra+'  '+dec
   endif
       
   IF sep NE -1 THEN BEGIN
       sepst=strtrim(sep,2)     ;take only one digit after decimal place
       title=title+' sep: '+strmid(sepst,0,strpos(sepst,'.')+2)
       title=title+' pa: '+strtrim(string(fix(pa)),2)
   ENDIF

   zoom_old = zoom
   xtitle_old=xtitle
   sub_old = subtitle

   ;; Display and prompt user.  Zoom if requested.
   REPEAT BEGIN

       tvasinh, imtot, zoom=zoom, xtitle=xtitle,title=title

       zoom = zoom_old

       IF NOT keyword_set(noprompt) THEN BEGIN 
           print,''
           print,'(n for next) (p for previous) (r to redisplay) (q to exit)'
           redo = strlowcase( get_kbrd(1) )
       ENDIF ELSE redo='n'    

       IF (redo EQ 'r') THEN BEGIN 
           print,'------------------------------------------------'
           print,'Want to zoom(y/n)?  n to display original.'
           zm =  strlowcase( get_kbrd(1) )
           print,''
           IF (zm EQ 'y') THEN BEGIN            ;Zooming
               zoom = 1
               xtitle=''
               subtitle=''
           ENDIF ELSE BEGIN                     ;Redisplay original
               xtitle=xtitle_old
               subtitle=sub_old
           ENDELSE 
       ENDIF ELSE IF (redo EQ 'p') THEN begin
           IF (i EQ 0) THEN BEGIN               ;At the first object.
               print,'------------------------------------------------'
               print,'No such object. Starting over'
               print,'------------------------------------------------'
               tmp = get_kbrd(1)
               i = i-1
           ENDIF ELSE BEGIN 
               i = i-2                          ;Going to previous object.
           ENDELSE 
           redo ='n'
       ENDIF ELSE IF (redo EQ 'q') THEN BEGIN 
           print,'Quitting get_atlas'
           ret = 'q'
           !p.charsize=char_old
           return
       ENDIF ELSE BEGIN 
           redo = 'n'
           xtitle=xtitle_old
           subtitle=sub_old
       ENDELSE 
    
   ENDREP UNTIL (redo EQ 'n')
  
   ret='next'
   zoom = zoom_old

   !p.charsize=char_old
   return 
END 


pro  get_atlas_missing, request, i, max, noprompt, silent=silent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; NAME:  
;    GET_ATLAS_MISSING
;       
; PURPOSE: 
;    Called by get_atlas 
;    figure out what to do if an atlas image is not found.
;	
;
; CALLING SEQUENCE: 
;    get_atlas_missing, request, i, max, noprompt, silent=silent
;      
; INPUTS: i, max, prompt
;       
; OUTPUTS: 
;
; OPTIONAL OUTPUT ARRAYS:
;
; INPUT KEYWORD PARAMETERS:
; 
; PROCEDURE: 
;	
;	
;
; REVISION HISTORY:  Erin Scott Sheldon UM 1/8/99
;	
;       
;                                      
;                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() LT 3 then begin
      print,'Syntax:  get_atlas_missing,request,i,max,noprompt,silent=silent'
      return
  endif

  print,''
  
  if keyword_set(noprompt) then begin
      i=i+1
      request = 'n'
      IF i EQ max THEN request = 'q'
      return
  endif else print,'(n next object)  (p previous) (q to exit)'

  request = get_kbrd(1)
  CASE request OF 
      'n': BEGIN
          i=i+1
          IF (i EQ max) THEN request = 'q'
          return
      END 
      'q': BEGIN & print,'Quitting get_atlas.' & return & END 
      'p': BEGIN
                if (i eq 0) then begin
                    print,'Cannot display object -1.'
                    print,'------------------------------------------------'
                    print,'No such object.  Starting over.'
                    print,'------------------------------------------------'
                    tmp=get_kbrd(20)
                    request='n'
                    return
                endif else begin
                    request='n'
                    i=i-1
                    return
                ENDELSE
            END 
        ELSE: BEGIN
            request = 'n'
            i=i+1
            IF (i EQ max) THEN request = 'q'
            return
        END 
  ENDCASE 

end

pro get_atlas_name, run, camcol, field, fname

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; NAME: 
;    GET_ATLAS_NAME
;
; PURPOSE: 
;    Subroutine of get_atlas.  Creates the name of atlas file.
;
; CALLING SEQUENCE:
;    get_atlas_name, run, camcol, field [, atlasname]
;
; INPUTS:  
;    run: sdss run number
;    camcol: camera column
;    field: field of the object
;
; OUTPUTS: 
;    atlasname
;
; REVISION HISTORY:
;    Erin Scott Sheldon  3/14/99
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() LT 3 THEN BEGIN
      print,'-Syntax: get_atlas_name, run, camcol, field [, atlasname]'
      return
  ENDIF 

  rs = run2string(run)
  cs = strtrim(string(camcol),2)
  fs = field2string(field)
  fname='fpAtlas-'+rs+'-'+cs+'-'+fs+'.fit '
  fname = strtrim(fname)

return
end


pro get_atlas_compute_sep,cat,index,sep,pa
;+
; NAME:
;    GET_ATLAS_COMPUTE_SEP
; PURPOSE:
;  compute the seperation in arc seconds and position angle
;  between an objects two children if it has two children
;  position angle is an angle between 0 degrees and 180 degrees
;  NOT 0 to 360 
;  so it is not really the position angle of ONE with
;  respect to the OTHER (ie. it is order independent)
;  0 degrees is horizontal and 90 degrees vertical 
;  cat is the sdss photo structure and index is the index of the parent
;  assumes two children are indices index+1 and index+2
;
; CALLING SEQUENCE:
;  get_atlas_compute_sep,cat,index,separation,position_angle
;
;Revision History:
;	David Johnston 5/20/99
;-

if n_params() LT 2 then begin
	print,'-syntax get_atlas_compute_sep,cat,index,sep,pa'
	return
endif

if cat(index).nchild ne 2 then begin
	print,'GET_ATLAS_COMPUTE_SEP : object must have two children'
	print,'to compute seperation and pa'
        sep = -1 & pa=-1
        return
endif

c1=cat(index+1).objc_colc
c2=cat(index+2).objc_colc
r1=cat(index+1).objc_rowc
r2=cat(index+2).objc_rowc
dc=c1-c2
dr=r1-r2

sep=.4*sqrt(dc^2+dr^2)
;.4 arcsec /pixel for sdss only

if sep lt .04 then begin
	;sep less than .1 pixels so define pa to be 0 by default
	;since it is not really meaningful
	pa=0.0
endif else begin
	pa=atan(dr,dc)
endelse

pa=pa*180.0/3.14159
if pa lt 0 then pa=180+pa

return
end


pro get_atlas, struct, index, clr=clr, dir=dir, $
               maxsize=maxsize, noprompt_in=noprompt_in, nodisplay=nodisplay,$
               imtot=imtot, imu=imu, img=img, imr=imr, imi=imi, imz=imz,$
               row0=row0, col0=col0, $
               drow=drow, dcol=dcol, $
               maguse=maguse,$
               silent=silent, hideradec=hideradec,$
               _extra=extra


  if N_params() LT 2 then begin
      print,'Syntax -  get_atlas, struct, index, clr=clr, dir=dir, '
      print,'       maxsize=maxsize, noprompt=noprompt, nodisplay=nodisplay,'
      print,'       imtot=imtot, imu=imu, img=img, imr=imr, imi=imi, imz=imz,'
      print,'       row0=row0, col0=col0, silent=silent, hideradec=hideradec,'
      print,'       _extra=extra'

      print,''
      print,'-If you want to zoom, enter r when prompted.'
      print,'Use doc_library,"get_atlas"  for more help'
      return
  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  On_error,2

  color=['u','g','r','i','z']
  num = ['0 ','1 ','2 ','3 ','4 ']

;;;  Define the sharable object file name and define the entry
;;;  point to sharable object file: sofile  Note this is 
;;;  different on different OS's These variables must be set in
;;;  sdssidl_setup.pro

  sdssidl_setup, /silent

;;;;;;;; which colors are we getting?           ;;;;;;;;;;;;;;;;;;;;
  
  
  ncolor = n_elements(clr)
  IF ncolor EQ 0 THEN BEGIN 
      clr = [0,1,2,3,4]
      ncolor = 5
      docolor = replicate(1, 5)
  ENDIF ELSE BEGIN 
      maxclr = max(clr, min=minclr)
      IF maxclr GT 4 OR minclr LT 0 THEN BEGIN 
          print,'clr must be in [0,4]'
          return
      ENDIF 
      docolor = intarr(5)
      docolor[clr] = 1
  ENDELSE 
  cnum = clr[sort(clr)]

;;;;;;;;;;;  Define the maximum size for each image ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  Default is [500,500]                   ;;;;;;;;;;;;;;;;;

  IF keyword_set(maxsize) THEN size=maxsize ELSE size=[500,500]

  IF keyword_set(noprompt_in) THEN noprompt=1 ELSE noprompt=0
;;; If zoom not set, don't zoom unless told to at the prompt  ;;;; 

  IF NOT keyword_set(zoom) THEN zoom=0
  zoom = zoom > 0 < 1

  ;; Will we be returning imtot?
  doimtot = arg_present(imtot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  If index is an array, loop over them all ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  get_atlas_checktags, struct, $
    mrun, mrerun, mcamcol, mfield, mid, nchild_exist, $
    maguse=maguse,$
    silent=silent

  max=n_elements(index)
  ntot = n_elements(struct)

  IF max EQ 1 THEN noprompt=1

  runold = -1
  rerunold = -1
  FOR i=0L, max-1 DO BEGIN

      JUMP:                ;;;;;;;; Used a goto, don't hate me ;;;;;;;;;;
 
      ;;;;;;;;;; Build up the filename for the field...;;;;;
   
      tind = index[i]

      run    = struct[tind].run
      rerun  = struct[tind].rerun
      camcol = struct[tind].camcol
      field  = struct[tind].field
      obj_id = struct[tind].id
      
      ;; No longer support earlier versions
      sofile = !GET_ATLAS53_SOFILE
      entry  = !GET_ATLAS53_ENTRY

      IF (run NE runold) OR (rerun NE rerunold) THEN BEGIN 

          ;; search for run/rerun
;          wst=where(!RUN_STATUS.run EQ run AND !RUN_STATUS.rerun EQ rerun,nst)
;          IF nst NE 0 THEN BEGIN 

              ;; do we know photo version?
;              photov = !RUN_STATUS[wst[0]].fpatlas_photo_v
;              IF photov EQ -1 THEN BEGIN 
;                  print
;                  print,'No fpAtlas version found: trying tsObj version'
;                  photov = !RUN_STATUS[wst[0]].tsobj_photo_v
;                  IF photov EQ -1 THEN BEGIN 
;                      photov = !RUN_STATUS[wst[0]].adatc_photo_v
;                      IF photov EQ -1 THEN BEGIN 
;                          print
;                          print,'Do not know photo version for run: '+$
;                                ntostr(run)+' rerun: '+ntostr(rerun)
;                          print,'Using photo version > 5.3 atlas reader'
;                          sofile = !GET_ATLAS53_SOFILE
;                          entry  = !GET_ATLAS53_ENTRY
;                      ENDIF 
;                  ENDIF 
;              ENDIF
              
              ;; 5.3 atlas format is altered
;              IF photov GE 5.3 THEN BEGIN 
;                  sofile = !GET_ATLAS53_SOFILE
;                  entry  = !GET_ATLAS53_ENTRY
;              ENDIF ELSE BEGIN 
;                  sofile = !GET_ATLAS_SOFILE
;                  entry  = !GET_ATLAS_ENTRY
;              ENDELSE 
        
;          ENDIF ELSE BEGIN
;              print
;              print,'No entry in !RUN_STATUS for run: '+$
;                      ntostr(run)+' rerun: '+ntostr(rerun)
;              print,'Using photo version > 5.3 atlas reader'
;              sofile = !GET_ATLAS53_SOFILE
;              entry  = !GET_ATLAS53_ENTRY
;          ENDELSE 
          
          ;; construct the atlas directory
          IF n_elements(dir) EQ 0 THEN BEGIN
              atlasdir = !sdss_data_dir+ntostr(run)+'/'+ntostr(rerun)+$
                '/objcs/'+ntostr(camcol)+'/'
          ENDIF ELSE atlasdir = dir

          runold = run
          rerunold = rerun
      ENDIF 
      ;;;;;;;  call subroutine get_atlas_name  ;;;;;;;

      get_atlas_name, run, camcol, field, fname
      fname=atlasdir+fname

      if (not keyword_set(silent) ) then begin
          print,'------------------------------------------------'
          print,'Index: '+ntostr(i)+'  Of: '+ntostr(max-1)
          print,'Run: '+ntostr(run)+' Rerun: '+ntostr(rerun)+$
            ' Camcol: '+ntostr(camcol)+' Field: '+ntostr(field)+$
            ' id: '+ntostr(obj_id)
          print,'File:',fname
          print,'------------------------------------------------'
      endif
      
      ;;;;;;;;;;;;;; Get the images ;;;;;;;;;;;

      ids = ntostr(obj_id)
      xs = intarr(5)
      ys = intarr(5)

      row0 = intarr(5)
      col0 = row0
      drow = row0
      dcol = row0
          
      IF docolor[0] THEN imu = lonarr(size[0], size[1]) ELSE imu=lonarr(1)
      IF docolor[1] THEN img = lonarr(size[0], size[1]) ELSE img=lonarr(1)
      IF docolor[2] THEN imr = lonarr(size[0], size[1]) ELSE imr=lonarr(1)
      IF docolor[3] THEN imi = lonarr(size[0], size[1]) ELSE imi=lonarr(1)
      IF docolor[4] THEN imz = lonarr(size[0], size[1]) ELSE imz=lonarr(1)
      
      size_im = size
          
      s=call_external(sofile, entry, $
                      imu, img, imr, imi, imz, $
                      docolor, $
                      size_im, fname, ids, row0, col0, drow, dcol)

      IF (s EQ 1) THEN BEGIN
          delvarx,imu,img,imr,imi,imz
          get_atlas_missing, request, i, max, noprompt,silent=silent
          CASE request OF
              'q': return
              ELSE: goto,JUMP
          ENDCASE
      ENDIF 
      IF size_im[0] NE size[0] OR size_im[1] NE size[1] THEN BEGIN 
          IF docolor[0] THEN imu = imu[0:size_im[0]-1, 0:size_im[1]-1]
          IF docolor[1] THEN img = img[0:size_im[0]-1, 0:size_im[1]-1]
          IF docolor[2] THEN imr = imr[0:size_im[0]-1, 0:size_im[1]-1]
          IF docolor[3] THEN imi = imi[0:size_im[0]-1, 0:size_im[1]-1]
          IF docolor[4] THEN imz = imz[0:size_im[0]-1, 0:size_im[1]-1]
      ENDIF 

      ;; Convert to signed and remove the sky
      IF NOT docolor[0] THEN delvarx, imu ELSE imu = imu-1000
      IF NOT docolor[1] THEN delvarx, img ELSE img = img-1000
      IF NOT docolor[2] THEN delvarx, imr ELSE imr = imr-1000
      IF NOT docolor[3] THEN delvarx, imi ELSE imi = imi-1000
      IF NOT docolor[4] THEN delvarx, imz ELSE imz = imz-1000

      IF NOT keyword_set(nodisplay) OR doimtot THEN BEGIN 
          get_atlas_combine, docolor, size_im, imu, img, imr, imi, imz, imtot
      ENDIF 

      ;; Call subroutine get_atlas_display if display is requested ;;;;;;;;

      if NOT keyword_set(nodisplay) then begin
	
          ;; see if we need to comput separation
          sep=-1 & pa = -1 
          IF nchild_exist THEN BEGIN
              
              ;; check nchild 
              IF (struct[index[i]].nchild EQ 2) THEN BEGIN 
                  ;; are there are two more objects after this?
                  IF ( index[i] LE ((ntot-1)-2) ) THEN BEGIN 
                      ;; are they the children?
                      IF( (struct[index[i]+1].id EQ (obj_id+1)) AND $
                          (struct[index[i]+2].id EQ (obj_id+2)) ) THEN BEGIN 
                          get_atlas_compute_sep, struct, index[i], sep, pa
                      ENDIF ELSE BEGIN 
                          IF NOT keyword_set(silent) THEN print,$
                            'GET_ATLAS: Cannot compute sep/pos angle: children not in structure'
                      ENDELSE 
                  ENDIF ELSE BEGIN
                      IF NOT keyword_set(silent) THEN print,$
                        'GET_ATLAS: Cannot compute sep/pos angle: children not in structure'
                  ENDELSE 
              ENDIF 
          ENDIF 
             
          get_atlas_display, struct[index[i]], imtot, i, ret, zoom, $
            run, camcol, $ 
            field,obj_id,cnum, rerun=rerun, noprompt=noprompt, struct=struct,$
            maguse=maguse,$
            silent=silent, sep=sep,pa=pa,_extra=extra, hideradec=hideradec

          if (ret eq 'q') then return
      endif

  endfor
  return
end









