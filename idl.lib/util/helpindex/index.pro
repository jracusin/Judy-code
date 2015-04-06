;+
;  NAME
;    INDEX
; 
;  Purpose:
;    Start the help index application which provide index
;    functionality for the PDF documentation through a graphical user
;    interface.
;
; CATEGORY:
;    Documentation.
;
;  Syntax:
;    index [, topic] [, /BLOCK] [, BOOK=value] [, OBJREF=value]
;
;  Arguments:
;    TOPIC - Optional string argument to start the search.
;
;  Keywords:
;    BLOCK - If set the help index starts in blocking mode.  The
;            default is non-blocking.
;
;    BOOK - A optional string to specify the pdf document to start the
;           searh.
;
;    OBJREF - A named variable that will receive an object reference
;             for the application.
;
;  Modification History:
;    Written by: AB, February 2003
;    Added index__define, index_app__define, index_edit__define
;    classes to the file. June 2003
;    Updated for IDL 6.0. August 2003
;    Updated for IDL 6.0.1. February 2003
;
;-
pro _index_app_event_handler,ev
  widget_control,ev.top,get_uvalue=obj
  handler=obj->_event_handler(ev.handler)
  call_method,handler,obj,ev
end

function index_app::init,tlb
  compile_opt idl2
  self.tlb=tlb
  widget_control,tlb,set_uvalue=self
  current=widget_info(tlb,/child)
  lastup=0
  while (current ne tlb) do begin
    proc=widget_info(current,/event_pro)
    if (proc ne '') and (proc ne '_INDEX_APP_EVENT_HANDLER') then begin
      widget_control, current, event_pro='_INDEX_APP_EVENT_HANDLER'
      if (size(evid,/type) eq 0) then evid=current else evid=[evid,current]
      if (size(evst,/type) eq 0) then evst=proc else evst=[evst,proc]
    endif
    if (lastup eq 0) then new=widget_info(current,/child)
    if not widget_info(new,/valid) or (lastup eq 1) then $
      new=widget_info(current,/sibling)
    if not widget_info(new,/valid) then begin
      new=widget_info(current,/parent)
      lastup=1
    endif else lastup=0
    current=new
  endwhile
  self._event_pro[0]=ptr_new(/allocate)
  self._event_pro[1]=ptr_new(/allocate)
  if (size(evid,/type) ne 0) then *self._event_pro[0]=evid
  if (size(evst,/type) ne 0) then *self._event_pro[1]=evst
  return,1
end

function index_app::_event_handler,id
  compile_opt idl2
  if n_elements(*self._event_pro[0]) eq 0 then return, 'EVENT'
  ind=where(id[0] eq *self._event_pro[0])
  if (ind[0] eq -1) then return, 'EVENT'
  return, (*self._event_pro[1])[ind[0]]
end

pro _index_app_cleanup, tlb
  widget_control, tlb, get_uvalue=obj
  obj_destroy, obj
end

pro index_app::xmanager, _extra=extra
  xmanager,'index_app__define',self.tlb,event_handler='_index_app_event_handler',cleanup='_index_app_cleanup',_extra=extra
end

pro index_app::cleanup
  ptr_free,self._event_pro
end

pro index_app__define
  s={index_app,tlb:0L,_event_pro:ptrarr(2)}
end

function index_edit::init, notify, file, group_leader=group
  self.notify_object=notify
  if (size(file,/type) eq 0) then file='~/.idlindex'+!version.release
  self.file=file
  tlb=widget_base(/column, group_leader=group)
  self->import
  self.wtext=widget_text(tlb, value=*self.data, /wrap, ysize=24, $
                         xsize=80, /edit)
  base=widget_base(tlb,/row)
  ok=widget_button(base,value='OK')
  cancel=widget_button(base,value='Cancel')
  apply=widget_button(base,value='Apply')
  widget_control, tlb, /realize
  return, self->index_app::init(tlb)
end

pro index_edit::import
  if ptr_valid(self.data) then ptr_free, self.data
  if (file_test(self.file) eq 0) then begin
    self.data=ptr_new(self.notify_object->defaultpref())
  endif else begin
    n=file_lines(self.file)
    self.data=ptr_new(strarr(n))
    openr,u,self.file,/get_lun
    readf,u,*self.data
    free_lun,u
  endelse
  if widget_info(self.wtext,/valid_id) then $
    widget_control, self.wtext, set_value=*self.data
end

pro index_edit::export
  if widget_info(self.wtext,/valid_id) then $
    widget_control, self.wtext, get_value=*self.data
  openw,u,self.file,/get_lun
  printf,u,*self.data, format='(a)'
  free_lun,u
end

pro index_edit::event, ev
  if (ev.id eq self.wtext) then return
  widget_control, ev.id, get_value=val
  case strlowcase(val) of
    'ok': begin
      self->export
      self.notify_object->applypref
      widget_control, self.tlb, /destroy
    end
    'cancel': widget_control, self.tlb, /destroy
    'apply': begin
      self->export
      self.notify_object->applypref
    end
    else:
  endcase
end

pro index_edit::cleanup
  ptr_free,self.data
  self->index_app::cleanup
end

pro index_edit__define
  compile_opt idl2
  s={index_edit, inherits index_app, file:'', data:ptr_new(), $
     notify_object:obj_new(), wtext:0}
end

function index::init, topic, book=book, nogui=nogui, ny=ny
  compile_opt idl2
  info=routine_info('index__define',/source)
  self.dir=strmid(info.path,0,strpos(info.path,'/',/reverse_search))
  self.match=ptr_new(-1)
;
  if (size(topic,/type) ne size('',/type)) then topic=''
  if (size(book,/type) ne size('',/type)) then self.book='mindex' $
  else if (strlen(book) eq 0) then self.book='mindex' else self.book=book
  if (size(ny,/type) eq 0) then self.ny=24 else self.ny=ny
  self->loadbook
  self->lookup, topic
;
  self.nogui=keyword_set(nogui)
  tlb=widget_base(/column,resource_name='helpindex',title='Help Index',$
                  /tlb_size_events)
  self->booklist, booklist=blist, displaylist=dlist, names=nlist
  pd=widget_droplist(tlb,uvalue=nlist,value=dlist, event_pro='droplistevent')
  widget_control,pd,set_droplist_select=(where(nlist eq self.book))[0]>0
  way=widget_base(tlb,/exclusive,/row, event_pro='wayevent')
  wayindex=widget_button(way,value='Index')
  wayfind=widget_button(way,value='Find')
  widget_control, wayindex, /set_button
;
  self.wtext=widget_text(tlb,value=topic,xsize=50,/editable,/all_events, $
                         event_pro='textevent',resource_name='wtext',$
                         /kbrd_focus_events)
  len_topic=strlen(topic)
  if (len_topic gt 0) then $
    widget_control, self.wtext, set_text_select=[len_topic,0]

  mini=self.top>0
  maxi=(mini+self.ny-1)<(self.n_topics-1)
  base=widget_base(tlb,/row)
  self.wlist=widget_text(base,ysize=self.ny,/all_events,event_pro='listevent',$
                         resource_name='wlist',/kbrd_focus_events,$
                         xsize=50)
  self.wscroll=widget_slider(base,min=1-self.n_topics, max=0, /vertical, $
                             xsize=10,value=0, /drag, uvalue=1-self.n_topics, $
                             event_pro='scrollevent', /suppress_value)
  
  go=widget_button(tlb,value='Display page', event_pro='gotopic')
  self.menubase=widget_button(tlb,value='Bookmarks/Preferences',/menu, $
                              event_pro='gobookmark')
  add=widget_button(self.menubase,value='Edit Bookmarks/Preferences',$
                    event_pro='editprefevent', /separator)
  
  if not self.nogui then begin
    widget_control,tlb,/realize
    widget_control, self.wtext, /input_focus
    geom_list=widget_info(self.wlist,/geometry)
    widget_control, tlb, tlb_get_size=geom_tlb
    self.pad=geom_tlb-[geom_list.scr_xsize, geom_list.scr_ysize]
  endif
;
  self.tlb=tlb
  self->applypref
  self->updatelist
  return,self->index_app::init(tlb)
end

function index::defaultpref
  compile_opt idl2
  case strmid(!version.release,0,3) of
    '5.6': bm=['Requierments for this Release:whatsnew:140',$
               'Functional List of IDL Routines:quickref:5',$
               'Alphabetical List of IDL Routines:quickref:23',$
               'Reserved Words:refguide:3927']
    '6.0': bm=['Requierments for this Release:whatsnew:117',$
               'Functional List of IDL Routines:quickref:5',$
               'Alphabetical List of IDL Routines:quickref:23',$
               'Reserved Words:refguide:3949']
    else: message,'Wrong IDL version'
  endcase
  res=['xsize: 40', 'ysize: 24', 'xoffset: 0', 'yoffset: 100',$
       'close_on_exit: 0',$
       'bookmarks_begin', bm, 'bookmarks_end']
  return, res
end

function index::readpref, file
  compile_opt idl2
  if file_test(file) eq 0 then return, self->defaultpref()
  n=file_lines(file)
  if (n eq 0) then return, self->defaultpref()
  openr,u,file,/get_lun
  txt=strarr(n)
  readf, u, txt
  free_lun, u
  return, txt
end

pro index::applypref, file
  compile_opt idl2
  if (size(file,/type) eq 0) then file='~/.idlindex'+!version.release
  txt=self->readpref(file)
  bookmarks=0
  bm=widget_info(self.menubase,/child)
  bm=widget_info(bm,/sibling)
  nc=0
  while widget_info(bm, /valid_id) do begin
    bms=(nc gt 0)?[bms,bm]:bm
    bm=widget_info(bm,/sibling)
    nc=nc+1
  endwhile
  b=0
  for i=0,n_elements(txt)-1 do begin
    t=strlowcase(strtrim(txt[i],2))
    if (t eq 'bookmarks_end') then begin
      bookmarks=0
      if (b lt nc) then for j=b,nc-1 do widget_control, bms[b], map=0
    endif
    if bookmarks then begin
      val=strpos(txt[i],':')
      uval=strmid(txt[i],val+1)
      val=strmid(txt[i],0,val)
      if (b ge nc) then $
        void=widget_button(self.menubase,value=val,uvalue=uval) $
      else widget_control, bms[b], set_value=val, set_uvalue=uval
      b=b+1
    endif else begin
      tp=strsplit(t,':',/extract)
      case strtrim(tp[0],2) of
        'xsize': begin
          widget_control, self.wlist, xsize=long(tp[1])
          widget_control, self.wtext, xsize=long(tp[1])
        end
        'ysize': widget_control, self.wlist, ysize=long(tp[1])
        'xoffset': widget_control, self.tlb, xoffset=long(tp[1])
        'yoffset': widget_control, self.tlb, yoffset=long(tp[1])
        'close_on_exit': self.close=long(tp[1])
        else:
      endcase
    endelse
    if (t eq 'bookmarks_begin') then bookmarks=1
  endfor
end

pro index::loadbook
  compile_opt idl2
  book=self.book
  filename=!dir+'/help'+'/'+book+'.index.gz'
  if (file_test(filename) eq 0) then begin
  filename=self.dir+'/'+book+'.index.gz'
    if (file_test(filename) eq 0) then $
      message,'File not found: '+filename
  endif
  openr, u, filename, /get_lun, /compress, /swap_if_little_endian
  title=''
  bname=''
  readf, u, bname, title
  n=[0,0]
  readu, u, n
  ind=lonarr(n[1])
  readu, u, ind
  data=strarr(n[0])
  readf, u, data
  free_lun, u
;
  if ptr_valid(self.index) then ptr_free,self.index 
  if ptr_valid(self.ind) then ptr_free,self.ind
  self.n_topics=n[0]
  self.n_ind=n[1]
  self.index=ptr_new(data,/no_copy)
  self.ind=ptr_new(ind,/no_copy)
end

pro index::test
  stop
end

pro index::updatelist
  if self.nogui then return
  if (self.way eq 1) then begin ; find
    if ((*self.match)[0] ne -1) then begin
      nm=n_elements(*self.match)
      widget_control, self.wscroll, get_uvalue=nn
      if (1-nm ne nn) then $
        widget_control, self.wscroll, set_slider_min=1-nm, set_uvalue=1-nm
      mini=self.top>0
      maxi=(mini+self.ny-1)<(nm-1)
      widget_control, self.wlist, $
        set_value=(*self.index)[(*self.match)[mini:maxi]]
    endif else begin
      widget_control, self.wlist, $
        set_value=['No matches found','Please try again']
    endelse
  endif else begin
    widget_control, self.wscroll, get_uvalue=nn
    nt=1-self.n_topics
    if (nt ne nn) then $
      widget_control, self.wscroll, set_slider_min=nt, set_uvalue=nt
    mini=self.top>0
    maxi=(mini+self.ny-1)<(self.n_topics-1)
    widget_control, self.wlist, set_value=(*self.index)[mini:maxi]
  endelse
  widget_control, self.wscroll, set_value=-self.top
  dy=self.sel_xyw[1]-self.top
  if (dy ge 0) and (dy le self.ny) then begin
    off=widget_info(self.wlist,text_xy_to_offset=[self.sel_xyw[0],dy])
    widget_control,self.wlist,set_text_select=[off,self.sel_xyw[2]]
  endif
end

pro index::addmatches
  if ((*self.match)[0] eq -1) then return
  nm=n_elements(*self.match)
  newm=*self.match
  for i=0, nm-1 do begin
    ind=(*self.match)[i]
    repeat begin
      num=-1
      str=(*self.index)[(ind>0)<(self.n_topics-1)]
      num=stregex(str,'[,][ ]*[0-9]+', length=len)
      if (num eq -1) then begin
        ind=ind+1
        if (ind ge self.n_topics) or (ind lt 0) then break
      endif
      if array_equal(newm ne ind,1) then newm=[newm,ind]
    endrep until (num ne -1)
  endfor
  *self.match = newm[sort(newm)]
end

pro index::lookup, topic
  compile_opt idl2
  if self.way then begin ; find
    if (topic eq '') then return
    *self.match=where(stregex(*self.index, topic, /boolean, /fold_case))
    self.top=0
    self->addmatches
    self->pagenum
    return
  endif
  n=n_elements(*self.ind)
  i=n 
  j=0
  while (ishft(i,-j) gt 0) do j=j+1
  i=ishft(1,j-1)
  j=i
  while (j ne 0) do begin
    if strlowcase(topic) gt strlowcase((*self.index)[(*self.ind)[i<(n-1)]]) $
      then i=i or ishft(j,-1) $
    else i=i and (not j) or ishft(j,-1)
    j=ishft(j,-1)
  endwhile
  self.top=(*self.ind)[(i+(i ne 0))<(n-1)]
  self->pagenum
end

pro index::pagenum, line=ind, dir=dir
  if self.way then begin
    self->pagefind, line=ind, dir=dir
    return
  endif
  if (size(ind,/type) eq 0) then ind=self.top
  if (size(dir,/type) eq 0) then dir=1
  bks='[ ]*[bld]*[dm]*[edg]*[img]*[itu]*[itd]*[obs]*[ref]*[sdf]*[use]*[wav]*'
  bkor='[bld]|[dm]|[edg]|[img]|[itu]|[itd]|[obs]|[ref]|[sdf]|[use]|[wav]'
  repeat begin
    num=-1
    str=(*self.index)[(ind>0)<(self.n_topics-1)]
    pref='[,][ ]*'
    expr='[0-9]+'
    if (self.book eq 'mindex') then $
      expr=expr+bks
    num=stregex(str,pref+expr, length=len, /fold_case)
    if (num eq -1) then begin
      ind=ind+dir
      if (ind ge self.n_topics) or (ind lt 0) then return
    endif else num=num+stregex(strmid(str,num,len),expr, $
                               length=len,/fold_case)
  endrep until (num ne -1)
  ; check for book on the next line if it's not present
  if (self.book eq 'mindex') then begin
    test=stregex(strmid(str,num,len), /fold_case, /boolean, bkor)
    if (test eq 0)then begin
      nstr=(*self.index)[(ind+1)<(self.n_topics-1)]
      if (stregex(nstr, /fold_case, length=nlen, bks) eq 0) then len=len+nlen+1
    endif
  endif
  self.pg=long(strmid(str,num,len))
  self.sel_xyw=[num,ind>0,len]
  dy=ind-self.top
  if (dy ge self.ny) then self.top=ind-self.ny+1<(self.n_topics-1)
  if (dy lt 0) then self.top=ind>0
end

pro index::pagefind, line=ind, dir=dir
  if (*self.match)[0] eq -1 then return
  if (size(ind,/type) eq 0) then ind=0
  if (size(dir,/type) eq 0) then dir=1
  nm=n_elements(*self.match)
  bks='[ ]*[bld]*[dm]*[edg]*[img]*[itu]*[itd]*[obs]*[ref]*[sdf]*[use]*[wav]*'
  repeat begin
    num=-1
    str=(*self.index)[(*self.match)[(ind>0)<(nm-1)]]
    pref='[,][ ]*'
    expr='[0-9]+'
    if (self.book eq 'mindex') then $
       expr=expr+bks
    num=stregex(str,pref+expr, length=len, /fold_case)
    if (num eq -1) then begin
      ind=ind+dir
      if (ind ge nm) or (ind lt 0) then return
    endif else num=num+stregex(strmid(str,num,len),expr,length=len, /fold_case)
  endrep until (num ne -1)
  self.pg=long(strmid(str,num,len))
  self.sel_xyw=[num,ind,len]
  dy=ind-self.top
  if (dy ge self.ny) then self.top=ind-self.ny+1<(nm-1)
  if (dy lt 0) then self.top=ind>0
end

pro index::gotopic, ev  ; can be called as an event (button)
  book=self.book
  if (book eq 'mindex') then begin
    sel=self.sel_xyw
    if (self.way) then num=(*self.match)[sel[1]] else num=sel[1]
    str=(*self.index)[num]
    if (sel[0]+sel[2] gt strlen(str)) then str=str+(*self.index)[num+1]
    str=strmid(str, sel[0], sel[2])
    trans = [['bld','building'], ['dm','dataminer'], $
             ['edg','edg'], ['img','image'], $
             ['obs','obsolete'], ['itu','itooluserguide'],$
             ['itd','itooldevguide'],['obs','obsolete'], $
             ['ref','refguide'], ['sdf','sdf'], $
             ['use','using'], ['wav','wavelet']]
    for i=0, n_elements(trans)/2-1 do begin
      if strpos(strlowcase(str),trans[0,i]) ne -1 then begin
        book=trans[1,i]
        break
      endif
    endfor
  endif
  online_help, page=self.pg, book=book+'.pdf'
end

pro index::booklist, booklist=res, displaylist=d, names=nam
  res=file_search([filepath('*.index.gz',subdir='help'),$
                   filepath(root=self.dir,'*.index.gz')])
  if (res[0] eq '') then message,'Index files not found'
  n=n_elements(res)
  if arg_present(d) or arg_present(name) then begin
    nam=strarr(n)
    d=strarr(n)
    for i=0,n-1 do begin
      line=''
      openr,lun,res[i], /get_lun, /compress
      readf,lun,line
      nam[i]=line
      readf,lun,line
      free_lun,lun
      d[i]=line
    endfor
  endif
  pos=strpos(res,'.index',/reverse_search)
  for i=0,n-1 do res[i]=strmid(res[i],0,pos[i])
end

pro index::move, pos
  if (abs(pos) eq 0.5) then begin
    ind=self.sel_xyw[1]>0  
    if self.way then begin
      nm=n_elements(*self.match)
      str=(*self.index)[(*self.match)[(ind>0)<(nm-1)]] 
    endif else str=(*self.index)[ind]
    num=-1
    if (pos gt 0) then begin
      cm=strpos(strmid(str,self.sel_xyw[0]),',')
      if (cm ne -1) then cm=cm+self.sel_xyw[0]
    endif else cm=strpos(strmid(str,0,self.sel_xyw[0]-2),',',/reverse_search)
    expr='[0-9]+'
    if (self.book eq 'mindex') then $
       expr=expr+'[ ]*[bld]*[dm]*[edg]*[img]*[obs]*[ref]*[use]*[wav]*'
    if (cm ne -1) then num=stregex(strmid(str,cm),expr,length=len,/fold_case)
    if (num eq -1) then begin
      dir=(pos gt 0)?1:-1
      self->pagenum, line=ind+dir, dir=dir
      self->updatelist
      return
    endif
    self.pg=long(strmid(str,cm+num,len))
    self.sel_xyw=[cm+num,self.sel_xyw[1],len]
    self->updatelist
  endif else begin
    self->pagenum, line=self.sel_xyw[1]+pos, dir=(pos gt 0)?1:-1
    self->updatelist
  endelse
end

pro index::droplistevent, ev
  widget_control,ev.id,get_uvalue=books
  book=books[ev.index]
  if (book eq self.book) then return
  self.book=book
  self->loadbook
  widget_control,self.wtext,get_value=topic
  self->lookup, topic
  self->updatelist
end

pro index::listevent,ev
  if (tag_names(ev, /structure_name) eq 'WIDGET_KBRD_FOCUS') then $
    self.focus=ev.enter
  if not self.focus then return
  if (tag_names(ev,/structure_name) eq 'WIDGET_TEXT_SEL') then begin
    ; mouse event
    pos=widget_info(self.wlist,text_offset_to_xy=ev.offset)
    xpos=self.sel_xyw[0]+self.sel_xyw[2]
    ypos=self.sel_xyw[1]-self.top
    yy=self.sel_xyw[1]
    len=strlen((*self.index)[yy])
    if (xpos gt len) then begin
      xpos=xpos-len-1
      ypos=ypos+1
    endif
    dx=pos[0]-xpos
    dy=pos[1]-ypos
    if (abs(ev.offset-self.dbl_clk[0]) lt 1) $
      and (systime(1)-self.dbl_clk[1] lt 0.25) then begin
      self->gotopic
    endif else begin
      self.dbl_clk=[self.dbl_clk[2:3],ev.offset,systime(1)]
    endelse
    if (abs(dy) gt 0) then self->move, dy $
    else begin
      step=0
      if (dx gt 1) then step=0.5
      if (dx lt -self.sel_xyw[2]) then step=-0.5
      if (step ne 0) then self->move, step $
      else self->updatelist
    endelse
  endif
  if (tag_names(ev,/structure_name) eq 'WIDGET_TEXT_CH') then begin
    case ev.ch of
      6b:  self->move,0.5       ; C-f right
      2b:  self->move,-0.5      ; C-b left
      16b: self->move,-1        ; C-p up
      14b: self->move,1         ; C-n down
      22b: self->move,self.ny   ; C-v page down
      7b:self->move,-self.ny    ; C-g page up
      9b: widget_control, self.wtext, /input_focus
      10b: self->gotopic
      else:
    endcase
  endif
end

; resize events
pro index::event, ev
  widget_control, self.wlist, scr_xsize=ev.x-self.pad[0], $
    scr_ysize=ev.y-self.pad[1]
  widget_control, self.wtext, scr_xsize=ev.x-self.pad[0]
  geom=widget_info(self.wlist,/geometry)
  self.ny=geom.ysize
  self->updatelist
end

pro index::textevent, ev
  if (tag_names(ev, /structure_name) eq 'WIDGET_KBRD_FOCUS') then $
    self.focus=ev.enter
  if not self.focus then return
  if (tag_names(ev,/structure_name) eq 'WIDGET_TEXT_CH') then begin
    noremove=1
    case ev.ch of
      6b:  self->move,0.5       ; C-f right
      2b:  self->move,-0.5      ; C-b left
      16b: self->move,-1        ; C-p up
      14b: self->move,1         ; C-n down
      22b: self->move,self.ny   ; C-v page down
      7b:self->move,-self.ny    ; C-g page up
      9b:  widget_control, self.wtext, /input_focus
      10b: self->gotopic
      17b: begin
        widget_control, self.tlb, /destroy
        return
      end
      else: noremove=0
    endcase
    if noremove and (self.nogui eq 0) then begin
      widget_control,self.wtext,get_value=str
      str=byte(str)
      ind=where(str ne ev.ch,complement=nind)
      if (ind[0] eq -1) then str='' else str=string(str[ind])
      if (nind[0] ne -1) then widget_control,self.wtext, $
        set_value=str,set_text_select=[nind[0],0]
    endif
  endif 
  widget_control, self.wtext, get_value=topic
  if (self.prev_topic ne topic) then begin
    self.prev_topic=topic
    widget_control,ev.id,get_value=topic
    self->lookup, topic
    self->updatelist
  endif
end

pro index::wayevent, ev
  widget_control, ev.id, get_value=val
  if (val eq 'Find') then begin
    self.way=1 
    widget_control, self.wtext, get_value=topic
    self->lookup, topic
    self->updatelist
  endif else begin
    self.way=0
    widget_control, self.wtext, get_value=topic
    self->lookup, topic
    self->updatelist
  endelse
end

pro index::scrollevent, ev
  self.top=-ev.value
  self->updatelist
end

pro index::gobookmark, ev
  widget_control, ev.id, get_uvalue=uval
  t=strsplit(uval,':',/extract)
  online_help, book=t[0], page=long(t[1])
end

pro index::editprefevent, ev
  x=obj_new('index_edit',self,group_leader=self.tlb)
  x->xmanager
end

pro index::cleanup
  if self.close then online_help, /quit
  ptr_free,[self.index,self.ind,self.match]
  self->index_app::cleanup
end

pro index__define
  compile_opt idl2
  s={index, inherits index_app, index:ptr_new(), ind:ptr_new(), $
     wtext:0, wlist:0, book:'', pg:0, n_topics:0, n_ind:0, $
     top:0, sel_xyw:[0,0,0], nogui:0b, pad:[0,0], ny:0, dir:'',$
     focus:0, prev_topic:'', way:0, match:ptr_new(), $
     dbl_clk:dblarr(4), wscroll:0, menubase:0, close:0 }
end

pro index, topic, objref=i, book=book, block=block
  if (size(topic,/type) eq 0) then topic=''
  i=obj_new('index', topic, book=book)
  i->xmanager, no_block=1-keyword_set(block)
end

