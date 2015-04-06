;+
; NAME:
;  SPECLINE_NAME
;
;
; PURPOSE:
;  Convert the restframe wavelength of a line to its name. 
;
;
; CATEGORY:
;  Spectra
;
;
; CALLING SEQUENCE:
;  name = specline_name(input_line, type=type, position=position)
;
;
; INPUTS:
;  line wavelength in angstroms
;
;
; OPTIONAL INPUTS:
;  none
;
; OUTPUTS:
;  the name in string form, e.g. 'OIII'
;
; CAVEATS:  This set of lines only works for 1d_22 for now, there are some in
;          1d_23 that don't match.  So don't panic if some lines don't show;
;          I'll fix it soon.
;
; OPTIONAL OUTPUTS:
;  type: what kind of line it is.
;  positions: used by plot_spec1d. A relative position on the plot device.
;
;
; MODIFICATION HISTORY:
;  Creation: 15-Aug-2003: Erin Sheldon UofChicago
;
;-


FUNCTION specline_name, input_line, type=type, position=position

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: name = specline_name(input_line, type=type, position=position)'
      return,-1
  ENDIF 

  COMMON specline_name_block, specline_struct

  delvarx, specline_struct
  IF n_elements(specline_struct) EQ 0 THEN BEGIN 
      specline_file = !sdss_spec_dir + 'line_names/line_names.par'
      yanny_read, specline_file, pdata

      specline_struct = *pdata
      ptr_free, pdata

  ENDIF 

  line_ids = specline_struct.line_index
  line_names = specline_struct.name
  line_types = specline_struct.line_type

  ;; Fix line names. Want to redo each time to account for device 
  ;; differences

  w=where(line_names EQ 'Ly_a',nw)
  IF nw NE 0 THEN line_names[w] = 'Ly!D'+!csym.alpha+'!N'

  w=where(line_names EQ 'H_a',nw)
  IF nw NE 0 THEN line_names[w] = 'H!D'+!csym.alpha+'!N'
  w=where(line_names EQ 'H_b',nw)
  IF nw NE 0 THEN line_names[w] = 'H!D'+!csym.beta+'!N'
  w=where(line_names EQ 'H_d',nw)
  IF nw NE 0 THEN line_names[w] = 'H!D'+!csym.delta+'!N'
  w=where(line_names EQ 'H_g',nw)
  IF nw NE 0 THEN line_names[w] = 'H!D'+!csym.gamma+'!N'


  w=where(line_names EQ 'H_h',nw)
  IF nw NE 0 THEN line_names[w] = 'H!Dh!N'
  w=where(line_names EQ 'H_y',nw)
  IF nw NE 0 THEN line_names[w] = 'H!Dy!N'

  s = sort(line_ids)
  line_ids = line_ids[s]
  line_names = line_names[s]
  line_types = line_types[s]

  ;; y-positions on graph
  nn = n_elements(line_ids)
  step = 0.05
  tpos = (1.-step) - (lindgen(nn) MOD 3)*step

  ;; round them off
  li = round(input_line)

  nin = n_elements(input_line)

  IF nin EQ 1 THEN BEGIN
      retnames = ''
      type = ''
      position = -1000.

      mm = where(line_ids EQ li[0], nmm)
      IF mm[0] NE -1 THEN BEGIN 
          retnames = line_names[mm]
          type = line_types[mm]
          position = tpos[mm]
      ENDIF 
  ENDIF ELSE BEGIN 

      retnames = strarr(nin)
      type = replicate('', nin)
      position = replicate(-1000., nin)

      match, li, line_ids, minput, mm

      IF mm[0] NE -1 THEN BEGIN 
          retnames[minput] = line_names[mm]
          type[minput] = line_types[mm]
          position[minput] = tpos[mm]
      ENDIF 
  ENDELSE 

  return,retnames

END 
