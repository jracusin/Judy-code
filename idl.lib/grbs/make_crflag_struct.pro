pro make_crflag_struct,crflag
  
  on_error, 2

  IF n_params() NE 1 THEN BEGIN
     print,'Syntax: make_crflag_struct,crflag_struct'
     print,''
     return
  END

  crflag={HIGH_LAT: 'D', $
          EI: 'D', $
          NORMAL: 'D', $
          JET: 'D', $
          ISM: 'D', $
          WIND: 'D', $
          NU2: 'D', $
          NU3: 'D', $
          P12: 'D', $
          P2: 'D', $
          UNIF_JET: 'D', $
          JET_EI: 'D', $
          U_SPEAD_JET: 'D', $
          U_NOSPREAD_JET: 'D', $
          STRUC_JET: 'D'}       ;, $
;                 : 'D', $
  
  
  return
END
