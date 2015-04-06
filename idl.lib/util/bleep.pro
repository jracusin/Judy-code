pro bleep,n=n
  ;; this beeps.  Duh.
  if n_elements(n) eq 0 then $
      n = 1
  for i=1,n do begin
    print,string([7B]),format='(1A,$)'
    wait,0.5
  endfor
  ;; it is less useful if terminal is set to visual bell
  return
end
