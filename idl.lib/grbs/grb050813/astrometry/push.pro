pro push, array, elements, new=new
if n_elements(array) eq 0 or keyword_set(new) then begin
    array=elements
endif else begin
    array=[array,elements]
endelse
return
end

