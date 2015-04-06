FUNCTION rotvec,n,a,v
  
;+
; NAME: ROTVEC
; 
; PURPOSE: ROTVEC RETURNS THE COMPONENTS OF A GIVEN VECTOR AFTER
;          A RIGHT HANDED SCREW ROTATION OF THE COORDINATE SYSTEM AROUND
;          THE N TH AXIS BY THE ANGLE A.
;
; CALLING SEQUENCE:
;         V_new=ROTVEC(N,A,V)
;
; INPUTS:
;         N = NUMBER OF AXIS USED FOR ROTATION (1,2,3).
;         A = ANGLE OF ROTATION IN DEGREES.
;         V = VECTOR TO BE ROTATED (DIM 3)
;
; OUTPUTS:
;         V_new = VECTOR THAT HAS BEEN ROTATED (DIM 3)
;
; MODIFIED:
;      ??/??/?? Written by DNB?
;      08/09/04 by JLR - translated into IDL
;  
;-
  
  
  if n_params() eq 0 then begin
     print,'syntax - v_new = rotvec(n,a,v)'
     print,'              N = axis number (1,2,3) = (x,y,z)'
     print,'              A = Angle of Rotation in Degrees'
     print,'              V = vector to be input'
     return,0
  endif 
  
  vnew=v
  aa=a*!dtor
  sina=sin(aa)
  cosa=cos(aa)
  u1=v[0]
  u2=v[1]
  u3=v[2]
  
  case n of 
     1: begin
        vnew[1] = (u2*cosa)+(u3*sina)
        vnew[2] = (u3*cosa)-(u2*sina)
     end
     
     2: begin
        vnew[2] = u3*cosa+u1*sina
        vnew[0] = u1*cosa-u3*sina
     end
     
     3: begin
        vnew[0]=u1*cosa+u2*sina
        vnew[1]=u2*cosa-u1*sina
     end
     
     else: return,0
     
  endcase
     
  return,vnew
end 
