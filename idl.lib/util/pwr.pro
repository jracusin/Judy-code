PRO pwr, X, A, F,pder  

  F = A[0] * X^(-A[1])
 ;If the procedure is called with four parameters, calculate the partial derivatives. 
  IF N_PARAMS() GE 4 THEN $ 
;    pder = [[X], [replicate(1.0, N_ELEMENTS(X))]] 
    pder = [[X^A[1]],[A[1]*A[0]*X^(A[1]-1)]]

END 
