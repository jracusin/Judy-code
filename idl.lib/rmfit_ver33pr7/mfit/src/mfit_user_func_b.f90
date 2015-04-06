
   FUNCTION MFIT_USER_FUNC_B  ( ENERGY, P )

   use MFIT_kinds

   real (kind=mfit_real) :: MFIT_USER_FUNC_B


   !  This subroutine function is provided so that the use can easily add
   !  a photon model to MFIT.
   !  The "B" designates the 2nd of the available user functions.
   !  This function is used as an additive function.
   !  This function is provided as an example / template.

   !  USE: This subroutine recives an energy and 10 parameters.   Using
   !  these, calculate the function value and place it into MFIT_USER_FUNC_B.
   !  Note that P(1) must be the overall amplitude (units: phot/s-cm2-keV) of
   !  the function and that you may skip some parameters (as long as you don't
   !  try to fit parameters that aren't used!).
   !  An optional step is to modify the description of the model and the
   !  parameter change limits specified in the file MFIT_FUNC.INFO.

   !  ADVANTAGES: Ease of adding another photon model function.

   !  DISADVANTAGES: Faster executing code would be obtained by directly adding
   !  a function to the source code of M_PHOTONS_MODEL--this would eliminate
   !  a large number of calls.

   !  This version contains an example function.
   !  Michael S. Briggs, UAH / MSFC ES-66, 19 June 1993.

   !  Arguments, all input:
   real (kind=mfit_real), intent (in) :: ENERGY                   ! keV

   !   P(1) must be amplitude parameter, phot/s-cm2-keV.   Others as needed.....
   real (kind=mfit_real), dimension (1:10), intent (in) :: P

   !  Output is solely by function value.

   !  Internal variables:
   real (kind=mfit_real) :: CALC


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   !  An example function: broken power law.
   !  Calculation uses logs as an intermediate to avoid overflow.

   !  P1 -- amplitude
   !  P2 -- pivot energy  (keep fixed)
   !  P3 -- index below break
   !  P4 -- break energy in keV
   !  P5 -- index above break

   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  IF ( ENERGY .LE. P(4) ) THEN
      CALC = P(3) * LOG (ENERGY / P(2) )
      CALC = MIN (CALC, 40.)
      MFIT_USER_FUNC_B = P(1) * EXP (CALC)
   ELSE
      CALC = LOG ( P(4) / P(2) ) * P(3)  +  LOG ( ENERGY / P(4) ) * P(5)
      CALC = MIN (CALC, 40.)
      MFIT_USER_FUNC_B = P(1) * EXP (CALC)
   END IF


   RETURN

   END FUNCTION MFIT_USER_FUNC_B


