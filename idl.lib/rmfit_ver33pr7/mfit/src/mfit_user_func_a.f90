
   FUNCTION MFIT_USER_FUNC_A  ( ENERGY, P )

   use MFIT_kinds

   real (kind=mfit_real) :: MFIT_USER_FUNC_A


   !  This subroutine function is provided so that the use can easily add
   !  a photon model to MFIT.
   !  The "A" designates the 1st of the available user functions.
   !  This function is used as an additive function.
   !  This function is provided as an example / template.

   !  USE: This subroutine recives an energy and 10 parameters.   Using
   !  these, calculate the function value and place it into MFIT_USER_FUNC_A.
   !  Note that P(1) must be the overall amplitude (units: phot/s-cm2-keV) of
   !  the function and that you may skip some parameters (as long as you don't
   !  try to fit parameters that aren't used!).
   !  An optional step is to modify the description of the model and the
   !  parameter change limits specified in the file MFIT_FUNC.INFO.

   !  ADVANTAGES: Ease of adding another photon model function.

   !  DISADVANTAGES: Faster executing code would be obtained by directly adding
   !  a function to the source code of M_PHOTONS_MODEL--this would eliminate
   !  a large number of calls.

   !  Michael S. Briggs, UAH / MSFC ES-66, 19 June 1993.

   !  Arguments, all input:
   real (kind=mfit_real), intent (in) :: ENERGY                   ! keV

   !   P(1) must be amplitude parameter, phot/s-cm2-keV.   Others as needed.....
   real (kind=mfit_real), dimension (1:10), intent (in) :: P

   !  Output is solely by function value.


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  This version contains an example function: Brainerd's power law model
   !  parameterized by tau_shift = tau / (1+z) rather than by tau.
   !  P1: overall amplitude
   !  P2: index
   !  P3: tau_shift, Thomson optical depth / (1+z)
   !  P4: z, redshift
   !  P5: metalicity in units of the solar abundance
   !  P6 to P10: not used.

   !******************************************************************************
   !  NAME CHANGED FROM KN_MODEL TO M_BRAINERD BY M. BRIGGS.
   !  ALSO:  FABS --> ABS.
   !  This codes was written on March 4, 1993, by Jerome James Brainerd.
   !  Modified to ( 2 + delta )/tau and tau
   !  on July 16, 1996, by Jerome James Brainerd.

   !  In this file is the source code for the program kn_model, which
   !  produces a spectrum by passing a power law spectrum through a scattering
   !  medium.


!! function M_BRAINERD  ( energy, normalize, deltauratio, tau, z, metal )
!! real (kind=mfit_real) :: M_BRAINERD

   !  FORMER Input arguments:
!! real (kind=mfit_real), intent (in) :: energy         ! Photon energy in keV.
   real (kind=mfit_real) :: normalize      ! Normalization of curve.  This value is returned at 100 keV.
   real (kind=mfit_real) :: deltauratio    ! ( delta + 2 )/tau.  A parameter setting the
                                           ! peak of the nu F_nu curve for z = 0.
                                           ! delta is the index of the unscattered power law.
   real (kind=mfit_real) :: tau            ! Thomson optical depth,
   real (kind=mfit_real) :: z              ! the cosmological redshift z,
   real (kind=mfit_real) :: metal          ! the metalicity in units of the solar abundance.


   !  internal variables:
   real (kind=mfit_real) :: EXP_ARG
   integer (kind=mfit_integer) :: i
   real (kind=mfit_real) :: en_norm
   real (kind=mfit_real) :: energy0, epsilon, eps2m
   real (kind=mfit_real) :: epsinv, eps2, eps2p1, ep1
   real (kind=mfit_real) :: s1, s2, s3
   real (kind=mfit_real) :: kn_cross1, kn_cross2, kn_cross3, kn_cross, kn_norm
   real (kind=mfit_real) :: photo_elec


   !******************************************************************************

   !  copy parameters of "user function" into "named" parameters of the particular function:

   normalize = P(1)
   deltauratio = P(2)
   z = P(4)
   TAU = P(3) * (1. + z)
   metal = P(5)

   !******************************************************************************

  if ( energy .le. 0. ) then
      MFIT_USER_FUNC_A = 0.
      write (6, *) "Negative energy received by function MFIT_USER_FUNC_A!", energy
      stop
   end if

   en_norm = 100.                   ! Normalization energy (100keV).
   energy0 = energy*( 1. + z )      ! Source photon energy in keV.
   epsilon = energy0/511.           ! Source photon energy in units of
                                    ! electron rest mass energy.

   eps2 = epsilon + epsilon
   epsinv = 1./epsilon
   eps2p1 = 1. + eps2
   ep1 = 1. + epsilon

   kn_cross1 = eps2*( 2. + eps2 )/( 2.*eps2p1*eps2p1 )

   if ( eps2 .gt. 0.2 ) then
      kn_cross2 = alog( eps2p1 ) - eps2/eps2p1
      kn_cross3 = epsinv * epsinv * ( eps2*( 1. + ep1*ep1/eps2p1 ) - 2.*ep1*alog( eps2p1 ) )
   else
      kn_cross2 = 0.5

      s1 = 1./3.
      s2 = 1./2.
      s3 = 1.

      eps2m = -eps2
      i = 1
      do while ( abs( eps2m ) .gt. 1.e-5 )
         kn_cross2 = kn_cross2 + eps2m*( i + 1 )/( i + 2 )
         s1 = s1 + eps2m*( i + 1 )/( i + 3 )
         s2 = s2 + eps2m*( i + 1 )/( i + 2 )
         s3 = s3 + eps2m
         i = i + 1
         eps2m = -eps2*eps2m
      end do
      kn_cross2 = kn_cross2*eps2*eps2
      kn_cross3 = eps2*( 4.*s1 - 4.*s2 + s3 )
   end if

   !  The Klein Neshina cross section at energy0.
   kn_cross = 3./8.*epsinv*( kn_cross1 + kn_cross2 + kn_cross3 )


   epsilon = en_norm*( 1. + z )/511.
   eps2 = epsilon + epsilon
   epsinv = 1./epsilon
   eps2p1 = 1. + eps2
   ep1 = 1. + epsilon

   kn_cross1 = eps2*( 2. + eps2 )/( 2.*eps2p1*eps2p1 )

   if ( eps2 .gt. 0.2 ) then
      kn_cross2 = alog( eps2p1 ) - eps2/eps2p1
      kn_cross3 = epsinv*epsinv * ( eps2*( 1. + ep1*ep1/eps2p1 ) - 2.*ep1*alog( eps2p1 ) )
   else
      kn_cross2 = 0.5

      s1 = 1./3.
      s2 = 1./2.
      s3 = 1.

      eps2m = -eps2
      i = 1
      do while ( abs( eps2m ) .gt. 1.e-5 )
         kn_cross2 = kn_cross2 + eps2m*( i + 1 )/( i + 2 )
         s1 = s1 + eps2m*( i + 1 )/( i + 3 )
         s2 = s2 + eps2m*( i + 1 )/( i + 2 )
         s3 = s3 + eps2m
         i = i + 1
         eps2m = -eps2*eps2m
      end do
      kn_cross2 = kn_cross2*eps2*eps2
      kn_cross3 = eps2*( 4.*s1 - 4.*s2 + s3 )
   end if

   !  The Klein Neshina cross section at the normalization energy.
   kn_norm = 3./8.*epsinv*( kn_cross1 + kn_cross2 + kn_cross3 )


   ! The photoelectrion cross section for a composition of hydrogen, helium, and heavy elements.
   photo_elec = ( 0.3990711 + 5.071327*metal ) / ( 1.135962 + 1.452396e-02*metal ) *      &
        ( ( energy0/5.11 )**( -3.059308 ) - ( en_norm*( 1. + z )/5.11 )**( -3.059308 ) )

   !  The attenuated spectrum.
   EXP_ARG = (deltauratio * tau - 2.) * alog (energy/en_norm) - tau * (kn_cross - kn_norm + photo_elec)
   EXP_ARG = MIN (EXP_ARG, 50.)
   MFIT_USER_FUNC_A = normalize * exp (EXP_ARG)

   RETURN

   END FUNCTION MFIT_USER_FUNC_A
