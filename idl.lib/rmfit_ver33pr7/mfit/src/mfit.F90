
!  2009 April 4--6: Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs.
!  2009 May 9 -- version 0.97: A long series of improvements ending on this date, by M. S. Briggs, 
!     including:
!      better error handling,
!      cleanup, reorganize, refactor code,
!      Castor C-statistic rather than Cash statistic.
!      Make likelihood & Castor calculations more robust.
!  2009 May 16 -- version 0.98, by M. S. Briggs: remove arguments MaxChan and MaxEbins in 
!     favor of calculating these values using the maxval intrinsic function.
!  2010 Jan 3 -- add a second external entry, mfit_FinePhotModel, which allows an easy way
!     to evaluate photon models for a vector of arbitrary energy values.


! #################################################################################################


module MFIT_parameters


   use MFIT_kinds


   ! ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++


   !   General comments pertainent to all versions of this file:
   !   This module contains declarations and parameter initializations
   !   for some parameters used by the MFIT program in declaring arrays.
   !   Michael S. Briggs, UAH / MSFC ES-64, 12 November 1992.

   ! ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++
   
   !   Comments re specific parameter values in this version of the module:


   ! *****  The values MAX_TERMS, MAX_PPT and MAX_PARAM must agree in three files !!  *****
   ! *****  The three files: mfit.F90, mfit_cparams.h and mfit__define.pro            *****
   ! *****  VERSION_LEN must agree in two of the files !!                             *****


   !  Maximum number of model terms allowed in model.   A model term is
   !  a separate physical model, e.g., power law, broken power, Gaussian.
   !  The model consists of the sum/product of all selected model terms:

   integer (kind=mfit_integer), parameter :: MAX_TERMS = 55


   !  Maximum number of parameters possible for a model term:
   !  Examples of parameters: amplitude, centroid, power law index, etc.

   integer (kind=mfit_integer), parameter :: MAX_PPT = 10            ! Max_Param_Per_Term


   !  Maximum total number of parameter in the model:
   !  Would always work: PARAMETER (MAX_PARAM = MAX_TERMS * MAX_PPT), but this
   !  value is much larger than would ever be actually be used.  So the
   !  user should never select a model with more than MAX_PARAM parameters
   !  (even counting both fixed and varying), hence we can save a fair
   !  amount of space:

   integer (kind=mfit_integer), parameter :: MAX_PARAM = 20


   !  length of MFIT Version string, not including terminating C NUL character
   integer (kind=mfit_integer), parameter :: VERSION_LEN = 40
   

   !  Fortran unit number for file (debug) output:
   integer (kind=mfit_integer), parameter :: MODULE_PARAM_FILE_UNIT = 61
   



! ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++ ++


end module MFIT_parameters


! #################################################################################################


!  The shared variables of this function are used to communicate between the function
!  M_TITARCHUK and the two functions that it numerically integrates, M_INTG1_TITARCHUK and M_INTG2_TITARCHUK.

module SHR_VARS_new_titarchuk


   use MFIT_kinds


   implicit none


   real (kind=mfit_real) :: MODULE_VAR_X_TIT
   real (kind=mfit_real) :: MODULE_VAR_N_TIT
   real (kind=mfit_real) :: MODULE_VAR_MAXI1_TIT
   real (kind=mfit_real) :: MODULE_VAR_MAXI2_TIT


end module SHR_VARS_new_titarchuk


! #################################################################################################


!  The shared variables of this module are used to communicate between function M_SUNYAEV and
!  the function that it numerically integrates, M_SUNY_INTG.

module SHR_VARS_sunyaev_titarchuk


   use MFIT_kinds


   implicit none


   real (kind=mfit_real) :: MODULE_VAR_X_ST
   real (kind=mfit_real) :: MODULE_VAR_N_ST


end module SHR_VARS_sunyaev_titarchuk


! #################################################################################################


module MFIT_MODULE


   use MFIT_kinds


   implicit none


   PRIVATE   ! change default to PRIVATE

   !  There are two public entry points, both accessible via Fortran and C via the iso_c_binding.
   !  1) the primary entry is subroutine MFIT: to perform a forward-folding fit/deconvolution
   !     of detector count data,
   !  2) a secondary entry point: subroutine mfit_FinePhotModel -- to evaluate the photon model
   !     at arbitrary energies provided as input.
   
   PUBLIC :: MFIT, mfit_FinePhotModel


contains


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE MFIT  ( DebugMask, fit_mode, NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_ENERGY, CHAN_WIDTH,                       &
           OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, PHOT_ENERGY, PHOT_WIDTH, DRM, RES_FRAC_AT511, RES_EXP,          &
           FIT_CHAN, USE_DET, TERM_USED, NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE,                                   &
           NUM_PARAM_OF_TERM, REL_CHANGE_LIMIT, ABS_CHANGE_LIMIT, REL_CONVERG, ABS_CONVERG, MAX_TRIES,                  &
           ENABLE_UNDETERMINE, UNDETERMINE_PRIORITY, UNDETERMINE_REL_REQ, UNDETERMINE_ABS_REQ, HIGHEST_UD_PRIORITY,     &
           PARAM_VARY, CHAN_OFFSET, PARAM, FIT_ERR, STATISTIC_ARG, DOF, NUM_VARY, PARAM_UNCER, ALPHA_ARG, COVAR_ARG,    &
           MODEL_CNT_RATE, PHOT_OBS_RATE, PHOT_OBS_SIG, NU_F_NU_DATA, NU_F_NU_SIG, NU_F_NU_MODEL,                       &
           MODEL_ENERGY, MODEL_PHOT_RATE, PHOT_MODEL_BYCHAN, MODEL_CR_VARI, DATA_CR_VARI, MFIT_VERSION_ARG  )           &
           BIND ( C, name="mfit" )


   use iso_c_binding

   use MFIT_parameters


   !  Model FIT or Michael's FIT:
   !  Subroutine to do linear and nonlinear fitting of gamma-ray spectra,
   !  incorporating the use of a Detector Response Matrix (DRM) to convert
   !  a model photon spectrum to a model detector count spectrum.
   !  Intended for use with BATSE data from any detector type and any data
   !  type.   It can do simultaneous fits to data from several detectors.
   !  To this program, a detector is a DRM.

   !  MSB, 2009 April 8: remove unused arguments: PREV_CHISQ, PREV_DOF and HAVE_MODEL.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH/MSFC ES-62, 21 September 1992.
   !  Major revision 1999 June.
   !  Revised by MSB 2008 Oct 31: added fitting via likelihood and Cash C statistic.
   !  Revised by MSB 2008 Nov 18 & 2007 Dec 4 to restore batch fitting with fixing
   !  of undetermined parameters.


   !   ### ### ### ### ### ### ### Input Arguments ### ### ### ### ### ### ###

   !  Extra debug output according to which bits are set:
   integer (kind=mfit_integer), intent (in) :: DebugMask

   !  1 = chisq, 2 = likelihood, 3 = Castor C-statistic:
   integer (kind=mfit_integer), intent (in) :: fit_mode

   !   ### ### ### Dimensions:

   !  the number of detectors.   Note that "a detector" = "a DRM", e.g.,
   !  summed data such as MER counts as 1 detector:
   integer (kind=mfit_integer), intent (in) :: NUM_DET

   !  the number of data channels:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN

   !  the number of energy bins on the input side of the DRM.  The photon
   !  model is evaluated for these bins:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_EBINS


   !   ### ### ### Count data:

   !  the center energies of each channel, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: CHAN_ENERGY

   !  the energy widths of each channel, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: CHAN_WIDTH

   !  the total (not background subtracted) observed count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: OBS_CRATE

   !  the model predicted background count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: BACK_CRATE

   !  the uncertainty (sigma) of the model background count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: BACK_CSIG

   !  the livetime, seconds:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: LIVE_TIME

   !   ### ### ### The DRM:

   !  the center energies of the DRM input bins, also called photon bins, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_ENERGY

   !  the widths of the photon bins, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_WIDTH

   !  the DRM, detector response matrix:
   !  Converts photons/s-cm^2-bin to counts/s-chan-detector so units are
   !  counts-cm^2-bin / photons-chan-detector.  Since counts, photons, bin
   !  and chan are not really units, this means that the units of the DRM
   !  are cm^2, which makes sense since the DRM is an elaboration of the
   !  concept of effective area.
   !  "Chan" are data channels and "bin" are the input or photon bins.
   !  Logical dimensions are # of photon bins X # of channels X # of detectors:
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: DRM

   !   ### ### ### Detector Properties:

   !  fractional energy resolution of detector = resolution FWHM / energy is
   !  approximated by a power law: RES_FRAC_AT511 * (energy / 511.) ** RES_EXP:
   real (kind=mfit_real), dimension (0: NUM_DET-1), intent (in) :: RES_FRAC_AT511
   real (kind=mfit_real), dimension (0: NUM_DET-1), intent (in) :: RES_EXP

   !   ### ### ### Requested Model and Fit:

   !  the range of channels included in the fit:
   integer (kind=mfit_integer), dimension (0:1, 0: NUM_DET-1), intent (in) :: FIT_CHAN

   !  the detectors included in the fit:
   logical (kind=mfit_logical), dimension (0: NUM_DET-1), intent (in) :: USE_DET

   !  value is 1 if a term was included in the model:
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED

   !   ### ### ### Properties of Models and Fitting Instructions:

   !  the highest numbered model term (photon function) in the table (and
   !  implemented within MFIT):
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL

   !  the term number of the highest numbered additive photon function:
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS

   !  the term number of the first additive line photon function:
   integer (kind=mfit_integer), intent (in) :: NT_ADD_LINE

   !  number of parameters of each term (photon function)
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM

   !  the change in parameter values per iteration may be limited in order
   !  to make the fitting process more robust.   The specifications of the
   !  limitations are read for the function configuration file.
   !  if both limits are non-zero, the largest is used.
   !  if both are zero, no change limit is imposed.
   !  REL_CHANGE_LIMIT: relative (fractional) limit of parameter value changes:
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: REL_CHANGE_LIMIT
   !  ABS_CHANGE_LIMIT: absolute magnitude limit on parameter value changes:
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: ABS_CHANGE_LIMIT

   !  Requirements for determining that a fit has converged.
   !  Within MAX_TRIES iterations, both the relative and absolute changes
   !  in chisq must be less than the specified values.
   real (kind=mfit_real), intent (in) :: REL_CONVERG
   real (kind=mfit_real), intent (in) :: ABS_CONVERG
   integer (kind=mfit_integer), intent (in) :: MAX_TRIES

   !  Controls fixing of parameters in batch fits, if one or more parameters
   !  are poorly determined in a batch fit.

   logical (kind=mfit_logical), intent (in) :: ENABLE_UNDETERMINE
   integer (kind=mfit_integer), dimension (MAX_TERMS, MAX_PPT), intent (in) :: UNDETERMINE_PRIORITY
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: UNDETERMINE_REL_REQ
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: UNDETERMINE_ABS_REQ
   integer (kind=mfit_integer), intent (in) :: HIGHEST_UD_PRIORITY

   !   ### ### ### Output labeling:

   !  the channel number of the first channel passed, used to label output:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: CHAN_OFFSET


   !   ### ### ### ### ### ### ### Input/Output Arguments ### ### ### ### ### ###

   !  TRUE iff the parameter is to be varied in the fit.
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (inout) :: PARAM_VARY

   !  The values of the photon function parameters.
   !  Only the parameters for used terms (photon functions) (TERM_USED) matter.
   !  On input: the guesses of the user.
   !  On output: the best fit values.
   !  Units are as appropriate for each parameter (see PARAM_UNITS).
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (inout) :: PARAM


   !   ### ### ### ### ### ### ### Output Arguments ### ### ### ### ### ### ###


   !   ### ### ### Calculated Model Properties:

   !  indicates whether an error occurred.   Zero means no error:
   integer (kind=mfit_integer), intent (out) :: FIT_ERR

   !  the chisq or -2 log likelihood value of model fit or evaluation:
   real (kind=mfit_real), intent (out) :: STATISTIC_ARG

   !  the degrees-of-freedom of the fit:
   integer (kind=mfit_integer), intent (out) :: DOF

   !  the number of varying parameters in the fit:
   integer (kind=mfit_integer), intent (out) :: NUM_VARY

   !   ### ### ### Fit Results:

   !  the uncertainty on each fitted parameter value, as obtained from
   !  fitting matrix (see Numerical Recipes).
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (out) :: PARAM_UNCER
   
   !  The curvature matrix, which is half of the Hessian matrix.
   !  The covariance matrix COVAR is the inverse of this matrix.
   real (kind=mfit_real), dimension (MAX_PARAM, MAX_PARAM), intent (out) :: ALPHA_ARG

   !  The covariance matrix obtained from the fitting process (see Numerical
   !  Recipes).   This matrix has NOT been normalized to be the Correlation Matrix.
   !  This matrix is indexed (to keep the size down) by only the varying
   !  parameters.    If one skips the non-varying parameters of PARAM_UNCER,
   !  then PARAM_UNCER () = SQRT (COVAR (I, I)).
   !  COVAR is the inverse of ALPHA.
   real (kind=mfit_real), dimension (MAX_PARAM, MAX_PARAM), intent (out) :: COVAR_ARG

   !  the model count rate, units counts/s-keV:
   !  obtained from the model photon rate by multiplying with the DRM:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1), intent (out) :: MODEL_CNT_RATE

   !  a model dependent quantity: the data converted into a photon rate:
   !  photons/s-cm^2-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: PHOT_OBS_RATE

   !  the uncertainties, sigma, on the previous variable:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: PHOT_OBS_SIG

   !  the model derived photon rate data in nu-Fnu units: keV^2 / s-cm^2-keV.
   !  this, like PHOT_OBS_RATE, is a model dependent quantity.
   !  it is d(energy flux) / d(log E).
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: NU_F_NU_DATA

   !  the uncertainties, sigma, on the previous variable:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: NU_F_NU_SIG

   !  the photon model in nu-Fnu units: keV^2 / s-cm^2-keV.
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1), intent (out) :: NU_F_NU_MODEL

   !  an array listing the energies for which the photon model quantitites,
   !  i.e., MODEL_PHOT_RATE and NU_F_NU_MODEL, are output:
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1), intent (out) :: MODEL_ENERGY

   !  the model photon rate, photons / s-cm^2-keV.   Term 0 is the total
   !  rate.   Also given is the rates of the individual model terms:
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: MAX_TERMS, 0: NUM_DET-1), intent (out) :: MODEL_PHOT_RATE

   !  the variances of the count rates.  The Poisson contribution is calculated
   !  from the model count rate as obtained from the photon model and the DRM.
   !  units counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: MODEL_CR_VARI

   !  the variances of the count rates.   The Poisson contribution is calculated
   !  from the observed count rate.
   !  units counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: DATA_CR_VARI

   !  the model photon rate for each data channel (not DRM photon bin):
   !  photons/s-cm^2-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0:NUM_DET-1), intent (out) :: PHOT_MODEL_BYCHAN

   !   ### ### ### Output Labeling:

   !  the version of this subroutine and its subroutines:
   CHARACTER (kind=C_CHAR, len=1), dimension (VERSION_LEN + 1), intent (out) :: MFIT_VERSION_ARG

   !                      *** end of arguments ***


   !  Internal variables:

   !    ### ### ### ### ### ### ###  Workspace Arrays  ### ### ### ### ### ###
   !  Most of these are "automatic" arrays in Fortran 95, that will be automatically
   !  created at run-time -- since their dimensions aren't known at compile time.
   !  Several have constant dimensions known at compile time.

   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1) :: FIRST_NONZERO

   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1) :: NUME_TO_USE
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_CHAN) -1, 0: NUM_DET-1) :: CHAN_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1) :: CHAN_NUME_TO_USE
   real (kind=mfit_real), dimension (7, 0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1) :: DIFF_EVAL
   integer (kind=mfit_integer), dimension (0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1) :: DIFF_NUME_TO_USE
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: NUM_DIFF

   logical (kind=mfit_logical), dimension (0: NUM_DET-1) :: USE_ALL_DET

   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM) :: ALPHA
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM) :: COVAR


   !    ### ### ### ### ### ### ###  Other  ### ### ### ### ### ###

   CHARACTER (len=VERSION_LEN) :: MFIT_VERSION
   character (len=32) :: BitString
   integer (kind=mfit_integer) :: i, ibit
   integer (kind=mfit_integer) :: Caller_Error
   logical (kind=mfit_logical) :: flag
   real (kind=mfit_DoubleReal) :: STATISTIC

   integer (kind=mfit_integer) :: ACTION

   integer (kind=mfit_integer) :: JTERM, KPARAM

   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: ZERO
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: NUMEBINS_MINUS
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: LAST_PHOT_BIN

   logical (kind=mfit_logical) :: UNDETERMINE_FLAG
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT) :: PARAM_VARY_UNDETERMINE


   !  lists the portions of MODEL_ENERGY, MODEL_PHOT_RATE & NU_F_NU_MODEL
   !  used and available for plotting.   If not all channels were fit, this
   !  is an extension (extrapolation) of the MODEL_PLOT range:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: FIRST_EXTRAP_PLOT
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: LAST_EXTRAP_PLOT

   !  lists the portions of MODEL_ENERGY, MODEL_PHOT_RATE & NU_F_NU_MODEL
   !  that overlap the data used in the fits:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: FIRST_MODEL_PLOT
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: LAST_MODEL_PLOT


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !    +    +    +    +    +    +    INITIALIZE    +    +    +    +    +    +


   FIT_ERR = 0

   
   !  Set the version:     (for various output made by this prog to read well, string should begin with "MFIT")
   MFIT_VERSION = 'MFIT F95 v1.1 2010 January 3'

   do i=1, VERSION_LEN
      MFIT_VERSION_ARG (i) = MFIT_VERSION (i:i)
   end do
   
   !   MFIT_VERSION_ARG is +1 longer than MFIT_VERSION, so there is always space for the terminating C null character.
   MFIT_VERSION_ARG (len_trim (MFIT_VERSION) + 1) = c_null_char


   if ( DebugMask /= 0 ) then
      call BitMask_to_String  ( DebugMask, BitString )
      write (6, 910) trim(MFIT_VERSION), DebugMask, DebugMask, trim(BitString)
 910  format ( / "Entering ", A, ", with DebugMask =", I0, " = 0x", Z0, " = ", A / )
    end if



   !    +    +    +    +    +    TEST INPUT    +    +    +    +    +    +    +


   !  We check the following variables for reasonableness and compatibility
   !  with the physical (=declared) sizes of the arrays:

   CALL M_TEST_INPUT  ( NUM_DET, NUM_CHAN, RES_FRAC_AT511, RES_EXP, FIT_MODE, FIT_CHAN, CHAN_OFFSET, USE_DET, OBS_CRATE )



   !    +    +    +    +    +    ARGUMENT OUTPUT    +    +    +    +    +    +    +


   !  Dump input arguments when variable DebugMask bits 3 & 4 (basic), 5 & 6 (detector data) or 7 (DRM) are set:

   flag = .false.
   do ibit=3, 7
      if ( btest (DebugMask, ibit) )  flag = .true.
   end do
   IF ( flag ) THEN

      CALL M_ARG_DUMP  ( DebugMask, NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET, CHAN_ENERGY, CHAN_WIDTH,      &
              OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, PHOT_ENERGY, PHOT_WIDTH, DRM, RES_FRAC_AT511, RES_EXP,      &
              MFIT_VERSION, REL_CONVERG, ABS_CONVERG, MAX_TRIES, PARAM_VARY, FIT_CHAN, USE_DET, TERM_USED,             &
              NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, PARAM )

   END IF                                                ! arg dump


   !    +    +    +    +    +    +    FIT SETUP    +    +    +    +    +    +    +    +


   !  Store the model, as input to MFIT, for possible restoration
   !  if we are doing batch fitting with the fixing of undetermined
   !  parameters.

   ACTION = 1    ! 1 = STORE
   CALL M_BATCH_GUESS ( ACTION, PARAM )

   !  for calculations to be done for all detectors:
   USE_ALL_DET = .TRUE.

   !  for each value of ichan and ldet, find the smallest pbin for which
   !  drm (pbin, ichan, ldet) is nonzero.  this will enable mphots_to_cnts
   !  to save time by skipping over zero elements of the drm:

   CALL M_DRM_NONZERO  ( NUM_DET, NUM_CHAN, NUM_EBINS, DRM, FIRST_NONZERO )


   !  Set up the lists of energies for which the photon model is to be evaluated:

   CALL MPHOT_EVAL_LIST  ( NUM_DET, NUM_CHAN, NUM_EBINS, PHOT_ENERGY, PHOT_WIDTH, CHAN_ENERGY, CHAN_WIDTH,   &
           RES_FRAC_AT511, RES_EXP, PHOT_EVAL, NUME_TO_USE, CHAN_EVAL, CHAN_NUME_TO_USE, DIFF_EVAL, DIFF_NUME_TO_USE, NUM_DIFF )


   !  output the energies used to evaluate the model for the fits, if requested by the user by bit 7 of DebugMask:

   IF ( btest (DebugMask, 8) ) THEN
      CALL MPHOT_EVAL_OUT  ( NUM_DET, NUM_EBINS, RES_FRAC_AT511, RES_EXP, PHOT_EVAL, NUME_TO_USE,   &
              PHOT_ENERGY, PHOT_WIDTH, DebugMask, MFIT_VERSION )
   END IF


   !  Calculate the data variances

   !  note that calculation is done for all detectors: USE_ALL_DET
   CALL MGET_DATA_CR_VARI  ( NUM_DET, NUM_CHAN, CHAN_OFFSET, CHAN_WIDTH, BACK_CSIG, LIVE_TIME, USE_ALL_DET,     &
            FIT_CHAN, OBS_CRATE, DATA_CR_VARI )


   !    +    +    +    +    +    +    FIT    +    +    +    +    +    +    +    +



3000 CONTINUE      ! branch back here to refit the same data



   IF ( btest (DebugMask, 1) ) THEN
      WRITE (6, 3030)
      3030  FORMAT ( / 14X, '******   ******    Starting Fit    ******   ******' / )
   END IF




   !  however we have arrived at a model, obtain the value of NUM_VARY:

   CALL M_MODEL_PROP (NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, PARAM_VARY, NUM_VARY )


   !      .      .      .      .      .      .      .      .      .      .      .
   
   
   !   Do a fit if there are any varying parameters in the model:
   
   if ( NUM_VARY > 0 )  then

   
      CALL M_DO_FIT  ( NUM_VARY, NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH, OBS_CRATE, BACK_CRATE, BACK_CSIG,                 &
              LIVE_TIME, USE_DET, CHAN_OFFSET, FIT_CHAN, PHOT_WIDTH, DRM, FIRST_NONZERO, NUM_TERMS_AVAIL, NUM_ADD_TERMS,     &
              NT_ADD_LINE, PARAM_VARY, REL_CHANGE_LIMIT, ABS_CHANGE_LIMIT, NUM_PARAM_OF_TERM, REL_CONVERG, ABS_CONVERG,      &
              MAX_TRIES, TERM_USED, PHOT_EVAL, NUME_TO_USE, DATA_CR_VARI, DebugMask, fit_mode, PARAM,                        &
              PARAM_UNCER, ALPHA, COVAR, STATISTIC, FIT_ERR )
   
   
      IF ( btest (DebugMask, 1) ) THEN
         if ( fit_err == 0 ) then
            WRITE (6, 3110) STATISTIC
      3110  FORMAT ( ' Fit converged to statistic=', 1PG12.5, '.   Error-free fit.' )
         else
            write (6, 3115) STATISTIC, FIT_ERR
      3115  FORMAT ( ' MFIT returning with statistic=', 1PG12.5, '.   Fit had ERROR', I10, '.' )
         end if
      END IF
   
   
   
      UNDETERMINE_FLAG = .FALSE.
   
      IF ( ENABLE_UNDETERMINE ) THEN
   
         CALL M_UNDETERMINED  ( DebugMask, NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, PARAM, PARAM_UNCER, PARAM_VARY,    &
                    UNDETERMINE_PRIORITY, UNDETERMINE_REL_REQ, UNDETERMINE_ABS_REQ, HIGHEST_UD_PRIORITY,          &
                    UNDETERMINE_FLAG, PARAM_VARY_UNDETERMINE )
   
         IF ( UNDETERMINE_FLAG ) THEN
   
            DO JTERM=1, NUM_TERMS_AVAIL
               DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)
                  PARAM_VARY (JTERM, KPARAM) = PARAM_VARY_UNDETERMINE (JTERM, KPARAM)
               END DO
            END DO
   
            ! redetermine NUM_VARY since we have fixed a parameter:
   
            CALL M_MODEL_PROP (NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, PARAM_VARY, NUM_VARY)
   
            !  Restore the model parameters back to their initial values,
            !  so that we restart from the correct values, rather than
            !  poor values that resulted from a failed fit.
   
            ACTION = 2    ! 2 = RESTORE
            CALL M_BATCH_GUESS ( ACTION, PARAM )
   
         END IF
   
      END IF      ! ENABLE_UNDETERMINE ?
   
   
      !  If we found a parameter to be undetermined, we need to repeat the fit, so we branch back to "3000".
      !  The fit is repeated with the parameter that could not be determined fixed.
   
      IF ( UNDETERMINE_FLAG )  GOTO 3000
      
   end if   !  num_vary > 0 ?



   !    +    +    +    +    +    +    PROCESS RESULTS    +    +    +    +    +    +    +    +


   !  Setup variables needed for spectral plots, etc., .....
   !  This is done whether the current values of the model parameters PARAM are the result of
   !  a fit, or there are no varying parameters and MFIT was just called to evaluate a fixed model.
   
   DOF = M_DOF ( NUM_DET, NUM_CHAN, USE_DET, CHAN_OFFSET, FIT_CHAN, LIVE_TIME, NUM_VARY )

   zero = 0
   numebins_minus = num_ebins - 1

   CALL M_USE_PHOTBINS  ( NUM_DET, USE_ALL_DET, NUM_TERMS_AVAIL, NT_ADD_LINE, zero, numebins_minus, maxval (NUM_EBINS),    &
           PHOT_EVAL, TERM_USED, PARAM, FIRST_PHOT_BIN, LAST_PHOT_BIN )

   CALL M_PLOT_SETUP  ( NUM_DET, NUM_CHAN, NUM_EBINS, NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE,              &
           PARAM, CHAN_OFFSET, FIT_CHAN, USE_DET, CHAN_ENERGY, CHAN_WIDTH, FIRST_NONZERO, PHOT_WIDTH, DRM,      &
           OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, TERM_USED, PHOT_EVAL, NUME_TO_USE, CHAN_EVAL,           &
           CHAN_NUME_TO_USE, DIFF_EVAL, DIFF_NUME_TO_USE, NUM_DIFF, FIT_ERR, FIRST_PHOT_BIN, LAST_PHOT_BIN,     &
           MODEL_CNT_RATE, MODEL_CR_VARI, PHOT_OBS_RATE, PHOT_OBS_SIG, PHOT_MODEL_BYCHAN,                       &
           FIRST_MODEL_PLOT, LAST_MODEL_PLOT, FIRST_EXTRAP_PLOT, LAST_EXTRAP_PLOT, MODEL_ENERGY,                &
           MODEL_PHOT_RATE, NU_F_NU_DATA, NU_F_NU_SIG, NU_F_NU_MODEL, Caller_Error )
   if ( FIT_ERR == 0  .and.  Caller_Error /= 0 )  FIT_ERR = Caller_Error + 5000000
           
          

   !  In the case that we didn't do a fit, the above two calls calculated the various output quantitites for
   !  the fixed model input by the user, EXCEPT we are missing the value of the "fitting" statistic,
   !  which is normally calculated by M_DO_FIT, except we skipped that call.
   !  Of course, ALPHA, COVAR and PARAM_UNCER are meaningless in this case.
   !  This case is considered OK and not an error -- the user input no varying parameters, so the fact that
   !  no fit was done is consistent with the input.
   
   if ( NUM_VARY == 0 ) then
      
      ALPHA = 0.0_mfit_DoubleReal
      COVAR = 0.0_mfit_DoubleReal
      PARAM_UNCER = 0.0_mfit_real
      
      if ( fit_mode .eq. 1 )  statistic = M_CALC_CHISQ  ( NUM_DET, NUM_CHAN, USE_DET, CHAN_OFFSET, FIT_CHAN,      &
                                 OBS_CRATE, BACK_CRATE, LIVE_TIME, MODEL_CNT_RATE, MODEL_CR_VARI )

      if ( fit_mode .eq. 2  .or.  fit_mode .eq. 3 )  statistic = M_CALC_LOGLIKE  ( fit_mode, NUM_DET, NUM_CHAN,        &
                        USE_DET, CHAN_OFFSET, FIT_CHAN, OBS_CRATE, BACK_CRATE, LIVE_TIME, CHAN_WIDTH, MODEL_CNT_RATE )
   
   end if    ! num_vary == 0 ?


   !   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


   !  optional debug output of the results:

   CALL M_OUTPUT  ( DebugMask, NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET,                          &
          CHAN_ENERGY, CHAN_WIDTH, OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, RES_FRAC_AT511, RES_EXP,     &
          MFIT_VERSION, FIT_CHAN, USE_DET, MODEL_CNT_RATE, PHOT_OBS_RATE, PHOT_OBS_SIG,                      &
          NU_F_NU_DATA, NU_F_NU_SIG, MODEL_ENERGY, MODEL_PHOT_RATE, FIRST_EXTRAP_PLOT, LAST_EXTRAP_PLOT,     &
          TERM_USED, NUM_TERMS_AVAIL, DOF, NUM_VARY, FIT_ERR, NUM_ADD_TERMS, NUM_PARAM_OF_TERM,              &
          PARAM, PARAM_VARY, PARAM_UNCER, fit_mode, STATISTIC, REL_CONVERG, ABS_CONVERG, MAX_TRIES,          &
          MODEL_CR_VARI, DATA_CR_VARI, PHOT_MODEL_BYCHAN, ALPHA, COVAR )
          


   !  "type-convert" several variables that are double-precision internally, but single-precision
   !  in the argument list:

   STATISTIC_ARG = real ( STATISTIC, mfit_real )

   !  Also zero the unused elements of ALPHA and COVAR (i.e., elements past the logical dimensions) 
   !  because the IDL code identifies the (used) size of the array by looking for the  portion.
   
   ALPHA_ARG = 0.0_mfit_real
   COVAR_ARG = 0.0_mfit_real

   ALPHA_ARG (1:NUM_VARY, 1:NUM_VARY) = real ( ALPHA (1:NUM_VARY, 1:NUM_VARY), mfit_real )
   COVAR_ARG (1:NUM_VARY, 1:NUM_VARY) = real ( COVAR (1:NUM_VARY, 1:NUM_VARY), mfit_real )



   RETURN                                            ! *** *** *** *** ***

   END SUBROUTINE MFIT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   !  Alternative entry (i.e., externally callable), to evaluate the photon model at arbitrary
   !  energies provided as input.   This work is done by  M_PHOTONS_MODEL -- this routine provides
   !  a simpler interface.  One of the complexities of M_PHOTONS_MODEL is that, for a "photon bin"
   !  (e.g., one energy of a DRM), the photon model can be averaged over 1, 3 or 7 evaluations.
   !  More than one evaluation is used for wide bins.  This routine interfaces to M_PHOTONS_MODEL,
   !  requesting a single evaluation for each energy of "Energies" -- it is up to the user to
   !  input energies that are closely enough spaced that the results are useful.


   subroutine mfit_FinePhotModel  ( NumEnergies, Energies, PARAM, TERM_USED, NUM_TERMS_AVAIL,     &
                  NUM_ADD_TERMS, PhotonFlux, ErrNum )  BIND ( C, name="mfit_FinePhotModel" )
   
   use iso_c_binding
   
   use MFIT_parameters
   
   
   integer (kind=mfit_integer), parameter :: NUM_DET = 1
   
   
   !  Input arguments:

   integer (kind=mfit_integer), intent (in) :: NumEnergies
   
   real (kind=mfit_real), dimension (0: NumEnergies -1), intent (in) :: Energies
   
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM
   
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   
   
   !  Output arguments:
   
   real (kind=mfit_real) , dimension (0: NumEnergies -1), intent (out) :: PhotonFlux
   
   integer (kind=mfit_integer), intent (out) :: ErrNum 
   
   
   !  Internal variables:
   
   !  energies in keV, 7 per photon bin:
   real (kind=mfit_real), dimension (7, 0: NumEnergies -1, 0: NUM_DET-1) :: PHOT_EVAL
   
   !  number of the energies to use:
   integer (kind=mfit_integer), dimension (0: NumEnergies -1, 0: NUM_DET-1) :: NUME_TO_USE
   
   !  bin range to calculate photon flux for:
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1) :: LAST_PHOT_BIN
   
   integer (kind=mfit_integer), parameter :: DO_TERM = 0      ! if zero, calculate all terms and their sum,
                                        ! if non-zero just do specified term
                                        
   logical (kind=mfit_logical), dimension (0: num_det-1) :: USE_DET
   
   
   real (kind=mfit_real), dimension (0: NumEnergies -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_PHOT_RATE
                               ! the calculated results for each chan & term.
                               ! Term 0 is the sum of the separate terms.
                               ! units: photons/s-cm2-keV

   !  flags whether any multiplicative terms were calculated:
   logical (kind=mfit_logical) :: MULT_FLAG

   !  the product of all of the multiplicative terms:
   real (kind=mfit_real), dimension (0: NumEnergies -1, 0: num_det-1) :: MULT_TERMS
   

   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   PHOT_EVAL ( 1, 0: NumEnergies -1, 0 ) = Energies
   NUME_TO_USE = 1
   FIRST_PHOT_BIN = 0
   LAST_PHOT_BIN = NumEnergies - 1
   USE_DET (0) = .TRUE.
   
   call M_PHOTONS_MODEL  ( NUM_DET, NumEnergies, PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,   &
                PARAM, TERM_USED, DO_TERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,                       &
                MODEL_PHOT_RATE, MULT_FLAG, MULT_TERMS, ErrNum )
   
   PhotonFlux = MODEL_PHOT_RATE ( 0: NumEnergies -1, 0, 0 )

   
   end subroutine mfit_FinePhotModel
   

! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   !  This subroutines converts a bit mask into a string of blanks, zeros and ones.
   !  Set (true) bits become "1"s in the string.   Unset bits that are less significant
   !  than the most signficant set bit become "0"s in the string.   Unset bits that
   !  are more significant than the most significant set-bit are rendered as blanks.
   !  Finally the string is left-justified, so actually the blanks end up on the right.
   !  M. S. Briggs, 2009 April 9.


   subroutine BitMask_to_String  ( BitMask, BitString )

   !  input argument:
   integer (kind=mfit_integer), intent (in) :: BitMask

   !  output argument:
   character (len=32), intent (out) :: BitString

   ! internal variables:
   integer (kind=mfit_integer) :: char_pos, bit_num
   character (len=1) :: Use_for_Zero


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  We assume that the BitMask has 32 bits.
   !  The bits are numbered from 0 to 31.  (From right to left -- btest takes care of this.)
   !  We'd like the characters of BitString to be numbered 31 to 0, right to left,
   !  which means from 31 as the first on the left, to 0 as the last on the right.
   !  But unlike array indices, there is no way to designate the range of character
   !  positions -- they must start with 1 and increase (in this case to 32).
   !  So to obtain the desired matching of bit and character positions, we
   !  maintain two indices.
   !  Bit are processed starting with most significant so that unset bits before the
   !  most-signficant set bit can be rendered as blanks rather than as "0".

   char_pos = 1
   Use_for_Zero = " "

   do bit_num=31, 0, -1

      if ( btest (BitMask, bit_num) ) then
         BitString (char_pos:char_pos) = "1"
         Use_for_Zero = "0"
      else
         BitString (char_pos:char_pos) = Use_for_Zero
      end if

      char_pos = char_pos + 1

   end do

   BitString = adjustl (BitString)


   ! special case: no set bits -- above code would return a blank string:
   if ( BitMask == 0 )  BitString = "0"


   return

   end subroutine BitMask_to_String


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_ARG_DUMP  ( DebugMask, NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET,                            &
                CHAN_ENERGY, CHAN_WIDTH, OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, PHOT_ENERGY, PHOT_WIDTH, DRM,    &
                RES_FRAC_AT511, RES_EXP, MFIT_VERSION, REL_CONVERG, ABS_CONVERG, MAX_TRIES, PARAM_VARY,                &
                FIT_CHAN, USE_DET, TERM_USED, NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, PARAM )


   !  This subroutine writes to the screen and to unit 30 the values
   !  of (almost?) all the input arguments of MFIT.  This is for debug purposes,
   !  to check that a program that calls MFIT_SUPER or MFIT is passing the
   !  arguments correctly.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-62, 21 September 1992.


   use MFIT_parameters


   !  Arguments, all input:

   integer (kind=mfit_integer), intent (in) :: DebugMask
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_ENERGY
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_ENERGY
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_FRAC_AT511
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_EXP
   CHARACTER (len=VERSION_LEN), intent (in) :: MFIT_VERSION

   real (kind=mfit_real), intent (in) :: REL_CONVERG
   real (kind=mfit_real), intent (in) :: ABS_CONVERG
   integer (kind=mfit_integer), intent (in) :: MAX_TRIES

   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM


   !  Internal variables:
   integer (kind=mfit_integer) :: LAST_INROW_NONZERO
   integer (kind=mfit_integer) :: ICHAN, JTERM, KPARAM, LDET, PBIN
   CHARACTER (len=4) :: VARY_STATUS
   CHARACTER (len=8) :: USE_STATUS
   real (kind=mfit_real) :: RIGHT_EDGE, LEFT_EDGE
   integer (kind=mfit_integer) :: ipass, nunit
   logical :: UnitOpen


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   do ipass=1, 2

      nunit=6
      if ( ipass == 2 ) then
         nunit = MODULE_PARAM_FILE_UNIT
         if ( btest (DebugMask, 4) .or. btest (DebugMask, 6) .or. btest (DebugMask, 7) )     &
            call Open_DebugOutput_File ( DebugMask, MFIT_VERSION )
      end if


      if ( ( ipass == 1 .and. btest (DebugMask, 3) )  .or.     &
           ( ipass == 2 .and. btest (DebugMask, 4) ) ) then

         WRITE (nunit, 1010)  trim (MFIT_VERSION)
          1010  FORMAT ( / ' <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>' //     &
                           ' ARGUMENT DUMP OF MFIT''s INPUT ARGUMENTS.' /            &
                           ' ARGUMENT DUMP PRODUCED BY SUBROUTINE M_ARG_DUMP OF ', A // )

         WRITE (nunit, 1025) NUM_DET, maxval (NUM_EBINS) , maxval (NUM_CHAN) , num_det
         1025  FORMAT ( ' Fit is requested for', I8, ' detectors.' /                   &
                        ' DRM dimensions: ebins X chan X detectors =', I8, I8, I8 //   &
                        ' Resolution parameters: fraction res at 511 keV,',            &
                        ' exponent to calc for other E' )

         DO LDET=0, NUM_DET - 1
            WRITE (nunit, 1022) LDET, RES_FRAC_AT511 (LDET), RES_EXP (LDET)
            1022  FORMAT ( 12X, I8, 12X, F7.4, 14X, F7.4 )
         END DO

      end if   ! basic arg output #1 ?



      !  Detector Data:

      if ( ( ipass == 1 .and. btest (DebugMask, 5) )  .or.     &
           ( ipass == 2 .and. btest (DebugMask, 6) ) ) then

         DO LDET=0, NUM_DET - 1

            WRITE (nunit, 1030) LDET
            1030  FORMAT ( /' *** Data for the', I3, 'th detector' / )

            WRITE (nunit, 1040) NUM_CHAN (LDET), CHAN_OFFSET (LDET)
            1040  FORMAT ( / ' Detector has', I8, ' channels' /                         &
                             ' The first channel passed to MFIT is numbered', I8 / )


            write (nunit, 1041)
            1041  format ( / 10X,  'chan    center E       width       OBS rate    BACKG. rate     BACKG. sigma      livetime' / )

            DO ICHAN=0, NUM_CHAN (LDET) - 1
               WRITE (nunit, 1042) CHAN_OFFSET (LDET) + ICHAN, CHAN_ENERGY (ICHAN, LDET), CHAN_WIDTH (ICHAN, LDET),      &
                   OBS_CRATE (ICHAN, LDET), BACK_CRATE (ICHAN, LDET), BACK_CSIG (ICHAN, LDET), LIVE_TIME (ICHAN, LDET)
               1042  FORMAT ( 7X, I6, 2 (2X, 1PG13.5), 4(2X, 1PG13.5) )
            END DO

         END DO   ! ldet

      end if   !  detector data output :



      !   DRM output:

      if ( ipass == 2  .and.  btest (DebugMask, 7) ) then

         DO LDET=0, NUM_DET - 1

            WRITE (nunit, 1050) NUM_EBINS (LDET)
            1050  FORMAT ( /// ' Detector has', I8, ' photon energy bins.' )

            WRITE (nunit, 1060) LDET
            1060  FORMAT ( /' The DRM for the', I3, 'th detector: ' /)

            DO PBIN=0, NUM_EBINS (LDET) - 1

               ! Count up the trailing zeros in this row of the DRM:
               LAST_INROW_NONZERO = NUM_CHAN (LDET) - 1
               DO WHILE (DRM (PBIN, LAST_INROW_NONZERO, LDET) .EQ. 0.0 .AND. LAST_INROW_NONZERO .GT. 0)
                  LAST_INROW_NONZERO = LAST_INROW_NONZERO - 1
               END DO
               ! special case to avoid illegal subscript above: row all zero:
               IF (DRM (PBIN, LAST_INROW_NONZERO, LDET) .EQ. 0.0) LAST_INROW_NONZERO = LAST_INROW_NONZERO - 1

               LEFT_EDGE = PHOT_ENERGY (PBIN, LDET) - 0.5 * PHOT_WIDTH (PBIN, LDET)
               RIGHT_EDGE = PHOT_ENERGY (PBIN, LDET) + 0.5 * PHOT_WIDTH (PBIN, LDET)

               WRITE (nunit, 1070) PBIN, LEFT_EDGE, RIGHT_EDGE, PHOT_ENERGY (PBIN, LDET), PHOT_WIDTH (PBIN, LDET)
               1070  FORMAT ( / ' Photon Energy bin', I4, ':' /              &
                                ' range:', 1PG11.4, ' to', 1PG11.4, '   center:', 1PG11.4, ' width:', 1PG11.4, ':' )

               IF (LAST_INROW_NONZERO .NE. NUM_CHAN (LDET) - 1) THEN
                  WRITE (nunit, 1080) NUM_CHAN (LDET) - 1 - LAST_INROW_NONZERO
                  1080  FORMAT ( ' trailing zeros suppressed from this row:', I5 )
               END IF

               WRITE (nunit, 1075) (DRM (PBIN, ICHAN, LDET), ICHAN=0, LAST_INROW_NONZERO)
               1075  FORMAT (5(1X, 1PG13.5))

            END DO        ! pbin

         END DO      ! ldet

      end if    ! output DRM ?



      !   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


      !  Yet more basic arguments....

      if ( ( ipass == 1 .and. btest (DebugMask, 3) )  .or.     &
           ( ipass == 2 .and. btest (DebugMask, 4) ) ) then

         WRITE (nunit, 7010) REL_CONVERG, ABS_CONVERG, MAX_TRIES
         7010 FORMAT ( // ' Convergence requirements, relative and absolute:', 2 (1X, 1PG11.3) /      &
                          ' Iterations without convergence limited to:', I5 / )


         WRITE (nunit, 7020) NUM_TERMS_AVAIL
         7020 FORMAT ( // 1X, I4, ' model terms are available.' //     &
                                  ' The terms active in the current model, and their parameters, are:' )

         DO JTERM=1, MIN (NUM_TERMS_AVAIL, MAX_TERMS)
            IF (TERM_USED (JTERM) .EQ. 1) THEN
               WRITE (nunit, 7030) JTERM
               7030  FORMAT ( / 1X, 'Term number: ', I5, 2X, ':' )
               DO KPARAM=1, MIN (NUM_PARAM_OF_TERM (JTERM), MAX_PPT)
                  VARY_STATUS = 'FIX '
                  IF (PARAM_VARY (JTERM, KPARAM)) VARY_STATUS = 'VARY'
                  WRITE (nunit, 7040) KPARAM, VARY_STATUS, PARAM (JTERM, KPARAM)
                  7040  FORMAT ( 8X, I4, 2X, A4, 2X, 1PG12.4 )
               END DO
            END IF
         END DO


         WRITE (nunit, 7050)
         7050 FORMAT ( // 'Data to be included in the fit:' / 'Detector       Used?      Fit Channels' )
         DO LDET=0, NUM_DET-1
            USE_STATUS = 'EXcluded'
            IF (USE_DET (LDET)) USE_STATUS = 'Included'
            WRITE (nunit, 7060) LDET, USE_STATUS, FIT_CHAN (0, LDET), FIT_CHAN (1, LDET)
            7060  FORMAT ( 1X, I3, 8X, A10, 4X, I5, 2X, I5 )
         END DO

         !   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


         WRITE (nunit, 4010)
         4010  FORMAT ( / ' ** HERE FOLLOW THE ARRAY SIZE PARAMETERS, WHICH ARE NOT ARGUMENTS ' /    &
                          ' ** THESE VALUES WERE DEFINED WHEN THIS SUBROUTINE WAS COMPILED.'/        &
                          ' ** ALL ROUTINES MUST USE THE SAME VALUES !' / )

         !   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

         WRITE (nunit, 2010) MAX_TERMS, MAX_PPT, MAX_PARAM
         2010  FORMAT ( / ' MAX_TERMS, MAX_PPT, MAX_PARAM =', I8, I8, I8 /                    &
                          ' END OF ARGUMENT DUMP !' //                                            &
                          ' <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>  <*>' // )


      end if   ! basic arg output #2 ?


#ifndef __G95__
      inquire ( unit=nunit, opened=UnitOpen )
      if ( UnitOpen )  flush (nunit)
#endif


   end do    !  pass: unit 6 or file


   RETURN

   END SUBROUTINE M_ARG_DUMP


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_BATCH_GUESS ( ACTION, PARAM )


   !  1) This subroutine stores previously generated guesses when
   !  ACTION = 1 (for "STORE").  These parameter values will be the defaults for
   !  later restoration.
   !  2) When ACTION = 2 (for "RESTORE") it generates model guesses by recalling
   !  the previously stored "default" guesses.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, 26 Feb 1993,   UAH/MSFC ES-66.
   !  rev. by MSB 2008 Nov 18 & 2008 Dec 4 to use ACTION to identify when to store
   !  or restore the current model.  Now MFIT has the responsibilty for determining
   !  when the current model should be stored or replaced via restore.


   use MFIT_parameters


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: ACTION    ! 1 = STORE guesses, 2 = RESTORE guesses


   !  Input/Output arguments:
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (inout) :: PARAM


   !  Internal variables:
   integer (kind=mfit_integer) :: JTERM, KPARAM


   !  The whole point of this subroutine is to store certain information
   !  under certain conditions and to return that information under other
   !  conditions -- thus these variables must be designated "SAVE".

   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), SAVE :: STORE_PARAM

   logical (kind=mfit_logical), SAVE :: STORE_DONE = .FALSE.


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  Before calling this subroutine, model guesses must have already been
   !  input.

   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

   !  Do we need to store?

   IF ( ACTION .EQ. 1 ) THEN         ! store

   !     Store current model guess info:

      STORE_DONE = .TRUE.

      DO KPARAM=1, MAX_PPT
         DO JTERM=1, MAX_TERMS
            STORE_PARAM (JTERM, KPARAM) = PARAM (JTERM, KPARAM)
         END DO
      END DO

   END IF                                ! need to store?


   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +


   !  Restore as "guesses" the previously stored guesses:

   IF ( ACTION .EQ. 2 ) THEN         ! restore
      IF (.NOT. STORE_DONE) WRITE (6, 1020)
1020  FORMAT ( // ' **** M_BATCH_GUESS ERROR: NO MODEL HAS BEEN STORED !')

      DO KPARAM=1, MAX_PPT
         DO JTERM=1, MAX_TERMS
            PARAM (JTERM, KPARAM) = STORE_PARAM (JTERM, KPARAM)
         END DO
      END DO
   END IF                                ! need to restore?


   RETURN

   END SUBROUTINE M_BATCH_GUESS


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^



   FUNCTION M_CALC_CHISQ  ( NUM_DET, NUM_CHAN, USE_DET, CHAN_OFFSET, FIT_CHAN, OBS_CRATE, BACK_CRATE,     &
               LIVE_TIME, MODEL_CNT_RATE, MODEL_CR_VARI )


   use MFIT_parameters


   real (kind=mfit_DoubleReal) :: M_CALC_CHISQ


   !   This function calculates chisq given the count rate data OBS_CRATE and
   !   BACK_CRATE, the model count rate, MODEL_CNT_RATE, and the model
   !   count rate variance, MODEL_CR_VARI.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !   Michael S. Briggs, UAH / MSFC ES-62, 30 July 1993.


   !   Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1), intent (in) :: MODEL_CNT_RATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: MODEL_CR_VARI


   !   Output: solely via function return.


   !   Internal variables:
   integer (kind=mfit_integer) :: ICHAN, LDET
   real (kind=mfit_DoubleReal) :: DIFF


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !   Compare the model count rates to the data to obtain chisq:

   !   Data is net count rate of total observed minus background predicted:
   !   OBS_CRATE - BACK_CRATE.
   !   Model count rate is MODEL_CNT_RATE.
   !   Model variance is used: MODEL_CR_VARI (this previously calculated
   !   variance includes the variance predicted from the total model count rate,
   !   source model counts + background counts, and the variance from the
   !   uncertainty of the background count rate, BACK_CSIG.

   !   Chisq is based only upon channels in the fit:
   !   1) selected detectors, as specified by USE_DET,
   !   2) selected channels for fitting, as specified by FIT_CHAN,
   !   3) channels with useable data, specified by LIVE_TIME > 0.

   M_CALC_CHISQ = 0.0

   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN
         DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
            IF (LIVE_TIME (ICHAN, LDET) .GT. 0.0) THEN
               DIFF = OBS_CRATE (ICHAN, LDET) - BACK_CRATE (ICHAN, LDET) - MODEL_CNT_RATE (ICHAN, 0, LDET)
               M_CALC_CHISQ = M_CALC_CHISQ + DIFF**2 / MODEL_CR_VARI (ICHAN, LDET)
            END IF
         END DO
      END IF
   END DO


   RETURN

   END FUNCTION M_CALC_CHISQ


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_CALC_LOGLIKE ( fit_mode, NUM_DET, NUM_CHAN, USE_DET, CHAN_OFFSET, FIT_CHAN,      &
               OBS_CRATE, BACK_CRATE, LIVE_TIME, CHAN_WIDTH, MODEL_CNT_RATE )


   use MFIT_parameters

   use GSL_interfaces


   real (kind=mfit_DoubleReal) :: M_CALC_LOGLIKE

   !   This routine calculates either (-2 log likelihood) or a related statistic, the
   !   [Castor] C-Statistic.   The C-Statistic is used by XSPEC -- it differs from
   !   -2 log likelihood by a value that is a constant for a particular dataset.
   !   As a constant, the difference between (-2 log likelihood) and the C-Statistic
   !   has no effect on the fit results since the derivatives of a constant with
   !   respect to the parameters of the photon model are zero.
   
   !   Implicity likelihood means likelihood for the Possion probability distribution.
   !   Therefore the input data must be raw counts, not background subtracted.
   !   Negative observed counts are impossible and will be illegal input to gsl_sf_lnfact
   !   (log of factorial function) -- the calling routine must not do this.
   !   This routine will never receive negative observed rates because this is one of
   !   the input validations of M_TEST_INPUT.
   
   !   Also, a Poisson process must have a non-negative rate -- below log (model_cnts)
   !   will cause difficulties if the total model prediction, background plus photon,
   !   is <= 0.
   
   !   Revised 2009 May 6 & 9 by MSB:
   !   Update the Cash statistic to be the Castor C-statistic, which is used by XSPEC
   !   under the name of C-statistic (sometimes erroneously the name Cash C statistic 
   !   is used in XSPEC documents.)    See 
   !   https://astrophysics.gsfc.nasa.gov/XSPECwiki/statistical_methods_in_XSPEC and
   !   http://heasarc.nasa.gov/lheasoft/xanadu/xspec/manual/XSappendixCash.html.
   !   Like the Cash statistic, the C-statistic is offset from (Possion) likelihood
   !   by a term that is constant for a particular dataset.

   !   Revised 2008 Oct 31:
   !   Now also calculates the Cash C Statistic when requested via fit_mode = 3.
   !   The difference between -2 log likelihood and the Cash C statistic is that
   !   the later leaves off a term that is constant for a given dataset.
   !   Since the term that is omitted for the C statistic has no dependence on
   !   the photon model, it makes no contribution to the derivatives w.r.t.
   !   the parameters of the photon model and therefore elsewhere in the
   !   code fit_mode = 2 (i.e., -2 likelihood) and fit_mode = 3 (i.e., Cash
   !   C statistic) are handled identically.

   !   This function calculates the -2 log likelihood for Poisson fluctuations
   !   in "source+background" interval assuming that there are no statistical
   !   fluctuations in the background.
   !   The observed count rate OBS_CRATE is converted into the integer number
   !   of observed counts OBS_CNTS using NINT to round the real number to the
   !   nearest integer -- this is rounding, including rounding UP, not
   !   truncation.   So there is no need for the trick of adding 0.5 to
   !   obtain rounding.
   !   The model counts MODEL_CNTS are calculated from the sum of the model
   !   counts for the source, MODEL_CNT_RATE, which was calculated by
   !   forward-folding the photon model, and the "exactly known" background
   !   count rate BACK_CRATE.
   !   The equation is easy to derive from the Poisson probability distribution.
   !   The likelihood is the sum of the probabilities of each channel, and we
   !   are doing the log.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !   Michael S. Briggs, UAH / MSFC SD-50, 1999 Oct 14.
   !   Michael S. Briggs, UAH / NSSTC, 2008 October 17: merge into
   !   production code of rmfit.


   !   Input arguments:
   integer (kind=mfit_integer), intent (in) :: fit_mode
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: chan_width
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1), intent (in) :: MODEL_CNT_RATE


   !   Output: solely via function return.


   !   Internal variables:
   integer (kind=mfit_integer) :: ICHAN, LDET
   integer (kind=mfit_integer) :: OBS_CNTS
   real (kind=mfit_real) :: MODEL_CNTS


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   if ( fit_mode .ne. 2 .and. fit_mode .ne. 3) then
      write (6, *) 'm_calc_loglike: invalid value of fit_mode:', fit_mode
      stop
   end if


   m_calc_loglike = 0.0

   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN
         DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
            IF (LIVE_TIME (ICHAN, LDET) .GT. 0.0) THEN

               obs_cnts = nint (obs_crate (ichan, ldet) * live_time (ichan, ldet) * chan_width (ichan, ldet))
               model_cnts = ( model_cnt_rate (ichan, 0, ldet) + back_crate (ichan, ldet) )        &
                       * live_time (ichan, ldet) * chan_width (ichan, ldet)
                       
                       
               !  A Possion rate must be positive.
               !  We have to enforce this requirement since we take the log of model_cnts below.
               !  In effect the model is forced to be positive regardless of parameter values when
               !  the fitting statistic is Likelihood or Castor C-Statistic.   This will make
               !  derivatives of the fitting function w.r.t. parameters zero, since the function
               !  becomes a constant when the parameters make it any negative value.

               if ( model_cnts <= 0.0_mfit_real )  model_cnts = tiny (model_cnts)

               if ( fit_mode .eq. 2 ) then
               
                    !  Likelihood
                  m_calc_loglike = m_calc_loglike + obs_cnts * log (model_cnts) - model_cnts - gsl_sf_lnfact (obs_cnts)
                  
               else
               
                    ! Castor C-statistic:
                  !  At first it might appear that the IF statement is unnecessary because it prevents the
                  !  the case obs_cnts == 0, and the term being added is has obs_cnts as a factor, so it is
                  !  zero anyway -- be we must avoid evaluating log (obs_cnts) when obs_cnts = 0.
                  !  obs_cnts = 0 is of course perfectly legal.   The limit, x->0, x log x = 0,
                  !  so the expressing that we are leaving out is indeed zero -- but the computer
                  !  will produce 0 * NaN = NaN.
                                       
                  m_calc_loglike = m_calc_loglike - model_cnts
                  if ( obs_cnts > 0 )  m_calc_loglike = m_calc_loglike +         &
                        obs_cnts * ( 1.0_mfit_real + log (model_cnts) - log ( real(obs_cnts, mfit_real) ) )
                                       
               end if

            END IF
         END DO
      END IF
   END DO

   m_calc_loglike = -2. * m_calc_loglike


   RETURN

   END FUNCTION M_CALC_LOGLIKE


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   subroutine Open_DebugOutput_File  ( DebugMask, MFIT_VERSION )


   use MFIT_parameters


   integer (kind=mfit_integer), intent (in) :: DebugMask
   CHARACTER (len=VERSION_LEN), intent (in) :: MFIT_VERSION


   integer (kind=mfit_integer) :: nunit
   character (len=8) :: date
   character (len=10) :: time
   character (len=13) :: DateString
   character (len=15) :: TimeString
   character (len=32) :: BitString

   logical :: FileAlreadyThere
   logical :: FileAlreadyOpen
   logical :: JustOpended


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   nunit = MODULE_PARAM_FILE_UNIT
   JustOpended = .FALSE.

   call date_and_time ( date, time )
   DateString = date(1:4) // "y " // date(5:6) // "m " // date(7:8) // "d"
   TimeString = time(1:2) // "h " // time(3:4) // "m " // time(5:10) // "s"


   inquire ( file="MFIT_DebugOutput.txt", exist=FileAlreadyThere )

   if ( FileAlreadyThere ) then

      inquire ( file="MFIT_DebugOutput.txt", opened=FileAlreadyOpen )

      if ( FileAlreadyOpen ) then

         !  We are done -- the file is already open !!!!!!
         return  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      else

         !  The file already exists but hasn't been opened on this run of MFIT.
         !  We re-open it in "append" mode so as not to erase the existing contents.
         !  This program only writes to the file, but "readwrite" may be necessary for "append".

         open ( file="MFIT_DebugOutput.txt", unit=nunit, status="old", action="readwrite", position="append" )
         JustOpended = .TRUE.

         write ( nunit, 1010 )  trim (MFIT_VERSION), DateString, TimeString
         1010  format ( // '**** **** "', A, '" reopening this file for **** ****' /        &
                           '**** **** Debug Output at ', A, 1X, A, '  **** ****' )

      end if

   else

      !  The file doesn't exist, so open a new file.

      open ( file="MFIT_DebugOutput.txt", unit=nunit, status="new", action="write" )
      JustOpended = .TRUE.

      write ( nunit, 1020 )  trim (MFIT_VERSION), DateString, TimeString
      1020  format ( // '**** **** "', A, '" creating this file for **** ****' /        &
                        '**** **** Debug Output at ', A, 1X, A, '  **** ****' )
   end if


   if ( JustOpended ) then
      call BitMask_to_String  ( DebugMask, BitString )
      write ( nunit, 910 )  DebugMask, DebugMask, trim(BitString)
 910  format ( / "with DebugMask =", I0, " = 0x", Z0, " = ", A // )
   end if


   return


   end subroutine Open_DebugOutput_File


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_COVAR_OUT         &
       ( DebugMask, NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, TERM_USED, PARAM_VARY, NUM_VARY, ALPHA, COVAR, MFIT_VERSION )


   !   This subroutine output the covariance matrix of the fit.
   !   It checks the covariance matrix for errors, i.e. negative diagonal
   !   terms.   It outputs the covariance matrix NORMALIZED, as explained below.
   !   Actually, the normalized covariance matrix is the matrix of
   !   correlation coefficients.
   !   It also outputs the global correlation coefficents, which, for each
   !   parameter, is the greatest correlation of that parameter with any
   !   linear combination of the other parameters.

   !   Not all model parameters need be varied in a model.   The used
   !   portion of COVAR is the upper-left corner (human rather than FORTRAN
   !   convention).   Hence we may access the used portion of COVAR via
   !   INDEX_1=1, NUM_VARY  and INDEX_2=1, NUM_VARY, while COVAR is dimensioned
   !   (MAX_PARAM, MAX_PARAM).  The model parameters are indexed by term JTERM
   !   and parameter of term KPARAM, while covariance matrix is indexed
   !   differently.    Each row (and each column) of the covariance matrix
   !   corresponds to one of the varying parameters of the model, but it
   !   is not indexed by KPARAM and JTERM, but rather by the count INDEX of
   !   the varying parameters.   The complication occurs of we wish to
   !   figure which term JTERM and parameter of term KPARAM correspond to a
   !   given value of INDEX.  To figure this out, as we loop through JTERM
   !   and KPARAM we increment INDEX whenever we encounter a varying parameter.

   !   Reference for some ideas herein: Eadie, Drijard, James, Roos & Sadoulet,
   !   Statistical Methods in Experimental Physics, Elsevier Science Pub. /
   !   North-Holland Physics Pub., 3rd reprint of 1st ed., 1988.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !   Michael S. Briggs, UAH / MSFC ES-62, 20 October 1992.


   use MFIT_parameters

   use GSL_interfaces


   !   Arguments, all input:
   integer (kind=mfit_integer), intent (in) :: DebugMask
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_VARY
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (in) :: ALPHA
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (in) :: COVAR
   CHARACTER (len=VERSION_LEN), intent (in) :: MFIT_VERSION


   !   Internal Variables:

   integer (kind=mfit_integer) :: ipass
   integer (kind=mfit_integer) :: NUNIT
   integer (kind=mfit_integer) :: JTERM, KPARAM
   integer (kind=mfit_integer) :: INDEX, INDEX_1, INDEX_2
   logical (kind=mfit_logical) :: FLAG

   real (kind=mfit_DoubleReal), dimension (NUM_VARY) :: XFER
   real (kind=mfit_real), dimension (MAX_PARAM) :: GLOBAL_CORR
   real (kind=mfit_DoubleReal) :: R_PROD
   logical :: UnitOpen


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   do ipass=1, 2

      NUNIT = 6

      if ( ipass == 2  .and.  btest (DebugMask, 19) ) then
         NUNIT = MODULE_PARAM_FILE_UNIT
         call Open_DebugOutput_File  ( DebugMask, MFIT_VERSION )
      end if


      if ( (ipass .eq. 1 .and. btest (DebugMask, 12))  .OR.  (ipass .eq. 2 .and. btest (DebugMask, 19)) ) then


         !  For all parameters varied in the fit, the diagonal element should
         !  be positive.  As error check, test that this is true.

         FLAG = .TRUE.
         INDEX = 0
         DO JTERM=1, NUM_TERMS_AVAIL
            IF (TERM_USED (JTERM) .EQ. 1) THEN
               DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)
                  IF (PARAM_VARY (JTERM, KPARAM)) THEN
                     INDEX = INDEX + 1
                     IF (COVAR (INDEX, INDEX) .LE. 0.0) THEN
                        FLAG = .FALSE.
                        WRITE (NUNIT, 7710) KPARAM, JTERM, COVAR (INDEX, INDEX)
                  7710  FORMAT ( // '### ERROR: Diagonal Covariance Matrix element', ' for', I3, 'th parameter of' /    &
                                   1X, I3, 'th term', ' is NOT positive:', 1PG13.5)
                     END IF
                  END IF
               END DO                                                      ! kparam
            END IF
         END DO                                                            ! jterm


         !   Output normed covariance matrix = matrix of correlation coefficients
         !   (Eadie et al, p. 22) The normalization is to divide covar (i, j) by
         !   sqrt (covar (i, i) * covar (j, j)).  That is to say, the co-variances
         !   (off-diagonal elements of covariance matrix) are divided by the sqrt of
         !   the variances (sqrt of diagonal elements) to create the correlation
         !   coefficients.   This process makes the diagonal elements identically
         !   one while the correlation coefficients (off-diagonal terms) will be
         !   between -1 and 1.

         IF (FLAG) THEN
            WRITE (NUNIT, 9040) (INDEX, INDEX=1, NUM_VARY)
      9040  FORMAT ( / '  The Normed Covariance matrix = Correlation Coefficient matrix =' // ( 8X, 10(3X, I2, 2X) ) )

            DO INDEX_1=1, NUM_VARY
               DO INDEX_2=1, NUM_VARY
                  XFER (INDEX_2) = COVAR (INDEX_1, INDEX_2) /                     &
                                  ( SQRT (abs ( COVAR (INDEX_1, INDEX_1) )) *     &
                                    SQRT (abs ( COVAR (INDEX_2, INDEX_2) )) )
               END DO
               WRITE (NUNIT, 9050) INDEX_1, (XFER (INDEX_2), INDEX_2=1, MIN (10, NUM_VARY))
         9050  FORMAT ( ' n    ', I2, 10F7.3 )
               IF (NUM_VARY .GT. 10)  WRITE (NUNIT, 9055) (XFER (INDEX_2), INDEX_2=11, NUM_VARY)
         9055  FORMAT ( ' N      ', 10F7.3 / )
            END DO
         END IF


         !  Now, for each varying parameter, calculate and output the global
         !  correlation coefficient (Eadie et al., p. 23).
         !  The global correlation coefficent for a parameter is the
         !  the max (over all linear combinations) of the correlation coefficent
         !  of the parameter with a linear combination of the other parameters.
         !  It ranges from 0 to 1, which would indicate complete correlation
         !  with some linear combination of the other parameters.  Eadie et al.
         !  give a simple way to calculate it from the diagonal elements of
         !  the covariance matrix and the diagonal elements of its inverse.
         !  This is simple, because COVAR was created as the inverse of ALPHA,
         !  so we don't even have to invert COVAR.

         IF (NUM_VARY .GT. 2) THEN

            !  calculate global correlation coeff and guard against illegal values:
            DO INDEX=1, NUM_VARY
               R_PROD = 1. / ( COVAR (INDEX, INDEX) * ALPHA (INDEX, INDEX) )
               IF (R_PROD .LE. 1.0) THEN
                  GLOBAL_CORR (INDEX) = real ( SQRT (1. - R_PROD), mfit_real )
               ELSE
                  GLOBAL_CORR (INDEX) = 9.99
               END IF
            END DO

            !  output:
            WRITE (NUNIT, 1040)  (INDEX, INDEX=1, NUM_VARY)
            1040  FORMAT ( / 'The global correlation coefficients of the varying parameters are:'  //  ( 7X, 10(3X, I2, 2X) ) )
            WRITE (NUNIT, 1050)  (GLOBAL_CORR (INDEX), INDEX=1, NUM_VARY)
            1050  FORMAT (' g     ', 10F7.3)

         END IF


      end if   !  pass vs DebugMask


#ifndef __G95__
      inquire ( unit=nunit, opened=UnitOpen )
      if ( UnitOpen )  flush (nunit)
#endif


   end do    ! pass


   RETURN

   END SUBROUTINE M_COVAR_OUT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_DO_FIT  ( NUM_VARY, NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH,                                            &
          OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, USE_DET, CHAN_OFFSET, FIT_CHAN, PHOT_WIDTH, DRM,                  &
          FIRST_NONZERO, NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE, PARAM_VARY, REL_CHANGE_LIMIT, ABS_CHANGE_LIMIT,    &
          NUM_PARAM_OF_TERM, REL_CONVERG, ABS_CONVERG, MAX_TRIES, TERM_USED, PHOT_EVAL, NUME_TO_USE, DATA_CR_VARI,       &
          DebugMask, fit_mode, PARAM, PARAM_UNCER, ALPHA, COVAR, STATISTIC, FIT_ERR )

   !  This subroutine does a fit, either linear or nonlinear.
   !  It is called by MFIT, MCHI_1D and MCHI_2D.

   !  The best parameter values are always obtained via M_MRQMIN.   Even
   !  if the model is linear in its use of its parameters, the problem is
   !  still nonlinear because of the use of model variances.   Consequently
   !  M_LFIT is only used to obtain a quick and good but imperfect solution
   !  for the linear parameters.    Using M_LFIT makes it easy for the user
   !  who need not enter any guesses for linear parameters.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-64, 9 January 1993.
   
   !  Change by MSB on 2009 May 3: this routine is no longer to be called for NUM_VARY = 0.
   !  It now only does fits; it is not to be called to evaluate a fixed model.


   use MFIT_parameters
   
   use GSL_interfaces


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: FIRST_NONZERO
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   integer (kind=mfit_integer), intent (in) :: NT_ADD_LINE
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: REL_CHANGE_LIMIT
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: ABS_CHANGE_LIMIT
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   !  convergence criteria for nonlinear fits:
   real (kind=mfit_real), intent (in) :: REL_CONVERG          ! relative convergence test
   real (kind=mfit_real), intent (in) :: ABS_CONVERG          ! absolute convergence test
   integer (kind=mfit_integer), intent (in) :: MAX_TRIES         ! # of iterations to try
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DATA_CR_VARI
   integer (kind=mfit_integer), intent (in) :: DebugMask
   integer (kind=mfit_integer), intent (in) :: fit_mode


   !  Input/Output arguments:
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (inout) :: PARAM


   !  Output arguments:
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (out) :: PARAM_UNCER
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (out) :: ALPHA
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (out) :: COVAR
   real (kind=mfit_DoubleReal), intent (out) :: STATISTIC


   !  fit_err = 0 for no error, =1 for mild non-convergence,
   !  >= 10 for serious err:
   !  Note that fit_err = 2, negative photon model, and fit_err = 9, negative
   !  count rate model, are detected latter by subroutine M_PLOT_SETUP.
   integer (kind=mfit_integer), intent (out) :: FIT_ERR


   !  Internal Variables:

   integer (kind=mfit_integer) :: INDEX
   integer (kind=mfit_integer) :: Caller_Error
   real (kind=mfit_DoubleReal) :: ALAMDA
   real (kind=mfit_DoubleReal) :: PREV_ALAMDA
   integer (kind=mfit_integer) :: ITRY                 ! index to do MRQMIN iterations in groups
   real (kind=mfit_DoubleReal) :: PREV_STATISTIC
   logical (kind=mfit_logical) :: FLAG
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT) :: PARAM_VARY_LINEAR
   integer (kind=mfit_integer) :: NUM_VARY_LINEAR
   integer (kind=mfit_integer) :: JTERM, KPARAM
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: LAST_PHOT_BIN
   CHARACTER (len=60) :: XFER_STRING
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: zero
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: numebins_minus
   integer (kind=mfit_integer) :: status


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !                   **** Initialize ****

   FIT_ERR = 0                    ! no error until one occurs
   STATISTIC = 0.0


   !  erase COVAR:

   COVAR = 0.0_mfit_DoubleReal


   !  erase the parameter uncertainties:

   PARAM_UNCER = 0.0


   IF (NUM_VARY .LE. 0) THEN
      FIT_ERR = 333
      WRITE (6, *) '**** M_DO_FIT ERROR: BAD VALUE OF NUM_VARY', NUM_VARY
      RETURN                                             ! **** **** **** ****
   END IF


   !  calculate bin ranges for which the model needs to be calculated:
   zero = 0
   numebins_minus = num_ebins - 1
   CALL M_USE_PHOTBINS  ( NUM_DET, USE_DET, NUM_TERMS_AVAIL, NT_ADD_LINE, zero, numebins_minus,    &
           maxval (NUM_EBINS), PHOT_EVAL, TERM_USED, PARAM, FIRST_PHOT_BIN, LAST_PHOT_BIN )



   !              **** DO FITS  ! ****


   !   write title line for iteration messages and possibly parameter guesses:
   IF ( btest (DebugMask, 1) ) THEN
      WRITE (6, 5010)
      5010  FORMAT ( // '     Statistic    log10 (lamda)' )
      XFER_STRING = M_PVALS (PARAM, PARAM_VARY)
      WRITE (6, 5020)  XFER_STRING
      5020  FORMAT ( 11X, 'the guesses: ', A60 )
   END IF


   !  load values into variables used to test for convergence that won't
   !  falsely indicate convergence:

   PREV_STATISTIC = 1.E+38
   PREV_ALAMDA = 1.E-37

   !  if at least one of the proportionality parameters, i.e. a first
   !  parameter of a term, is selected for variation, then do a
   !  preliminary linear fit to determine the best values of the
   !  varying proportionality parameters.   We make the preliminary
   !  model linear by holding all the nonlinear parameters fixed
   !  regardless of what the user requested.

   !  are any linear parameters varying?
   FLAG = .FALSE.
   DO JTERM=1, NUM_ADD_TERMS
      IF (PARAM_VARY (JTERM, 1))  FLAG = .TRUE.
   END DO

   !  if so, make a preliminary linear model, varying only the linear
   !  parameters the user requested to vary, and holding fixed all other
   !  parameters:

   IF (FLAG) THEN

      !  setup what param are to vary in preliminary linear fit and
      !  count of varying parameters in prelim linear fit = count
      !  of varying ADDITIVE terms in full model:

      DO KPARAM=1, MAX_PPT
         DO JTERM=1, NUM_TERMS_AVAIL
            PARAM_VARY_LINEAR (JTERM, KPARAM) = .FALSE.
         END DO
      END DO
      NUM_VARY_LINEAR = 0
      DO JTERM=1, NUM_ADD_TERMS
         PARAM_VARY_LINEAR (JTERM, 1) = PARAM_VARY (JTERM, 1)
         IF (PARAM_VARY (JTERM, 1))  NUM_VARY_LINEAR = NUM_VARY_LINEAR + 1
      END DO

      !  do the preliminary linear fit,

      CALL M_LFIT  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET, CHAN_WIDTH, OBS_CRATE,                     &
            BACK_CRATE, BACK_CSIG, LIVE_TIME, NUM_VARY_LINEAR, PARAM_VARY_LINEAR, NUM_TERMS_AVAIL,         &
            NUM_ADD_TERMS, USE_DET, FIT_CHAN, PHOT_WIDTH, DRM, FIRST_NONZERO, TERM_USED, PHOT_EVAL,        &
            NUME_TO_USE, DebugMask, DATA_CR_VARI, FIRST_PHOT_BIN, LAST_PHOT_BIN, PARAM, PREV_STATISTIC, Caller_Error )

      IF ( Caller_Error /= 0 ) THEN
         FIT_ERR = Caller_Error + 2000000
         WRITE (6, 1510)  Caller_Error
   1510  FORMAT ( // ' ### ERROR: PRELIMINARY LINEAR FIT FAILED:', I6 / )
         PREV_STATISTIC = 1.E+38
         RETURN                                             ! **** **** **** ****
      END IF

   END IF                                        ! prelim linear fit ?




   ALAMDA = -1.                ! flag 1st call to MRQMIN

   !  .     .     .     .     .     .     .     .     .     .     .     .     .

   !  Loop doing calls to MRQMIN until either: 1) convergence of statistic is
   !  achieved, or 2) MAX_TRIES calls have been done w/o achieving
   !  convergence.

   DO ITRY=1, MAX_TRIES

      !  nonlinear fit:
      CALL M_MRQMIN  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET, CHAN_WIDTH, OBS_CRATE,                   &
             BACK_CRATE, BACK_CSIG, LIVE_TIME, PARAM_VARY, NUM_VARY, NUM_TERMS_AVAIL, NUM_ADD_TERMS,       &
             NT_ADD_LINE, NUM_PARAM_OF_TERM, USE_DET, FIT_CHAN, PHOT_WIDTH, DRM, FIRST_NONZERO,            &
             TERM_USED, PHOT_EVAL, NUME_TO_USE, REL_CHANGE_LIMIT, ABS_CHANGE_LIMIT, DATA_CR_VARI,          &
             fit_mode, DebugMask, PARAM, ALAMDA, ALPHA, STATISTIC, Caller_Error )

      IF ( Caller_Error /= 0 ) THEN
         FIT_ERR = Caller_Error + 2100000
         WRITE (6, 5520)  Caller_Error
         5520  FORMAT ( // ' ### ERROR: NONLINEAR FIT FAILED!', I6 )
         RETURN                                             ! **** **** **** ****
      END IF


      !  optionally write out statistic value for this iteration:
      !  even more optionally, write out up to values of up to 6 varying param:

      IF ( btest (DebugMask, 1) ) THEN
         XFER_STRING = M_PVALS (PARAM, PARAM_VARY)
         WRITE (6, 5525)  STATISTIC, LOG10 (ALAMDA), XFER_STRING
         5525  FORMAT ( 1X, 1PG17.9, 1X, 0PF4.0, 1X, A60 )
      END IF


      !  Only test for convergence if no error:
      !  If converged break out of ITRY do-loop:
      !  All convergence tests must be satisfied: they are:
      !  1) relative change of statistic,
      !  2) absolute change of statistic,
      !  3) statistic has not increased--should never happen but check
      !     in case an increase is caused by an error,
      !  4) alamda has not increased--this flags that statistic increased inside of MRQMIN:

      IF ( (PREV_STATISTIC - STATISTIC) / STATISTIC .LE. REL_CONVERG  .AND.          &
           (PREV_STATISTIC - STATISTIC) .LE. ABS_CONVERG  .AND.                      &
               STATISTIC .LE. PREV_STATISTIC  .AND.  ALAMDA .LE. PREV_ALAMDA ) THEN
         GOTO 8888
      END IF

      !  If convergence has not been achieved push STATISTIC values up stack:

      PREV_STATISTIC = STATISTIC
      PREV_ALAMDA = ALAMDA

   END DO                                            ! ITRY loop


   !  If we fall out of ITRY do-loop rather than breaking out to 8888,
   !  then we did not obtain convergence:
   !  Two cases: mild nonconvegence, 1) defined as being within a factor
   !  of 5 of passing both convergence tests, 2) being far from
   !  convergence.  1st case is considered to be a mild error, 2nd a
   !  severe error.
   !  We don't test ALAMDA because M_MRQMIN increases ALMADA for even
   !  extremely small M_MRQMIN-internal increases of STATISTIC:

   IF ( (PREV_STATISTIC - STATISTIC) / STATISTIC .LE. 5. * REL_CONVERG .AND.        &
         PREV_STATISTIC - STATISTIC .LE. 5. * ABS_CONVERG .AND. STATISTIC .LE. PREV_STATISTIC) THEN
      FIT_ERR = 1                              ! mild nonconvergence
      WRITE (6, 8875)  MAX_TRIES
      8875  FORMAT ( / ' ###  M_DO_FIT: Mild error: STATISTIC ALMOST converged in', I3, ' iterations!' )
   ELSE
      FIT_ERR = 10                             ! severe nonconvergence
      WRITE (6, 8870)  MAX_TRIES
      8870  FORMAT ( / ' ###  M_DO_FIT: ERROR: Convergence of STATISTIC not achieved in', I3, ' iterations!' )
      RETURN                                          ! **** **** ****
   END IF


   !     *** *** branch here when convergence has been achieved:

8888  CONTINUE



   !              **** Done with fits ! ****

   !     !     !     !     !     !     !     !     !     !     !     !     !     !


   !  We have converged to a nonlinear fit, invert ALPHA to obtain COVAR:

   status = invert_matrix_with_gsl ( NUM_VARY, MAX_PARAM, ALPHA, COVAR )
   
   if ( status /= 0 ) then

      FIT_ERR = 2200000 + status
      write (6, '( / "Failed to invert alpha to obtain covar: status=", I6 / )' )  STATUS
      
      PARAM_UNCER = 0
      
   else

      !  Unpack diagonal elements of COVAR into PARAM_UNCER.  The square
      !  root of the diagonal elements is, under certain assumptions,
      !  an estimate of the uncertainty in the model parameters.
      !  Do ad hoc fix of negative elements.   Warning of negative elements is
      !  given in M_COVAR_OUT.
   
      INDEX = 0
      DO JTERM=1, NUM_TERMS_AVAIL
         DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)
            IF (PARAM_VARY (JTERM, KPARAM)) THEN
               INDEX = INDEX + 1
               PARAM_UNCER (JTERM, KPARAM) =  real ( SQRT (ABS (COVAR (INDEX, INDEX))), mfit_real )
            END IF
         END DO
      END DO

   end if



   RETURN

   END SUBROUTINE M_DO_FIT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_DOF  ( NUM_DET, NUM_CHAN, USE_DET, CHAN_OFFSET, FIT_CHAN, LIVE_TIME, NUM_VARY )


   !  Evaluates Degrees-of-Freedom (DOF) of current model.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-66, 31 May 1993.


   use MFIT_parameters


   !  Output: solely via function return!
   integer (kind=mfit_integer) :: M_DOF


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: LIVE_TIME
   integer (kind=mfit_integer), intent (in) :: NUM_VARY


   !  Internal variables:
   integer (kind=mfit_integer) :: ICHAN, LDET


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  Figure degrees of freedom of this model.
   !  1) Count up number of data channels used in the fit:
   !     a) selected detectors, as specified by USE_DET,
   !     b) selected channels, as specified by FIT_CHAN,
   !     c) channels with useable data, specified by LIVE_TIME > 0.
   !  2) Subtract number of varying parameters in the model.

   M_DOF = 0
   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN
         DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
            IF (LIVE_TIME (ICHAN, LDET) .GT. 0.0)  M_DOF = M_DOF + 1
         END DO
      END IF
   END DO
   M_DOF = M_DOF - NUM_VARY


   RETURN

   END FUNCTION M_DOF


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_DRM_NONZERO  ( NUM_DET, NUM_CHAN, NUM_EBINS, DRM, FIRST_NONZERO )


   !  For each value of ichan and ldet, this subroutine finds the smallest
   !  pbin for which DRM (pbin, ichan, ldet) is nonzero.  This is the first
   !  nonzero element of the "column" specified by ichan & ldet (it would be a
   !  column without quotation marks if the matrix were two dimensional).
   !  The result is placed into FIRST_NONZERO (ichan, ldet).
   !  It is used to save time in subroutine MPHOTS_TO_CNTS, which uses
   !  FIRST_NONZERO to skip over zero elements of the DRM.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-64, 4 November 1992.


   use MFIT_parameters


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM


   !  Output arguments:
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: FIRST_NONZERO


   !  Internal variables:
   integer (kind=mfit_integer) :: LDET, ICHAN, PBIN


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  Erase:

   FIRST_NONZERO = 0


   !  In each "column" of DRM, find smallest pbin for which DRM is nonzero:

   DO LDET=0, NUM_DET - 1
      DO ICHAN=0, NUM_CHAN (LDET)- 1

         PBIN = 0
         DO WHILE (DRM (PBIN, ICHAN, LDET) .EQ. 0.0 .AND. PBIN .LT. NUM_EBINS (LDET) - 1)
            PBIN = PBIN + 1
         END DO
         FIRST_NONZERO (ICHAN, LDET) = PBIN

         !  handle special case of column containing only zeros:
         IF (DRM (FIRST_NONZERO (ICHAN, LDET), ICHAN, LDET) .EQ. 0.0)       &
            FIRST_NONZERO (ICHAN, LDET) = FIRST_NONZERO (ICHAN, LDET) + 1

      END DO                                              ! ichan
   END DO                                                 ! ldet


   RETURN

   END SUBROUTINE M_DRM_NONZERO


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_LFIT  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET, CHAN_WIDTH, OBS_CRATE,             &
          BACK_CRATE, BACK_CSIG, LIVE_TIME, NUM_VARY, PARAM_VARY, NUM_TERMS_AVAIL, NUM_ADD_TERMS,     &
          USE_DET, FIT_CHAN, PHOT_WIDTH, DRM, FIRST_NONZERO, TERM_USED, PHOT_EVAL, NUME_TO_USE,       &
          DebugMask, DATA_CR_VARI, FIRST_PHOT_BIN, LAST_PHOT_BIN, PARAM, CHISQ, ErrNum )


   !  This routine is no longer primary, that is, even for linear models
   !  we must obtain the best solution by calling M_MRQMIN.    This is because
   !  the use of model variances causes the problem to be nonlinear even if
   !  the model is linear in its use of its parameters.   This routine is still
   !  useful, because it can quickly produce almost best values for the linear
   !  parameters.   The parameter values produced by this subroutine are
   !  excellent guesses to input to M_MRQMIN.

   !  Numerical Recipes subroutine modified by Michael S. Briggs,
   !  UAH / MSFC ES-64, June - October 1992.
   !  Extensively modified for use in MFIT, for example, array PARAM
   !  (which was A originally) is now 2 dimensional, and only the parameters
   !  which are linear are modified by this subroutine.
   !  Also two pass approach, first with data variances, then with model variances.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.


   use MFIT_parameters

   use GSL_interfaces


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_EBINS
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   integer (kind=mfit_integer), intent (in) :: NUM_VARY                                       ! N.R. --> MFIT
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: FIRST_NONZERO
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE
   integer (kind=mfit_integer), intent (in) :: DebugMask
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DATA_CR_VARI
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: LAST_PHOT_BIN


   !  Input / Output argument:
   !  Array is indexed PARAM (JTERM, KPARAM).   For each model term JTERM,
   !  parameter KPARAM=1 is the overall proportionality constant, and hence
   !  the model is linear in all parameters KPARAM=1.   Only
   !  parameters KPARAM = 1 are modified by this subroutine: the others
   !  must be input with "good" values, which are held fixed.
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (inout) :: PARAM           ! N.R. --> A


   !  Output arguments:
   real (kind=mfit_DoubleReal), intent (out) :: CHISQ
   integer (kind=mfit_integer), intent (out) :: ErrNum


   !  Internal variables:

   !  Workspace arrays:
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM) :: COVAR
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM) :: BETA
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM) :: Vector_X
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_CNT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_PHOT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1) :: MODEL_CR_VARI_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1) :: MULT_TERMS

   !  other internal variables:
   logical (kind=mfit_logical) :: ERROR
   logical (kind=mfit_logical) :: MULT_FLAG
   real (kind=mfit_real) :: YDATA
   integer (kind=mfit_integer) :: PBIN
   integer (kind=mfit_integer) :: ICHAN
   integer (kind=mfit_integer) :: JTERM, JTERM_2
   integer (kind=mfit_integer) :: INDEX, INDEX_1, INDEX_2
   integer (kind=mfit_integer) :: KPARAM
   integer (kind=mfit_integer) :: LDET
   real (kind=mfit_real) :: WT
   integer (kind=mfit_integer) :: DO_TERM
   integer (kind=mfit_integer) :: IPASS
   CHARACTER (len=5) :: FIT_TYPE
   CHARACTER (len=60) :: STRING
   integer (kind=mfit_integer) :: status, Caller_Error


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   ErrNum = 0                  ! no error until one occurs

   !  Check that only amplitude parameters of additive terms are selected for
   !  fitting.  Amplitude parameters have KPARAM=1.

   DO KPARAM=1, MAX_PPT
      DO JTERM=1, NUM_TERMS_AVAIL
         IF (KPARAM .GT. 1 .OR. JTERM .GT. NUM_ADD_TERMS) THEN
            IF (PARAM_VARY (JTERM, KPARAM)) THEN
               WRITE (6, *)  '**** ERROR: MFIT SUBROUTINE M_LFIT: MODEL IS NOT LINEAR !'
               WRITE (6, *) JTERM, KPARAM, PARAM_VARY (JTERM, KPARAM)
               ErrNum = 3001
               RETURN
            END IF
         END IF
      END DO
   END DO

   !  so that the user doesn't have to guess amplitude parameters, make an
   !  arbitrary guess for the first pass.   latter passes will use the
   !  solution of the previous pass.   any nonzero value is suitable since
   !  we divide by the parameter value below.

   DO JTERM=1, NUM_ADD_TERMS
      IF (PARAM_VARY (JTERM, 1))  PARAM (JTERM, 1) = 1.
   END DO

   IF (NUM_VARY .LE. 0) THEN
      WRITE (6, *) '**** MFIT SUBROUTINE M_LFIT ERROR: NO VARYING TERMS !'
      ErrNum = 3002
      RETURN
   END IF

   !  Initialize:

   CHISQ = 367.

   COVAR = 0.0_mfit_DoubleReal
   BETA = 0.0_mfit_DoubleReal


   !  get the model count rates.   We need each individual additive term,
   !  and each one must by multiplied by the multiplicative terms.

   DO_TERM = 0
   CALL M_PHOTONS_MODEL  ( NUM_DET, maxval (NUM_EBINS), PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,        &
           PARAM, TERM_USED, DO_TERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS, MODEL_PHOT_RATE_WS,  &
           MULT_FLAG, MULT_TERMS, ErrNum )

   IF (MULT_FLAG) THEN
      DO LDET=0, NUM_DET - 1
         IF (USE_DET (LDET)) THEN
            DO JTERM=1, NUM_ADD_TERMS
               IF (TERM_USED (JTERM) .EQ. 1) THEN
                  DO PBIN=FIRST_PHOT_BIN (JTERM, LDET), LAST_PHOT_BIN (JTERM, LDET)
                     MODEL_PHOT_RATE_WS (PBIN, JTERM, LDET) = MODEL_PHOT_RATE_WS (PBIN, JTERM, LDET) * MULT_TERMS (PBIN, LDET)
                  END DO
               END IF
            END DO
         END IF
      END DO
   END IF

   CALL MPHOTS_TO_CNTS  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH, USE_DET, CHAN_OFFSET, FIT_CHAN,       &
           FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, TERM_USED, NUM_ADD_TERMS, PHOT_WIDTH, DRM,       &
           MODEL_PHOT_RATE_WS, MODEL_CNT_RATE_WS )


   !  In order to do model variance chisq fit, fit must be done iteratively--
   !  1st pass uses data variances, latter passes use model variances
   !  calculated from model of previous pass:
   !  Since this routine is always followed by M_MRQMIN, we only bother doing
   !  two passes.


   DO IPASS=1, 2

   !     on 1st pass, place the data variances into MODEL_CR_VARI_WS.
   !     on latter passes, this array already contains the model variances,
   !     calculated at the bottom of the ipass do loop.

      IF (IPASS .EQ. 1) THEN
         DO LDET=0, NUM_DET - 1
            IF (USE_DET (LDET)) THEN
               DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
                  MODEL_CR_VARI_WS (ICHAN, LDET) = DATA_CR_VARI (ICHAN, LDET)
               END DO
            END IF
         END DO
      END IF


      !  Loop through data, both selected detectors and selected channels that
      !  have data (missing data is flagged via live_time <= 0),
      !  accumulating coefficients of the normal equations (i.e. covar & beta):

      DO LDET=0, NUM_DET - 1
         IF (USE_DET (LDET)) THEN
            DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
               IF (LIVE_TIME (ICHAN, LDET) .GT. 0.0) THEN

                  !  Subtract off dependencies on known pieces of the fitting
                  !  function, i.e. Subtract from data the background rate
                  !  (input to MFIT) and all the fixed terms of model
                  !  so that we will compare varying model to data less fixed
                  !  terms.  A fixed term has its amplitude parameter (kparam=1)
                  !  fixed:

                  YDATA = OBS_CRATE (ICHAN, LDET) - BACK_CRATE (ICHAN, LDET)
                  DO JTERM=1, NUM_ADD_TERMS
                     IF (.NOT. PARAM_VARY (JTERM, 1))  YDATA = YDATA - MODEL_CNT_RATE_WS (ICHAN, JTERM, LDET)
                  END DO

                  !  Build COVAR and BETA arrays:
                  !  BETA and COVAR are indexed by INDEX_x = count of varying
                  !  parameters (and thus their indices vary from 1 to NUM_VARY)
                  !  while PARAM and PARAM_VARY are indexed (JTERM, KPARAM),
                  !  with varying parameters only placed into BETA and COVAR.
                  !  INDEX_1 and INDEX_2 count the varying terms = terms
                  !  with varying amplitude parameters; INDEX_1 cooresponds
                  !  to JTERM_1, etc.

                  INDEX_1 = 0
                  DO JTERM=1, NUM_ADD_TERMS
                     IF (PARAM_VARY (JTERM, 1)) THEN
                        INDEX_1 = INDEX_1 + 1
                        WT = MODEL_CNT_RATE_WS (ICHAN, JTERM, LDET) / (PARAM (JTERM, 1) * MODEL_CR_VARI_WS (ICHAN, LDET))
                        BETA (INDEX_1) = BETA (INDEX_1) + YDATA * WT
                        INDEX_2 = 0
                        DO JTERM_2=1, JTERM
                           IF (PARAM_VARY (JTERM_2, 1)) THEN
                              INDEX_2 = INDEX_2 + 1
                              COVAR (INDEX_1, INDEX_2) =     &
                              COVAR (INDEX_1, INDEX_2) + WT * MODEL_CNT_RATE_WS (ICHAN, JTERM_2, LDET) / PARAM (JTERM_2, 1)
                           END IF     ! varying term?
                        END DO        ! jterm_2
                     END IF        ! varying term?
                  END DO       ! jterm_1
               END IF      ! data in chan?
            END DO       ! ichan
         END IF       ! selected det?
      END DO        ! ldet


      ! Complete other triangle of COVAR (use symmetry of array and thus don't do computations twice):

      IF (NUM_VARY .GT. 1) THEN
         DO INDEX_1=2, NUM_VARY
            DO INDEX_2=1, INDEX_1 - 1
               COVAR (INDEX_2, INDEX_1) = COVAR (INDEX_1, INDEX_2)
            END DO
         END DO
      END IF


      ERROR = M_validate_matrix  ( NUM_VARY, covar )

      if (ERROR) then
         ErrNum = 3003
         write (6, *) 'bad covar in M_LFIT'
         RETURN
      end if

      ERROR = M_validate_vector  ( NUM_VARY, BETA )

      if (ERROR) then
         ErrNum = 3004
         write (6, *) 'bad beta in M_LFIT'
         RETURN
      end if


      if ( btest (DebugMask, 2) )  call Output_Condition_Number ( "Prelim. linear fit", NUM_VARY, MAX_PARAM, COVAR )


      !  Solve the linear system COVAR * X = BETA for X -- X is the parameter values !

      status = solve_linear_system_with_gsl  ( NUM_VARY, MAX_PARAM, COVAR, BETA, Vector_X )

      if ( status /= 0 ) then
         ErrNum = status + 10000
         write (6, '( / "Linear fit: failed to solve linear system: status=", I6 / )' )  STATUS
         return
      end if


      !  Unpack solution from Vector_X into PARAM:

      INDEX = 0
      DO JTERM=1, NUM_ADD_TERMS
         IF (PARAM_VARY (JTERM, 1)) THEN
            INDEX = INDEX + 1
            PARAM (JTERM, 1) = real ( Vector_X (INDEX), mfit_real )
         END IF
      END DO


      !  get count rate model & count rate variances for new model coefficients:
      !  to get these first must calculate photon model for new model coeff.
      !  on all passes but the last, these (esp. the model variances) are used
      !  to calculate the next solution; they are also used below to calculate
      !  chisq.   The individual terms are only needed for another pass; only
      !  the total model is needed to calculate chisq.

      DO_TERM = 0
      CALL M_PHOTONS_MODEL  ( NUM_DET, maxval (NUM_EBINS), PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,     &
               PARAM, TERM_USED, DO_TERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,                  &
               MODEL_PHOT_RATE_WS, MULT_FLAG, MULT_TERMS, Caller_Error )
               
      !   We'll hope that the photon model is somewhat useful, and won't immediately return...
      if ( Caller_Error /= 0 )  ErrNum = Caller_Error + 10000
         

      IF (IPASS .NE. 2 .AND. MULT_FLAG) THEN
         DO LDET=0, NUM_DET - 1
            IF (USE_DET (LDET)) THEN
               DO JTERM=1, NUM_ADD_TERMS
                  IF (TERM_USED (JTERM) .EQ. 1) THEN
                     DO PBIN=FIRST_PHOT_BIN (JTERM, LDET), LAST_PHOT_BIN (JTERM, LDET)
                        MODEL_PHOT_RATE_WS (PBIN, JTERM, LDET) = MODEL_PHOT_RATE_WS (PBIN, JTERM, LDET) * MULT_TERMS (PBIN, LDET)
                     END DO
                  END IF
               END DO
            END IF
         END DO
      END IF

      CALL MPHOTS_TO_CNTS  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH, USE_DET, CHAN_OFFSET, FIT_CHAN,        &
              FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, TERM_USED, NUM_ADD_TERMS, PHOT_WIDTH, DRM,        &
              MODEL_PHOT_RATE_WS, MODEL_CNT_RATE_WS )
      CALL MGET_MODEL_CR_VARI  ( NUM_DET, NUM_CHAN, CHAN_OFFSET, CHAN_WIDTH, BACK_CRATE, BACK_CSIG,      &
              LIVE_TIME, USE_DET, FIT_CHAN, MODEL_CNT_RATE_WS, MODEL_CR_VARI_WS )


      !  Calculate Chi-Square:

      CHISQ = M_CALC_CHISQ  ( NUM_DET, NUM_CHAN, USE_DET, CHAN_OFFSET, FIT_CHAN, OBS_CRATE, BACK_CRATE,    &
                    LIVE_TIME, MODEL_CNT_RATE_WS, MODEL_CR_VARI_WS )

      IF ( btest (DebugMask, 1) ) THEN
         IF (IPASS .EQ. 1) THEN
            FIT_TYPE = 'Data'
         ELSE
            FIT_TYPE = 'Model'
         END IF
         STRING = M_PVALS (PARAM, PARAM_VARY)
         WRITE (6, 1010)  CHISQ, FIT_TYPE, trim (STRING)
         1010  FORMAT ( 1X, 1PG17.9, ' <-- Chisq from Linear Fit with ', A, ' variances: ', A )
      END IF

   END DO                                                           ! ipass


   RETURN

   END SUBROUTINE M_LFIT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_MODEL_DERIV  ( NUM_DET, NUM_CHAN, NUM_EBINS, FIT_CHAN, USE_DET,                          &
          CHAN_OFFSET, CHAN_WIDTH, PARAM, PARAM_VARY, PHOT_WIDTH, NUM_TERMS_AVAIL, NUM_ADD_TERMS,        &
          DRM, FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, PHOT_EVAL, NUME_TO_USE, TERM_USED,          &
          MODEL_CNT_DERIV, ErrNum )


   !  This subroutine is designed to be called by Numerical Recipes
   !  MRQMIN / MRQCOF (modified).   It returns the derivative of the
   !  model count rate.

   !  Major revision 14 June 1993 by MSB: we now calculate the derivative
   !  of the model photon flux and then xform to counts space.   This reduces
   !  the number of multiplications by the DRM and makes it possible to do
   !  some of the derivatives analytically.
   !  MSB 1996 Jan 16: got multiplicative functions working.  Abandon
   !  analytic derivatives.  Analytic deriviaties had no significant
   !  accuracy or cpu speed advantage.   All derivatives are now evaluated
   !  numerically by finitie differences.

   !  For each additive/subtractive term, it is required that the value of the
   !  model term be proportional to the value of the first parameter for that
   !  term, i.e. 1) the term is zero if its first parameter is zero, and
   !  2) the model is linear if only first parameters of the various additive/
   !  subtractive terms are varied.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-64, 17 November 1992.


   use MFIT_parameters


   !  Input variables:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_EBINS
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: LAST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: FIRST_NONZERO
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED

   !  Output arguments:
   real (kind=mfit_real), dimension (MAX_PARAM, 0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: MODEL_CNT_DERIV
   integer (kind=mfit_integer), intent (out) :: ErrNum


   !  Internal variables:

   !  Workspace arrays:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_CNT_DERIV_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_PHOT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1) :: MULT_TERMS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_PHOT_RATE_2
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_PHOT_DERIV

   !  Other internal arrays:
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT) :: PARAM_COPY
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT) :: PARAM_ONE
   integer (kind=mfit_integer), dimension (0: MAX_TERMS) :: TERM_K_USED

   integer (kind=mfit_integer) :: Caller_Error
   logical (kind=mfit_logical) :: MULT_FLAG
   integer (kind=mfit_integer), dimension (MAX_TERMS, MAX_PPT) :: A_INDEX
   integer (kind=mfit_integer) :: T_INDEX
   integer (kind=mfit_integer) :: JTERM, KPARAM
   integer (kind=mfit_integer) :: ICHAN
   integer (kind=mfit_integer) :: PBIN
   integer (kind=mfit_integer) :: LDET
   integer (kind=mfit_integer) :: DO_TERM
   real (kind=mfit_real) :: TEMP_U, TEMP_L
   logical (kind=mfit_logical) :: FLAG
   


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   
   
   !  *** Initialize:
   
   ErrNum = 0

   !  Setup the index which counts the varying parameters:
 
   T_INDEX = 0
   DO JTERM=1, MAX_TERMS
      DO KPARAM=1, MAX_PPT
         A_INDEX (JTERM, KPARAM) = 0
         IF (PARAM_VARY (JTERM, KPARAM)) THEN
            T_INDEX = T_INDEX + 1
            A_INDEX (JTERM, KPARAM) = T_INDEX
         END IF
      END DO
   END DO


   !  Setup a copy of the parameter values:

   PARAM_COPY = PARAM


   !  Setup another copy of the parameter values, with the 1st parameter
   !  of each ADDITIVE term set to one:

   PARAM_ONE = PARAM
   PARAM_ONE (1:NUM_ADD_TERMS, 1) = 1.0


   !  Zero the output for safety:

   MODEL_CNT_DERIV = 0.0


   !
   !  *** CALCULATE THE DERIVATIVES W.R.T. VARYING PARAMETERS:


   !  First we calculate the derivatives in photons units.
   !  We do the do-loops in the order that is most run-time efficient: namely, the outer loop is KPARAM:

   DO KPARAM=1, MAX_PPT
      !  For each value of KPARAM, the calculation should only be done if for some
      !  value of JTERM there is a varying parameter:
      FLAG = .FALSE.
      DO JTERM=1, NUM_TERMS_AVAIL
         IF (PARAM_VARY (JTERM, KPARAM))  FLAG = .TRUE.
      END DO

      IF (FLAG) THEN

         DO JTERM=1, NUM_TERMS_AVAIL
            IF (PARAM_VARY (JTERM, KPARAM)) THEN

               !  It is essential to zero the following array, since we sometimes sum into it 
               !  and/or leave unaltered values that are obviously zero.

               DO LDET=0, NUM_DET - 1
                  IF (USE_DET (LDET)) THEN
                     DO PBIN=FIRST_PHOT_BIN (JTERM, LDET), LAST_PHOT_BIN (JTERM, LDET)
                        MODEL_PHOT_DERIV (PBIN, JTERM, LDET) = 0.0
                     END DO
                  END IF
               END DO


               !  For additive/subtractive terms ONLY, which are linear, kparam=1 is a special & easy
               !  case since the 1st parameters are always proportionality constants and therefore the
               !  derivatives are equal to the model evaluated with the first parameters set to one:

               IF (JTERM .LE. NUM_ADD_TERMS .AND. KPARAM .EQ. 1) THEN

                  !  Get the model photon flux which is the same as the derivative for this special case.  
                  !  But this additive term must be correctly scaled by multiplying by the product of the multiplicative terms:
                  
                  CALL M_PHOTONS_MODEL  ( NUM_DET, maxval (NUM_EBINS), PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,   &
                             PARAM_ONE, TERM_USED, JTERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,               &
                             MODEL_PHOT_DERIV, MULT_FLAG, MULT_TERMS, Caller_Error )
                  if ( Caller_Error /= 0 )  ErrNum = 25000 + Caller_Error
                             
                  IF (MULT_FLAG) THEN
                     DO LDET=0, NUM_DET - 1
                        IF (USE_DET (LDET)) THEN
                           DO PBIN=FIRST_PHOT_BIN (JTERM, LDET), LAST_PHOT_BIN (JTERM, LDET)
                              MODEL_PHOT_DERIV (PBIN, JTERM, LDET) = MODEL_PHOT_DERIV (PBIN, JTERM, LDET) * MULT_TERMS (PBIN, LDET)
                           END DO
                        END IF
                     END DO
                  END IF

               ELSE                               ! kparam=1 for add. term ?

                  !  For additive terms, we need only calculate that 1 additive term (which is 
                  !  automatically multiplied by all of the multiplicative terms), while for 
                  !  multiplicative terms, we must evaluate their impact on the total model:
                  
                  DO_TERM = JTERM
                  IF (JTERM .GT. NUM_ADD_TERMS) DO_TERM = 0

                  !  For parameters which are not proportionality parameters, numerically evaluate
                  !  the derivatives as symmetric finite differences:
                  !  We evaluate the photon model for two values of the parameter, one just above 
                  !  and the other just below the current parameter value.    We then use the 
                  !  difference of the two model values to calculate the derivative.

                  !  We prefer to figure the offsets of the parameter via multiplicate factors, but
                  !  this is not possible if the parameter = 0:

                  IF (PARAM (JTERM, KPARAM) .NE. 0.0) THEN
                     TEMP_U = 1.005 * PARAM (JTERM, KPARAM)
                     TEMP_L = 0.995 * PARAM (JTERM, KPARAM)
                  ELSE
                     TEMP_U = +0.001
                     TEMP_L = -0.001
                  END IF

                  !  upper:
                  PARAM_COPY (JTERM,KPARAM) = TEMP_U
                  ! Get the model photon flux:
                  CALL M_PHOTONS_MODEL  ( NUM_DET, maxval (NUM_EBINS), PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,   &
                            PARAM_COPY, TERM_USED, DO_TERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,             &
                            MODEL_PHOT_RATE_WS, MULT_FLAG, MULT_TERMS, Caller_Error )
                  if ( Caller_Error /= 0 )  ErrNum = 26000 + Caller_Error

                  ! lower:
                  PARAM_COPY (JTERM,KPARAM) = TEMP_L
                  CALL M_PHOTONS_MODEL  ( NUM_DET, maxval (NUM_EBINS) , PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,   &
                            PARAM_COPY, TERM_USED, DO_TERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,              &
                            MODEL_PHOT_RATE_2, MULT_FLAG, MULT_TERMS, Caller_Error )
                  if ( Caller_Error /= 0 )  ErrNum = 27000 + Caller_Error

                  ! restore value in array param_copy for latter use:
                  PARAM_COPY (JTERM, KPARAM) = PARAM (JTERM, KPARAM)

                  ! calculate derivative as difference:
                  ! See 2nd edition of Numerical Recipes for subtleties:
                  DO LDET=0, NUM_DET - 1
                     IF (USE_DET (LDET)) THEN
                        DO PBIN=FIRST_PHOT_BIN (DO_TERM, LDET), LAST_PHOT_BIN (DO_TERM, LDET)
                           MODEL_PHOT_DERIV (PBIN, JTERM, LDET) =      &
                           (MODEL_PHOT_RATE_WS (PBIN, DO_TERM, LDET) - MODEL_PHOT_RATE_2 (PBIN, DO_TERM, LDET)) / (TEMP_U - TEMP_L)
                        END DO
                     END IF
                  END DO

                  !  Individual additive terms must be correctly scaled by multiplying by the product 
                  !  of all of the multiplicative terms.   This has already been done for term 0.
                  !  If we are dealing with an additive term, the parameter varied above will be 
                  !  for that term and the multiplicative terms will have the same values in both 
                  !  calls to m_photons_model.
       
                  IF (DO_TERM .NE. 0 .AND. MULT_FLAG) THEN
                     DO LDET=0, NUM_DET - 1
                        IF (USE_DET (LDET)) THEN
                           DO PBIN=FIRST_PHOT_BIN (JTERM, LDET), LAST_PHOT_BIN (JTERM, LDET)
                              MODEL_PHOT_DERIV (PBIN, JTERM, LDET) = MODEL_PHOT_DERIV (PBIN, JTERM, LDET) * MULT_TERMS (PBIN, LDET)
                           END DO
                        END IF
                     END DO
                  END IF


               END IF                                   ! kparam=1?
            END IF                                         ! varying param ?
         END DO                                            ! jterm


         !  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %
         !       CONVERT DERIV FROM PHOTON TO COUNT UNITS AND PACK INTO OUTPUT ARRAY:
         !  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %


         !  "Truncate" derivative, i.e. min out extremely large values, 
         !  so as to prevent overflows in next subroutine call:

         DO LDET=0, NUM_DET - 1
            IF (USE_DET (LDET)) THEN
               DO JTERM=1, NUM_TERMS_AVAIL
                  IF (PARAM_VARY (JTERM, KPARAM)) THEN
                     DO PBIN=FIRST_PHOT_BIN (JTERM, LDET), LAST_PHOT_BIN (JTERM, LDET)
                        MODEL_PHOT_DERIV (PBIN, JTERM, LDET) = MIN (MODEL_PHOT_DERIV (PBIN, JTERM, LDET), +1.E+14)
                        MODEL_PHOT_DERIV (PBIN, JTERM, LDET) = MAX (MODEL_PHOT_DERIV (PBIN, JTERM, LDET), -1.E+14)
                     END DO
                  END IF
               END DO
            END IF
         END DO

         !  We now have, in photon units, the derivative of all varying parameters which have a
         !  specific value of KPARAM.    I.e., all derivatives w.r.t. PARAM (JTERM, KPARAM) for one value
         !  of KPARAM for whatever values of JTERM such that PARAM (JTERM, KPARAM) is a varying parameter.
         !  We don't do all the derivatives (i.e., all values of KPARAM) at once because the array would
         !  be too big.  We now convert to count units:


         TERM_K_USED (0) = -1
         DO JTERM=1, NUM_TERMS_AVAIL
            IF (PARAM_VARY (JTERM, KPARAM)) THEN
               TERM_K_USED (JTERM) = 1
            ELSE
               TERM_K_USED (JTERM) = -1
            END IF
         END DO

         CALL MPHOTS_TO_CNTS  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH, USE_DET, CHAN_OFFSET, FIT_CHAN,         &
                 FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, TERM_K_USED, NUM_TERMS_AVAIL, PHOT_WIDTH, DRM,     &
                 MODEL_PHOT_DERIV, MODEL_CNT_DERIV_WS )

         !  Pack the answer in the compressed (non-sparse) output array:
         !  array MODEL_CNT_DERIV_WS only has 3 indices while four are needed.
         !  In this pass MODEL_CNT_DERIV_WS is for a specific value of KPARAM.
         !  Array MODEL_CNT_DERIV also only has 3 indices, but one (INDEX) is used to represent 2 indices (JTERM, KPARAM).   
         !  This array is compressed (with 4 indices it would be huge) by using only the used values of (JTERM, KPARAM). 
         !  INDEX is the count of varying parameters counted by using JTERM as the outer loop, KPARAM as the inner loop.
         !  Pack MODEL_CNT_DERIV_WS into smaller array MODEL_CNT_DERIV:

         DO LDET=0, NUM_DET - 1
            IF (USE_DET (LDET)) THEN
               DO JTERM=1, NUM_TERMS_AVAIL
                  IF (PARAM_VARY (JTERM, KPARAM)) THEN
                     DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
                        MODEL_CNT_DERIV (A_INDEX (JTERM, KPARAM), ICHAN, LDET) = MODEL_CNT_DERIV_WS (ICHAN, JTERM, LDET)
                     END DO
                  END IF
               END DO
            END IF
         END DO


         !  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %  %


      END IF                                    ! some param vary for kparam?
   END DO                                                  ! kparam


   RETURN

   END SUBROUTINE M_MODEL_DERIV


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_MODEL_PROP ( NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, PARAM_VARY, NUM_VARY )


   !  Counts up the number of varying parameters in the model.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-66, 31 May 1993.


   use MFIT_parameters


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY


   !  Output arguments:
   integer (kind=mfit_integer), intent (out) :: NUM_VARY


   !  Internal variables:
   integer (kind=mfit_integer) :: JTERM, KPARAM


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  Calculate NUM_VARY = number of varying parameters = count of elements
   !  of PARAM_VARY that are .TRUE.

   NUM_VARY = 0

   DO JTERM=1, NUM_TERMS_AVAIL
      DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)
         IF ( PARAM_VARY (JTERM, KPARAM) )  NUM_VARY = NUM_VARY + 1
      END DO
   END DO


   RETURN

   END SUBROUTINE M_MODEL_PROP


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_MRQCOF_CHISQ  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET, CHAN_WIDTH,                      &
           OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, PARAM, PARAM_VARY, NUM_VARY, NUM_TERMS_AVAIL,       &
           NUM_ADD_TERMS, NT_ADD_LINE, NUM_PARAM_OF_TERM, USE_DET, FIT_CHAN, PHOT_WIDTH, DRM,               &
           FIRST_NONZERO, TERM_USED, PHOT_EVAL, NUME_TO_USE, DATA_CR_VARI, ALPHA, BETA, CHISQ, ErrNum )


   !  Adapted from subroutine MRQCOF of Numerical Recipes (Press et al.).
   !  It is called by M_MRQMIN in order to evaluate the linearized fitting
   !  matrix ALPHA and vector BETA.   The subroutine MRQMIN is an
   !  implementation of the Levenberg-Marquardt nonlinear fitting algorithm.
   !  It has been adapted to be specialized for the fitting package MFIT.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-64, 17 November 1992.
   !  MSB 20 Aug 93: add FACTOR to correctly calculate BETA for model variances.


   use MFIT_parameters
   
   use GSL_interfaces


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   integer (kind=mfit_integer), intent (in) :: NT_ADD_LINE
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: FIRST_NONZERO
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DATA_CR_VARI

   !  Output arguments:
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (out) :: ALPHA
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM), intent (out) :: BETA
   real (kind=mfit_DoubleReal), intent (out) :: CHISQ
   integer (kind=mfit_integer), intent (out) :: ErrNum


   !  Internal variables:

   !  Workspace arrayss:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_CNT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_PHOT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1) :: MODEL_CR_VARI_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1) :: MULT_TERMS

   !  Other internal variables:
   logical (kind=mfit_logical) :: MULT_FLAG
   integer (kind=mfit_integer) :: DO_TERM
   real (kind=mfit_real) :: DY
   real (kind=mfit_real) :: WT
   real (kind=mfit_real), dimension (MAX_PARAM, 0: maxval (NUM_CHAN) -1, 0: NUM_DET-1) :: MODEL_CNT_DERIV
   integer (kind=mfit_integer) :: INDEX_1, INDEX_2
   integer (kind=mfit_integer) :: JTERM, KPARAM
   integer (kind=mfit_integer) :: JTERM_2
   integer (kind=mfit_integer) :: ICHAN, LDET
   real (kind=mfit_real) :: FACTOR
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: LAST_PHOT_BIN
   integer (kind=mfit_integer) :: I_KPV
   integer (kind=mfit_integer), dimension (MAX_TERMS) :: VARYING_PARAM_OF_TERM
   integer (kind=mfit_integer) :: ITEST
   integer (kind=mfit_integer) :: IPV_1, IPV_2
   integer (kind=mfit_integer), dimension (0: num_det-1) :: zero
   integer (kind=mfit_integer), dimension (0: num_det-1) :: numebins_minus
   integer (kind=mfit_integer) :: Caller_Error


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  Initialize:
   
   ErrNum = 0                  ! no error until one occurs

   ALPHA (1:NUM_VARY, 1:NUM_VARY) = 0.0_mfit_DoubleReal
   BETA (1:NUM_VARY) = 0.0_mfit_DoubleReal


   !  for efficiency in the deep loops setting up these arrays allows us to
   !  avoid testing PARAM_VARY and actually achieves significant run-time
   !  savings:

   ITEST = 0
   DO JTERM=1, NUM_TERMS_AVAIL
      I_KPV = 0
      IF (TERM_USED (JTERM) .EQ. 1) THEN
         DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)
            IF (PARAM_VARY (JTERM, KPARAM)) I_KPV = I_KPV + 1
         END DO
      END IF
      VARYING_PARAM_OF_TERM (JTERM) = I_KPV
      ITEST = ITEST + I_KPV
   END DO
   IF (ITEST .NE. NUM_VARY) THEN
      WRITE (6, *) '### M_MRQCOF: INTERNAL ERROR RE ITEST !', ITEST, NUM_VARY
      STOP
   END IF


   !  calculate for which photon bins the model needs to be calculated and
   !  the DRM multiplecation done:

   zero = 0
   numebins_minus = num_ebins - 1

   CALL M_USE_PHOTBINS  ( NUM_DET, USE_DET, NUM_TERMS_AVAIL, NT_ADD_LINE, zero, numebins_minus,     &
             maxval (NUM_EBINS), PHOT_EVAL, TERM_USED, PARAM, FIRST_PHOT_BIN, LAST_PHOT_BIN )


   !  get the derivatives of the model count rates:

   CALL M_MODEL_DERIV  ( NUM_DET, NUM_CHAN, NUM_EBINS, FIT_CHAN, USE_DET, CHAN_OFFSET, CHAN_WIDTH,        &
            PARAM, PARAM_VARY, PHOT_WIDTH, NUM_TERMS_AVAIL, NUM_ADD_TERMS, DRM,                           &
            FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, PHOT_EVAL, NUME_TO_USE, TERM_USED, MODEL_CNT_DERIV, Caller_Error )
   if ( Caller_Error /= 0 )  ErrNum = 41000 + Caller_Error


   !  get the model count rates and variances:
   !  (the previous call to M_MODEL_DERIV uses MODEL_CNT_RATE_WS as a workspace,
   !  and does not leave the correct values therein)

   DO_TERM = 0
   CALL M_PHOTONS_MODEL  ( NUM_DET, maxval (NUM_EBINS) , PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,      &
             PARAM, TERM_USED, DO_TERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,                  &
             MODEL_PHOT_RATE_WS, MULT_FLAG, MULT_TERMS, Caller_Error )
   if ( Caller_Error /= 0 )  ErrNum = 42000 + Caller_Error
             
   CALL MPHOTS_TO_CNTS  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH, USE_DET, CHAN_OFFSET, FIT_CHAN,        &
             FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, TERM_USED, NUM_ADD_TERMS, PHOT_WIDTH, DRM,      &
             MODEL_PHOT_RATE_WS, MODEL_CNT_RATE_WS )
   CALL MGET_MODEL_CR_VARI  ( NUM_DET, NUM_CHAN, CHAN_OFFSET, CHAN_WIDTH, BACK_CRATE, BACK_CSIG,           &
             LIVE_TIME, USE_DET, FIT_CHAN, MODEL_CNT_RATE_WS, MODEL_CR_VARI_WS )


   !  Loop through all of the data: detectors and channels thereof, skip
   !  unused detectors and channels lacking data:
   !  Sum up ALPHA and BETA:

   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN                                  ! use det ?
         DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
            IF (LIVE_TIME (ICHAN, LDET) .GT. 0.0) THEN          ! have data ?

               DY = OBS_CRATE (ICHAN, LDET) - BACK_CRATE (ICHAN, LDET) - MODEL_CNT_RATE_WS (ICHAN, 0, LDET)

               !  "factor" is needed to calculate beta when using model
               !  variances, which also depend on the model parameters.  It
               !  reduces to one if model variances = data vari.:
               FACTOR = (MODEL_CR_VARI_WS (ICHAN, LDET) + DATA_CR_VARI (ICHAN, LDET)) /    &
                          (2. * MODEL_CR_VARI_WS (ICHAN, LDET))

               !  we obtain just the varying parameters by looping over JTERM
               !  and IPV_x.   This is equivalent to looping over jterm &
               !  kparam AND testing PARAM_VARY.
               !  INDEX_x counts the varying parameters:

               INDEX_1 = 0
               DO JTERM=1, NUM_TERMS_AVAIL
                  DO IPV_1=1, VARYING_PARAM_OF_TERM (JTERM)
                     INDEX_1 = INDEX_1 + 1
                     WT = MODEL_CNT_DERIV (INDEX_1, ICHAN, LDET) / MODEL_CR_VARI_WS (ICHAN, LDET)
                     BETA (INDEX_1) = BETA (INDEX_1) + DY * WT * FACTOR

                     !  another loop over the varying parameters:

                     INDEX_2 = 0
                     DO JTERM_2=1, NUM_TERMS_AVAIL
                        DO IPV_2=1, VARYING_PARAM_OF_TERM (JTERM_2)
                           INDEX_2 = INDEX_2 + 1
                           ALPHA (INDEX_1, INDEX_2) =         &
                           ALPHA (INDEX_1, INDEX_2) + WT * MODEL_CNT_DERIV (INDEX_2, ICHAN, LDET)
                        END DO   ! varying param loop 2
                     END DO           ! term loop 2
                  END DO         ! varying param loop 1
               END DO                 ! term loop 1

            END IF      ! have data ?
         END DO       ! ICHAN
      END IF       ! use det ?
   END DO      ! LDET


   !  get chisq for the current parameter values:
   CHISQ = M_CALC_CHISQ  ( NUM_DET, NUM_CHAN, USE_DET, CHAN_OFFSET, FIT_CHAN, OBS_CRATE, BACK_CRATE,     &
               LIVE_TIME, MODEL_CNT_RATE_WS, MODEL_CR_VARI_WS )
               

   if ( gsl_finite (CHISQ) == 0 )  ErrNum = 43000


   RETURN

   END SUBROUTINE M_MRQCOF_CHISQ


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_MRQCOF_LOGLIKE  ( fit_mode, NUM_DET, NUM_CHAN, NUM_EBINS,                                 &
          CHAN_OFFSET, CHAN_WIDTH, OBS_CRATE, BACK_CRATE, LIVE_TIME, PARAM, PARAM_VARY, NUM_VARY,         &
          NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE, NUM_PARAM_OF_TERM, USE_DET, FIT_CHAN,              &
          PHOT_WIDTH, DRM, FIRST_NONZERO, TERM_USED, PHOT_EVAL, NUME_TO_USE, ALPHA, BETA, LOG_LIKE, ErrNum )


   !  Adapted from subroutine MRQCOF of Numerical Recipes (Press et al.).
   !  It is called by M_MRQMIN in order to evaluate the linearized fitting
   !  matrix ALPHA and vector BETA.   The subroutine MRQMIN is an
   !  implementation of the Levenberg-Marquardt nonlinear fitting algorithm.
   !  It has been adapted to be specialized for the fitting package MFIT.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  M. S. Briggs, merged back into code 2008 Oct 5.
   !  M. S. Briggs, 1999 Oct 14.  This version calculates alpha and beta
   !  for -2 log likelihood, rather than for chisq.
   !  The likelihood function is the sum of the Poisson probabilities
   !  for obtaining the observed counts given the total model counts,
   !  background + source.   The error on the background is neglected.

   !  Michael S. Briggs, UAH / MSFC ES-64, 17 November 1992.
   !  MSB 20 Aug 93: add FACTOR to correctly calculate BETA for model variances.


   use MFIT_parameters
   
   use GSL_interfaces


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: fit_mode
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   integer (kind=mfit_integer), intent (in) :: NT_ADD_LINE
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: FIRST_NONZERO
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE

   !  Output arguments:
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (out) :: ALPHA
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM), intent (out) :: BETA
   real (kind=mfit_DoubleReal), intent (out) :: LOG_LIKE
   integer (kind=mfit_integer), intent (out) :: ErrNum


   !  Internal variables:

   !  Workspace arrays:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_CNT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_PHOT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1) :: MULT_TERMS

   !  Other internal variables:
   logical (kind=mfit_logical) :: MULT_FLAG
   integer (kind=mfit_integer) :: DO_TERM
   real (kind=mfit_real), dimension (MAX_PARAM, 0: maxval (NUM_CHAN) -1, 0: NUM_DET-1) :: MODEL_CNT_DERIV
   integer (kind=mfit_integer) :: INDEX_1, INDEX_2
   integer (kind=mfit_integer) :: JTERM, KPARAM
   integer (kind=mfit_integer) :: JTERM_2
   integer (kind=mfit_integer) :: ICHAN, LDET
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: LAST_PHOT_BIN
   integer (kind=mfit_integer) :: I_KPV
   integer (kind=mfit_integer), dimension (MAX_TERMS) :: VARYING_PARAM_OF_TERM
   integer (kind=mfit_integer) :: ITEST
   integer (kind=mfit_integer) :: IPV_1, IPV_2
   integer (kind=mfit_integer), dimension (0: num_det-1) :: zero
   integer (kind=mfit_integer), dimension (0: num_det-1) :: numebins_minus

   real (kind=mfit_real) :: tot_model_crate
   real (kind=mfit_real) :: a_fac, b_fac
   integer (kind=mfit_integer) :: Caller_Error


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  Initialize:
   
   ErrNum = 0                  ! no error until one occurs
   
   ALPHA (1:NUM_VARY, 1:NUM_VARY) = 0.0_mfit_DoubleReal
   BETA (1:NUM_VARY) = 0.0_mfit_DoubleReal


   !  for efficiency in the deep loops setting up these arrays allows us to
   !  avoid testing PARAM_VARY and actually achieves significant run-time
   !  savings:

   ITEST = 0
   DO JTERM=1, NUM_TERMS_AVAIL
      I_KPV = 0
      IF (TERM_USED (JTERM) .EQ. 1) THEN
         DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)
            IF (PARAM_VARY (JTERM, KPARAM)) I_KPV = I_KPV + 1
         END DO
      END IF
      VARYING_PARAM_OF_TERM (JTERM) = I_KPV
      ITEST = ITEST + I_KPV
   END DO
   IF (ITEST .NE. NUM_VARY) THEN
      WRITE (6, *) '### M_MRQCOF: INTERNAL ERROR RE ITEST !', ITEST, NUM_VARY
      STOP
   END IF


   !  calculate for which photon bins the model needs to be calculated and
   !  the DRM multiplecation done:

   zero = 0
   numebins_minus = num_ebins - 1

   CALL M_USE_PHOTBINS  ( NUM_DET, USE_DET, NUM_TERMS_AVAIL, NT_ADD_LINE, zero, numebins_minus,    &
            maxval (NUM_EBINS), PHOT_EVAL, TERM_USED, PARAM, FIRST_PHOT_BIN, LAST_PHOT_BIN )


   ! get the derivatives of the model count rates:

   CALL M_MODEL_DERIV  ( NUM_DET, NUM_CHAN, NUM_EBINS, FIT_CHAN, USE_DET, CHAN_OFFSET, CHAN_WIDTH,          &
           PARAM, PARAM_VARY, PHOT_WIDTH, NUM_TERMS_AVAIL, NUM_ADD_TERMS, DRM,                              &
           FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, PHOT_EVAL, NUME_TO_USE, TERM_USED, MODEL_CNT_DERIV, Caller_Error )
   if ( Caller_Error /= 0 )  ErrNum = 44000 + Caller_Error


   !  get the model count rates and variances:
   !  (the previous call to M_MODEL_DERIV uses MODEL_CNT_RATE_WS as a workspace,
   !  and does not leave the correct values therein)

   DO_TERM = 0
   CALL M_PHOTONS_MODEL  ( NUM_DET, maxval (NUM_EBINS) , PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,       &
             PARAM, TERM_USED, DO_TERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,                    &
             MODEL_PHOT_RATE_WS, MULT_FLAG, MULT_TERMS, Caller_Error )
   if ( Caller_Error /= 0 )  ErrNum = 45000 + Caller_Error
   
   CALL MPHOTS_TO_CNTS  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH, USE_DET, CHAN_OFFSET, FIT_CHAN,       &
             FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, TERM_USED, NUM_ADD_TERMS, PHOT_WIDTH, DRM,     &
             MODEL_PHOT_RATE_WS, MODEL_CNT_RATE_WS )


   !  Loop through all of the data: detectors and channels thereof, skip
   !  unused detectors and channels lacking data:
   !  Sum up ALPHA and BETA:

   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN                                  ! use det ?
         DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
            IF (LIVE_TIME (ICHAN, LDET) .GT. 0.0) THEN          ! have data ?

               tot_model_crate = MODEL_CNT_RATE_WS (ICHAN, 0, LDET) + BACK_CRATE (ICHAN, LDET)

               b_fac = 2. * (obs_crate (ichan, ldet) / tot_model_crate - 1.) * live_time (ichan, ldet) * chan_width (ichan, ldet)

               a_fac = 2. * obs_crate (ichan, ldet) / tot_model_crate**2 * live_time (ichan, ldet) * chan_width (ichan, ldet)

               !  we obtain just the varying parameters by looping over JTERM
               !  and IPV_x.   This is equivalent to looping over jterm &
               !  kparam AND testing PARAM_VARY.
               !  INDEX_x counts the varying parameters:

               INDEX_1 = 0
               DO JTERM=1, NUM_TERMS_AVAIL
                  DO IPV_1=1, VARYING_PARAM_OF_TERM (JTERM)
                     INDEX_1 = INDEX_1 + 1

                     BETA (INDEX_1) = BETA (INDEX_1) + b_fac * model_cnt_deriv (index_1, ichan, ldet)

                     !  another loop over the varying parameters:

                     INDEX_2 = 0
                     DO JTERM_2=1, NUM_TERMS_AVAIL
                        DO IPV_2=1, VARYING_PARAM_OF_TERM (JTERM_2)
                           INDEX_2 = INDEX_2 + 1
                           ALPHA (INDEX_1, INDEX_2) =                     &
                           ALPHA (INDEX_1, INDEX_2) + a_fac *             &
                              model_cnt_deriv (index_1, ichan, ldet) *    &
                              model_cnt_deriv (index_2, ichan, ldet)
                        END DO   ! varying param loop 2
                     END DO           ! term loop 2
                  END DO         ! varying param loop 1
               END DO                 ! term loop 1

            END IF     ! have data ?
         END DO      ! ICHAN
      END IF      ! use det ?
   END DO      ! LDET


   !  get log_likelihood for the current parameter values:
   LOG_LIKE = M_CALC_LOGLIKE  ( fit_mode, NUM_DET, NUM_CHAN, USE_DET, CHAN_OFFSET, FIT_CHAN,    &
                   OBS_CRATE, BACK_CRATE, LIVE_TIME, CHAN_WIDTH, MODEL_CNT_RATE_WS )
                   
                   
   if ( gsl_finite (LOG_LIKE) == 0 )  ErrNum = 46000


   RETURN

   END SUBROUTINE M_MRQCOF_LOGLIKE


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_MRQMIN ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET, CHAN_WIDTH,                    &
           OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, PARAM_VARY, NUM_VARY,                      &
           NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE, NUM_PARAM_OF_TERM, USE_DET, FIT_CHAN,      &
           PHOT_WIDTH, DRM, FIRST_NONZERO, TERM_USED, PHOT_EVAL, NUME_TO_USE,                      &
           REL_CHANGE_LIMIT, ABS_CHANGE_LIMIT, DATA_CR_VARI, fit_mode, DebugMask,                  &
           PARAM, ALAMDA, ALPHA, statistic, ErrNum )


   !  Numerical Recipes (Press et al.) subroutine heavily modified to be specific
   !  for the MFIT subroutine package.  This subroutine is an implementation
   !  of the Levenberg-Marquardt nonlinear fitting algorithm.
   !  See also Data Reduction and Error Analysis for the Physical Sciences by
   !  Bevington.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  This code merged back into the production code on 2008 October 14:
   !  Modified by MSB 1999 Oct 14 to do either minimization of chisq or of
   !  -2 log likelikhood, as requested by FIT_MODE.
   !  The principal change is to call either MRQCOF_CHISQ or MRQCOF_LOGLIKE
   !  to obtain ALPHA, BETA and statistic = chisq or -2 log_like
   !  We use -2 log_like so that both statistics are  minimized.
   !  The equations for ALPHA and BETA are based on -2 log_like.

   !  Michael S. Briggs, UAH / MSFC ES-62, June 1992, 17 November 1992.
   
   
   !  For this algorithm to work, the values of some variables must be retained across
   !  calls / iterations.
   !  Some variables have their values retained because they are arguments to this
   !  subroutine, and the values persist because the variables persist across calls
   !  to this routine because they exist across calls to this routine because they
   !  are owned by the caller (or higher).
   !  Other variables have their values persist even though they are local variables of
   !  this subroutine because they are declared with the "SAVE" attribute.
   !  Some of the variables whose values must persist across calls/iterations:
   !  ALPHA, BETA, ALAMDA, old_statistic, PARAM, ...
   !  (Of course, when a new problem is started, flaged with ALAMDA < 0, all variables
   !  are reset.)


   use MFIT_parameters

   use GSL_interfaces


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_VARY
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   integer (kind=mfit_integer), intent (in) :: NT_ADD_LINE
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: FIRST_NONZERO
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   real (kind=mfit_real) , dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) ::PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: REL_CHANGE_LIMIT
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: ABS_CHANGE_LIMIT
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DATA_CR_VARI
   integer (kind=mfit_integer), intent (in) :: fit_mode
   integer (kind=mfit_integer), intent (in) :: DebugMask

   !  Input / Output arguments:
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (inout) :: PARAM
   real (kind=mfit_DoubleReal), intent (inout) :: ALAMDA
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (inout) :: ALPHA

   !  Output arguments:
   real (kind=mfit_DoubleReal), intent (out) :: statistic
   integer (kind=mfit_integer), intent (out) :: ErrNum


   !  Internal Variables:

   logical (kind=mfit_logical) :: ERROR
   real (kind=mfit_real) :: SIGN
   real (kind=mfit_real) :: CHANGE_LIMIT
   integer (kind=mfit_integer) :: INDEX, INDEX_1
   integer (kind=mfit_integer) :: JTERM, KPARAM
   real (kind=mfit_DoubleReal) :: DELTA_GREEK
   integer (kind=mfit_integer) :: status
   integer (kind=mfit_integer) :: Caller_Error
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM) :: DELTA_PARAM
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM) :: ALPHA_TRY
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM) :: BETA_TRY


   !  Internal variables which must persist across calls to this subroutine:

   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), SAVE :: PARAM_TRY
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM), SAVE :: BETA
   real (kind=mfit_DoubleReal), SAVE :: old_statistic


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   ErrNum = 0                          ! no error until 1 occurs



   !  ALAMDA < 0 flags a special case, first call to start fitting:
   !   (Could be revised data / model / model parameters -- MFIT has been called afresh....)

   IF (ALAMDA .LT. 0.0) THEN

      ! Replace special first call flag value with a good starting value.
      ! Other then the special flag values (0, <0), ALAMDA controls the "split" between
      ! the two methods of algorithm.

      ALAMDA=0.001

      PARAM_TRY = PARAM



      !  Calculate initial ALPHA and BETA, initial value of statistic, etc.:

      if ( fit_mode .ne. 1  .and.  fit_mode .ne. 2  .and.  fit_mode .ne. 3 ) then
         write (6, *) 'm_mrqmin: FATAL ERROR: Unrecognized value for fit_mode:', fit_mode
         stop
      end if

      if ( fit_mode .eq. 1 ) then

         CALL M_MRQCOF_CHISQ  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET, CHAN_WIDTH,                  &
                OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, PARAM, PARAM_VARY, NUM_VARY,               &
                NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE, NUM_PARAM_OF_TERM, USE_DET, FIT_CHAN,      &
                PHOT_WIDTH, DRM, FIRST_NONZERO, TERM_USED, PHOT_EVAL, NUME_TO_USE, DATA_CR_VARI,        &
                ALPHA, BETA, statistic, Caller_Error )
         if ( Caller_Error /= 0 )  then
            ErrNum = 4100000 + Caller_Error
            RETURN
         end if
         old_statistic = statistic

      end if

      if ( fit_mode .eq. 2  .or.  fit_mode .eq. 3 ) then

         CALL M_MRQCOF_LOGLIKE  ( fit_mode, NUM_DET, NUM_CHAN, NUM_EBINS,                                         &
                 CHAN_OFFSET, CHAN_WIDTH, OBS_CRATE, BACK_CRATE, LIVE_TIME, PARAM, PARAM_VARY, NUM_VARY,          &
                 NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE, NUM_PARAM_OF_TERM, USE_DET, FIT_CHAN,               &
                 PHOT_WIDTH, DRM, FIRST_NONZERO, TERM_USED, PHOT_EVAL, NUME_TO_USE, ALPHA, BETA, statistic, Caller_Error )
         if ( Caller_Error /= 0 )  then
            ErrNum = 4200000 + Caller_Error
            RETURN
         end if
         old_statistic = statistic

      end if


      ERROR = M_validate_matrix ( NUM_VARY, ALPHA )
      if (ERROR) then
         ErrNum = 33000
         write (6, *) 'M_MRQMIN: initial calculation of ALPHA fails'
         return
      end if
      ERROR = M_validate_vector ( NUM_VARY, BETA )
      if (ERROR) then
         ErrNum = 34000
         write (6, *) 'M_MRQMIN: initial calculation of BETA fails'
         return
      end if


   END IF     ! end first call initializations, ALAMDA < 0





   !  Regular case -- for ALAMDA > 0  (which also includes ALAMBA = 0 since that case replaces the value of ALAMDA):
   !  a fit iteration -- try to improve the statistic with a better solution for the parameter values:


   !  Alter linearized fitting matrix, by augmenting its diagonal elements:

   ALPHA_TRY (1:NUM_VARY, 1:NUM_VARY) = ALPHA (1:NUM_VARY, 1:NUM_VARY)

   DO INDEX_1=1, NUM_VARY
      ALPHA_TRY (INDEX_1, INDEX_1) = ALPHA (INDEX_1, INDEX_1) * (1.0_mfit_DoubleReal + ALAMDA)
   END DO


   if ( btest (DebugMask, 2) )  call Output_Condition_Number ( "Nonlinear fit", NUM_VARY, MAX_PARAM, ALPHA_TRY )



   !  Solve the linear system ALPHA_TRY * DELTA_PARAM = BETA for DELTA_PARAM -- DELTA_PARAM is the parameter changes to make:

   status = solve_linear_system_with_gsl  ( NUM_VARY, MAX_PARAM, ALPHA_TRY, BETA, DELTA_PARAM )

   if ( status /= 0 ) then
      ErrNum = 40000 + status
      write (6, '( / "Non-Linear fit: failed to solve linear system: status=", I6 / )' )  STATUS
      return
   end if



   !  Setup new trial parameters PARAM_TRY as PARAM + DELTA_PARAM.
   !  However, for robustness in some cases we limit the size of DELTA_PARAM.
   !  This keeps a step from jumping too far and making a mistake, at the cost
   !  of taking more iterations to converge.
   !  We loop through the varying parameters via looping JTERM and KPARAM and
   !  testing PARAM_VARY.   INDEX counts the varying parameters.

   INDEX = 0
   DO JTERM=1, NUM_TERMS_AVAIL
      DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)

         IF (PARAM_VARY (JTERM, KPARAM)) THEN
            INDEX = INDEX + 1

            !  next IF statement added to make program more robust: it prevents
            !  any parameter from changing, per iteration, by the LARGER
            !  of REL_CHANGE_LIMIT * current parameter value OR
            !  ABS_CHANGE_LIMIT.    If both are zero no change limit is
            !  applied.   If only one is zero the other is used.   If only
            !  REL_CHANGE_LIMIT is specified for a parameter, then that
            !  parameter can never change sign!   These limits make the
            !  program more robust by preventing an excessively large parameter
            !  change which make jump into the never-never land of parameter
            !  space, i.e. too far from the correct local minimum to converge
            !  to it.

            CHANGE_LIMIT = MAX (REL_CHANGE_LIMIT (JTERM, KPARAM) * ABS (PARAM (JTERM, KPARAM)), ABS_CHANGE_LIMIT (JTERM, KPARAM))
            IF (CHANGE_LIMIT .GT. 0.0 .AND. ABS (DELTA_PARAM (INDEX)) .GT. CHANGE_LIMIT) THEN
               SIGN = +1.
               IF (DELTA_PARAM (INDEX) .LT. 0.0)  SIGN = -1.
               DELTA_PARAM (INDEX) = SIGN * CHANGE_LIMIT

               !  Since Band's GRB model (terms 5 & 6) requires beta < alpha,
               !  we prevent alpha and/or beta changing in a way that would
               !  cause beta >= alpha.   We restrict beta to increases of 1/3 of
               !  (alpha - beta) and alpha to decreases of 1/3 of (alpha- beta).
               !  Note that the sum of the two fractions is < 1.
               !  Alpha is the 3rd parameter, beta the 4th:

               IF (JTERM .EQ. 5) THEN
                  DELTA_GREEK = ABS (PARAM (5, 3) - PARAM (5, 4))
                  IF (KPARAM .EQ. 3) THEN
                     IF (DELTA_PARAM (INDEX) .LT. 0.0)  DELTA_PARAM (INDEX) = MAX ( DELTA_PARAM (INDEX), -DELTA_GREEK / 3. )
                  END IF
                  IF (KPARAM .EQ. 4) THEN
                     IF (DELTA_PARAM (INDEX) .GT. 0.0)  DELTA_PARAM (INDEX) = MIN ( DELTA_PARAM (INDEX),  DELTA_GREEK / 3. )
                  END IF
               END IF                        ! special code for Band model 5

               IF (JTERM .EQ. 6) THEN
                  DELTA_GREEK = ABS (PARAM (6, 3) - PARAM (6, 4))
                  IF (KPARAM .EQ. 3) THEN
                     IF (DELTA_PARAM (INDEX) .LT. 0.0)  DELTA_PARAM (INDEX) = MAX ( DELTA_PARAM (INDEX), -DELTA_GREEK / 3. )
                  END IF
                  IF (KPARAM .EQ. 4) THEN
                     IF (DELTA_PARAM (INDEX) .GT. 0.0)  DELTA_PARAM (INDEX) = MIN ( DELTA_PARAM (INDEX), DELTA_GREEK / 3. )
                  END IF
               END IF                        ! special code for Band model 6

            END IF   ! apply change limit?


            !  Adjust the parameters with the steps in DELTA_PARAM, as calculated
            !  by solving the linear system ALPHA_TRY * DELTA_PARAM = BETA, then modified
            !  just above if the steps are too large:

            PARAM_TRY (JTERM, KPARAM) = PARAM (JTERM, KPARAM) + real ( DELTA_PARAM (INDEX), mfit_real )


         END IF                                               ! param vary ?
      END DO                                                  ! KPARAM
   END DO                                                     ! JTERM


   !  Calculate the new matrix ALPHA_TRY and vector BETA_TRY, and the statistic (CHISQ or LOG_LIKE)
   !  for the new trial parameters PARAM_TRY:

   if ( fit_mode .eq. 1 ) then

      CALL M_MRQCOF_CHISQ  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET, CHAN_WIDTH,               &
              OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, PARAM_TRY, PARAM_VARY, NUM_VARY,       &
              NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE, NUM_PARAM_OF_TERM, USE_DET,            &
              FIT_CHAN, PHOT_WIDTH, DRM, FIRST_NONZERO, TERM_USED, PHOT_EVAL, NUME_TO_USE,        &
              DATA_CR_VARI, ALPHA_TRY, BETA_TRY, statistic, Caller_Error )
      if ( Caller_Error /= 0 )  ErrNum = 4300000 + Caller_Error

   end if

   if ( fit_mode .eq. 2  .or.  fit_mode .eq. 3 ) then

      CALL M_MRQCOF_LOGLIKE ( fit_mode, NUM_DET, NUM_CHAN, NUM_EBINS,                                          &
              CHAN_OFFSET, CHAN_WIDTH, OBS_CRATE, BACK_CRATE, LIVE_TIME, PARAM_TRY, PARAM_VARY, NUM_VARY,      &
              NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE, NUM_PARAM_OF_TERM, USE_DET, FIT_CHAN,               &
              PHOT_WIDTH, DRM, FIRST_NONZERO, TERM_USED, PHOT_EVAL, NUME_TO_USE, ALPHA_TRY, BETA_TRY, statistic, Caller_Error )
      if ( Caller_Error /= 0 )  ErrNum = 4400000 + Caller_Error

   end if


   IF ( ErrNum /= 0 )  RETURN


   !  MSB 14 Jan 93: no longer consider chisq unchanged a failure--before
   !  this increased ALAMDA which flagged calling routine to do another
   !  iteration.  This change agrees with CURFIT (Bevington) implementation:

   !MSB  IF (CHISQ .LT. OCHISQ) THEN   replaced 14 Jan 1993
   !MSB  IF (CHISQ .LE. OCHISQ) THEN   replaced 1999 Oct 11

   if ( statistic .le. old_statistic ) then

      !  The new solution is better:
      !  make alamda smaller, copy ALPHA_TRY into ALPHA and BETA_TRY into BETA,
      !  make the trial parameters the current parameter values, i.e., PARAM_TRY into PARAM.
      !  Revised 1996 March 12: alamda = 1.E-30 is effectively zero.  Don't
      !  decrease below this value to avoid log10 (alamda) being illegal.

      IF (ALAMDA .GT. 1.E-30)  ALAMDA = 0.1 * ALAMDA

      old_statistic = statistic

      ALPHA (1:NUM_VARY, 1:NUM_VARY) = ALPHA_TRY (1:NUM_VARY, 1:NUM_VARY)
      BETA (1:NUM_VARY) = BETA_TRY (1:NUM_VARY)
      PARAM = PARAM_TRY

   ELSE

      !  new solution is worse!
      !  make alamda larger:

      ALAMDA = 10. * ALAMDA
      statistic = old_statistic

   END IF


   RETURN


   END SUBROUTINE M_MRQMIN


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_OUTPUT  ( DebugMask, NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_OFFSET,           &
          CHAN_ENERGY, CHAN_WIDTH, OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, RES_FRAC_AT511, RES_EXP,     &
          MFIT_VERSION, FIT_CHAN, USE_DET, MODEL_CNT_RATE, PHOT_OBS_RATE, PHOT_OBS_SIG,                      &
          NU_F_NU_DATA, NU_F_NU_SIG, MODEL_ENERGY, MODEL_PHOT_RATE, FIRST_EXTRAP_PLOT, LAST_EXTRAP_PLOT,     &
          TERM_USED, NUM_TERMS_AVAIL, DOF, NUM_VARY, FIT_ERR, NUM_ADD_TERMS, NUM_PARAM_OF_TERM,              &
          PARAM, PARAM_VARY, PARAM_UNCER, fit_mode, STATISTIC, REL_CONVERG, ABS_CONVERG, MAX_TRIES,          &
          MODEL_CR_VARI, DATA_CR_VARI, PHOT_MODEL_BYCHAN, ALPHA, COVAR )


   use MFIT_parameters



   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH/MSFC ES-62, 21 September 1992, revised 13 June 1994



   !                     ***  Input arguments:  ***

   integer (kind=mfit_integer), intent (in) :: DebugMask

   !  the number of detectors.   Note that "a detector" = "a DRM", e.g.,
   !  summed data such as MER counts as 1 detector:
   integer (kind=mfit_integer), intent (in) :: NUM_DET

   !  the number of data channels:
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_CHAN

   !  the number of energy bins on the input side of the DRM.  The photon
   !  model is evaluated for these bins:
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS

   !  the channel number of the first channel passed, used to label output:
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET

   !  the center energies of each channel, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_ENERGY

   !  the energy widths of each channel, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH

   !  the total (not background subtracted) observed count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE

   !  the model predicted background count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE

   !  the uncertainty (sigma) of the model background count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG

   !  the livetime, seconds:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME

   !  fractional energy resolution of detector = resolution FWHM / energy is
   !  approximated by a power law: RES_FRAC_AT511 * (energy / 511.) ** RES_EXP:
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_FRAC_AT511
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_EXP

   !  the version of this subroutine and its subroutine:
   CHARACTER (len=VERSION_LEN), intent (in) :: MFIT_VERSION

   !  the range of channels included in the fit:
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN

   !  the detectors included in the fit:
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET

   !  the model count rate, units counts/s-keV:
   !  obtained from the model photon rate by multiplying with the DRM:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1), intent (in) :: MODEL_CNT_RATE

   !  a model dependent quantity: the data converted into a photon rate:
   !  photons/s-cm^2-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: PHOT_OBS_RATE

   !  the uncertainties, sigma, on the previous variable:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: PHOT_OBS_SIG

   !  the model derived photon rate data in nu-Fnu units: keV^2 / s-cm^2-keV.
   !  this, like PHOT_OBS_RATE, is a model dependent quantity.
   !  it is d(energy flux) / d(log E).
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: NU_F_NU_DATA

   !  the uncertainties, sigma, on the previous variable:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: NU_F_NU_SIG

   !  an array listing the energies for which the photon model quantitites,
   !  i.e., MODEL_PHOT_RATE and NU_F_NU_MODEL, are output:
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: num_det-1), intent (in) :: MODEL_ENERGY

   !  the model photon rate, photons / s-cm^2-keV.   Term 0 is the total
   !  rate.   Also given is the rates of the individual model terms:
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: MAX_TERMS, 0: num_det-1), intent (in) :: MODEL_PHOT_RATE

   !  lists the portions of MODEL_ENERGY, MODEL_PHOT_RATE & NU_F_NU_MODEL
   !  used and available for plotting.   If not all channels were fit, this
   !  is an extension (extrapolation) of the MODEL_PLOT range:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: FIRST_EXTRAP_PLOT
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: LAST_EXTRAP_PLOT

   !  value is 1 if a term was included in the model:
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED

   !  the total number of model terms built into MFIT (available):
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL

   !  the degrees-of-freedom of the fit:
   integer (kind=mfit_integer), intent (in) :: DOF

   !  the number of varying parameters in the fit:
   integer (kind=mfit_integer), intent (in) :: NUM_VARY

   !  indicates whether an error occured.   Zero means no error:
   integer (kind=mfit_integer), intent (in) :: FIT_ERR


   !                         "NEW" ARGUMENTS!

   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS

   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM

   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM

   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY

   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_UNCER

   !  1 = chisq, 2 = likelihood, 3 = Castor C statistic:
   integer (kind=mfit_integer), intent (in) :: fit_mode
   
   real (kind=mfit_DoubleReal), intent (in) :: STATISTIC

   real (kind=mfit_real), intent (in) :: REL_CONVERG

   real (kind=mfit_real), intent (in) :: ABS_CONVERG

   integer (kind=mfit_integer), intent (in) :: MAX_TRIES

   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: MODEL_CR_VARI

   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DATA_CR_VARI

   !  the model photon rate for each data channel (not DRM photon bin):
   !  photons/s-cm^2-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: PHOT_MODEL_BYCHAN

   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (in) :: ALPHA
   real (kind=mfit_DoubleReal), dimension (MAX_PARAM, MAX_PARAM), intent (in) :: COVAR


   !                   *** end of arguments ***


   !               ***  Internal variables:  ***

   integer :: i
   logical :: flag


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   ! +    +    +    +    +    WRITE ASCII FIT INFO    +    +    +    +    +    +


   !  This command writes information about the fit to an ASCII file OR to the screen.


   flag = .FALSE.
   do i=9, 11
      if ( btest (DebugMask, i) )  flag = .TRUE.
   end do
   do i=16, 18
      if ( btest (DebugMask, i) )  flag = .TRUE.
   end do
   IF ( flag ) THEN
      CALL M_RESULTS_1_OUT ( DebugMask, NUM_DET, NUM_CHAN, NUM_EBINS, FIT_CHAN,  USE_DET,    &
               NUM_TERMS_AVAIL, NUM_ADD_TERMS, NUM_PARAM_OF_TERM, TERM_USED, PARAM_VARY, PARAM, NUM_VARY,       &
               fit_mode, STATISTIC, DOF, PARAM_UNCER, MFIT_VERSION,                                             &
               FIT_ERR, RES_FRAC_AT511, RES_EXP, REL_CONVERG, ABS_CONVERG, MAX_TRIES,                           &
               CHAN_ENERGY, CHAN_WIDTH, CHAN_OFFSET )
   END IF


   IF ( btest (DebugMask, 12)  .OR.  btest (DebugMask, 19) )  THEN
      CALL M_COVAR_OUT  ( DebugMask, NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, TERM_USED, PARAM_VARY,    &
                NUM_VARY, ALPHA, COVAR, MFIT_VERSION )
   END IF


   IF ( btest (DebugMask, 20) )  THEN
      CALL M_RESULTS_2_OUT  ( NUM_DET, NUM_CHAN, CHAN_OFFSET, CHAN_ENERGY, CHAN_WIDTH, USE_DET, FIT_CHAN,    &
                NUM_TERMS_AVAIL, TERM_USED, OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, MODEL_CNT_RATE,            &
                PHOT_OBS_RATE, PHOT_OBS_SIG, FIRST_EXTRAP_PLOT, LAST_EXTRAP_PLOT, MODEL_ENERGY, MODEL_PHOT_RATE,    &
                MODEL_CR_VARI, DATA_CR_VARI, NU_F_NU_DATA, NU_F_NU_SIG, PHOT_MODEL_BYCHAN, DebugMask, MFIT_VERSION )
   END IF


   RETURN                                            ! *** *** *** *** ***

   END SUBROUTINE M_OUTPUT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   !  NAME CHANGED FROM KN_MODEL TO M_BRAINERD BY M. BRIGGS.
   !  ALSO:  FABS --> ABS.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  This codes was written on March 4, 1993, by Jerome James Brainerd.
   !  Modified to ( 2 + delta )/tau and tau
   !  on July 16, 1996, by Jerome James Brainerd.

   !  In this file is the source code for the program kn_model, which
   !  produces a spectrum by passing a power law spectrum through a scattering medium.

   function M_BRAINERD  ( energy, normalize, deltauratio, tau, z, metal )

   real (kind=mfit_real) :: M_BRAINERD

   !  Input arguments:
   real (kind=mfit_real), intent (in) :: energy         ! Photon energy in keV.
   real (kind=mfit_real), intent (in) :: normalize      ! Normalization of curve.  This value is returned at 100 keV.
   real (kind=mfit_real), intent (in) :: deltauratio    ! ( delta + 2 )/tau.  A parameter setting the
                                                        ! peak of the nu F_nu curve for z = 0.
                                                        ! delta is the index of the unscattered power law.
   real (kind=mfit_real), intent (in) :: tau            ! Thomson optical depth,
   real (kind=mfit_real), intent (in) :: z              ! the cosmological redshift z,
   real (kind=mfit_real), intent (in) :: metal          ! the metalicity in units of the solar abundance.


   !  internal variables:
   real (kind=mfit_real) :: EXP_ARG
   integer (kind=mfit_integer) :: i
   real (kind=mfit_real) :: en_norm
   real (kind=mfit_real) :: energy0, epsilon, eps2m
   real (kind=mfit_real) :: epsinv, eps2, eps2p1, ep1
   real (kind=mfit_real) :: s1, s2, s3
   real (kind=mfit_real) :: kn_cross1, kn_cross2, kn_cross3, kn_cross, kn_norm
   real (kind=mfit_real) :: photo_elec

   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   if ( energy .le. 0. ) then
      M_BRAINERD = 0.
      write (6, *) "Negative energy received by function M_BRAINERD!", energy
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
      kn_cross2 = log( eps2p1 ) - eps2/eps2p1
      kn_cross3 = epsinv * epsinv * ( eps2*( 1. + ep1*ep1/eps2p1 ) - 2.*ep1* log( eps2p1 ) )
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
      kn_cross2 = log( eps2p1 ) - eps2/eps2p1
      kn_cross3 = epsinv*epsinv * ( eps2*( 1. + ep1*ep1/eps2p1 ) - 2.*ep1* log( eps2p1 ) )
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
   EXP_ARG = (deltauratio * tau - 2.) * log (energy/en_norm) - tau * (kn_cross - kn_norm + photo_elec)
   EXP_ARG = MIN (EXP_ARG, 50.)
   M_BRAINERD = normalize * exp (EXP_ARG)


   return

   end function M_BRAINERD


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_IS_CROSS  ( E )

   real (kind=mfit_real) :: M_IS_CROSS


   !  Calculates interstellar photoelectric cross-section per hydrogen atom
   !  according to Morrison and McCammon, 1983, ApJ, 270: 119--122.
   !  Not so accurate above 10 keV due to neglect of Compton scattering.
   !  Above 10 keV, uses my extrapolation of 10 keV photoelectric.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH/MSFC, 1996 Jan. 18.


   !  Input argument:
   real (kind=mfit_real), intent (in) :: E            ! energy in keV to calculate cross-section for


   !  Output solely by function return.

   !  Units of cross section are 1E-24 cm^2.
   !  The absorption is done as Transmitted Flux = Unabsorbed Flux X
   !  exp -(D * M_IS_CROSS_E).
   !  With D the column density in units of 1E+24 H atom (equivalent) cm^-2,
   !  the argument of the exponential is dimensionless.
   !  MSB, 1999 Oct 4.


   !  Internal variables:

   integer (kind=mfit_integer) :: I

   !  Table 2 of Morrison and McCammon.    ! E_SEG is lower edges of E ranges of segments
   real (kind=mfit_real), PARAMETER :: E_SEG (14) = (/                         &
     0.030,   0.100,   0.284,   0.400,   0.532,   0.707,    0.867,    1.303,   &
     1.840,   2.471,   3.210,   4.038,   7.111,   8.331  /)

   real (kind=mfit_real), PARAMETER :: C0 (14) = (/                            &
      17.3,    34.6,    78.1,    71.4,    95.5,   308.9,    120.6,    141.3,   &
     202.7,   342.7,   352.2,   433.9,   629.0,   701.2  /)

   real (kind=mfit_real), PARAMETER :: C1 (14) = (/                            &
     608.1,   267.9,    18.8,    66.8,   145.8,  -308.6,    169.3,    146.8,   &
     104.7,    18.7,    18.7,    -2.4,    30.9,    25.2  /)

   real (kind=mfit_real), PARAMETER :: C2 (14) = (/                            &
    -2150.,  -476.1,     4.3,   -51.4,   -61.1,   294.0,    -47.7,    -31.5,   &
     -17.0,     0.0,     0.0,    0.75,     0.0,     0.0  /)


   ! *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *


   IF (E .LT.  0.030) THEN
      WRITE (6, *) 'ERROR: M_IS_CROSS: E TOO SMALL:', E
      STOP
   END IF

   IF (E .GE. 10.0) THEN
      I = 14
      M_IS_CROSS = (C0(I) + C1(I)*10. + C2(I)*100.) / E**3
      RETURN
   END IF

   !  E is between 0.030 and 10 keV: find place in table:

   I = 1
   DO WHILE (I .LE. 13 .AND. E .GT. E_SEG (I+1))
      I = I + 1
   END DO

   M_IS_CROSS = (C0(I) + C1(I)*E + C2(I)*E**2) / E**3


   RETURN

   END FUNCTION M_IS_CROSS


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_IS_Z_CROSS  ( E_OBS, Z )

   real (kind=mfit_real) :: M_IS_Z_CROSS


   !  Calculates interstellar photoelectric cross-section per hydrogen atom
   !  according to Morrison and McCammon, 1983, ApJ, 270: 119--122.
   !  Not so accurate above 10 keV due to neglect of Compton scattering.
   !  Above 10 keV, uses my extrapolation of 10 keV photoelectric.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Revised by MSB 1999 Sept. 30 to do interstellar absorption for
   !  absorbing material at cosmological distances.

   !  Michael S. Briggs, UAH/MSFC, 1996 Jan. 18.


   !  Input arguments:
      real (kind=mfit_real), intent (in) :: E_OBS      ! energy of photon in Observer's frame, keV
      real (kind=mfit_real), intent (in) :: Z          ! cosmological redshift of source


   !  Output solely by function return.

   !  Units of cross section are 1E-24 cm^2.
   !  The absorption is done as Transmitted Flux = Unabsorbed Flux X
   !  exp -(D * M_IS_CROSS_E).
   !  With D the column density in units of 1E+24 H atom (equivalent) cm^-2,
   !  the argument of the exponential is dimensionless.
   !  MSB, 1999 Oct 4.


   !  Internal variables:

   real (kind=mfit_real) :: E            ! energy in keV to calculate cross-section for

   integer (kind=mfit_integer) :: I

   !  Table 2 of Morrison and McCammon.    ! E_SEG is lower edges of E ranges of segments
   real (kind=mfit_real), PARAMETER :: E_SEG (14) = (/                         &
     0.030,   0.100,   0.284,   0.400,   0.532,   0.707,    0.867,    1.303,   &
     1.840,   2.471,   3.210,   4.038,   7.111,   8.331  /)

   real (kind=mfit_real), PARAMETER :: C0 (14) = (/                            &
      17.3,    34.6,    78.1,    71.4,    95.5,   308.9,    120.6,    141.3,   &
     202.7,   342.7,   352.2,   433.9,   629.0,   701.2  /)

   real (kind=mfit_real), PARAMETER :: C1 (14) = (/                            &
     608.1,   267.9,    18.8,    66.8,   145.8,  -308.6,    169.3,    146.8,   &
     104.7,    18.7,    18.7,    -2.4,    30.9,    25.2  /)

   real (kind=mfit_real), PARAMETER :: C2 (14) = (/                            &
    -2150.,  -476.1,     4.3,   -51.4,   -61.1,   294.0,    -47.7,    -31.5,   &
     -17.0,     0.0,     0.0,    0.75,     0.0,     0.0  /)


! *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *


   E = E_OBS * (1. + Z)

   IF (E .LT.  0.030) THEN
      WRITE (6, *) 'ERROR: M_IS_Z_CROSS: E TOO SMALL:', E
      STOP
   END IF

   IF (E .GE. 10.0) THEN
      I = 14
      M_IS_Z_CROSS = (C0(I) + C1(I)*10. + C2(I)*100.) / E**3
      RETURN
   END IF

   !  E is between 0.030 and 10 keV: find place in table:

   I = 1
   DO WHILE (I .LE. 13 .AND. E .GT. E_SEG (I+1))
      I = I + 1
   END DO

   M_IS_Z_CROSS = (C0(I) + C1(I)*E + C2(I)*E**2) / E**3


   RETURN

   END FUNCTION M_IS_Z_CROSS


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_OTTB_INTG  ( E, params )  bind ( C, name="m_ottb_intg" )

   use iso_c_binding

   real (kind=mfit_DoubleReal) :: M_OTTB_INTG

   real (kind=mfit_DoubleReal), VALUE, intent (in) :: E
   type (c_ptr), intent (in) :: params     ! not used


   !  function to be numerically integrated in order to evaluate the photon number flux OTTB mode.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / ES-84 MSFC.


! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   M_OTTB_INTG = EXP (-E) / E


   RETURN

   END FUNCTION M_OTTB_INTG


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_PHOTONS_MODEL  ( NUM_DET, ACTUAL_DIM, PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,   &
                PARAM, TERM_USED, DO_TERM, USE_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,                &
                MODEL_PHOT_RATE, MULT_FLAG, MULT_TERMS, ErrNum )


   !  Subroutine to evaluate model differential flux at the ENERGYs.

   !  The model terms are evaluated in three ways:
   !  1) additive/subtractive continuum terms,
   !       these are evaluated 1, 3 or 7 times per photon bin, as instructed
   !       by NUME_TO_USE, and then averaged,
   !  2) additive/subtractive line terms,
   !       these Gaussian lines use the error function to automatically
   !       calculate the mean of the function over the photon bin, so they
   !       only use the bin edges, PHOT_EVAL (6, ...) & PHOT_EVAL (7, ...),
   !  3) multiplicative terms:
   !       a) the detector renormalization function is evaluated once per
   !          bin (like #2) since it is energy independent,
   !       b) the other multiplicative terms (like #1) are evaluated 1, 3, or
   !          7 times per bin.

   !  Term 0, if calculated = (sum of all additive terms) X (product of all
   !  multiplicative terms).
   !  The individual additive terms are not multiplied by the multiplicative
   !  terms.   Any extra variable, MULT_TERMS, holds the product of all the
   !  multiplicative terms.

   !  If a multiplicative term is in use and the model is evaluated multiple
   !  times per photon bin, an approximation is used: the additive terms
   !  are evaluated & averaged, then each multiplicative term is evaluated
   !  & averaged, and then they are multiplied.   The more exact approach
   !  would be to do the averaging after the multiplication.    This
   !  approximation is good if either the additive terms or the multiplicative
   !  term(s) are approximately constant across the photon bin.

   !  It doesn't make much sense to call this term with DO_TERM = term # of
   !  a multiplicative term, since a multiplicative term is pretty useless
   !  without an additive/subtractive term to multiply.

   !  If you modify this subroutine be sure to modify (as needed):
   !      MFIT_FUNC.INFO (file),
   !      M_MODEL_DERIV (subroutine),
   !      M_GAUSS_SEARCH (subroutine).

   !  Major changes:

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  MSB 19 June 1993: changed arguments: now receive list of 7 energies
   !  per photon bin (PHOT_EVAL) and number of energies per bin (NUME_TO_USE)
   !  (=1, 3, or 7) to evaluate model for.  For continuum terms, evaluate the
   !  model at the requested number of energies per bin and take the average.
   !  For line models, which autmotically calculate the mean of the function
   !  over the bin, use the 6th and 7th energies, which are the photon bin
   !  edges.  Also change logic for deciding when Gaussian terms need to be
   !  evaluated.   Also now use TERM_USED to decide what terms are active.
   !  MSB 27 July 1993: added multiplicative terms, changed some of the
   !  previous terms.
   !  MSB 17 March 1996: got the multiplicative terms to work.
   !  MSB 10 June 1996: added some additional terms, renumbered many.
   !  MSB 11 Dec 1996: add multiplicative broken power laws.
   
   !  MSB, 2009 July 12:
   !  Removed one restriction that was probably never necessary.
   !  Made two changes that are possible because "modern" processors are more robust w.r.t.
   !  numeric irregularities, such as underflow and overlflow.   As I remember, on the VAX,
   !  even underflow was a fatal exception!  Now underflow causes sub-normal values or zero,
   !  and overflow +infinity.   The first is no problem what so ever, and the second will
   !  cause a recoverable error that will identify that something is wrong with the fit,
   !  which is probably desirable.  So:
   !  1) Removed some restrictions on the values of the parameters alpha and beta of
   !  the Band GRB function: ALPHA_6, BETA_5 and BETA_6.
   !  No longer require that alpha > -2 for the original formula of the Band function
   !  (this was probably never necessary).
   !  No longer require that Beta > -10 for both versions of the Band function -- this
   !  was done to avoid underflow -- no longer necessary.
   !  2) Change the implementation of several functions to directly code the functions,
   !  rather than having an intermediate calcualtion in the log of the function,
   !  and using a "min" to truncate the value before taking the exponent to obtain
   !  the function -- this was done to avoid overflow.   The direct implementation is
   !  more readable, and as commented above, identifying peculiar results is probably
   !  beneficial.   Functions that were recoded: 
   !     1 = power law,
   !     2 = broken power law,
   !     (3 = double broken power law was already directly coded)
   !     5 = Epeak version of Band's GRB function
   !     6 = original version of Band's GRB function
   !     7 = "Comptonized", Epeak formulation
   !     8 = "Comptonized", cutoff energy formulation


   use iso_c_binding, only: c_funptr, c_funloc

   use MFIT_parameters

   use GSL_interfaces


   !  Input variables:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), intent (in) :: ACTUAL_DIM                        ! physical dim next 2 arrays
   !  energies in keV, 7 per photon bin:
   real (kind=mfit_real), dimension (7, 0: ACTUAL_DIM -1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   !  number of the energies to use:
   integer (kind=mfit_integer), dimension (0: ACTUAL_DIM -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE
   !  bin range to calculate photon flux for:
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: LAST_PHOT_BIN
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   integer (kind=mfit_integer), intent (in) :: DO_TERM      ! if zero, calculate all terms and their sum,
                                        ! if non-zero just do specified term
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS


   !  Output variables:
   !  the next array is either MODEL_PHOT_RATE or MODEL_PHOT_RATE_WS depending on which was passed as an argument:
   real (kind=mfit_real), dimension (0: ACTUAL_DIM -1, 0: MAX_TERMS, 0: num_det-1), intent (out) :: MODEL_PHOT_RATE
                               ! the calculated results for each chan & term.
                               ! Term 0 is the sum of the separate terms.
                               ! units: photons/s-cm2-keV

   !  flags whether any multiplicative terms were calculated:
   logical (kind=mfit_logical), intent (out) :: MULT_FLAG

   !  the product of all of the multiplicative terms:
   real (kind=mfit_real), dimension (0: ACTUAL_DIM -1, 0: num_det-1), intent (out) :: MULT_TERMS
   
   integer (kind=mfit_integer), intent (out) :: ErrNum


   !  Internal variables:

   interface

      function MFIT_USER_FUNC_A ( energy, p )
         use MFIT_kinds
         real (kind=mfit_real) :: MFIT_USER_FUNC_A
         real (kind=mfit_real), intent (in) :: energy
         real (kind=mfit_real), dimension (1:10), intent (in) :: p
      end function MFIT_USER_FUNC_A

      function MFIT_USER_FUNC_B ( energy, p )
         use MFIT_kinds
         real (kind=mfit_real) :: MFIT_USER_FUNC_B
         real (kind=mfit_real), intent (in) :: energy
         real (kind=mfit_real), dimension (1:10), intent (in) :: p
      end function MFIT_USER_FUNC_B

   end interface


   integer (kind=mfit_integer) :: E_CNT
   integer (kind=mfit_integer) :: JTERM, LDET, PBIN

   real (kind=mfit_real) :: ENERGY
   real (kind=mfit_real) :: RATIO
   real (kind=mfit_real) :: GFAC
   real (kind=mfit_real) :: GSIGMA
   real (kind=mfit_real) :: PULL
   real (kind=mfit_real) :: ARG_LEFT, ARG_RIGHT

   real (kind=mfit_real) :: GAUSS, LORENTZ
   real (kind=mfit_real) :: LOG10_FWHM
   real (kind=mfit_real) :: ARG, B, M, PCOSH
   real (kind=mfit_real) :: ARG_PIV, PCOSH_PIV
   real (kind=mfit_real) :: ALPHA_5, BETA_5
   real (kind=mfit_real) :: ALPHA_6, BETA_6
   real (kind=mfit_real) :: AMP_19, AMP_20, AMP_22

   type (c_funptr) :: integrand_ptr
   real (kind=mfit_DoubleReal) :: LOW_LIM, UP_LIM, I_OTTB
   real (kind=mfit_DoubleReal) :: ErrEst
   integer (kind=mfit_integer) :: status
   real (kind=mfit_real) :: result
   integer (kind=mfit_integer) :: error_arg

   real (kind=mfit_real), parameter :: PI = 3.141592
   real (kind=mfit_real), parameter :: GCONST = 2.35482


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   
   ErrNum = 0


   !  Predefintion of certain quantities used by Terms 5 & 6: Band's GRB model:
   !  We impose the requirement that BETA < ALPHA in order to prevent the
   !  calculation from being illegal.
   !  We impose the requirement that BETA > -10. to prevent math lib problems.
   !  Revised by MSB 1997 Jan 21: require ALPHA > -2 to prevent log (negative).
   
   !  Revised 2009 July 12 by MSB: relaxed some of the restrictions, since
   !  "modern" processors are more robust, and rather than crashing for
   !  underflow or overflow, silently generate subnormals, zero or +inf, etc.
   !  Certainly underflow is now fully acceptable, so we can allow beta to
   !  assume large negative values.
   !  The requirements on alpha & beta come from the function "making sense" --
   !  that the function be convex upwards and that the break energy or Epeak be
   !  positive.  The requirement that "alpha > -2" is only for the Epeak formulation,
   !  so it has been deleted from the original formulation.

   ALPHA_5 = PARAM (5, 3)
   ALPHA_5 = MAX (ALPHA_5, -1.9999)
   BETA_5 = MIN (PARAM (5, 4), ALPHA_5 - 1.E-5)
   !BETA_5 = MAX (BETA_5, -10.)
   ALPHA_6 = PARAM (6, 3)
   !ALPHA_6 = MAX (ALPHA_6, -1.9999)
   BETA_6 = MIN (PARAM (6, 4), ALPHA_6 - 1.E-5)
   !BETA_6 = MAX (BETA_6, -10.)


   !  Precalculation of certain energy-independent quantities used by
   !:  TERM 4: [Michael Briggs'] Smoothly broken power law:
   !:  it has a non-sharp single break:
   !:  parameter 1 = amplitude, 2 = pivot energy (keep fixed),
   !:  3 = index below break energy, 4 = break energy,
   !:  5 = break scale (in decades, i.e. log10 (keV)), 6 = index above break:

   PCOSH_PIV = 0.0
   B = 0.0
   M = 0.0
   IF (TERM_USED (4) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 4)) THEN
      B = (PARAM (4, 3) + PARAM (4, 6)) / 2.
      M = (PARAM (4, 6) - PARAM (4, 3)) / 2.
      ARG_PIV = LOG10 (PARAM (4, 2) / PARAM (4, 4)) / PARAM (4, 5)
      !:  PCOSHP: evaluate [cosh (arg)]**(m * param (4, 5)) carefully via
      !  its log in order to avoid overflow:
      IF (ARG_PIV .LT. -6.0) THEN
         PCOSH_PIV = M * PARAM (4, 5) * (-ARG_PIV - LOG (2.0))
      ELSE
         IF (ARG_PIV .GT. +6.0) THEN
            PCOSH_PIV = M * PARAM (4, 5) * (ARG_PIV - LOG (2.0))
         ELSE
            PCOSH_PIV = M * PARAM (4, 5) * (LOG ((EXP (ARG_PIV) + EXP(-ARG_PIV)) / 2.0))
         END IF
      END IF
   END IF

   !  Precalculation of const used in term 19.  Value is independent of energy, so remove from loop:

   AMP_19 = 0.0
   IF (TERM_USED (19) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 19)) THEN
      IF (PARAM (19, 2) .NE. -1.0) THEN
         AMP_19 = PARAM (19, 1) * (PARAM (19, 2) + 1.) /   &
                (PARAM (19, 4) ** (PARAM (19, 2) + 1.) -   &
                 PARAM (19, 3) ** (PARAM (19, 2) + 1.))
      ELSE
         AMP_19 = PARAM (19, 1) / LOG (PARAM (19, 4) / PARAM (19, 3))
      END IF
   END IF

   !  Precalculation of const used in term 20.  Value is independent of energy, so remove from loop:

   AMP_20 = 0.0
   IF (TERM_USED (20) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 20)) THEN
      IF (PARAM (20, 2) .NE. -2.0) THEN
         AMP_20 = PARAM (20, 1) * (PARAM (20, 2) + 2.) /    &
                (PARAM (20, 4) ** (PARAM (20, 2) + 2.) -    &
                 PARAM (20, 3) ** (PARAM (20, 2) + 2.))
      ELSE
         AMP_20 = PARAM (20, 1) / LOG (PARAM (20, 4) / PARAM (20, 3))
      END IF
   END IF

   !  Precalculation of const used in term 22.  Value is independent of energy, so remove from loop:

   AMP_22 = 0.0
   IF (TERM_USED (22) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 22)) THEN
      AMP_22 = PARAM (22, 1) / (PARAM (22, 2) *     &
        (EXP (-PARAM (22, 3) / PARAM (22, 2)) -     &
         EXP (-PARAM (22, 4) / PARAM (22, 2)) ))
   END IF

   !  Precalculation of integral used in term 21.   The integral does not
   !  depend on the energy for which we are evaluating the function, so it
   !  can be brought out of the loop.    It does depend on the parameters,
   !  obviously.
   I_OTTB = 0.0    ! to suppress incorrect compiler warning
   IF (TERM_USED (21) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 21)) THEN
      LOW_LIM = PARAM (21, 3) / PARAM (21, 2)
      UP_LIM = PARAM (21, 4) / PARAM (21, 2)

      integrand_ptr = c_funloc ( M_OTTB_INTG )
      status = integrate_using_gsl ( LOW_LIM, UP_LIM, 0.0_mfit_DoubleReal, 2.0E-5_mfit_DoubleReal, integrand_ptr, I_OTTB, ErrEst )
      if ( status /= 0 ) then
         ErrNum = status
         write (*, '( / "Numeric integration failure in subroutine M_PHOTONS_MODEL for OTTB factor.  status=", I6 / )' )  status
      end if

   END IF


!  *      *      *      *      *      *      *      *      *      *      *


   !  Zero out terms to be processed:
   !  If DO_TERM = 0, zero out all active terms; if DO_TERM .NE. 0,
   !  zero out selected term DO_TERM.
   !  Zeroing is essential: for most terms we sum into MODEL_PHOT_RATE & for
   !  some terms we only calculate the nonzero elements.

   IF (DO_TERM .EQ. 0) THEN
      DO LDET=0, NUM_DET - 1
         DO JTERM=0, NUM_TERMS_AVAIL
            IF (TERM_USED (JTERM) .EQ. 1) THEN
               DO PBIN=FIRST_PHOT_BIN (0, LDET), LAST_PHOT_BIN (0, LDET)
                  MODEL_PHOT_RATE (PBIN, JTERM, LDET) = 0.0
               END DO
            END IF
         END DO
      END DO
   ELSE
      DO LDET=0, NUM_DET - 1
         DO PBIN=FIRST_PHOT_BIN (0, LDET), LAST_PHOT_BIN (0, LDET)
            MODEL_PHOT_RATE (PBIN, DO_TERM, LDET) = 0.0
         END DO
      END DO
   END IF

   !  loop through all detectors and energy bins and possibly multiple
   !  energies per energy bin:

   !  each model term is evaluated if a standard IF statment is statisfied:
   !  evaluate each term JTERM and their sum if DO_TERM = 0, else just do
   !  the specified term JTERM = DO_TERM;
   !  also, we need only evaluate selected terms, as indicated by
   !  TERM_USED (JTERM) = 1.

   DO LDET=0, NUM_DET - 1

      IF (USE_DET (LDET)) THEN                            ! det used ?

         DO PBIN=FIRST_PHOT_BIN (0, LDET), LAST_PHOT_BIN (0, LDET)



         !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +
         !           ADDITIVE/SUBRACTIVE CONTINUUM TERMS:
         !  Note that the evaluations are summed into the output array: this is
         !  needed if the model is sampled more than once per photon bin
         !  (i.e. NUME_TO_USE .GT. 1) for the purpose of averaging over the bin.
         !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


            DO E_CNT=1, NUME_TO_USE (PBIN, LDET)

               ENERGY = PHOT_EVAL (E_CNT, PBIN, LDET)

               !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 1: power law: A * (energy/E0)**S:
               !:  param 2 is pivot energy E0  = fake param not to be varied:

               IF (TERM_USED (1) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 1)) THEN
               
                  MODEL_PHOT_RATE (PBIN, 1, LDET) = MODEL_PHOT_RATE (PBIN, 1, LDET) + PARAM (1, 1) *     &
                             (ENERGY / PARAM (1, 2)) ** PARAM (1, 3)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 2: broken power law, single break:
               !:  param 1 = amplitude, 2 = pivot energy = fake param not to be varied,
               !:  3 = index below break, param 4 = break E, 5 = index above break:

               IF (TERM_USED (2) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 2)) THEN
                  IF (ENERGY .LE. PARAM (2, 4)) THEN

                     MODEL_PHOT_RATE (PBIN, 2, LDET) = MODEL_PHOT_RATE (PBIN, 2, LDET) + PARAM (2, 1) *      &
                              (ENERGY / PARAM (2, 2)) ** PARAM (2, 3)
                              
                  ELSE

                     MODEL_PHOT_RATE (PBIN, 2, LDET) = MODEL_PHOT_RATE (PBIN, 2, LDET) + PARAM (2, 1) *      &
                               (PARAM (2, 4) / PARAM (2, 2)) ** PARAM (2, 3) * (ENERGY / PARAM (2, 4)) ** PARAM (2, 5)
                     
                  END IF
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
               !
               !:  TERM 3: broken power law, double break: (parameter #, parameter name):
               !:  (1 = amplitude), (2 = pivot energy = fake param not to be varied),
               !:  (3 = index < BE1), (4 = BE1), (5 = index > BE1), (6 = BE2),
               !:  (7 = index > BE2):

               IF (TERM_USED (3) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 3)) THEN

                  !  below 1st break:
                  IF (ENERGY .LE. PARAM (3, 4)) THEN
                     MODEL_PHOT_RATE (PBIN, 3, LDET) = MODEL_PHOT_RATE (PBIN, 3, LDET) +    &
                          PARAM (3, 1) * (ENERGY / PARAM (3, 2)) ** PARAM (3, 3)
                  END IF

                  ! between 1st and 2nd breaks:
                  IF (ENERGY .GT. PARAM (3, 4) .AND. ENERGY .LE. PARAM (3, 6)) THEN
                     MODEL_PHOT_RATE (PBIN, 3, LDET) = MODEL_PHOT_RATE (PBIN, 3, LDET) +     &
                          PARAM (3, 1) * (PARAM (3, 4) / PARAM (3, 2)) ** PARAM (3, 3) * (ENERGY / PARAM (3, 4)) ** PARAM (3, 5)
                  END IF

                  ! above 2nd break:
                  IF (ENERGY .GT. PARAM (3, 6)) THEN
                     MODEL_PHOT_RATE (PBIN, 3, LDET) = MODEL_PHOT_RATE (PBIN, 3, LDET) +     &
                          PARAM (3, 1) * (PARAM (3, 4) / PARAM (3, 2)) ** PARAM (3, 3) *     &
                              (PARAM (3, 6) / PARAM (3, 4)) ** PARAM (3, 5) * (ENERGY / PARAM (3, 6)) ** PARAM (3, 7)
                  END IF
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 4: [Michael Briggs'] Smoothly broken power law:
               !:  it has a non-sharp single break:
               !:  parameter 1 = amplitude, 2 = pivot energy (keep fixed),
               !:  3 = index below break energy, 4 = break energy,
               !:  5 = break scale (in decades, i.e. log10 (keV)), 6 = index above break:
               !:  See above for the precalculation of certain quantities that do not
               !:  depend upon energy.    Dividing the expresion by the value of the
               !:  function at the pivot energy (except for the amplitude) greatly reduces
               !:  the cross-correlation of parameters.

               IF (TERM_USED (4) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 4)) THEN
                  ARG = LOG10 (ENERGY / PARAM (4, 4)) / PARAM (4, 5)
               !: PCOSH: evaluate [cosh (arg)]**(m * param (4, 5)) carefully via
               !  its log in order to avoid overflow:
                  IF (ARG .LT. -6.0) THEN
                     PCOSH = M * PARAM (4, 5) * (-ARG - LOG (2.0))
                  ELSE
                     IF (ARG .GT. +4.0) THEN
                        PCOSH = M * PARAM (4, 5) * (ARG - LOG (2.0))
                     ELSE
                        PCOSH = M * PARAM (4, 5) * (LOG ((EXP (ARG) + EXP(-ARG)) / 2.0))
                     END IF
                  END IF
                  MODEL_PHOT_RATE (PBIN, 4, LDET) = MODEL_PHOT_RATE (PBIN, 4, LDET) +      &
                         PARAM (4, 1) * (ENERGY/PARAM (4, 2))**B * 10.**(PCOSH - PCOSH_PIV)
               !  was exp (pcosh - pcosh_piv) instead of 10**().
               !  changed on 1997 Nov 25 at request of R. Preece by M. Briggs to
               !  make index parameters actually indexes.
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 5: EPEAK Band's Model: with parameters 1 = amplitude, 2 = Epeak,
               !:  3 = alpha, & 4 = beta:
               !:  The modification is to use "Epeak" instead of E0:
               !:  [Note: Old E0 = Epeak / (2. + alpha)]
               !:  ALPHA_5 and BETA_5 are setup above:
               !:  ALPHA_5 = PARAM (5, 3), BETA_5 = MIN (PARAM (5, 4), ALPHA_5 - 1.E-6).
               !:  This prevents illegal operations and can be regarded as a minor
               !:  redefinition of the equation.

               IF (TERM_USED (5) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 5)) THEN

                  !     is E < (alpha - beta) * Epeak / (2. + alpha): ?:
                  IF (ENERGY .LE. (ALPHA_5 - BETA_5) * PARAM (5, 2) / (2. + ALPHA_5)) THEN
                  
                     MODEL_PHOT_RATE (PBIN, 5, LDET) = MODEL_PHOT_RATE (PBIN, 5, LDET) + PARAM (5, 1) *      &
                         (ENERGY / 100.)**ALPHA_5 * exp (-ENERGY * (2. + ALPHA_5) / PARAM (5, 2) )

                  ELSE
     
                     MODEL_PHOT_RATE (PBIN, 5, LDET) = MODEL_PHOT_RATE (PBIN, 5, LDET) + PARAM (5, 1) *               &
                          ( (ALPHA_5 - BETA_5) * PARAM (5, 2) / ( 100. * (2. + ALPHA_5) ) ) ** (ALPHA_5 - BETA_5) *   &
                          exp (BETA_5 - ALPHA_5) * (ENERGY / 100.) ** BETA_5
      
                  END IF
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 6: OLD Band's Model: with parameters 1 = amplitude, 2 = E0,
               !:  3 = alpha & 4 = beta:
               !:  ALPHA_6 and BETA_6 are setup above:
               !:  ALPHA_6 = PARAM (6, 3), BETA_6 = MIN (PARAM (6, 4), ALPHA_6 - 1.E-6).
               !:  This prevents illegal operations and can be regarded as a minor
               !:  redefinition of the equation.

               IF (TERM_USED (6) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 6)) THEN

                  ! is E < (alpha - beta) * E0 ?:
                  IF (ENERGY .LT. (ALPHA_6 - BETA_6) * PARAM (6, 2)) THEN
 
                     MODEL_PHOT_RATE (PBIN, 6, LDET) = MODEL_PHOT_RATE (PBIN, 6, LDET) + PARAM (6, 1) *      &
                            (ENERGY / 100.) ** ALPHA_6 * exp (-ENERGY / PARAM (6, 2))

                  ELSE

                     MODEL_PHOT_RATE (PBIN, 6, LDET) = MODEL_PHOT_RATE (PBIN, 6, LDET) + PARAM (6, 1) *       &
                          ( (ALPHA_6 - BETA_6) * PARAM (6, 2) / 100. ) ** (ALPHA_6 - BETA_6) *                &
                          exp (BETA_6 - ALPHA_6) * (ENERGY / 100.) ** BETA_6
                          
                  END IF
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 7: "Comptonized": P1 * exp (- E (P3+2.) / P2) * (E/P4)**P3:
               !:  parameters: P1 = amplitude, P2 = Epeak, P3 = index,
               !:  P4 = pivot energy (should be fixed at typically 100 keV):
               !:  Revised 2009 July 12 by MSB: no longer do the calculation via logs to avoid overflows.              

               IF (TERM_USED (7) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 7)) THEN
               
                  MODEL_PHOT_RATE (PBIN, 7, LDET) = MODEL_PHOT_RATE (PBIN, 7, LDET) + PARAM (7, 1) *                       &
                         exp (-ENERGY * (2. + PARAM (7, 3) ) / PARAM (7, 2) ) * (ENERGY / PARAM (7, 4) ) ** PARAM (7, 3)
                         
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 8: "Comptonized": P1 * exp (-E/P2) * (E/P4)**P3:
               !:  parameters: P1 = amplitude, P2 = E-folding energy, P3 = index,
               !:  P4 = pivot energy (should be fixed at typically 100 keV):
               !:  This is the "old" parameterization.               

               IF (TERM_USED (8) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 8)) THEN
                  
                  MODEL_PHOT_RATE (PBIN, 8, LDET) = MODEL_PHOT_RATE (PBIN, 8, LDET) + PARAM (8, 1) *        &
                        exp (-ENERGY / PARAM (8, 2)) * (ENERGY / PARAM (8, 4) ) ** PARAM (8, 3)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 9: J.J. Brainerd's model: central source has power law
               !:  spectrum which is modified by scattering in thick atmosphere; spectrum
               !:  is further modified by cosmological redshift.  parameters are:
               !:  P1 = amplitude, revised P2 = (delta + 2) / tau, P3 = delta = unscattered
               !:  power law index, P4 = optical depth tau, P5 = cosmological redshift z
               !:  and P6 = metalicity (solar = 1):

               IF (TERM_USED (9) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 9)) THEN
                  MODEL_PHOT_RATE (PBIN, 9, LDET) = MODEL_PHOT_RATE (PBIN, 9, LDET) +     &
                       M_BRAINERD (ENERGY, PARAM (9, 1), PARAM (9, 2), PARAM (9, 3), PARAM (9, 4), PARAM (9, 5))
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 10: Log Normal: w parameters: amplitude, mu, sigma.
               !  see pp. 79-80 of W. T. Eadie et al., Statistical Methods in Experimental Physics.

               IF (TERM_USED (10) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 10)) THEN
                  PULL = (LOG (ENERGY) - PARAM (10, 2)) / PARAM (10, 3)
                  IF (ABS (PULL) .LE. 12.)  MODEL_PHOT_RATE (PBIN, 10, LDET) = MODEL_PHOT_RATE (PBIN, 10, LDET) +    &
                         PARAM (10, 1) / (SQRT (2. * PI) * PARAM (10, 3)) / ENERGY * EXP (-0.5 * PULL**2)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 11: Gaussian (log10 E): w parameters: amplitude, centroid, log10 fwhm:
               !  different from log normal (see p. 79 of Eadie et al.) because it lacks
               !  factor 1/E.   With different parameter values, a log normal function
               !  can be obtained.

               IF (TERM_USED (11) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 11)) THEN
                  GSIGMA = PARAM (11, 3) / GCONST
                  GSIGMA = MAX (GSIGMA, 1.E-5)
                  PULL = (LOG10 (ENERGY) - LOG10 (PARAM (11, 2))) / GSIGMA
                  IF (ABS (PULL) .LE. 12.)  MODEL_PHOT_RATE (PBIN, 11, LDET) = MODEL_PHOT_RATE (PBIN, 11, LDET) +    &
                         PARAM (11, 1) / (GSIGMA * SQRT (2. * PI)) * EXP (-0.5 * PULL**2)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 12: Gaussian (log10 E) w changing width: with parameters:
               !:  amplitude, centroid, log10 fwhm at centroid,
               !:  slope vs log10 E of log10 fwhm:

               IF (TERM_USED (12) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 12)) THEN
                  LOG10_FWHM = PARAM (12, 3) + PARAM (12, 4) * (LOG10 (ENERGY) - LOG10 (PARAM (12, 2)))
                  GSIGMA = LOG10_FWHM / GCONST
                  GSIGMA = MAX (GSIGMA, 1.E-5)
                  PULL = (LOG10 (ENERGY) - LOG10 (PARAM (12, 2))) / GSIGMA
                  IF (ABS (PULL) .LE. 12.)  MODEL_PHOT_RATE (PBIN, 12, LDET) = MODEL_PHOT_RATE (PBIN, 12, LDET) +     &
                         PARAM (12, 1) / (GSIGMA * SQRT (2. * PI)) * EXP (-0.5 * PULL**2)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 13: Sunyaev-Titarchuk Comptonization A: with parameters: amplitude,
               !:  electron energy, optical depth and geometry factor:
               !:  geometry factor must by either 3 (sphere) or 12 (disk):

               IF (TERM_USED (13) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 13)) THEN
                  call M_SUNYAEV (PARAM (13, 1), PARAM (13, 2), PARAM (13, 3), PARAM (13, 4), ENERGY, result, error_arg )
                  MODEL_PHOT_RATE (PBIN, 13, LDET) = MODEL_PHOT_RATE (PBIN, 13, LDET) + result
                  if ( error_arg /= 0 )   ErrNum = error_arg
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 14: Sunyaev-Titarchuk Comptonization B: with parameters: amplitude,
               !:  electron energy, optical depth and geometry factor:
               !:  geometry factor must by either 3 (sphere) or 12 (disk):

               IF (TERM_USED (14) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 14)) THEN
                  call M_SUNYAEV (PARAM (14, 1), PARAM (14, 2), PARAM (14, 3), PARAM (14, 4), ENERGY, result, error_arg )
                  MODEL_PHOT_RATE (PBIN, 14, LDET) = MODEL_PHOT_RATE (PBIN, 14, LDET) + result
                  if ( error_arg /= 0 )  ErrNum = error_arg
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 15: OTTB A: optically-thin thermal brem:
               !:  A * exp [-(E-Epiv)/kT] * (E/Epiv)**-1
               !:  P1 = A, P2 = kT, P3 = Epiv.

               IF (TERM_USED (15) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 15)) THEN
                  MODEL_PHOT_RATE (PBIN, 15, LDET) = MODEL_PHOT_RATE (PBIN, 15, LDET) +     &
                       PARAM (15, 1) * EXP (-(ENERGY - PARAM (15, 3)) / PARAM (15, 2)) / (ENERGY / PARAM (15, 3))
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
               !
               !:  TERM 16: OTTB B: optically-thin thermal brem:
               !:  A * exp [-(E-Epiv)/kT] * (E/Epiv)**-1
               !:  P1 = A, P2 = kT, P3 = Epiv.

               IF (TERM_USED (16) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 16)) THEN
                  MODEL_PHOT_RATE (PBIN, 16, LDET) = MODEL_PHOT_RATE (PBIN, 16, LDET) +     &
                       PARAM (16, 1) * EXP (-(ENERGY - PARAM (16, 3)) / PARAM (16, 2)) / (ENERGY / PARAM (16, 3))
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 17: Black Body: A * E**2 / (exp (E/kT) - 1): parameters:
               !:  amplitude and kT:

               IF (TERM_USED (17) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 17)) THEN
                  RATIO = ENERGY / PARAM (17, 2)
                  IF (RATIO .LT. 85.)  MODEL_PHOT_RATE (PBIN, 17, LDET) = MODEL_PHOT_RATE (PBIN, 17, LDET) +     &
                        PARAM (17, 1) * ENERGY**2 / (EXP (RATIO) - 1.)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 18: Black Body B: A * E**2 / (exp (E/kT) - 1): parameters:
               !:  amplitude and kT:

               IF (TERM_USED (18) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 18)) THEN
                  RATIO = ENERGY / PARAM (18, 2)
                  IF (RATIO .LT. 85.)  MODEL_PHOT_RATE (PBIN, 18, LDET) = MODEL_PHOT_RATE (PBIN, 18, LDET) +    &
                        PARAM (18, 1) * ENERGY**2 / (EXP (RATIO) - 1.)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 19: power law parameterized by photon number flux between E1 & E2:
               !:  parameters: P1 = photon number flux (photons/s-cm2 between E1 & E2),
               !:  P2 = index, P3 = E1, P4 = E2.    P3 and P4 should be fixed.
               !:  Usual amplitude AMP_19 (photons/s-cm2-keV) is P1 * (P2+1) /
               !:  (E2**(P2+1) - E1**(P2+1))
               !:  Special case: P2 = -1, AMP_19 is P1 / log (P4 / P3).
               !:  See above (outside of loop) for calculation of AMP_19.

               IF (TERM_USED (19) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 19)) THEN
                  MODEL_PHOT_RATE (PBIN, 19, LDET) = MODEL_PHOT_RATE (PBIN, 19, LDET) + AMP_19 * ENERGY**PARAM (19, 2)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 20: power law parameterized by energy flux between E1 & E2:
               !:  parameters: P1 = energy flux (keV/s-cm2 between E1 & E2),
               !:  P2 = index, P3 = E1, P4 = E2.    P3 and P4 should be fixed.
               !:  Usual amplitude AMP_20 (photons/s-cm2-keV) is P1 * (P2+2) /
               !:  (E2**(P2+2) - E1**(P2+2)).
               !:  Special case: P2 = -2, AMP_20 is P1 / log (P4 / P3).
               !:  See above (outside of loop) for calculation of AMP_20.

               IF (TERM_USED (20) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 20)) THEN
                  MODEL_PHOT_RATE (PBIN, 20, LDET) = MODEL_PHOT_RATE (PBIN, 20, LDET) + AMP_20 * ENERGY**PARAM (20, 2)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 21: OTTB parameterized by photon numnber flux between E1 & E2:
               !:  parameters: P1 = photon number flux (photons/s-cm2 between E1 & E2),
               !:  P2 = kT, P3 = E1, P4 = E2.  P3 and P4 should be fixed.
               !:  Requires numeric integration, but not as a function of the energy
               !:  for which the photon is being evaulated, hence the integral is above,
               !:  before the loop.

               IF (TERM_USED (21) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 21)) THEN
                  MODEL_PHOT_RATE (PBIN, 21, LDET) = MODEL_PHOT_RATE (PBIN, 21, LDET) +     &
                       PARAM (21, 1) / real ( I_OTTB, mfit_real ) * EXP (-ENERGY / PARAM (21, 2)) / ENERGY
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 22: OTTB parameterized by energy flux between E1 & E2: parameters:
               !:  P1 = energy flux (kev/s-cm2 between E1 & E2), P2 = kT, P3 = E1, P4 = E2.
               !:  P3 and P4 should be fixed.   See above for calculation of AMP_22, removed
               !:  from energy loop:

               IF (TERM_USED (22) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 22)) THEN
                  MODEL_PHOT_RATE (PBIN, 22, LDET) = MODEL_PHOT_RATE (PBIN, 22, LDET) +     &
                       AMP_22 * EXP (-ENERGY / PARAM (22, 2)) / ENERGY
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 23: Yang Soong's model for Her X-1.   See M_SOONG for info.

               IF (TERM_USED (23) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 23)) THEN
                  MODEL_PHOT_RATE (PBIN, 23, LDET) = MODEL_PHOT_RATE (PBIN, 23, LDET) +      &
                       M_SOONG ( ENERGY, PARAM (23, 1), PARAM (23, 2), PARAM (23, 3),        &
                                PARAM (23, 4), PARAM (23, 5), PARAM (23, 6), PARAM (23, 7), PARAM (23, 8) )
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 24: Tanaka's pulsar model.  See M_TANAKA for info.

               IF (TERM_USED (24) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 24)) THEN
                  MODEL_PHOT_RATE (PBIN, 24, LDET) = MODEL_PHOT_RATE (PBIN, 24, LDET) +      &
                       M_TANAKA ( ENERGY, PARAM (24, 1), PARAM (24, 2), PARAM (24, 3),       &
                                PARAM (24, 4), PARAM (24, 5), PARAM (24, 6), PARAM (24, 7), PARAM (24, 8), PARAM (24, 9) )
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 25: Titarchuk

               IF (TERM_USED (25) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 25)) THEN
                  call M_TITARCHUK (PARAM (25, 1), PARAM (25, 2), PARAM (25, 3), PARAM (25, 4), ENERGY, result, error_arg )
                  MODEL_PHOT_RATE (PBIN, 25, LDET) = MODEL_PHOT_RATE (PBIN, 25, LDET) + result
                  if ( error_arg /= 0 )  ErrNum = error_arg
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 26:  Optically Thin Thermal Synchrotron:
               !:  (e.g, Liang et al., 1983, ApJ, vol 271, p. 776, eq. A2)

               IF (TERM_USED (26) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 26)) THEN
                  ARG = (ENERGY / PARAM (26, 2)) ** 0.33333
                  MODEL_PHOT_RATE (PBIN, 26, LDET) = MODEL_PHOT_RATE (PBIN, 26, LDET) + PARAM (26, 1) * EXP (-ARG)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 27 (RMK): Thermal Bremsstrahlung spectrum from XSPEC including
               !:  variable He/H density ratio. Parameters are:
               !:  P1 = amplitude, P2 = temperature (keV), P3 = He/H ratio

               IF (TERM_USED (27) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 27)) THEN
                  MODEL_PHOT_RATE (PBIN, 27, LDET) = MODEL_PHOT_RATE (PBIN, 27, LDET) +     &
                       M_VBREMSS ( ENERGY, PARAM (27,1), PARAM (27,2), PARAM (27,3) )
               END IF


               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERMS 28 to 31: SPARES # 3 to # 6

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 32: User Function A: User links function subroutine MFIT_USER_FUNC_A with MFIT:

               IF (TERM_USED (32) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 32)) THEN
                  MODEL_PHOT_RATE (PBIN, 32, LDET) = MODEL_PHOT_RATE (PBIN, 32, LDET) +      &
                     MFIT_USER_FUNC_A ( ENERGY, PARAM (32, 1:10) )
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 33: User Function B: User links function subroutine MFIT_USER_FUNC_B with MFIT:

               IF (TERM_USED (33) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 33)) THEN
                  MODEL_PHOT_RATE (PBIN, 33, LDET) = MODEL_PHOT_RATE (PBIN, 33, LDET) +    &
                     MFIT_USER_FUNC_B ( ENERGY, PARAM (33, 1:10) )
               END IF


               !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +
               !           MULTIPLICATIVE TERMS:
               !  These are the multiplicative terms that may need averaging across bins:
               !  Note that the evaluations are summed into the output array: this is
               !  needed if the model is sampled more than once per photon bin
               !  (i.e. NUME_TO_USE .GT. 1) for the purpose of averaging over the bin.
               !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


               ! .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 41: Low Energy Cutoff: P1 = cutoff energy, P2 = folding energy
               !:  for energy > P1, term = 1,
               !:  for energy < P1, term = (E / P1)**(P1/P2) * exp [-(P1 - E) / P2]
               !:  The power law makes the derivative zero at E = P1.

               IF (TERM_USED (41) .EQ. 1) THEN
                  IF (ENERGY .GE. PARAM (41, 1)) THEN
                     MODEL_PHOT_RATE (PBIN, 41, LDET) = MODEL_PHOT_RATE (PBIN, 41, LDET) + 1.
                  ELSE
                     MODEL_PHOT_RATE (PBIN, 41, LDET) = MODEL_PHOT_RATE (PBIN, 41, LDET) +      &
                         (ENERGY / PARAM (41, 1))**(PARAM (41, 1) / PARAM (41, 2)) * EXP ((PARAM (41, 1) - ENERGY) / PARAM (41, 2))
                  END IF
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 42: High Energy Cutoff: P1 = cutoff energy, P2 = folding energy
               !:  for energy < P1, term = 1,
               !:  for energy > P1, term = (E / P1)**(P1 / P2) * exp [(P1 - E) / P2]
               !:  The power law makes the derivative zero at E = P1.

               IF (TERM_USED (42) .EQ. 1) THEN
                  IF (ENERGY .LE. PARAM (42, 1)) THEN
                     MODEL_PHOT_RATE (PBIN, 42, LDET) = MODEL_PHOT_RATE (PBIN, 42, LDET) + 1.
                  ELSE
                     MODEL_PHOT_RATE (PBIN, 42, LDET) = MODEL_PHOT_RATE (PBIN, 42, LDET) +      &
                         (ENERGY / PARAM (42, 1))**(PARAM (42, 1) / PARAM (42, 2)) * EXP ((PARAM (42, 1) - ENERGY) / PARAM (42, 2))
                  END IF
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 43: multiplicative power law.   No amplitude since it multiples
               !:  additive terms that already have amplitudes.    Parameters are
               !:  P1 = index and P2 = pivot energy, which should be fixed.

               IF (TERM_USED (43) .EQ. 1) THEN
                  MODEL_PHOT_RATE (PBIN, 43, LDET) = MODEL_PHOT_RATE (PBIN, 43, LDET) + (ENERGY / PARAM (43, 2)) ** PARAM (43, 1)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 44: interstellar absorption.   see M_IS_CROSS for comments.

               IF (TERM_USED (44) .EQ. 1) THEN
                  MODEL_PHOT_RATE (PBIN, 44, LDET) = MODEL_PHOT_RATE (PBIN, 44, LDET) + EXP (-PARAM (44, 1) * M_IS_CROSS (ENERGY))
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 45: exp (-P1 * Gaussian): P2 = centroid, P3 = FWHM:

               IF (TERM_USED (45) .EQ. 1) THEN
                  GSIGMA = PARAM (45, 3) / GCONST
                  GSIGMA = MAX (GSIGMA, 1.E-5)
                  PULL = (ENERGY - PARAM (45, 2)) / GSIGMA
                  GAUSS = EXP (-0.5 * PULL**2) / (GSIGMA * SQRT (2. * PI))
                  MODEL_PHOT_RATE (PBIN, 45, LDET) = MODEL_PHOT_RATE (PBIN, 45, LDET) + EXP (PARAM (45, 1) * GAUSS)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 46: exp (-P1 * Gaussian): P2 = centroid, P3 = FWHM:

               IF (TERM_USED (46) .EQ. 1) THEN
                  GSIGMA = PARAM (46, 3) / GCONST
                  GSIGMA = MAX (GSIGMA, 1.E-5)
                  PULL = (ENERGY - PARAM (46, 2)) / GSIGMA
                  GAUSS = EXP (-0.5 * PULL**2) / (GSIGMA * SQRT (2. * PI))
                  MODEL_PHOT_RATE (PBIN, 46, LDET) = MODEL_PHOT_RATE (PBIN, 46, LDET) + EXP (PARAM (46, 1) * GAUSS)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 47: exp (Lorentzian) = exp (P1 / [P3**2 + (energy - P2)**2])

               IF (TERM_USED (47) .EQ. 1) THEN
                  LORENTZ = PARAM (47, 1) / (PARAM (47, 3)**2 + (ENERGY - PARAM (47, 2))**2)
                  MODEL_PHOT_RATE (PBIN, 47, LDET) = MODEL_PHOT_RATE (PBIN, 47, LDET) + EXP (LORENTZ)
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 48: multiplicative broken power law.   No amplitude since it
               !:  multiples additive terms that already have amplitudes.    Parameters are
               !:  P1 = index below break, P2 = break energy, P3 = index above break.

               IF (TERM_USED (48) .EQ. 1) THEN
                  IF (ENERGY .GE. PARAM (48, 2)) THEN
                     MODEL_PHOT_RATE (PBIN, 48, LDET) = MODEL_PHOT_RATE (PBIN, 48, LDET) +      &
                         (ENERGY / PARAM (48, 2)) ** PARAM (48, 3)
                  ELSE
                     MODEL_PHOT_RATE (PBIN, 48, LDET) =MODEL_PHOT_RATE (PBIN, 48, LDET) +       &
                         (ENERGY / PARAM (48, 2)) ** PARAM (48, 1)
                  END IF
               END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 49: interstellar absorption from absorber at cosmological
               !  distance.   see M_IS_Z_CROSS for comments.

               IF (TERM_USED (49) .EQ. 1) THEN
                  MODEL_PHOT_RATE (PBIN, 49, LDET) = MODEL_PHOT_RATE (PBIN, 49, LDET) +    &
                       EXP (-PARAM (49, 1) * M_IS_Z_CROSS (ENERGY, PARAM (49, 2)))
               END IF


               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERMS 50 to 53: SPARES # 12 to 15

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 54: User Function C: User links function subroutine MFIT_USER_FUNC_C with MFIT:

!              IF (TERM_USED (54) .EQ. 1) THEN
!                 MODEL_PHOT_RATE (PBIN, 54, LDET) = MODEL_PHOT_RATE (PBIN, 54, LDET) +   &
!                     MFIT_USER_FUNC_C ( ENERGY, PARAM (54, 1), PARAM (54, 2),            &
!                         PARAM (54, 3), PARAM (54, 4), PARAM (54, 5), PARAM (54, 6),     &
!                         PARAM (54, 7), PARAM (54, 8), PARAM (54, 9), PARAM (54, 10) )
!              END IF

               !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

               !:  TERM 55: User Function D: User links function subroutine MFIT_USER_FUNC_D
               !:   with MFIT:

!              IF (TERM_USED (55) .EQ. 1) THEN
!                 MODEL_PHOT_RATE (PBIN, 55, LDET) = MODEL_PHOT_RATE (PBIN, 55, LDET) +   &
!                     MFIT_USER_FUNC_D ( ENERGY, PARAM (55, 1), PARAM (55, 2),            &
!                         PARAM (55, 3), PARAM (55, 4), PARAM (55, 5), PARAM (55, 6),     &
!                         PARAM (55, 7), PARAM (55, 8), PARAM (55, 9), PARAM (55, 10) )
!              END IF


               !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


            END DO                                        ! E_CNT


            !   divide by number of evaluations per bin, if not 1:
            !   this is to average the function over the bin:

            IF (NUME_TO_USE (PBIN, LDET) .GT. 1) THEN
               IF (DO_TERM .NE. 0) THEN
                  MODEL_PHOT_RATE (PBIN, DO_TERM, LDET) = MODEL_PHOT_RATE (PBIN, DO_TERM, LDET) / NUME_TO_USE (PBIN, LDET)
               ELSE
                  DO JTERM=1, NUM_TERMS_AVAIL
                     MODEL_PHOT_RATE (PBIN, JTERM, LDET) = MODEL_PHOT_RATE (PBIN, JTERM, LDET) / NUME_TO_USE (PBIN, LDET)
                  END DO
               END IF
            END IF


            !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +
            !           ADDITIVE/SUBRACTIVE LINE TERMS:
            !  these terms are Gaussian lines evaluated using the error function:
            !  they therefore explicitly evaluate the Gaussian averaged over the
            !  bin and therefore no explicit averaging (E_CNT loop above) is needed.

            !  Since a Gaussian is essentially zero outside of +/- 2*FWHM, the lines
            !  need only be evaluated if either (or both) bin edges are within
            !  2*FWHM of the centroid.   The left bin edge is the 6th element of
            !  PHOT_EVAL, and the right bin edge the 7th element.
            !  The bin ranges which meet these conditions are stored in FIRST_PHOT_BIN
            !  & LAST_PHOT_BIN.

            !  Since we are not averaging via multiple evaluations per bin, there is
            !  no need to sum into MODEL_PHOT_RATE.
            !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


            !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

            !:  TERM 34: Gaussian line A: with parameters: amplitude, centroid, fwhm:
            !:  Done as mean of function over bin via ERFCC rather than function
            !:  value at center of bin:

            IF (TERM_USED (34) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 34)) THEN
               IF (PBIN .GE. FIRST_PHOT_BIN (34, LDET) .AND. PBIN .LE. LAST_PHOT_BIN (34, LDET)) THEN
                  GFAC = SQRT (2.) * PARAM (34, 3) / GCONST
                  GFAC = MAX (GFAC, 1.E-5)
                  ARG_LEFT  = (PHOT_EVAL (6, PBIN, LDET) - PARAM (34, 2)) / GFAC
                  ARG_RIGHT = (PHOT_EVAL (7, PBIN, LDET) - PARAM (34, 2)) / GFAC
                  MODEL_PHOT_RATE (PBIN, 34, LDET) = PARAM (34, 1) * 0.5 *                                                   &
         real ( (gsl_sf_erfc ( real (ARG_LEFT, c_double) ) - gsl_sf_erfc ( real (ARG_RIGHT, c_double) )), mfit_real ) /      &
                          (PHOT_EVAL (7, PBIN, LDET) - PHOT_EVAL (6, PBIN, LDET))
               END IF
            END IF

            !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

            !:  TERM 35: Gaussian line B: with parameters: amplitude, centroid, fwhm:
            !:  Done as mean of function over bin via ERFCC rather than function
            !:  value at center of bin:

            IF (TERM_USED (35) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 35)) THEN
               IF (PBIN .GE. FIRST_PHOT_BIN (35, LDET) .AND. PBIN .LE. LAST_PHOT_BIN (35, LDET)) THEN
                  GFAC = SQRT (2.) * PARAM (35, 3) / GCONST
                  GFAC = MAX (GFAC, 1.E-5)
                  ARG_LEFT  = (PHOT_EVAL (6, PBIN, LDET) - PARAM (35, 2)) / GFAC
                  ARG_RIGHT = (PHOT_EVAL (7, PBIN, LDET) - PARAM (35, 2)) / GFAC
                  MODEL_PHOT_RATE (PBIN, 35, LDET) = PARAM (35, 1) * 0.5 *                                                   &
         real ( (gsl_sf_erfc ( real (ARG_LEFT, c_double) ) - gsl_sf_erfc ( real (ARG_RIGHT, c_double) )), mfit_real ) /      &
                          (PHOT_EVAL (7, PBIN, LDET) - PHOT_EVAL (6, PBIN, LDET))
               END IF
            END IF

            !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

            !:  TERM 36: Gaussian line C: with parameters: amplitude, centroid, fwhm:
            !:  Done as mean of function over bin via ERFCC rather than function
            !:  value at center of bin:

            IF (TERM_USED (36) .EQ. 1 .AND. (DO_TERM .EQ. 0 .OR. DO_TERM .EQ. 36)) THEN
               IF (PBIN .GE. FIRST_PHOT_BIN (36, LDET) .AND. PBIN .LE. LAST_PHOT_BIN (36, LDET)) THEN
                  GFAC = SQRT (2.) * PARAM (36, 3) / GCONST
                  GFAC = MAX (GFAC, 1.E-5)
                  ARG_LEFT  = (PHOT_EVAL (6, PBIN, LDET) - PARAM (36, 2)) / GFAC
                  ARG_RIGHT = (PHOT_EVAL (7, PBIN, LDET) - PARAM (36, 2)) / GFAC
                  MODEL_PHOT_RATE (PBIN, 36, LDET) = PARAM (36, 1) * 0.5 *                                                   &
         real ( (gsl_sf_erfc ( real (ARG_LEFT, c_double) ) - gsl_sf_erfc ( real (ARG_RIGHT, c_double) )), mfit_real ) /      &
                          (PHOT_EVAL (7, PBIN, LDET) - PHOT_EVAL (6, PBIN, LDET))
               END IF
            END IF

            !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

            !:  TERMS 37 to 39: SPARES # 7 to 9

            !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


            !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +
            !           MULTIPLICATIVE TERMS:
            !  These are the multiplicative terms that never need averaging across bins:
            !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +

            !  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

            !:  TERM 40: (Relative) Effective Area Correction:
            !:  1996 July 7: now Effective Area Correction rather than former Detector
            !:  Renormalization; consequently divide rather than multiply by parameter.

            IF (TERM_USED (40) .EQ. 1) THEN
               IF (LDET .GE. 1 .AND. LDET .LE. MAX_PPT) THEN
                  MODEL_PHOT_RATE (PBIN, 40, LDET) = 1. / PARAM (40, LDET)
               ELSE
                  MODEL_PHOT_RATE (PBIN, 40, LDET) = 1.
               END IF
            END IF

         !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


         END DO                                        ! PBIN
      END IF                                           ! det used ?
   END DO                                              ! LDET

   !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


   !  flag whether any multiplicative terms were calculated.
   MULT_FLAG = .FALSE.
   DO JTERM=NUM_ADD_TERMS+1, NUM_TERMS_AVAIL
      IF (TERM_USED (JTERM) .EQ. 1) MULT_FLAG = .TRUE.
   END DO

   !  calculate the product of all of the multiplicative terms:
   DO LDET=0, NUM_DET - 1
      DO PBIN=FIRST_PHOT_BIN (0, LDET), LAST_PHOT_BIN (0, LDET)
         MULT_TERMS (PBIN, LDET) = 1.
      END DO
   END DO

   IF (MULT_FLAG) THEN
      DO LDET=0, NUM_DET - 1
          IF (USE_DET (LDET)) THEN
             DO JTERM=NUM_ADD_TERMS+1, NUM_TERMS_AVAIL
                IF (TERM_USED (JTERM) .EQ. 1) THEN
                   DO PBIN=FIRST_PHOT_BIN (JTERM, LDET), LAST_PHOT_BIN (JTERM, LDET)
                      MULT_TERMS (PBIN, LDET) = MULT_TERMS (PBIN, LDET) * MODEL_PHOT_RATE (PBIN, JTERM, LDET)
                   END DO
                END IF
            END DO
         END IF
      END DO
   END IF


   !  We have now evaluated all of the requested, active model terms.
   !  If requested via DO_TERM=0, calculate total model by summing all
   !  selected additive terms (note that array elements jterm=0 were zeroed
   !  above), then multiplying by the product of the multiplicative terms.

   IF (DO_TERM .EQ. 0) THEN

      DO LDET=0, NUM_DET - 1
         IF (USE_DET (LDET)) THEN
            DO JTERM=1, NUM_ADD_TERMS
               IF (TERM_USED (JTERM) .EQ. 1) THEN
                  DO PBIN=FIRST_PHOT_BIN (0, LDET), LAST_PHOT_BIN (0, LDET)
                     MODEL_PHOT_RATE (PBIN, 0, LDET) = MODEL_PHOT_RATE (PBIN, 0, LDET) + MODEL_PHOT_RATE (PBIN, JTERM, LDET)
                  END DO
               END IF
            END DO
         END IF
      END DO

      IF (MULT_FLAG) THEN
         DO LDET=0, NUM_DET - 1
            IF (USE_DET (LDET)) THEN
               DO PBIN=FIRST_PHOT_BIN (0, LDET), LAST_PHOT_BIN (0, LDET)
                  MODEL_PHOT_RATE (PBIN, 0, LDET) =          &
                  MODEL_PHOT_RATE (PBIN, 0, LDET) * MULT_TERMS (PBIN, LDET)
               END DO
            END IF
         END DO
      END IF

   END IF                                                ! do_term = 0 ?


   RETURN

   END SUBROUTINE M_PHOTONS_MODEL


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_PLOT_SETUP  ( NUM_DET, NUM_CHAN, NUM_EBINS, NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE,           &
           PARAM, CHAN_OFFSET, FIT_CHAN, USE_DET, CHAN_ENERGY, CHAN_WIDTH, FIRST_NONZERO, PHOT_WIDTH, DRM,         &
           OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME, TERM_USED, PHOT_EVAL, NUME_TO_USE, CHAN_EVAL,              &
           CHAN_NUME_TO_USE, DIFF_EVAL, DIFF_NUME_TO_USE, NUM_DIFF, FIT_ERR, FIRST_PHOT_BIN, LAST_PHOT_BIN,        &
           MODEL_CNT_RATE, MODEL_CR_VARI, PHOT_OBS_RATE, PHOT_OBS_SIG, PHOT_MODEL_BYCHAN,                          &
           FIRST_MODEL_PLOT, LAST_MODEL_PLOT, FIRST_EXTRAP_PLOT, LAST_EXTRAP_PLOT, MODEL_ENERGY,                   &
           MODEL_PHOT_RATE, NU_F_NU_DATA, NU_F_NU_SIG, NU_F_NU_MODEL, ErrNum )

   !  Quantities calculated by this subroutine:
   !  All of these are calculated for all detectors and all channels in
   !  the plot channel range, whether or not they were included in the fit:
   !  I) For each channel:
   !     A) variances of count rates, calculated solely from data,
   !     B) model count rates, total model and individual terms,
   !        counts / s-keV-detector
   !     C) variances of count rates, calculated from model count rates and
   !        input background count rate sigmas,
   !     E) photon "data" rates, photons / s-keV-cm^2
   !     F) photon "data" sigmas,
   !     H) photon "data" in units of nu-F-nu: keV^2 / s-cm^2-keV
   !        [nu-Fnu is really d(energyflux)/d(log E)]
   !     I) photon model for the channel
   !  II) At high resolution (six times per channel):
   !     F) model photon curve, total model and individual terms,
   !        photons / s-keV-cm^2
   !     G) model photon curve, total model only,
   !        in nu-F-nu units: keV^2 / s-cm^2-keV

   !  The photon "data" are calculated in the following model dependent manner:
   !  1) Model photon rates are calculated from model parameters,
   !  2) Model predicted counts are obtained via DRM,
   !  3) Comparison of observed and predicted counts defines efficiencies,
   !  4) The efficiencies are used to convert the observed counts to
   !     "observed" photons.
   !  5) The photon "data" sigmas are calculated from the model.
   !  The nu-F-nu "data" are also model dependent: the mean energy of the
   !  photons in each channel is calculated by evaluating the model at 7
   !  energies across the bin.   This mean energy and the bin width are
   !  use to convert the photon "data", photons / s-keV-cm^2, into nu-F-nu,
   !  keV^2 / s-cm^2-keV

   !  Some of the calculations herein appear very cryptic unless one studies
   !  subroutine MPHOT_EVAL_LIST, which creates the arrays PHOT_EVAL &
   !  NUME_TO_USE, CHAN_EVAL & CHAN_NUME_TO_USE, and DIFF_EVAL &
   !  DIFF_NUME_TO_USE.   The prescription for what these arrays contain is
   !  relied upon by this subroutine.


   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  1999 JULY 19.  MSB.  was major bug, using MODEL_PHOT_RATE_WS and MULT_TERMS
   !  where I am now using MODEL_CNT_RATE_WS and MULT_TERMS_CNT.
   !  Not sure whether all big arrays are still needed.
   !
   !  RSM, November, 1999, removed DET_NAMES argument.

   !  Michael S. Briggs, UAH/MSFC ES-62, 4 October 1992, revised 30 June 93.
   !  Revised 29 August 1993: made EFF 2-dimensional so that latter sections
   !  of this subroutine could use it to decide if calculations are
   !  possible and meaningful.
   !  Revised 4 November 1993: improved calculation of nu-F-nu "data" values.
   !  Revised 24 Feb 1994: CV_TALKY < 0 suppresses negative photon/count
   !  error messages.
   !  Revised 7 July 1994 by MSB: bug fix re calculation of nu-F-nu data.  When
   !  one or more channels had their efficiencies EFF undefined (e.g., because
   !  they were input with LIVETIME <= 0), then the index DPLOT was not
   !  incremented, thereby causing NU_F_NU_DATA and _SIG to be incorrectly
   !  calculated for higher channels.   The fix is in the ELSE branch of
   !  ERR > 0: DPLOT = DPLOT + 6.


   use MFIT_parameters


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_EBINS
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   integer (kind=mfit_integer), intent (in) :: NT_ADD_LINE
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_ENERGY
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: FIRST_NONZERO
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: CHAN_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: CHAN_NUME_TO_USE
   real (kind=mfit_real), dimension (7, 0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1), intent (in) :: DIFF_EVAL
   integer (kind=mfit_integer), dimension (0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1), intent (in) :: DIFF_NUME_TO_USE
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_DIFF

   integer (kind=mfit_integer), intent (inout) :: FIT_ERR

   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: LAST_PHOT_BIN


   !  Output arguments:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1), intent (out) :: MODEL_CNT_RATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: MODEL_CR_VARI
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: PHOT_OBS_RATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: PHOT_OBS_SIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: PHOT_MODEL_BYCHAN
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (out) :: FIRST_MODEL_PLOT
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (out) :: LAST_MODEL_PLOT
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (out) :: FIRST_EXTRAP_PLOT
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (out) :: LAST_EXTRAP_PLOT
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: num_det-1), intent (out) :: MODEL_ENERGY
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: MAX_TERMS, 0: num_det-1), intent (out) :: MODEL_PHOT_RATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: NU_F_NU_DATA
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: NU_F_NU_SIG
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: num_det-1), intent (out) :: NU_F_NU_MODEL
   integer (kind=mfit_integer), intent (out) :: ErrNum


   !  Internal variables:

   !  Workspace arrays:
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_PHOT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1) :: MODEL_CNT_RATE_WS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1) :: MULT_TERMS
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1) :: MULT_TERMS_CNT

   !  Other internal variables:
   integer (kind=mfit_integer), dimension (0:1, 0: NUM_DET-1) :: ALL_CHAN
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1) :: MULT_TERMS_PLT
   logical (kind=mfit_logical) :: MULT_FLAG
   real (kind=mfit_real) :: FACTOR
   integer (kind=mfit_integer) :: DPLOT, DPLOT_MIN
   integer (kind=mfit_integer) :: ICHAN, JTERM, LDET, PBIN
   integer (kind=mfit_integer) :: DO_TERM
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1) :: EFF
   real (kind=mfit_real) :: LEFT_PLOT_E, RIGHT_PLOT_E
   real (kind=mfit_real) :: LEFT_FIT_E, RIGHT_FIT_E
   logical (kind=mfit_logical), dimension (0: NUM_DET-1) :: USE_ALL_DET
   integer (kind=mfit_integer) :: CIND
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: FIRST_CHAN_INDEX
   integer (kind=mfit_integer), dimension (0: NUM_DET-1) :: LAST_CHAN_INDEX
   integer (kind=mfit_integer) :: IS
   real (kind=mfit_real) :: EF_SUM, F_SUM, E_MEAN
   integer (kind=mfit_integer) :: NEG_CNT
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: FIRST_CHAN_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: LAST_CHAN_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: FIRST_EXTRAP_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: NUM_DET-1) :: LAST_EXTRAP_BIN
   integer (kind=mfit_integer) :: Caller_Error


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   ErrNum = 0

   !  Erase output stuff for safety:
   !  If we can't calculate PHOT... because the photon model is exactly
   !  zero or negative, PHOT_OBS_RATE will be large negative
   !  numbers to flag that the value is missing.

   DO LDET=0, NUM_DET - 1
      DO ICHAN=0, NUM_CHAN (LDET) - 1
         PHOT_OBS_RATE (ICHAN, LDET) = -9.9E+36
         PHOT_OBS_SIG (ICHAN, LDET) = 0.0
         NU_F_NU_DATA (ICHAN, LDET) = -9.9E+36
         NU_F_NU_SIG (ICHAN, LDET) = 0.0
         PHOT_MODEL_BYCHAN (ICHAN, LDET) = -9.9E+36
      END DO
   END DO

   DO LDET=0, NUM_DET - 1
      FIRST_MODEL_PLOT (LDET) = 0
      LAST_MODEL_PLOT (LDET) = -1
      FIRST_EXTRAP_PLOT (LDET) = 0
      LAST_EXTRAP_PLOT (LDET) = -1
   END DO

   DO LDET=0, NUM_DET - 1
      DO JTERM=0, MAX_TERMS
         DO PBIN=0, 6 * maxval (NUM_CHAN) 
            MODEL_PHOT_RATE (PBIN, JTERM, LDET) = 0.0
         END DO
      END DO
   END DO

   DO LDET=0, NUM_DET - 1
      DO PBIN=0, 6 * maxval (NUM_CHAN) 
         NU_F_NU_MODEL (PBIN, LDET) = 0.0
      END DO
   END DO



   !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


   !  setup arrays specifying all available channels.  Replaces FIRST & LAST
   !  _CHAN_FIT, which are normally used to omit channels we don't care about.

   DO LDET=0, NUM_DET - 1
      ALL_CHAN (0, LDET) = CHAN_OFFSET (LDET)
      ALL_CHAN (1, LDET) = NUM_CHAN (LDET) - 1 + CHAN_OFFSET (LDET)
   END DO


   !  Will calculate stuff for all detectors, whether or not used in fit,
   !  so define a special version of USE_DET:

   DO LDET=0, NUM_DET - 1
      USE_ALL_DET (LDET) = .TRUE.
   END DO


   !  Convert the data from count units to photon units via forward
   !  folding technique.    Define efficiencies vs channels as ratio of
   !  model counts over model photons.   Assume these efficiences are valid:
   !  use them to convert observed counts to "observed photons".
   !  We do this for all detectors, whether or not included in fit, via
   !  using USE_ALL_DET instead of USE_DET:

   !  Obtain model count rates:

   DO_TERM = 0
   CALL M_PHOTONS_MODEL ( NUM_DET, maxval (NUM_EBINS) , PHOT_EVAL, NUME_TO_USE, FIRST_PHOT_BIN, LAST_PHOT_BIN,    &
            PARAM, TERM_USED, DO_TERM, USE_ALL_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,            &
            MODEL_PHOT_RATE_WS, MULT_FLAG, MULT_TERMS, Caller_Error )
   if ( Caller_Error /=0 )  ErrNum = Caller_Error + 810000

   CALL MPHOTS_TO_CNTS ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH, USE_ALL_DET, CHAN_OFFSET, ALL_CHAN,    &
            FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, TERM_USED, NUM_ADD_TERMS, PHOT_WIDTH, DRM,      &
            MODEL_PHOT_RATE_WS, MODEL_CNT_RATE )

   !  Obtain model count rate variances:

   CALL MGET_MODEL_CR_VARI  ( NUM_DET, NUM_CHAN, CHAN_OFFSET, CHAN_WIDTH, BACK_CRATE, BACK_CSIG,     &
            LIVE_TIME, USE_ALL_DET, ALL_CHAN, MODEL_CNT_RATE, MODEL_CR_VARI )

   !  Obtain photon model on channel binning for use in calculating
   !  efficiences.
   !  We do this for all detectors, whether or not included in fit, via
   !  using USE_ALL_DET instead of USE_DET:
   !  Store photon model on channel binning (sum of all terms only):

   DO LDET=0, NUM_DET - 1
      FIRST_CHAN_INDEX (LDET) = 0
      LAST_CHAN_INDEX (LDET) = NUM_CHAN (LDET) - 1
   END DO
   CALL M_USE_PHOTBINS ( NUM_DET, USE_ALL_DET, NUM_TERMS_AVAIL, NT_ADD_LINE, FIRST_CHAN_INDEX, LAST_CHAN_INDEX,    &
            maxval (NUM_CHAN), CHAN_EVAL, TERM_USED, PARAM, FIRST_CHAN_BIN, LAST_CHAN_BIN )
   DO_TERM = 0
   CALL M_PHOTONS_MODEL ( NUM_DET, maxval (NUM_CHAN), CHAN_EVAL, CHAN_NUME_TO_USE, FIRST_CHAN_BIN, LAST_CHAN_BIN,    &
            PARAM, TERM_USED, DO_TERM, USE_ALL_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS,                   &
            MODEL_CNT_RATE_WS, MULT_FLAG, MULT_TERMS_CNT, Caller_Error )
   if ( Caller_Error /=0 )  ErrNum = Caller_Error + 820000

   DO LDET=0, NUM_DET - 1
      DO ICHAN=0, NUM_CHAN (LDET) - 1
         PHOT_MODEL_BYCHAN (ICHAN, LDET) = MODEL_CNT_RATE_WS (ICHAN, 0,LDET)
      END DO
   END DO

   !  Now calculate the "observed photons":
   !  Can't calculate "observed photons" if no data as flagged by LIVE_TIME = 0.
   !  (LIVE_TIME < 0 flags data not to be used in fit but to be plotted);
   !  Can't calculate "observed photons" if model count rate is zero, this
   !  might happen if photon rate is very low or DRM has very small response
   !  into this channel;
   !  Can't calculate "observed photons" if model photon rate is zero, this
   !  might happen if model underflows, e.g. exp (-large number).

   DO LDET=0, NUM_DET - 1

   !  do calculation for all chan requested for processing (livetime test:
   !  zero livetime flags bad data) and positive photon model and positive
   !  count model.    If can't do calculation save EFF as zero for use below.

      DO ICHAN=0, NUM_CHAN (LDET) - 1
         IF (LIVE_TIME (ICHAN, LDET) .NE. 0.0 .AND.     &
             PHOT_MODEL_BYCHAN (ICHAN, LDET) .GT. 0.0 .AND. MODEL_CNT_RATE (ICHAN, 0, LDET) .GT. 0.0) THEN

            EFF (ICHAN, LDET) = MODEL_CNT_RATE (ICHAN, 0, LDET) / PHOT_MODEL_BYCHAN (ICHAN, LDET)
            PHOT_OBS_RATE (ICHAN, LDET) = (OBS_CRATE (ICHAN, LDET) - BACK_CRATE (ICHAN, LDET)) / EFF (ICHAN, LDET)
            PHOT_OBS_SIG (ICHAN, LDET) = SQRT (MODEL_CR_VARI (ICHAN, LDET)) / EFF (ICHAN, LDET)

         ELSE
            EFF (ICHAN, LDET) = 0.0
         END IF
      END DO

   END DO


   !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


   !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


   !  Now calculate the photon model at higher energy resolution for the
   !  purpose of plotting a smooth curve.
   !  We do this for all detectors, whether or not included in fit.
   !  We calculate the curve at 6 points per channel over
   !  the energy range of the plot channels.   We record the length
   !  of the entire curve, FIRST_EXTRAP_PLOT to LAST_EXTRAP_PLOT,
   !  and for detectors included in he fit, the subsection of the curve that
   !  covers only the fit channel range, FIRST_MODEL_PLOT to LAST_MODEL_PLOT.
   !  The array used for calculating the curve, DIFF_EVAL, has already been
   !  setup with the appropriate energies.    Here we need only figure out
   !  what range of elements correspond to the energies of the plot channel
   !  range and the fit channel range:

   DO LDET=0, NUM_DET - 1

      CIND = 0
      LEFT_PLOT_E  = CHAN_ENERGY (CIND, LDET) - 0.5 * CHAN_WIDTH (CIND, LDET) - 0.01
      CIND = NUM_CHAN (LDET) - 1
      RIGHT_PLOT_E = CHAN_ENERGY (CIND, LDET) + 0.5 * CHAN_WIDTH (CIND, LDET) + 0.01

      DPLOT_MIN = 6 * maxval (NUM_CHAN) 
      DO DPLOT=0, NUM_DIFF (LDET) - 1
         IF (DIFF_EVAL (1, DPLOT, LDET) .GE. LEFT_PLOT_E .AND. DIFF_EVAL (1, DPLOT, LDET) .LE. RIGHT_PLOT_E) THEN

            MODEL_ENERGY (DPLOT, LDET) = DIFF_EVAL (1, DPLOT, LDET)
            DPLOT_MIN = MIN (DPLOT_MIN, DPLOT)
            LAST_EXTRAP_PLOT (LDET) = DPLOT

         ELSE
            MODEL_ENERGY (DPLOT, LDET) = 0.0
         END IF
      END DO
      FIRST_EXTRAP_PLOT (LDET) = DPLOT_MIN

   !  We have made the list of energies for detector LDET and
   !  stored its used portion (FIRST_EXTRAP_PLOT to LAST_EXTRAP_PLOT).  Now
   !  figure what subsection (FIRST_MODEL_PLOT to LAST_MODEL_PLOT) of the
   !  curve spans the fit channel range, if this detector was included
   !  in the fit:

      IF (USE_DET (LDET)) THEN

         CIND = FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET)
         LEFT_FIT_E  = CHAN_ENERGY (CIND, LDET) - 0.5 * CHAN_WIDTH (CIND, LDET) - 0.01
         CIND = FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
         RIGHT_FIT_E = CHAN_ENERGY (CIND, LDET) + 0.5 * CHAN_WIDTH (CIND, LDET) + 0.01

         FIRST_MODEL_PLOT (LDET) = FIRST_EXTRAP_PLOT (LDET)
         DO WHILE (FIRST_MODEL_PLOT (LDET) .LE. LAST_EXTRAP_PLOT (LDET) .AND.    &
               MODEL_ENERGY (FIRST_MODEL_PLOT (LDET), LDET) .LT. LEFT_FIT_E)
            FIRST_MODEL_PLOT (LDET) = FIRST_MODEL_PLOT (LDET) + 1
         END DO

         LAST_MODEL_PLOT (LDET) = LAST_EXTRAP_PLOT (LDET)
         DO WHILE (LAST_MODEL_PLOT (LDET) .GT. FIRST_EXTRAP_PLOT (LDET) .AND.     &
               MODEL_ENERGY (LAST_MODEL_PLOT (LDET), LDET) .GT. RIGHT_FIT_E)
            LAST_MODEL_PLOT (LDET) = LAST_MODEL_PLOT (LDET) - 1
         END DO
      ELSE
         FIRST_MODEL_PLOT (LDET) = -1
         LAST_MODEL_PLOT (LDET) = -2
      END IF                                                   ! use det ?

   END DO                                                      ! ldet

   !  Above we erase the output array MODEL_PHOT_RATE since M_PHOTONS_MODEL
   !  doesn't do this in order to save time--M_PHOTONS_MODEL assumes that the
   !  calling routine is smart enough to ignore the unused elements but this
   !  might not be true of a routine calling MFIT:

   !  Now obtain photon model at six times the resolution of the photon bins,
   !  this is used to plot the photon model:
   !  We do this for all detectors, whether or not included in fit, via
   !  using USE_ALL_DET instead of USE_DET:

   CALL M_USE_PHOTBINS  ( NUM_DET, USE_ALL_DET, NUM_TERMS_AVAIL, NT_ADD_LINE, FIRST_EXTRAP_PLOT, LAST_EXTRAP_PLOT,   &
             6 * maxval (NUM_CHAN) +1, DIFF_EVAL, TERM_USED, PARAM, FIRST_EXTRAP_BIN, LAST_EXTRAP_BIN )
   DO_TERM = 0
   CALL M_PHOTONS_MODEL (NUM_DET, 6 * maxval (NUM_CHAN) +1, DIFF_EVAL, DIFF_NUME_TO_USE, FIRST_EXTRAP_BIN, LAST_EXTRAP_BIN,       &
           PARAM, TERM_USED, DO_TERM, USE_ALL_DET, NUM_TERMS_AVAIL, NUM_ADD_TERMS, MODEL_PHOT_RATE,  &
           MULT_FLAG, MULT_TERMS_PLT, Caller_Error )
   if ( Caller_Error /=0 )  ErrNum = Caller_Error + 830000


   ! +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


   !  Give an error message if the total photon flux model is negative anywhere:
   !  Also set FIT_ERR to report this error, unless a more severe error has already occured:

   NEG_CNT = 0
   DO LDET=0, NUM_DET - 1
      DO PBIN=FIRST_MODEL_PLOT (LDET), LAST_MODEL_PLOT (LDET)
         IF (MODEL_PHOT_RATE (PBIN, 0, LDET) .LT. 0.0) THEN
            FIT_ERR = MAX (2, FIT_ERR)
            NEG_CNT = NEG_CNT + 1
            IF (NEG_CNT .LE. 20) THEN
               WRITE (6, 3010) MODEL_PHOT_RATE (PBIN, 0, LDET), MODEL_ENERGY (PBIN, LDET)
               3010  FORMAT ( / ' *** M_PLOT_SETUP ERROR: Total photon model =', 1PG10.3, ' < 0 at', 1PG12.4, ' keV' /)
               IF (NEG_CNT .EQ. 20) WRITE (6, 3030)
               3030  FORMAT ( / ' *** Further negative photon model messages are suppressed.'/)
            END IF
         END IF
      END DO
   END DO

   !  Give another error message if the total count rate model is negative
   !  anywhere in the fit channel range:
   !  Also set FIT_ERR to report this error, unless a more severe error has already occured:

   NEG_CNT = 0
   DO LDET=0, NUM_DET - 1
      DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET),   &
               FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
         IF (MODEL_CNT_RATE (ICHAN, 0, LDET) .LT. 0.0) THEN
            FIT_ERR = MAX (9, FIT_ERR)
            NEG_CNT = NEG_CNT + 1
            IF (NEG_CNT .LE. 20) THEN
               WRITE (6, 3020) ICHAN, LDET, MODEL_CNT_RATE (ICHAN, 0, LDET)
               3020  FORMAT ( / ' *** M_PLOT_SETUP ERROR: chan', I5, ' of the', I3, 'th detector, ' /    &
                                ' has model count rate', 1PG12.4, ', which is < 0 !' /)
               IF (NEG_CNT .EQ. 20) WRITE (6, 3040)
               3040  FORMAT ( / ' *** Further negative count model messages are suppressed.'/)
            END IF
         END IF
      END DO
   END DO


   !  +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +


   !  Calculate nu-F-nu: ("data" and model):
   !  Note that both are model dependent since they depend upon the photon model
   !  Note that the definition of nu-F-nu is really d(energy flux)/d(log E) !
   !
   !  Now calculate nu-F-nu "data" from photon rate "data":
   !  Transform delta(number flux)/delta(E) with units photons / s-cm^2-keV into
   !  delta(E flux)/delta(log E) with units keV^2 / s-cm^2-keV.
   !  1) To transform delta(number flux)/delta(E) into delta(number flux),
   !     which is the number of photons in the channel, multiply by channel
   !     width CHAN_WIDTH.
   !  2) To transform to delta(energy flux), which is the energy flux in this
   !     bin, multiply by mean energy of photons in the bin, E_MEAN.   The
   !     value of E_MEAN is found by numeric integration of the photon model.
   !  3) To transform to delta(energy flux) / delta (log E), divide by
   !     delta (log E) = log (chan upper edge) - log (chan lower edge)
   !     = log ([chan upper edge] / [chan lower edge])

   !  Photon "data" PHOT_OBS_RATE etc. is not available if the efficiency EFF could not be calculated.

   DO LDET=0, NUM_DET - 1
      DPLOT = FIRST_EXTRAP_PLOT (LDET)
      DO ICHAN=0, NUM_CHAN (LDET) - 1
         IF (EFF (ICHAN, LDET) .GT. 0.0) THEN

            !  calculate effective mean energy of photons in the bin based
            !  upon the model photon funtion: this is obviously model dependent:
            EF_SUM = 0.0
            F_SUM = 0.0
            DO IS=1, 7
               EF_SUM = EF_SUM + MODEL_ENERGY (DPLOT, LDET) * MODEL_PHOT_RATE (DPLOT, 0, LDET)
               F_SUM = F_SUM + MODEL_PHOT_RATE (DPLOT, 0, LDET)
               DPLOT = DPLOT + 1
            END DO
            !  each channel has 7 energies, the last one of which is the first
            !  energy for the next channel:
            DPLOT = DPLOT - 1
            IF (F_SUM .GT. 0.0) THEN
               E_MEAN = EF_SUM / F_SUM
            ELSE
               E_MEAN = 0.0
            END IF

            !  finally, calculate nu-F-nu for the "data":
            FACTOR = E_MEAN * CHAN_WIDTH (ICHAN, LDET) / LOG (CHAN_EVAL (7, ICHAN, LDET) / CHAN_EVAL (6, ICHAN, LDET))
            NU_F_NU_DATA (ICHAN, LDET) = PHOT_OBS_RATE (ICHAN, LDET) * FACTOR
            NU_F_NU_SIG (ICHAN, LDET) = PHOT_OBS_SIG (ICHAN, LDET) * FACTOR

         ELSE

            !  when we skip the calculation for a channel, we must increment DPLOT so it will be correct for higher channels:
            DPLOT = DPLOT + 6

         END IF                                                ! eff > 0 ?
      END DO                                                   ! ichan
   END DO                                                      ! ldet


   !  Now calculate nu-F-nu model: photons / s-cm^2-keV --> keV^2 / s-cm^2-keV.
   !  In above calculation we multiplied by Emean * deltaE / delta(log E),
   !  here, for the differential curve, we simply multiply by E**2:

   DO LDET=0, NUM_DET - 1
      DO PBIN=FIRST_EXTRAP_PLOT (LDET), LAST_EXTRAP_PLOT (LDET)
         NU_F_NU_MODEL (PBIN, LDET) = MODEL_PHOT_RATE (PBIN, 0, LDET) * MODEL_ENERGY (PBIN, LDET)**2
      END DO
   END DO


   RETURN

   END SUBROUTINE M_PLOT_SETUP


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_PVALS  ( PARAM, PARAM_VARY )


   use MFIT_parameters


   CHARACTER (len=60) :: M_PVALS


   !  This function packs the values of up to 6 of the varying parameters into the output string (function return).

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.


   !  Input arguments:
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM

   !  Output is solely via function return:

   !  Internal Variables:
   integer (kind=mfit_integer) :: PCNT, JTERM, KPARAM


! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   M_PVALS = ' '
   PCNT = 0

   DO JTERM=1, MAX_TERMS
      DO KPARAM=1, MAX_PPT
         IF (PARAM_VARY (JTERM, KPARAM) .AND. PCNT .LT. 6) THEN
            PCNT = PCNT + 1
            IF (ABS (PARAM (JTERM, KPARAM)) .LT. 9.9E+9 .AND.   &
                ABS (PARAM (JTERM, KPARAM)) .GT. 1.E-9) THEN
               WRITE (M_PVALS (10*PCNT-9:10*PCNT), '(1PG10.3E1)' )  PARAM (JTERM,KPARAM)
            ELSE
               WRITE (M_PVALS (10*PCNT-9:10*PCNT), '(1PG10.2)' )  PARAM (JTERM,KPARAM)
            END IF
         END IF
      END DO
   END DO



   RETURN

   END FUNCTION M_PVALS


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_EFORMAT  ( ENERGY )

   CHARACTER (len=7) :: M_EFORMAT


   !  This function write an ENERGY into the string M_EFORMAT with a format depending on the value of the energy.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, 20 Sept 1993.

   !  Input argument:
   real (kind=mfit_real), intent (in) :: ENERGY


   !  Output is solely via function return.


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   IF (ENERGY .LT. 0.0) THEN
      M_EFORMAT = 'NEGATIV'
   ELSE
      IF (ENERGY .LE. 99.99) THEN
         WRITE (M_EFORMAT, '(F7.2)' ) ENERGY
      ELSE
         IF (ENERGY .LE. 9999.9) THEN
            WRITE (M_EFORMAT, '(F7.1)' ) ENERGY
         ELSE
            IF (ENERGY .LE. 999999.) THEN
               WRITE (M_EFORMAT, '(F7.0)' ) ENERGY
            ELSE
               IF (ENERGY .LE. 9.99E+9) THEN
                  WRITE (M_EFORMAT, '(1PG7.2E1)' ) ENERGY
               ELSE
                  M_EFORMAT = '*******'
               END IF
            END IF
         END IF
      END IF
   END IF


   RETURN

   END FUNCTION M_EFORMAT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_RESULTS_1_OUT  ( DebugMask, NUM_DET, NUM_CHAN, NUM_EBINS, FIT_CHAN, USE_DET,      &
                 NUM_TERMS_AVAIL, NUM_ADD_TERMS, NUM_PARAM_OF_TERM, TERM_USED, PARAM_VARY, PARAM, NUM_VARY,          &
                 fit_mode, STATISTIC, DOF, PARAM_UNCER, MFIT_VERSION,                                                &
                 FIT_ERR, RES_FRAC_AT511, RES_EXP, REL_CONVERG, ABS_CONVERG, MAX_TRIES,                              &
                 CHAN_ENERGY, CHAN_WIDTH, CHAN_OFFSET )


   !  Subroutine to output results of model fit: chisq, selected model terms,
   !  parameter values and uncertainties, etc.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-62, 23 September 1992.


   use MFIT_parameters

   use GSL_interfaces


   !  Arguments, all input:
   integer (kind=mfit_integer), intent (in) :: DebugMask
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NUM_ADD_TERMS
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM
   integer (kind=mfit_integer), intent (in) :: NUM_VARY
   
   !  1 = chisq, 2 = likelihood, 3 = Castor C statistic:
   integer (kind=mfit_integer), intent (in) :: fit_mode

   real (kind=mfit_DoubleReal), intent (in) :: STATISTIC
   integer (kind=mfit_integer), intent (in) :: DOF
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_UNCER
   CHARACTER (len=VERSION_LEN), intent (in) :: MFIT_VERSION
   integer (kind=mfit_integer), intent (in) :: FIT_ERR
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_FRAC_AT511
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_EXP
   real (kind=mfit_real), intent (in) :: REL_CONVERG
   real (kind=mfit_real), intent (in) :: ABS_CONVERG
   integer (kind=mfit_integer), intent (in) :: MAX_TRIES
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_ENERGY
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET


   !  Internal Variables:

   integer (kind=mfit_integer) :: ipass
   integer (kind=mfit_integer) :: JTERM, KPARAM, LDET
   integer (kind=mfit_integer) :: INDEX, UPAR
   integer (kind=mfit_integer) :: TERM_USED_CNT
   integer (kind=mfit_integer) :: DET_USED_CNT
   CHARACTER (len=70) :: ERR_MSG
   real (kind=mfit_real) :: LEFT_E, RIGHT_E
   CHARACTER (len=7) :: XFER_LEFT, XFER_RIGHT
   integer (kind=mfit_integer) :: NUM_USEABLE_PARAM

   real (kind=mfit_real) :: CHI_PROB

   integer (kind=mfit_integer) :: NUNIT
   logical :: UnitOpen


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !                      **** Initialize: ****

   do ipass=1, 2

      NUNIT = 6
      IF ( ipass .eq. 2 )  then
         NUNIT = MODULE_PARAM_FILE_UNIT
         if ( btest (DebugMask, 17)  .or.  btest (DebugMask, 16) )  call Open_DebugOutput_File  ( DebugMask, MFIT_VERSION )
      end if



      !  **** Calculate probability of chisq assuming model correct (see Numerical Recipes eq. 6.2.18): ****

      IF ( FIT_MODE == 1  .AND.  STATISTIC .GT. 0.0  .AND.  DOF .GT. 0 ) THEN
         CHI_PROB = real ( gsl_cdf_chisq_Q ( STATISTIC, real ( DOF, kind=c_double ) ), mfit_real )
      ELSE
         !  Not a chisq fit  OR  bad chisq --> bad probability
         CHI_PROB = -9.9
      END IF



      IF ( (ipass .eq. 1 .and. btest (DebugMask, 10))  .OR.  (ipass .eq. 2 .and. btest (DebugMask, 17)) ) THEN

         WRITE (NUNIT, 7010)  trim (MFIT_VERSION)
         7010  FORMAT ( ///, ' %  *%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*  %' /   &
                      '   Program trail:' /  ' q MFIT Version: ', A / )


         !  Output DRM dimensions, number of detectors passed to MFIT & # used;
         !  and for each detector: name, chan range, ebin range & ebin range
         !  used in calculations; also resolution description of each detector:

         DET_USED_CNT = 0
         DO LDET=0, NUM_DET - 1
            IF (USE_DET (LDET)) DET_USED_CNT = DET_USED_CNT + 1
         END DO

         WRITE (NUNIT, 7020) maxval (NUM_EBINS)  - 1, maxval (NUM_CHAN)  - 1, num_det - 1
         7020  FORMAT (' d DRM Dimensions: photon bins = 0:', I4, ', channels = 0:', I4, ', detectors = 0:', I3)
         WRITE (NUNIT, 7025) NUM_DET, DET_USED_CNT
         !  this was simplified because of the removal of arguments:
         7025  FORMAT ( ' B ', I3, ' detectors were passed to MFIT,', I3, ' were included in the fit:' /   &
                    '     det used?  Chan avail     Ebin avail' )
         WRITE (NUNIT, 7030) ( LDET, USE_DET (LDET),                               &
            CHAN_OFFSET (LDET), NUM_CHAN (LDET) + CHAN_OFFSET (LDET) - 1, 0, NUM_EBINS (LDET) - 1,   LDET=0, NUM_DET - 1)
         7030  FORMAT ( (' b   ', I2, 2X, L1, 2X, 2(3X, I4, 1X, I4, 3X)) )

         WRITE (NUNIT, 7040)
         7040  FORMAT ( /'     frac res at 511   resolution exponent')
         WRITE (NUNIT, 7050) ( LDET, RES_FRAC_AT511 (LDET), RES_EXP (LDET),   LDET=0, NUM_DET - 1 )
         7050  FORMAT ( (' R   ', I2, 3X, F7.4, 11X, F7.3) )



         ! Output detectors and channels used in fit:

         DO LDET=0, NUM_DET - 1
            LEFT_E = CHAN_ENERGY (FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), LDET) -          &
               0.5 * CHAN_WIDTH  (FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), LDET)
            RIGHT_E = CHAN_ENERGY (FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET), LDET) +         &
               0.5 *  CHAN_WIDTH  (FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET), LDET)
            IF (USE_DET (LDET)) THEN
               XFER_LEFT  = M_EFORMAT (LEFT_E)
               XFER_RIGHT = M_EFORMAT (RIGHT_E)
               WRITE (NUNIT, 1010) FIT_CHAN (0, LDET), FIT_CHAN (1, LDET), XFER_LEFT, XFER_RIGHT, LDET
               1010  FORMAT (' r Fit to chan', I4, ' to', I4, ', =', A, ' to', A, ' keV, of detector', I3 )
            END IF
         END DO


      END IF      ! ipass vs DebugMask bits 10 & 17




      IF ( (ipass .eq. 1 .and. btest (DebugMask, 9))  .OR.  (ipass .eq. 2 .and. btest (DebugMask, 16)) ) THEN

         !                      **** More output ****

         !  Output statistic, what kind of statistic it is, probability if the statistic is chisq.

         WRITE (NUNIT, 9020)  STATISTIC, DOF, NUM_VARY, REL_CONVERG, ABS_CONVERG, MAX_TRIES
         9020  FORMAT ( / ' x Fit has STATISTIC =', 1PG12.5, ' with', I4, ' dof &', I3, ' varying parameters.' /   &
                  '   Nonlinear convergence tests:' /                                                              &
                  ' Q Relative change <', 1PG10.4E1, ' AND Absolute change <', 1PG10.4E1 /                         &
                  ' Q Giveup after', I4, ' iterations.' )

         if ( fit_mode == 1 )  WRITE (NUNIT, 9025)  CHI_PROB
         9025  FORMAT (   '  The STATISTIC is Chi-Square !' /   &
                          '   The probability of the data exceeding this chi-square, assuming'/                    &
                          ' z the model is correct, is', 1PG10.2 )
         
         if ( fit_mode == 2 )  WRITE (NUNIT, '( "  The STATISTIC is -2 LOG LIKELIHOOD !" // )' )
         if ( fit_mode == 3 )  WRITE (NUNIT, '( "  The STATISTIC is the Castor C statistic !" // )' )



         !  **** Output the current model selection and parameter values: ****

         WRITE (NUNIT, '( "    A  U  P" )' )


         TERM_USED_CNT = 0              ! counts used terms
         DO JTERM=1, NUM_TERMS_AVAIL
            IF (TERM_USED (JTERM) .EQ. 1) TERM_USED_CNT = TERM_USED_CNT + 1
         END DO
         WRITE (NUNIT, 9310) TERM_USED_CNT
         9310 FORMAT (' o *** The FITTED PARAMETERS of the', I3, ' terms are:' /)


         INDEX = 0                      ! counts varying param
         UPAR = 0                       ! counts used param
         DO JTERM=1, NUM_TERMS_AVAIL

            !  determine number of parameters for this term.   Normally recorded
            !  in NUM_PARAM_OF_TERM, but for term #NUM_ADD_TERMS + 1, detector
            !  renormalization, we override this value based upon the number of
            !  detectors avail:
            NUM_USEABLE_PARAM = NUM_PARAM_OF_TERM (JTERM)
            IF (JTERM .EQ. NUM_ADD_TERMS + 1)  NUM_USEABLE_PARAM = MIN (NUM_PARAM_OF_TERM (JTERM), NUM_DET - 1)

            ! term used in model?:
            IF (TERM_USED (JTERM) .EQ. 1) THEN                       ! term used ?

               WRITE (NUNIT, 9245) JTERM, NUM_USEABLE_PARAM
               9245  FORMAT ( ' t ', I2, 'th term:', T68, '(', I2, ' param:)' )

               !  output parameter values for this term:
               DO KPARAM=1, NUM_USEABLE_PARAM
                  UPAR = UPAR + 1

                  !  was parameter varying in model?
                  IF (PARAM_VARY (JTERM, KPARAM)) THEN              ! param vary ?
                     INDEX = INDEX + 1

                     !  output value of parameter varied in model fit:
                     WRITE (NUNIT, 9215) UPAR, INDEX, KPARAM, PARAM (JTERM, KPARAM), PARAM_UNCER (JTERM, KPARAM)
                     9215  FORMAT ( ' v ', I2, 1X, I2, 1X, I2, 1X, ' VARY: ', 1PG11.4, ' +/- ', 1PG10.3 )

                  ELSE                                              ! param vary ?

                     !   output fixed parameter value:
                     WRITE (NUNIT, 9225) UPAR, KPARAM, PARAM (JTERM, KPARAM)
                     9225  FORMAT (' f ', I2, 4X, I2, 1X, '  FIX: ', 1PG11.4 )

                  END IF                                            ! param vary ?
               END DO                                                  ! kparam
            END IF                                                   ! term used ?
         END DO                                                        ! jte


         !  **** Output whether or not this is a fit and also any error messages:

         ERR_MSG = '?'
            
         !  Do we have a good fit?
         IF ( NUM_VARY > 0 .AND. FIT_ERR .LT. 10 .AND. FIT_ERR >= 0 ) THEN

            ERR_MSG = 'Acceptable'

            WRITE (NUNIT, 9023)  trim (MFIT_VERSION)
            9023  FORMAT ( / ' h      ******  FIT completed by ', A, '  ******' / )

         ELSE

            IF ( NUM_VARY == 0 .AND. FIT_ERR == 0 ) THEN
            
               ERR_MSG ='E *** REMINDER: THIS IS NOT A FIT -- ONLY A MODEL EVALUATION.'
               
               WRITE (NUNIT, 9026)  trim (MFIT_VERSION)
               9026  FORMAT ( / '     *** Model Results Follow.  This NOT a fit: no varying parameters! ***' //    &
                          ' h   *** Output of ', A, ' at ***' )
            ELSE
               WRITE (NUNIT, 9027)  trim (MFIT_VERSION)
               9027  FORMAT ( / 17X, '*** NO Model has been input OR severe ERROR ! *** ' //   &
                               ' h  *** Output of ', A )
            END IF

         END IF
         
         if ( NUM_VARY > 0  .and.  FIT_ERR == 0 )  ERR_MSG = 'NO ERROR'


         !  Warning message if Band's GRB model has apparently illegal parameter
         !  values (i.e, if beta >= alpha):

         IF (TERM_USED (5) .EQ. 1 .AND. PARAM (5, 4) .GE. PARAM (5, 3))  WRITE (NUNIT, 8010)
         8010  FORMAT ( // 1X,    'E  *** ERROR: Beta >= alpha is illegal !' /    &
                        '  The program actually used beta = alpha - 1.E-5',   /)


         !  Warning message if Band's GRB model has apparently illegal parameter
         !  values (i.e, if alpha <= -2):

         IF (TERM_USED (5) .EQ. 1 .AND. PARAM (5, 3) .LT. -2.0)  WRITE (NUNIT, 8030)
         8030  FORMAT ( // 1X,    'E  *** ERROR: alpha <= -2 is illegal !' /     &
                        '  The program actually used alpha = -1.9999',   /)


         !     ** ** ** ** **  Output messages re fit errors  ** ** ** ** **

         !  a list of identified errors:
         IF (FIT_ERR .EQ. 1)   ERR_MSG = 'E *** WARNING: FIT WAS MILDLY NONCONVERGENT !'
         IF (FIT_ERR .EQ. 2)   ERR_MSG = 'E *** WARNING: NEGATIVE PHOTON RATE MODEL !'
         IF (FIT_ERR .EQ. 9)   ERR_MSG = 'E *** WARNING: NEGATIVE COUNT RATE MODEL !'

         !  unidentified errors:
         IF (ERR_MSG .EQ. '?')  ERR_MSG = 'E *** *** SEVERE ERROR !    FIT_ERR ='

         !  if have an error: write out message, appending error #:
         IF ( FIT_ERR /= 0 )  WRITE (NUNIT, 2030) ERR_MSG, FIT_ERR
         2030  FORMAT ( // 1X, A, 1X, I10 )



      END IF   ! ipass vs DebugMask bits 9 & 16


#ifndef __G95__
      inquire ( unit=nunit, opened=UnitOpen )
      if ( UnitOpen )  flush (nunit)
#endif


   end do   ! pass >> output unit 6 or DebugFile


   RETURN

   END SUBROUTINE M_RESULTS_1_OUT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_RESULTS_2_OUT  ( NUM_DET, NUM_CHAN, CHAN_OFFSET, CHAN_ENERGY, CHAN_WIDTH,                 &
                    USE_DET, FIT_CHAN, NUM_TERMS_AVAIL, TERM_USED, OBS_CRATE, BACK_CRATE, BACK_CSIG, LIVE_TIME,  &
                    MODEL_CNT_RATE, PHOT_OBS_RATE, PHOT_OBS_SIG, FIRST_EXTRAP_PLOT, LAST_EXTRAP_PLOT,            &
                    MODEL_ENERGY, MODEL_PHOT_RATE, MODEL_CR_VARI, DATA_CR_VARI, NU_F_NU_DATA, NU_F_NU_SIG,       &
                    PHOT_MODEL_BYCHAN, DebugMask, MFIT_VERSION )


   !  Program to output results of model fit:
   !  For each detector, outputs by channel: input data (i.e. count rate,
   !  sigma) and the model predicted count rate and chisq for the channel.
   !  For each detector, outputs by channel: input data (energy range of
   !  the channel) and the model predicted photon flux.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-62, 23 September 1992.


   use MFIT_parameters


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_ENERGY
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1), intent (in) :: MODEL_CNT_RATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: PHOT_OBS_RATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: PHOT_OBS_SIG
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: FIRST_EXTRAP_PLOT
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: LAST_EXTRAP_PLOT
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: num_det-1), intent (in) :: MODEL_ENERGY
   real (kind=mfit_real), dimension (0: 6 * maxval (NUM_CHAN), 0: MAX_TERMS, 0: num_det-1), intent (in) :: MODEL_PHOT_RATE
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: MODEL_CR_VARI
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DATA_CR_VARI
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: NU_F_NU_DATA
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: NU_F_NU_SIG
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: PHOT_MODEL_BYCHAN
   integer (kind=mfit_integer), intent (in) :: DebugMask
   CHARACTER (len=VERSION_LEN), intent (in) :: MFIT_VERSION


   !  Internal Variables:
   integer (kind=mfit_integer) :: ICHAN, JTERM, LDET, PBIN
   real (kind=mfit_real) :: CHI_TERM                    ! chisq of a single channel
   real (kind=mfit_DoubleReal) :: CHI_DET           ! chisq summed over all chan of a single detector
   real (kind=mfit_DoubleReal) :: CHI_SUM              ! total chisq, all channels of all detectors
   integer (kind=mfit_integer) :: J
   integer (kind=mfit_integer) :: JMAX                 ! min of 4 or number of terms used
   integer (kind=mfit_integer), dimension (4) :: TERM_INDEX              ! list term #s of up to 4 used terms
   CHARACTER (len=15) :: STRING
   CHARACTER (len=1) :: PARSE
   real (kind=mfit_real) :: DIFF
   real (kind=mfit_real) :: DEDUCED_CNTS
   real (kind=mfit_real) :: TEMP_ENERGY
   CHARACTER (len=11) :: STRING_11
   CHARACTER (len=10) :: STRING_10
   CHARACTER (len=7) :: E_STRING_1, E_STRING_2
   integer (kind=mfit_integer) :: NUNIT
   logical :: UnitOpen


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   NUNIT = MODULE_PARAM_FILE_UNIT
   call Open_DebugOutput_File  ( DebugMask, MFIT_VERSION )


   !  obtain the value of JTERM for up to 4 of the used terms in the model:

   JMAX = 0
   DO JTERM=1, NUM_TERMS_AVAIL
      IF (TERM_USED (JTERM) .EQ. 1) THEN
         IF (JMAX .LT. 4) THEN
            JMAX = JMAX + 1
            TERM_INDEX (JMAX) = JTERM
         END IF
      END IF
   END DO

   CHI_SUM = 0.0

   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

   !  loop through the detectors, outputting for each detector included in fit:

      DO LDET=0, NUM_DET - 1

      !  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .

      !  output vs channel the data: (squeeze it all in!):

      WRITE (NUNIT, 3010) LDET
      3010  FORMAT ( /// 18X, '###   ###   ###   ###   ###   ###   ###   ###' /                  &
        ' w Raw Data for', I3, 'th Detector:' //                                             &
        '   Chan     E range    Observ.Rate  Back.Rate  Back.sigma  Livetime    deduced' /   &
        '             (keV)           (counts / s-keV-det)            (s)       obs cnts' )

      DO ICHAN=0, NUM_CHAN (LDET) - 1

         DEDUCED_CNTS = OBS_CRATE (ICHAN, LDET) * CHAN_WIDTH (ICHAN, LDET) * ABS (LIVE_TIME (ICHAN, LDET))
         IF (DEDUCED_CNTS .GE. 0.0) THEN
            WRITE (STRING_11, '(1PG11.5)' )  DEDUCED_CNTS
         ELSE
            WRITE (STRING_11, '(1PG11.4)' )  DEDUCED_CNTS
         END IF

         IF (LIVE_TIME (ICHAN, LDET) .LT. 0.0) THEN
            WRITE (STRING_10, '(1PG10.3)' )  LIVE_TIME (ICHAN, LDET)
         ELSE
            IF (LIVE_TIME (ICHAN, LDET) .LT. 9.9E+19) THEN
               WRITE (STRING_10, '(1PG10.5E1)' )  LIVE_TIME (ICHAN, LDET)
            ELSE
               WRITE (STRING_10, '(1PG10.4)' )  LIVE_TIME (ICHAN, LDET)
            END IF
         END IF

         TEMP_ENERGY = CHAN_ENERGY (ICHAN, LDET) - 0.5 * CHAN_WIDTH (ICHAN, LDET)
         E_STRING_1 = M_EFORMAT (TEMP_ENERGY)
         TEMP_ENERGY = CHAN_ENERGY (ICHAN, LDET) + 0.5 * CHAN_WIDTH (ICHAN, LDET)
         E_STRING_2 = M_EFORMAT (TEMP_ENERGY)

         WRITE (NUNIT, 3020) ICHAN + CHAN_OFFSET (LDET), E_STRING_1, E_STRING_2,          &
              OBS_CRATE (ICHAN, LDET), BACK_CRATE (ICHAN, LDET), BACK_CSIG (ICHAN, LDET), STRING_10, STRING_11
         3020  FORMAT (1X, 'e', I4, 1X, A, 1X, A, 1X, 1PG11.4, 1X, 1PG11.4, 1X, 1PG10.3, 1X, A10, 1X, A11)

      END DO                                                 ! ichan

   !  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .

   !  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .

   !     output vs channel the fit results in counts space including the implied chisquares:

      CHI_DET = 0.0

      WRITE (NUNIT, 1010) LDET
      1010  FORMAT ( /// 18X, '+++   +++   +++   +++   +++   +++   +++   +++' /                &
         ' K Count Rate Fit information for', I3,'th Detector = Detector:' //              &
         '              Net         Model        Model                            Data' /  &
         '   Chan   Count Rate   Count Rate      Sigma        Chi-Sq             Sigma' /  &
         '                   (counts / s-keV-det)' )

      DO ICHAN=0, NUM_CHAN (LDET) - 1

         ! identify four cases and end output line differently for each case:

         PARSE = 'C'                              ! det/chan not used in fit
         IF ( USE_DET (LDET) .AND. LIVE_TIME (ICHAN, LDET) .GT. 0.0 .AND.      &
                ICHAN .GE. FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET) .AND.       &
                ICHAN .LE. FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET) ) THEN

            !  Case 1: have data and it was used in fit, output chisq for this channel:
            DIFF = OBS_CRATE (ICHAN, LDET) - BACK_CRATE (ICHAN, LDET) - MODEL_CNT_RATE (ICHAN, 0, LDET)
            CHI_TERM = DIFF**2 / MODEL_CR_VARI (ICHAN, LDET)
            CHI_SUM = CHI_SUM + CHI_TERM
            CHI_DET = CHI_DET + CHI_TERM
            WRITE (STRING, '(1PG13.5, 2X)' ) CHI_TERM
            PARSE = 'c'                              ! det/chan used in fit
         ELSE
            IF (LIVE_TIME (ICHAN, LDET) .LE. 0.0) THEN
               !  Case 2: no data, as flagged by non-positive sigma:
               STRING = '<-- Bad Data *'
            ELSE
                  IF (.NOT. USE_DET (LDET)) THEN
                  !  Case 3: detector not included in fit:
                  STRING = 'Det not in fit'
               ELSE
                  !  Case 4: channel not included in fit:
                  STRING = 'Chan not in fit'
               END IF
            END IF
         END IF

         !  Now output the line with the customized termination STRING and customized prefix PARSE:

         WRITE (NUNIT, 9580) PARSE, ICHAN + CHAN_OFFSET (LDET), OBS_CRATE (ICHAN, LDET) - BACK_CRATE (ICHAN, LDET),    &
             MODEL_CNT_RATE (ICHAN, 0, LDET), SQRT (MODEL_CR_VARI (ICHAN, LDET)), STRING, SQRT (DATA_CR_VARI (ICHAN, LDET))
         9580  FORMAT (1X, A, I4, 2X, 1PG12.4, 1X, 1PG12.4, 2X, 1PG12.4, 2X, A, 3X, 1PG12.4)

      END DO                                                 ! ichan

      IF (USE_DET (LDET)) WRITE (NUNIT, 1020) CHI_DET
      1020  FORMAT (' s Chi-Square for this detector:', 16X, 1PG13.5)

      !  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .

      !  output vs channel the channel energy range and model photon flux,
      !  regular photon number flux units and nuFnu units:


      WRITE (NUNIT, 9590) LDET
      9590  FORMAT ( /// 18X, '+++   +++   +++   +++   +++   +++   +++   +++' /                 &
         ' H Photon Rate Fit information for',I3,'th Detector = Detector:' //               &
         '              "Data"     "Data" P.R.       Model         "Data"        nuFnu' /   &
         '    Chan   Photon Rate      Sigma       Photon Rate       nuFnu        Sigma' /   &
         '                     (photons / s-keV-cm^2)               (keV^2/s-cm^2-keV)' )

      DO ICHAN=0, NUM_CHAN (LDET) - 1
         WRITE (NUNIT, 9610)  ICHAN + CHAN_OFFSET (LDET), PHOT_OBS_RATE (ICHAN, LDET), PHOT_OBS_SIG (ICHAN, LDET),   &
             PHOT_MODEL_BYCHAN (ICHAN, LDET), NU_F_NU_DATA (ICHAN, LDET), NU_F_NU_SIG (ICHAN, LDET)
         9610  FORMAT (' p ', I4, 3X, 1PG12.4, 1X, 1PG12.4, 4X, 1PG12.4, 4X, 1PG12.4, 1X, 1PG12.4)
      END DO

   !  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .

   !  If requested, output on a fine energy scale (i.e. six times per
   !  channel) the total photon model and up to 4 of the model terms.

   WRITE (NUNIT, 8510) (TERM_INDEX (J), J=1, JMAX)
   8510  FORMAT ( /// 18X, '+++   +++   +++   +++   +++   +++   +++   +++' /           &
      ' M Photon Model on a fine scale: Total and up to 4 component terms:' /    &
      '     Energy       Model Photon Rates' /                                   &
      '      (keV)      (photons /s-keV-cm^2)'  /                                &
      '  Terms:          NET     ', 4 (6X, I2, 6X) )

   DO PBIN=FIRST_EXTRAP_PLOT (LDET), LAST_EXTRAP_PLOT (LDET)
      E_STRING_1 = M_EFORMAT (MODEL_ENERGY (PBIN, LDET))
      WRITE (NUNIT, 8520) E_STRING_1, MODEL_PHOT_RATE (PBIN, 0, LDET), (MODEL_PHOT_RATE (PBIN, TERM_INDEX (J), LDET), J=1, JMAX)
      8520  FORMAT ( ' m ', A, 2X, 1PG12.4, 4(2X, 1PG12.4) )
   END DO




   END DO                                                        ! ldet

   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

   IF (NUM_DET .GT. 1) WRITE (NUNIT, 9585) CHI_SUM
   9585  FORMAT ( / ' S Total Chi-Square for all detectors:', 10X, 1PG13.5)


#ifndef __G95__
    inquire ( unit=nunit, opened=UnitOpen )
    if ( UnitOpen )  flush (nunit)
#endif


   RETURN

   END SUBROUTINE M_RESULTS_2_OUT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_SOONG  ( ENERGY, I0, ALPHA, E_B, E_FOLD, E_W, E_CEN, FWHM, E_PIV )

   real (kind=mfit_real) :: M_SOONG


   !  Function for Her X-1 from Yang Soong's thesis, p. 93

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-66, 1995 March 28, rev 1996 June 10.

   !   Arguments, all input:
   real (kind=mfit_real), intent (in) :: ENERGY    ! keV
   real (kind=mfit_real), intent (in) :: I0        ! amplitude
   real (kind=mfit_real), intent (in) :: ALPHA     ! power law index
   real (kind=mfit_real), intent (in) :: E_B       ! break energy
   real (kind=mfit_real), intent (in) :: E_FOLD    ! folding energy
   real (kind=mfit_real), intent (in) :: E_W       ! equivalent width
   real (kind=mfit_real), intent (in) :: E_CEN     ! centroid
   real (kind=mfit_real), intent (in) :: FWHM      ! FWHM
   real (kind=mfit_real), intent (in) :: E_PIV     ! pivot energy

   !   Output is solely by function value.

   !   Internal variables:
   real (kind=mfit_real) :: MF, G


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   IF (ENERGY .LE. E_B) THEN
      M_SOONG = I0 * (ENERGY / E_PIV)**(-ALPHA)
   ELSE
      MF = EXP (E_B / E_FOLD) * E_B * (E_B / E_PIV)**(-ALPHA)
      M_SOONG = I0 * MF * EXP (-ENERGY / E_FOLD) / ENERGY
   END IF

   G = 0.94 / FWHM * EXP (-2.76 *  ((ENERGY - E_CEN) / FWHM)**2)
   M_SOONG = M_SOONG * (1. - E_W * G)


   RETURN

   END FUNCTION M_SOONG


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_SUNY_INTG  ( T, params )  bind ( C, name="m_suny_intg" )

   use iso_c_binding

   use SHR_VARS_sunyaev_titarchuk

   !  This function is numerically integrated using a GSL routine, which is
   !  called from MFIT Fortran 95 function M_SUNYAEV.  M_SUNYAEV passes
   !  some parameters "on the side", bypassing the GSL interface, using
   !  module SHR_VARS_sunyaev_titarchuk.  These parameters are constant
   !  during an integration, during which the GSL routine varies the integration
   !  variable T.


   real (kind=mfit_DoubleReal) :: M_SUNY_INTG


   !   Input argument:
   real (kind=mfit_DoubleReal), VALUE, intent (in) :: T        ! integration variable
   type (c_ptr), intent (in) :: params     ! not used



   !   function to be numerically integrated by M_SUNYAEV in order to evaluate
   !   the Sunyaev-Titarchuk model.
   !   Michael S. Briggs, UAH / ES-66 MSFC.

   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   M_SUNY_INTG = T**(MODULE_VAR_N_ST-1.) * EXP (-T) * (1. + T / MODULE_VAR_X_ST) ** (MODULE_VAR_N_ST + 3.)


   RETURN

   END FUNCTION M_SUNY_INTG


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   subroutine M_SUNYAEV  ( AMPLITUDE, ELECTRON_E, TAU, GEO_FAC, PHOTON_E, result, error )

   use GSL_interfaces

   use SHR_VARS_sunyaev_titarchuk

   !  Function to be integrated: M_SUNY_INTG.
   !  The GSL numerical intergration routine integrates M_SUNY_INTG versus one variable.
   !  On the side, bypassing the GSL interface, this function passes parameters
   !  to the integrand function using the module SHR_VARS_sunyaev_titarchuk.


   !  Evaluates Sunyaev-Titarchuk Comptonization spectrum with overall
   !  strength AMPLITUDE, electron energy ELECTRON_E, and optical depth TAU,
   !  based upon geometry factor GEO_FAC.
   !  Gives spectrum as function of photon energy PHOTON_E.

   !  See Sunyaev & Titarchuk, Astron. Astrohpys., vol. 86, pp. 121-138, 1980;
   !  Pat Nolan, UCSD Ph.D thesis, esp. Appendix 1, 1982;
   !  High Energy Astrophysics by J. Katz.
   !  My notation, etc, follows Pat Nolan's thesis, Appendix 1.
   !  The S-T Comptonization spectrum is giving by an integral from 0 to
   !  infinity.  The basic method herein is numerical integration.


   !  revised 2009 May 3 by MSB: converted to a subroutine since it now has two
   !  outputs: the spectrum evaluated at the parameter values, and an error return.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Revised MSB 1996 June 12: back to using common block.
   !  Revised MSB 26 July 1993: new parameterization:
   !  old parameters were: amplitude, electron energy, gamma.
   !  new parameters: amplitude, electron energy, optical depth & geometryfactor
   !  The first two are unchanged; we now calculate gamma from the optical
   !  depth and the geometry factor.   That latter should never have a value
   !  other than 3 or 12, for a sphere and a disk respectively.
   !  Michael S. Briggs, 16 Oct 1990.


   !   Input variables:
   real (kind=mfit_real), intent (in) :: AMPLITUDE       ! normalizes the strength
   real (kind=mfit_real), intent (in) :: ELECTRON_E      ! scattering electron energy, keV
   real (kind=mfit_real), intent (in) :: TAU             ! dimensionless optical depth
   real (kind=mfit_real), intent (in) :: GEO_FAC  ! geometry factor, only values: 3 for sphere, 12 for disk
   real (kind=mfit_real), intent (in) :: PHOTON_E        ! emitted photon energy, keV
   
   !   Output variables:
   real (kind=mfit_real), intent (out) :: result
   integer (kind=mfit_integer), intent (out) :: error


   !   Internal variables:
   real (kind=mfit_real) :: GAMMA          ! param related to optical depth
   real (kind=mfit_DoubleReal) :: A, B           ! interval to integrate over
   real (kind=mfit_DoubleReal) :: ANSWER         ! value of integral
   real (kind=mfit_DoubleReal) :: ErrEst         ! error estimate on result
   type (c_funptr) :: integrand_ptr
   integer (kind=mfit_integer) :: status

   real (kind=mfit_real), parameter :: PI = 3.141592


   !  function to be integrated: M_SUNY_INTG


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   error = 0

   MODULE_VAR_X_ST = PHOTON_E / ELECTRON_E
   GAMMA = PI**2 * 511. / (GEO_FAC * ELECTRON_E * (TAU + 0.666666)**2)
   MODULE_VAR_N_ST = -1.5 + SQRT (GAMMA + 2.25)

   !   Do integral from 0 to +infinity:
   !   Given the exp (-integrand) in M_SUNY_INTG, +80 is a decent approximation to +infinity:
   A = 0.
   B = 80.
   integrand_ptr = c_funloc ( M_SUNY_INTG )
   status = integrate_using_gsl ( A, B, 0.0_mfit_DoubleReal, 2.0E-5_mfit_DoubleReal, integrand_ptr, ANSWER, ErrEst )
   if ( status /= 0 ) then
      error = status
      write (*, '( / "Numeric integration failure in subroutine M_SUNYAEV.  status=", I6 / )' )  status
   end if
   
   result = AMPLITUDE * MODULE_VAR_X_ST**2 * EXP (-MODULE_VAR_X_ST) * real ( ANSWER, mfit_real )


   RETURN

   END subroutine M_SUNYAEV


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_TANAKA (ENERGY, AMPLITUDE, ALPHA, KT, TAU1, TAU2, NUMLINES, E_L, W, EPIVOT)

   real (kind=mfit_real) :: M_TANAKA


   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Program by Bob Wilson?   Revised by M. Briggs.
   !  Reference: J. E. Grove, et al., ApJ, 438: L25--L28 (1995).

   ! Output solely by function return.


   ! The input arguments.
   real (kind=mfit_real), intent (in) :: ENERGY
   real (kind=mfit_real), intent (in) :: AMPLITUDE
   real (kind=mfit_real), intent (in) :: ALPHA
   real (kind=mfit_real), intent (in) :: KT           ! the folding energy
   real (kind=mfit_real), intent (in) :: TAU1, TAU2
   real (kind=mfit_real), intent (in) :: NUMLINES
   real (kind=mfit_real), intent (in) :: E_L          ! the line centroid
   real (kind=mfit_real), intent (in) :: W            ! line_width
   real (kind=mfit_real), intent (in) :: EPIVOT


   ! Internal variables:
   integer (kind=mfit_integer) :: N
   integer (kind=mfit_integer) :: LOOPEND
   real (kind=mfit_real) :: C_E          ! the continuum term
   real (kind=mfit_real) :: C_EPIVOT
   real (kind=mfit_real) :: L_E          ! the absorption profile
   real (kind=mfit_real) :: L_EPIVOT
   real (kind=mfit_real) :: USE_TAU


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   LOOPEND = INT (NUMLINES+0.001)
   IF (LOOPEND .NE. 0 .AND. LOOPEND .NE. 1 .AND. LOOPEND .NE. 2) THEN
      WRITE (6, *) 'M_TANAKA ERROR: BAD NUMLINES:', NUMLINES
      STOP
   END IF

   !   ......... compute C_E (the continuum term)  ........................
   C_E = (ENERGY ** (-ALPHA)) * EXP((-ENERGY) / KT)
   C_EPIVOT = (EPIVOT ** (-ALPHA)) * EXP((-EPIVOT) / KT)

   !   ......... compute L_E (the absorption profile) .....................
   L_E = 0.0
   L_EPIVOT = 0.0
   USE_TAU = 0.0

   DO N = 1, LOOPEND
      IF (N .EQ. 1) USE_TAU = TAU1
      IF (N .EQ. 2) USE_TAU = TAU2

      L_E = L_E + (USE_TAU * ((N * W) ** 2) * (ENERGY / (N * E_L)) ** 2)  /   &
                  ( (ENERGY - (N * E_L)) ** 2 + (N * W) ** 2)

      L_EPIVOT = L_EPIVOT + (USE_TAU * ((N * W) ** 2) * (EPIVOT / (N * E_L)) ** 2)  /   &
                        ( (EPIVOT - (N * E_L)) ** 2 + (N * W) ** 2)

   END DO

   ! ................ compute the result ..................
   M_TANAKA = AMPLITUDE * C_E * EXP (L_EPIVOT - L_E)/ C_EPIVOT


    RETURN

    END FUNCTION M_TANAKA


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_TEST_INPUT  ( NUM_DET, NUM_CHAN, RES_FRAC_AT511, RES_EXP, FIT_MODE,     &
                FIT_CHAN, CHAN_OFFSET, USE_DET, OBS_CRATE )

   !  This subroutine checks its arguments for legality.   Variables are
   !  checked to see that they are reasonable, e.g. number of detectors >= 0,
   !  and also to see that they are compatible with the physical (=declared)
   !  sizes of various arrays.  This subroutine is called by MFIT
   !  to check some of MFIT's input arguments and also to check compatibility
   !  of variables read from a previous session file (command RS) with the
   !  current physical (=declared) sizes of various arrays.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-64, 22 November 1992.
   !  MSB 30 May 93: add RES_FRAC_AT511 & RES_EXP.


   use MFIT_parameters


   ! Arguments, all input:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_CHAN
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_FRAC_AT511
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_EXP
   integer (kind=mfit_integer), intent (in) :: fit_mode
   integer (kind=mfit_integer), dimension (0:1, 0: NUM_DET-1), intent (in) :: FIT_CHAN
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: CHAN_OFFSET
   logical (kind=mfit_logical), dimension (0: NUM_DET-1), intent (in) :: USE_DET
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (in) :: OBS_CRATE

   ! Internal variables:
   integer (kind=mfit_integer) :: ICHAN, LDET


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  Check input vs max array sizes:

   IF (NUM_DET .LT. 1) THEN
     WRITE (6, *) '*** MFIT: ERROR # 1: BAD VALUE OF NUM_DET !', NUM_DET
      STOP
   END IF

   DO LDET=0, NUM_DET - 1

      !  test resolution for reasonableness & legality:
      IF (RES_FRAC_AT511 (LDET) .LT. 0.04 .OR.    &
          RES_FRAC_AT511 (LDET) .GT. 0.3 .OR.     &
          RES_EXP (LDET) .LT. -1.0 .OR. RES_EXP (LDET) .GT. -0.1) THEN
         WRITE (6, *)  ' *** MFIT: ERROR # 7: SUSPICIOUS OR BAD VALUES FOR DETECTOR RESOLUTION !'
         WRITE (6, *) LDET, RES_FRAC_AT511 (LDET), RES_EXP (LDET)
      END IF

   END DO   ! LDET
   
   
   !  Selecting Likelihood (fit_mode = 2) or Castor C-statistic) (fit_mode = 3) implies
   !  that the data are Possion.   Negative observed rates are impossible under this
   !  assumption and cannot be handled by M_CALC_LOGLIKE.   Negative observed rates
   !  might occur if background-subtracted data is mistakenly (under the cases fit_mode = 2
   !  or 3) passed to MFIT.
   
   if ( fit_mode .eq. 2  .or.  fit_mode .eq. 3 ) then
   
      DO LDET=0, NUM_DET - 1
         IF (USE_DET (LDET)) THEN
            DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET), FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
               if ( obs_crate (ichan, ldet)  <  0.0_mfit_real ) then
                  
                  write (6, *)  'In fit_mode', fit_mode, ', the statistics must be Possion and no observed rate can be negative!'
                  write (6, *)  ldet, ichan, chan_offset (ldet), obs_crate (ichan, ldet)
                  STOP

               end if
            end do
         end if
       end do
   
   end if


   RETURN

   END SUBROUTINE M_TEST_INPUT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_INTG1_TITARCHUK  ( T, params )  bind ( C, name="m_intg1_titarchuk" )

   use iso_c_binding

   use SHR_VARS_new_titarchuk

   !  This function is numerically integrated using a GSL routine, which is
   !  called from MFIT Fortran 95 function M_TITARCHUK.  M_TITARCHUK passes
   !  some parameters "on the side", bypassing the GSL interface, using
   !  module SHR_VARS_new_titarchuk.  These parameters are constant during an
   !  integration, during which the GSL routine varies the integration variable T.


   real (kind=mfit_DoubleReal) :: M_INTG1_TITARCHUK


   !  modified by S.N.Zhang for Titarchuk model of optically thin media
   !  function to be numerically integrated in order to evaluate the new Titarchuk model.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / ES-66 MSFC.     revised 1996 June 12.


   !  Input argument:
   real (kind=mfit_DoubleReal), VALUE, intent (in) :: T          ! interation variable
   type (c_ptr), intent (in) :: params        ! not used


   !  Internal variables:
   real (kind=mfit_real) :: TEMP
   real (kind=mfit_real) :: t4


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   t4 = real ( T, mfit_real )
   
   TEMP = (MODULE_VAR_N_TIT-1.0) * log (t4) - t4 + (MODULE_VAR_N_TIT+3.0) * log (t4+MODULE_VAR_X_TIT)
   M_INTG1_TITARCHUK = EXP (TEMP-MODULE_VAR_MAXI1_TIT)            ! maxi avoid overflow


   RETURN

   END FUNCTION M_INTG1_TITARCHUK


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_INTG2_TITARCHUK  ( T, params )  bind ( C, name="m_intg2_titarchuk" )

   use iso_c_binding

   use SHR_VARS_new_titarchuk

   !  This function is numerically integrated using a GSL routine, which is
   !  called from MFIT Fortran 95 function M_TITARCHUK.  M_TITARCHUK passes
   !  some parameters "on the side", bypassing the GSL interface, using
   !  module SHR_VARS_new_titarchuk.  These parameters are constant during an
   !  integration, during which the GSL routine varies the integration variable T.


   real (kind=mfit_DoubleReal) :: M_INTG2_TITARCHUK


   !  modified by S.N.Zhang for Titarchuk model of optically thin media
   !  function to be numerically integrated in order to evaluate the new Titarchuk model.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / ES-66 MSFC, revised 1996 June 12.


   !  Input argument:
   real (kind=mfit_DoubleReal), VALUE, intent (in) :: T        ! interation variable
   type (c_ptr), intent (in) :: params     ! not used


   !  Internal variable:
   real (kind=mfit_real) :: TEMP
   real (kind=mfit_real) :: t4


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   t4 = real ( T, mfit_real )

   TEMP = ( 2.0 * MODULE_VAR_N_TIT + 3.0) * log (t4) - t4
   M_INTG2_TITARCHUK = EXP (TEMP - MODULE_VAR_MAXI2_TIT)      !to avoid overflow


   RETURN

   END FUNCTION M_INTG2_TITARCHUK


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   subroutine M_TITARCHUK  ( AMPLITUDE, ELECTRON_E, TAU, GEO_FAC, PHOTON_E, result, error )

   use GSL_interfaces

   use SHR_VARS_new_titarchuk
   

   !  Functions to be integrated: M_INTG1_TITARCHUK and M_INTG2_TITARCHUK
   !  The GSL numerical intergration routine integrates these functions versus one variable.
   !  On the side, bypassing the GSL interface, this function passes parameters
   !  to the integrand functions using the module SHR_VARS_new_titarchuk.


   !  Evaluates Titarchuk Comptonization spectrum with overall
   !  strength AMPLITUDE, electron energy ELECTRON_E, and optical depth TAU,
   !  based upon geometry factor GEO_FAC (3 for Spherical and 12 for Disk)
   !  Gives spectrum as function of photon energy PHOTON_E.

   !  See Hua and Titarchuk, ApJ, 1995;
   !  S.N. Zhang, 16 Oct 1990. Currently only applicable for optically thin case
   !  where Tau<2.  Based on M. Briggs original program for S-T model.
   
   !  revised 2009 May 3 by MSB: converted to a subroutine since it now has two
   !  outputs: the spectrum evaluated at the parameter values, and an error return.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.


   !  Input variables:
   real (kind=mfit_real), intent (in) :: AMPLITUDE                        ! normalizes the strength
   real (kind=mfit_real), intent (in) :: ELECTRON_E                       ! scattering electron energy, keV
   real (kind=mfit_real), intent (in) :: TAU                              ! dimensionless optical depth
   real (kind=mfit_real), intent (in) :: GEO_FAC  ! geometry factor, only values: 0 for sphere, 1 for disk
                                                  ! MSB: I think it should be:    3,            12
   real (kind=mfit_real), intent (in) :: PHOTON_E                         ! emitted photon energy, keV

   !  Output variables
   real (kind=mfit_real), intent (out) :: result
   integer (kind=mfit_integer), intent (out) :: error

   !  Internal variables:
   real (kind=mfit_real) :: GAMMA,GAMMA0                ! param related to optical depth
   real (kind=mfit_DoubleReal) :: A, B                    ! interval to integrate over
   real (kind=mfit_DoubleReal) :: ANSWER1,ANSWER2        ! value of integral
   real (kind=mfit_DoubleReal) :: ErrEst                !  error estimate on result
   type (c_funptr) :: integrand_ptr
   real (kind=mfit_real) :: BETA, Z, N0
   real (kind=mfit_real) :: THETA      !ELECTRON_E/511 KEV
   real (kind=mfit_real) :: BB, CC, T_MAX
   integer (kind=mfit_integer) :: status

   real (kind=mfit_real), parameter :: PI = 3.141592


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   error = 0

   MODULE_VAR_X_TIT = PHOTON_E / ELECTRON_E
   THETA = ELECTRON_E/511.
   Z = PHOTON_E/511.

   IF (GEO_FAC .GE. 4.0) THEN
      BETA = (PI**2/12/(TAU+0.666667)**2)*(1-EXP(-1.35*TAU)) + 0.45*EXP(-3.7*TAU)* log(10./3./TAU)
   ELSE
      BETA = (PI**2/3/(TAU+0.666667)**2)*(1-EXP(-0.7*TAU)) + EXP(-1.4*TAU)* log(4./3./TAU)
   ENDIF

   GAMMA0 = BETA*(8+15*THETA)/THETA/(8+19*THETA)
   GAMMA = GAMMA0*(1+4.6*Z+1.1*Z**2)/THETA
   N0 = -1.5 + SQRT (GAMMA0 + 2.25)
   MODULE_VAR_N_TIT = -1.5 + SQRT (GAMMA + 2.25)

   !  Do integral from 0 to +infinity:
   !  Given the exp (-integrand) in M_TITARCHUK_INTEGRAND, +80 is a decent
   !  approximation to +infinity:

   BB = MODULE_VAR_X_TIT - 2 * MODULE_VAR_N_TIT-2
   CC = -(MODULE_VAR_N_TIT-1) * MODULE_VAR_X_TIT
   T_MAX=0.5*(-BB+sqrt(BB**2-4*CC))
   MODULE_VAR_MAXI1_TIT = (MODULE_VAR_N_TIT-1)* log(T_MAX)-T_MAX+(MODULE_VAR_N_TIT+3)* log(T_MAX+MODULE_VAR_X_TIT)
   A = T_MAX-sqrt(MODULE_VAR_N_TIT)*5
   if(A.lt.0) A=0.
   B = T_MAX+sqrt(MODULE_VAR_N_TIT)*5+10

   integrand_ptr = c_funloc ( M_INTG1_TITARCHUK )
   status = integrate_using_gsl ( A, B, 0.0_mfit_DoubleReal, 3.0E-4_mfit_DoubleReal, integrand_ptr, ANSWER1, ErrEst )
   if ( status /= 0 ) then
      error = status
      write (*, '( / "Numeric integration failure in subroutine M_TITARCHUK.  A.  status=", I6 / )' )  status
   end if

   T_MAX = 2*MODULE_VAR_N_TIT+3
   MODULE_VAR_MAXI2_TIT = (2*MODULE_VAR_N_TIT+3)* log(T_MAX)-T_MAX
   A = T_MAX - sqrt(MODULE_VAR_N_TIT)*5
   if(A.lt.0) A=0.
   B = T_MAX + sqrt(MODULE_VAR_N_TIT)*5+10

   integrand_ptr = c_funloc ( M_INTG2_TITARCHUK )
   status = integrate_using_gsl ( A, B, 0.0_mfit_DoubleReal, 3.0E-4_mfit_DoubleReal, integrand_ptr, ANSWER2, ErrEst )
   if ( status /= 0 ) then
      error = status
      write (*, '( / "Numeric integration failure in subroutine M_TITARCHUK.  A.  status=", I6 / )' )  status
   end if


   result = log (AMPLITUDE) - MODULE_VAR_X_TIT - (N0+1.0) * log (MODULE_VAR_X_TIT)     &
            + log ( real (ANSWER1, mfit_real) ) + MODULE_VAR_MAXI1_TIT                 &
            - log ( real (ANSWER2, mfit_real) ) - MODULE_VAR_MAXI2_TIT
   result = EXP (result)


   RETURN

   END subroutine M_TITARCHUK


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_UNDETERMINED ( DebugMask, NUM_TERMS_AVAIL, NUM_PARAM_OF_TERM, PARAM, PARAM_UNCER, PARAM_VARY,   &
                 UNDETERMINE_PRIORITY, UNDETERMINE_REL_REQ, UNDETERMINE_ABS_REQ, HIGHEST_UD_PRIORITY,           &
                 UNDETERMINE_FLAG, PARAM_VARY_UNDETERMINE )

   !  This subroutine identifies "undetermined" parameters (acutally,
   !  ill-determined) and fixes them.
   !  It is called only in batch mode; in interactive mode the user makes
   !  such decisions.
   !  An "undetermined" parameter is one whose uncertainty is excessive.
   !  The definitions of excessive, UNDETERMINE_REL_REQ and UNDETERMINE_ABS_REQ,
   !  are read by subroutine MFILE_READ from the file passed in argument
   !  MFIT_INFO_FILE.  The first requirement is a relative one, i.e.,
   !  PARAM_UNCER is compared to UNDETERMINE_REL_REQ * PARAM, while the second
   !  is an absolute requirement.    Both requirements are used in an OR mode:
   !  the parameter is undetermined if its uncertainty exceeds either requirement.
   !  If the value of a requirement is <= 0, then that requirement is not used.
   !  The testing for indeterminant parameters are done in the order of
   !  the priorities UNDETERMINE_PRIORIY, also read from MFIT_INFO_FILE.
   !  Parameters of priority 0 are never tested.   The higest or first priority
   !  is 1, the second 2, etc.    If there are ties, the parameters are tested
   !  in the order in which they appear in the file.
   !  If we find an undetermined parameter, we set UNDETERMINE_FLAG to so
   !  indicated, and fix that parameter in PARAM_VARY_UNDETERMINE.
   !  We then return to MFIT (w/o checking for additional undetermined params),
   !  where the best guesses will be restored, the parameter variations setup
   !  from PARAM_VARY_UNDETERMINE, and the fit redone.    We only fix one
   !  undetermined parameter at at time, because fixing one may make others
   !  determined.
   !  You may, of course, copy the file pointed to by MFIT_INFO_FILE, edit it
   !  to change the parameters controlling the determination of "undetermined",
   !  and then change the file passed in the argument MFIT_INFO_FILE to point
   !  to your version of the file.   This is done outside of MFIT by WINGSPAN.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Minor revisions by MSB, 2008 Dec 4.
   !  Michael S. Briggs, 3 Oct 1993.    MSFC ES-66 / UAH


   use MFIT_parameters


   !  Input variables:
   integer (kind=mfit_integer), intent (in) :: DebugMask
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), dimension (MAX_TERMS), intent (in) :: NUM_PARAM_OF_TERM
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_UNCER
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM_VARY
   integer (kind=mfit_integer), dimension (MAX_TERMS, MAX_PPT), intent (in) :: UNDETERMINE_PRIORITY
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: UNDETERMINE_REL_REQ
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: UNDETERMINE_ABS_REQ
   integer (kind=mfit_integer), intent (in) :: HIGHEST_UD_PRIORITY


   !  Output arguments:
   logical (kind=mfit_logical), intent (out) :: UNDETERMINE_FLAG
   logical (kind=mfit_logical), dimension (MAX_TERMS, MAX_PPT), intent (out) :: PARAM_VARY_UNDETERMINE


   !  Internal variables:
   integer (kind=mfit_integer) :: IPRIORITY
   integer (kind=mfit_integer) :: JTERM
   integer (kind=mfit_integer) :: KPARAM


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  mark that there are no undetermined parameters until we find one:
   UNDETERMINE_FLAG = .FALSE.

   !  copy current variation status to PARAM_VARY_UNDETERMINE as a starting point:
   DO JTERM=1, NUM_TERMS_AVAIL
      DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)
         PARAM_VARY_UNDETERMINE (JTERM, KPARAM) = PARAM_VARY (JTERM, KPARAM)
      END DO
   END DO


   !  search for an undetermine parameter--fix the first one we find:

   DO IPRIORITY=1, HIGHEST_UD_PRIORITY
      DO JTERM=1, NUM_TERMS_AVAIL
         DO KPARAM=1, NUM_PARAM_OF_TERM (JTERM)

            ! does parameter (JTERM, KPARAM) have priority IPRIORITY for
            ! testing for being undetermined?   Is this parameter varying?
            IF ( PARAM_VARY (JTERM, KPARAM) .AND. UNDETERMINE_PRIORITY (JTERM, KPARAM) .EQ. IPRIORITY ) THEN

               ! is the parameter undetermined according to absolute requirement?
               IF ( UNDETERMINE_ABS_REQ (JTERM, KPARAM) .GT. 0.0 .AND.   &
                    PARAM_UNCER (JTERM, KPARAM) .GT. UNDETERMINE_ABS_REQ (JTERM, KPARAM) )  UNDETERMINE_FLAG = .TRUE.

               ! is the parameter undeterimined according to relative requirement?
               IF ( UNDETERMINE_REL_REQ (JTERM, KPARAM) .GT. 0.0 .AND.                                                     &
                    PARAM_UNCER (JTERM, KPARAM) .GT. ABS (PARAM (JTERM, KPARAM) * UNDETERMINE_REL_REQ (JTERM, KPARAM)) )   &
                        UNDETERMINE_FLAG = .TRUE.

               ! if this parameter is undetermined, fix it and return to do
               ! another fit; also, if control variables so indicated, output this decision:

               IF (UNDETERMINE_FLAG) THEN

                  if ( btest (DebugMask, 24) )  WRITE (6, 1010)  KPARAM, JTERM, PARAM (JTERM, KPARAM), PARAM_UNCER (JTERM, KPARAM)
                  1010  FORMAT ( / 'Parameter', I4, ' of term', I4, ' is undetermined:' /                   &
                       ' Param value is', 1PG12.5, ' +/- ', 1PG11.4, '.  Will fix param and redo fit !' )

                  PARAM_VARY_UNDETERMINE (JTERM, KPARAM) = .FALSE.

                  ! This return point means that a parameter has been fixed !!!
                  RETURN                                    ! **************

               END IF      !  UNDETERMINE_FLAG ?

            END IF                    ! varying param of priority IPRIORITY?
         END DO                                    ! kparam
      END DO                                       ! jterm
   END DO                                          ! ipriority


   !  If we get here, no currently varying parameters are undetermined!
   if ( btest (DebugMask, 24) )  WRITE (6, 1020)
   1020  FORMAT ( / 'No parameters were undetermined.' )


   RETURN

   END SUBROUTINE M_UNDETERMINED


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE M_USE_PHOTBINS    &
         ( NUM_DET, USE_DET, NUM_TERMS_AVAIL, NT_ADD_LINE, FIRST_BIN_CALC, LAST_BIN_CALC, ACTUAL_DIM,  &
          PHOT_EVAL, TERM_USED, PARAM, FIRST_PHOT_BIN, LAST_PHOT_BIN )


   !  This subroutine determines the photon bin range for which it is
   !  neccessary to calculate the photon model and multiply by the DRM.
   !  Only doing the model calculation and DRM multiplication for the
   !  photon bins in which the model is nonzero saves significant amounts
   !  of cpu time.    The only case where we can decrease the bin range
   !  from the default is for lines, since lines are essentially zero for
   !  some photon bins.
   !  The required photon bin range, FIRST_PHOT_BIN to LAST_PHOT_BIN, is a
   !  function of the model parameters PARAM and so must be recalculated
   !  whenever these have been changed.

   !  Note: argument dummy arguemnt PHOT_EVAL is actually PHOT_EVAL except on
   !  some calls by M_PLOT_SETUP:

   !  dummy argument name    typical          alternative             alternative
   !  name                   actual arg       actual arg 1            actual arg 2
   !  FIRST_BIN_CALC, LAST.. 0 to num_ebins-1 FIRST_CHAN_INDEX        FIRST_EXTRAP_PLOT
   !  PHOT_EVAL              PHOT_EVAL        CHAN_EVAL               DIFF_EVAL
   !  ACTUAL_DIM             maxval (NUM_EBINS)          maxval (NUM_CHAN)       6 * maxval (NUM_CHAN) + 1
   !  FIRST_PHOT_BIN         FIRST_PHOT_BIN   FIRST_CHAN_BIN          FIRST_EXTRAP_BIN

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-66, 21 November 1993, rev. 1996 June 11.


   use MFIT_parameters


   !  Input variables:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET         ! sometimes USE_ALL_DET
   integer (kind=mfit_integer), intent (in) :: NUM_TERMS_AVAIL
   integer (kind=mfit_integer), intent (in) :: NT_ADD_LINE
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: FIRST_BIN_CALC
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: LAST_BIN_CALC
   integer (kind=mfit_integer), intent (in) :: ACTUAL_DIM
   real (kind=mfit_real), dimension (7, 0: ACTUAL_DIM - 1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED
   real (kind=mfit_real), dimension (MAX_TERMS, MAX_PPT), intent (in) :: PARAM


   !  Output variables:
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (out) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (out) :: LAST_PHOT_BIN


   !  Internal variables:
   real (kind=mfit_real) :: ENERGY
   integer (kind=mfit_integer) :: JTERM
   integer (kind=mfit_integer) :: LDET


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   DO JTERM=1, NUM_TERMS_AVAIL
      IF (TERM_USED (JTERM) .EQ. 1) THEN

         ! Calculate the photon bins FIRST_PHOT_BIN to LAST_PHOT_BIN for
         ! which we must calculate the derivatives.    We start with the user
         ! requested range, FIRST_BIN_CALC (LDET) to LAST_BIN_CALC (LDET).
         ! These are input via control variables and will normally be all bins
         ! of the DRM.  For some terms, namely additive lines, we know that for
         ! many of the bins, the model and its derivatives will be zero, so we
         ! can skip the photon derivation evaluation for these bins.   More
         ! importantly, we can save much time by skipping part of the DRM
         ! multiplication.
         !
         ! Start with the defaults:
         DO LDET=0, NUM_DET - 1
            FIRST_PHOT_BIN (JTERM, LDET) = FIRST_BIN_CALC (LDET)
            LAST_PHOT_BIN (JTERM, LDET) = LAST_BIN_CALC (LDET)
         END DO

         ! For additive line terms, decrease photon bin range to include
         ! only bins whose energy range [=phot_eval (6) to phot_eval (7)]
         ! overlaps energies for which the line model is significantly
         ! nonzero.   The Gaussian lines are deemed to be nonzero for
         ! +/- 2 * FWHM from their centroid, i.e., PARAM (2) +/- 2 * PARAM(3).
         ! Note that 2 * FWHM is 4.7 sigma!
         ! The variable NT_ADD_LINE tells us where the additive lines start.
         ! It is hardwired that there are 3 such lines.

         IF ( JTERM .EQ. NT_ADD_LINE .OR.       &
              JTERM .EQ. NT_ADD_LINE + 1 .OR.   &
              JTERM .EQ. NT_ADD_LINE + 2 ) THEN

            DO LDET=0, NUM_DET - 1
               IF (USE_DET (LDET)) THEN

                  ! if highest energy of bin is < start energy of line, skip:
                  ENERGY = PHOT_EVAL (7, FIRST_PHOT_BIN (JTERM, LDET), LDET)

                  DO WHILE ( ENERGY .LT. PARAM (JTERM,2) - 2. * PARAM (JTERM,3) .AND.   &
                             FIRST_PHOT_BIN (JTERM, LDET) .LE. LAST_BIN_CALC (LDET) )

                     FIRST_PHOT_BIN (JTERM, LDET) = FIRST_PHOT_BIN (JTERM, LDET) + 1

                     IF ( FIRST_PHOT_BIN (JTERM, LDET) .LE. LAST_BIN_CALC (LDET) )   &
                         ENERGY = PHOT_EVAL (7, FIRST_PHOT_BIN (JTERM, LDET), LDET)

                  END DO

                  ! if lowest energy of bin is > end energy of line, skip bin:
                  ENERGY = PHOT_EVAL (6, LAST_PHOT_BIN (JTERM, LDET), LDET)

                  DO WHILE ( ENERGY .GT. PARAM (JTERM,2) + 2. * PARAM (JTERM,3) .AND.    &
                             LAST_PHOT_BIN (JTERM, LDET) .GE. FIRST_BIN_CALC (LDET) )

                     LAST_PHOT_BIN (JTERM, LDET) = LAST_PHOT_BIN (JTERM, LDET) - 1

                     IF ( LAST_PHOT_BIN (JTERM, LDET) .GE. FIRST_BIN_CALC (LDET) )   &
                         ENERGY = PHOT_EVAL (6, LAST_PHOT_BIN (JTERM, LDET), LDET)

                  END DO

               END IF                                          ! used det ?
            END DO                                             ! ldet
         END IF                                                ! line term ?

      END IF                                               ! used term?
   END DO                                                  ! jterm



   !  Now handle JTERM=0, which is sum of all terms.
   !  Photon bin range is superset of the bin ranges of the individual terms:

   DO LDET=0, NUM_DET - 1
      FIRST_PHOT_BIN (0, LDET) = LAST_BIN_CALC (LDET)
      LAST_PHOT_BIN (0, LDET) = FIRST_BIN_CALC (LDET)
   END DO

   DO LDET=0, NUM_DET - 1
      DO JTERM=1, NUM_TERMS_AVAIL
         IF (TERM_USED (JTERM) .EQ. 1) THEN
            FIRST_PHOT_BIN (0, LDET) = MIN (FIRST_PHOT_BIN (0, LDET), FIRST_PHOT_BIN (JTERM, LDET))
            LAST_PHOT_BIN (0, LDET) =  MAX (LAST_PHOT_BIN (0, LDET), LAST_PHOT_BIN (JTERM, LDET))
         END IF
      END DO
   END DO


   RETURN

   END SUBROUTINE M_USE_PHOTBINS


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_VBREMSS (EKEV, ANORM, TKEV, AB)

   real (kind=mfit_real) :: M_VBREMSS

   !  XSPEC: SUBROUTINE xsbrmv(ear, ne, param, ifl, photar, photer), file xsbrmv.f.

   !---
   ! XSPEC model subroutine
   ! simple thermal bremsstrahlung, based on GSFC routine
   ! FBG, as derived by GSFC for low temperature (<20 keV) plasmas
   ! the number of photons in the bin is estimated by a simple 2
   ! point approximation to the integral. Includes variable He/H.
   !---
   ! see ADDMOD for parameter descriptions
   ! number of model parameters: 1
   !       ekev    Phot. energy in keV
   !       anorm   norm fact. in units (3.02e-15/4/pi/R^2) Int n_e n_H dV
   !  tkev  kT (keV) Plasma temperature in keV
   !  ab n(He)/n(H)      Helium to Hydrogen ratio
   ! intrinsic energy range:
   !  Emine=epsilon, Emax=infinity
   ! algorithm:
   !  Uses the function Gaunt, which returns the photon spectrum,
   !  based on the GSFC routine FBG
   !---
   ! 10 July 2002 - rmk        Adapted for RMFIT (along with gaunt.f)
   ! 28 July 1988 - kaa
   ! 16 Dec 1994 - kaa         switched to Gaunt routine and included
   !                           1/sqrt(T) factor to make the normalization
   !                           independent of T. Normalization is now :
   !                             K = (3.02e-15/4/pi/R^2) Int n_e n_H dV
   !
   !---

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.


   real (kind=mfit_real), intent (in) :: EKEV, ANORM, TKEV, AB


   IF ( (TKEV .GT. 0.) .AND. (EKEV .LT. 50*TKEV) .AND. (EKEV .GT. 0.) ) THEN

         M_VBREMSS = ANORM * (GAUNT(EKEV,TKEV,1.) + 4*AB*GAUNT(EKEV,TKEV,2.)) * EXP(-EKEV/TKEV) / EKEV / SQRT(TKEV)

   ELSE

      M_VBREMSS = 0.0

   ENDIF


   RETURN

   END FUNCTION M_VBREMSS


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION GAUNT (E, Q, Z)

   real (kind=mfit_real) :: GAUNT

   real (kind=mfit_real), intent (in) :: E, Q, Z


   ! Calculation of the Gaunt factor based on the program given in
   ! Kellogg, Baldwin, & Koch (ApJ 199, 299). Fixes supplied by Larry
   ! Molnar and Jack Hughes for inaccuracies in the polynomial fits
   ! to the Karzas & Latter (1961) numerical values. The routine by
   ! Kurucz for the low T case was supplied by Larry Molnar.

   ! arguments
   !  e r i:energy (keV)
   !  q r i: temperature (keV)
   !  z r i: ion charge used for correction
   ! returned value
   !  gaunt r r: the gaunt factor

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Eliminate Equivalence statement in favor of reshape instrinic function.  Also eliminate
   !  Data statements.  MSB, 2009 April 17


   real (kind=mfit_real), dimension (6), PARAMETER :: GAM2 = (/  .7783,  1.2217,  2.6234,  4.3766,  20.,  70.  /)
   real (kind=mfit_real), dimension (6), PARAMETER :: GAM3 = (/ 1.,      1.7783,  3.,      5.6234,  10.,  30.  /)

   real (kind=mfit_real) :: gam1, gam, u, u2, u1, ai, t, ak, u12
   real (kind=mfit_real) :: g, born, g1, g2, p, t2, t4, t8, u122, u12im
   integer (kind=mfit_integer) :: n, m, m1


   real (kind=mfit_real), dimension (6, 7, 3), PARAMETER :: A =                                                    &
        reshape ( (/ 1.001,    1.004,    1.017,    1.036,    1.056,    1.121,    1.001,    1.005,    1.017,        &
                     1.046,    1.073,    1.115,     .9991,   1.005,    1.03,     1.055,    1.102,    1.176,        &
                      .997,    1.005,    1.035,    1.069,    1.134,    1.186,     .9962,   1.004,    1.042,        &
                     1.1,      1.193,    1.306,     .9874,    .9962,   1.047,     1.156,   1.327,    1.485,        &
                      .9681,    .9755,   1.020,    1.208,    1.525,    1.965,      .3029,   .1616,    .04757,      &
                      .013,     .0049,   -.0032,    .4905,    .2155,    .08357,    .02041,  .00739,   .00029,      &
                      .654,     .2833,    .08057,   .03257,   .00759,  -.00151,   1.029,    .391,     .1266,       &
                      .05149,   .01274,   .00324,   .9569,    .4891,    .1764,     .05914,  .01407,   -.00024,     &
                     1.236,     .7579,    .326,     .1077,    .028,     .00548,   1.327,   1.017,      .6017,      &
                      .205,     .0605,    .00187,  -1.323,    -.254,   -.01571,   -.001,   -.000184,   .00008,     &
                    -4.762,    -.3386,   -.03571,   -.001786, -.0003,   .00001,  -6.349,   -.4206,    -.02571,     &
                     -.003429, -.000234,  .00005, -13.231,    -.59,    -.04571,   -.005714,-.000445,  -.00004,     &
                    -7.672,    -.6852,   -.0643,    -.005857, -.00042,  .00004,  -7.143,   -.9947,    -.12,        &
                     -.01007,  -.000851, -.00004,  -3.175,   -1.116,   -.2270,    -.01821, -.001729,   .00023 /),  &
                     (/  6, 7, 3  /)  )


   ! if temperature or energy is zero or if energy/temperature is
   ! too high then give up.

   IF ( (Q.EQ.0.) .OR. (e.GT.50.*q) .OR. (E.EQ.0) ) THEN
      gaunt = 0.
      RETURN
   ENDIF

   ! Convert the energy and temperature to the units used by Karzas & Latter

   U = E/Q
   GAM = Z*Z*.01358/Q
   GAM1 = MIN(GAM*1000., 100.)

   ! If we are in the high T regime then use Kurucz's algorithm

   IF (GAM.GT.0.1) THEN
      CALL kurucz(u, gam, g)
      gaunt = g
      RETURN
   ENDIF

   ! Calculate the Born approximation

   g = 1.
   U2 = U*U
   U1 = U*0.5
   T = U1/3.75
   u12 = u1*0.5
   IF (u12 .LE. 1.) THEN

      t2 = t*t
      t4 = t2*t2
      t8 = t4*t4
      AI = 1. + 3.5156229*T2 + 3.089942*T4 + 1.2067492*T2*t4 +   &
                 .2659732*T8 + .0360768*T8*t2 + .0045813*T8*t4
      u122 = u12*u12
      AK = - log(U12)*AI - .57721566 +       &
           u122*(.4227842+U122*(.23069756+   &
           U122*(.0348859+u122*(.00262698+   &
           U122*(.0001075+U122*.0000074)))))

   ELSE

      u12im = -1./u12
      AK = 1.25331414 + u12im*(.07832358+u12im*(.02189568+u12im*(.01062446+    &
                        u12im*(.00587872+u12im*(.0025154+u12im*.00053208)))))
      AK = AK/(EXP(U1)*SQRT(U1))

   ENDIF

   BORN = .5513*EXP(U1)*AK

   ! If Born approximation is valid then go with it

   IF ( gam1.LT.1. ) THEN
      gaunt = born
      RETURN
   ENDIF

   ! All that is left to do is the polynomial approximation

   IF (u.LT.0.003) THEN
      u = .003
      u2 = u*u
   ENDIF
   IF (u.LE.0.03) THEN
      n = 1
   ELSEIF (u.LE.0.30) THEN
      n = 2
   ELSEIF (u.LE.1.0) THEN
      n = 3
   ELSEIF (u.LE.5.0) THEN
      n = 4
   ELSEIF (u.LE.15.0) THEN
      n = 5
   ELSE
      n = 6
   ENDIF
   IF (gam1.LE.1.773) THEN
      m = 1
   ELSEIF (gam1.LE.3.0) THEN
      m = 2
   ELSEIF (gam1.LE.5.6234) THEN
      m = 3
   ELSEIF (gam1.LE.10.) THEN
      m = 4
   ELSEIF (gam1.LE.30.) THEN
      m = 5
   ELSE
      m = 6
   ENDIF

   M1 = M + 1
   G1 = (A(N,M,1)+A(N,M,2)*U+A(N,M,3)*U2)*BORN
   G2 = (A(N,M1,1)+A(N,M1,2)*U+A(N,M1,3)*U2)*BORN
   P = (GAM1-GAM3(M))/GAM2(M)

   gaunt = (1.-P)*G1 + P*G2


   RETURN

   END FUNCTION GAUNT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   !  routine for high T case (from Larry Molnar)

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.
   !  Eliminate data statement in favor of initalizating on declaration statement using reshape.


   SUBROUTINE KURUCZ (UIN, GAM, GAUNT)

   real (kind=mfit_real), intent (in) :: UIN, GAM
   real (kind=mfit_real), intent (out) :: GAUNT

   integer (kind=mfit_integer) :: J, K
   real (kind=mfit_real) :: RJ, RK, T, U

   real (kind=mfit_real), dimension (7, 12), parameter :: YA =                   &
       reshape ( (/ 5.40, 5.25, 5.00, 4.69, 4.48, 4.16, 3.85, 4.77, 4.63,        &
                    4.40, 4.13, 3.87, 3.52, 3.27, 4.15, 4.02, 3.80, 3.57, 3.27,  &
                    2.98, 2.70, 3.54, 3.41, 3.22, 2.97, 2.70, 2.45, 2.20, 2.94,  &
                    2.81, 2.65, 2.44, 2.21, 2.01, 1.81, 2.41, 2.32, 2.19, 2.02,  &
                    1.84, 1.67, 1.50, 1.95, 1.90, 1.80, 1.68, 1.52, 1.41, 1.30,  &
                    1.55, 1.56, 1.51, 1.42, 1.33, 1.25, 1.17, 1.17, 1.30, 1.32,  &
                    1.30, 1.20, 1.15, 1.11, 0.86, 1.00, 1.15, 1.18, 1.15, 1.11,  &
                    1.08, 0.59, 0.76, 0.97, 1.09, 1.13, 1.10, 1.08, 0.38, 0.53,  &
                    0.76, 0.96, 1.08, 1.09, 1.09 /),  (/  7, 12  /)  )

   RJ = 2.*LOG10(GAM) + 3.
   J = int (RJ, mfit_integer)
   RJ = J
   RK = 2.*LOG10(UIN) + 9.
   K = int (RK, mfit_integer)
   IF (K .LT. 1) K = 1
   RK = K

   IF (J .GE. 7  .OR.  J .LT. 1  .OR.  K .GE. 12)  THEN
      GAUNT = 0.
   ELSE
      T = (LOG10(GAM)-(RJ-3.)/2.)/0.5
      U = (LOG10(UIN)-(RK-9.)/2.)/0.5
      GAUNT = (1.-T)*(1.-U)*YA(J, K) + T*(1.-U)*YA(J+1, K) + T*U*YA(J+1, K+1) + (1.-T)*U*YA(J, K+1)
   ENDIF

   RETURN

   END SUBROUTINE KURUCZ


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE MGET_DATA_CR_VARI    &
       ( NUM_DET, NUM_CHAN, CHAN_OFFSET, CHAN_WIDTH, BACK_CSIG, LIVE_TIME, USE_DET, CHAN_RANGE, OBS_CRATE, DATA_CR_VARI )

   !  Calculates variances, not sigmas!
   !  "CR" stands for Count Rate.
   !  Calculates variance of SOURCE count rate for each channel in requested
   !  channel range for all selected detectors.   The variance calculated
   !  is a data rather than model variance in that it is calculated from
   !  the data count rate.

   !  There are two terms, of two types, that are summed to form the variance:

   !  1) Poission fluctuations in the counts that the detector received
   !     during this livetime,
   !     The variance of this term is the square root of the total number of
   !     counts (not count rate), transformed to count rate.
   !     REVISION: if there were zero counts, we use one count instead.
   !  2) The uncertainty in the background count rate, BACK_CSIG, as input to
   !     MFIT.

   !  There are two contributions to the total observed count rate, and thus
   !  to item 1, namely, the source counts and the background counts.
   !  These are, by defintion, included in the total observed rate OBS_CRATE.

   !  The total number of observed counts should be a non-negative integer.
   !  When zero counts were observed, we calculate the Poission fluctuation
   !  variance as if one count had been observed--if we used zero counts then
   !  the calculated Poission variance would be zero, which is clearly an
   !  underestimate.   When the observed counts are zero or one, it is likely
   !  that the estimated uncertainty in the background rate will dominate the
   !  total uncertainty.
   !

   !  We only do the calculation for:
   !  i) selected detectors, as specified by USE_DET,
   !  ii) selected channel range, as specified by CHAN_RANGE,
   !  iii) channels with data, as specified by LIVE_TIME not equal to zero.
   !  (Note that LIVE_TIME < 0 is allowed: this specifies that the channel
   !  has data suitable for inclusion in plots but that the channel is not
   !  to be used in fits.)

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-66, July 1993.
   !  Revised MSB 28 Sept 1993: special case if zero counts in a channel.


   use MFIT_parameters


   !  Input arguments:

   !  the number of detectors.   Note that "a detector" = "a DRM", e.g.,
   !  summed data such as MER counts as 1 detector:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   
   !  the number of data channels:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN

   !  the channel number of the first channel passed:
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET

   !  the energy widths of the channels, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH

   !  the uncertainty (sigma) of the model background count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG

   !  the livetime, seconds:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME

   !  the detectors included in the fit:
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET

   !  the channel range to process; may be either FIT_CHAN or ALL_CHAN:
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: CHAN_RANGE

   !  the total (not background subtracted) observed count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: OBS_CRATE


   !  Output arguments:
   !  the variance of the source signal calculated from the data (and the
   !  background model) units are (counts/s-keV)**2:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: DATA_CR_VARI


   !  Internal variables:
   integer (kind=mfit_integer) :: ICHAN
   integer (kind=mfit_integer) :: LDET
   real (kind=mfit_real) :: FACTOR


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN                                  ! used det?
         DO ICHAN=CHAN_RANGE (0, LDET) - CHAN_OFFSET (LDET),   &
                  CHAN_RANGE (1, LDET) - CHAN_OFFSET (LDET)

         IF (LIVE_TIME (ICHAN, LDET) .NE. 0.0) THEN          ! have data?

            FACTOR = 1. / ( ABS (LIVE_TIME (ICHAN, LDET)) * CHAN_WIDTH (ICHAN, LDET) )
            DATA_CR_VARI (ICHAN, LDET) = BACK_CSIG (ICHAN, LDET)**2 +    &
                        FACTOR * MAX (OBS_CRATE (ICHAN, LDET), FACTOR)

            ELSE
               DATA_CR_VARI (ICHAN, LDET) = 0.0
            END IF

         END DO
      END IF
   END DO


   RETURN

   END SUBROUTINE MGET_DATA_CR_VARI


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE MGET_MODEL_CR_VARI   &
        ( NUM_DET, NUM_CHAN, CHAN_OFFSET, CHAN_WIDTH, BACK_CRATE, BACK_CSIG, LIVE_TIME, USE_DET,     &
           CHAN_RANGE, MODEL_CNT_RATE, MODEL_CR_VARI)

   !  Calculates variances, not sigmas!
   !  "CR" stands for Count Rate.
   !  Calculates variance of SOURCE count rate for each channel in requested
   !  channel range for all selected detectors.   The variance calculated
   !  is a model rather than data variance in that it is calculated from
   !  the model count rate.

   !  There are three terms that are summed to form the variance: 1A, 1B and
   !  2, as described below:

   !  There are two kinds of contributions to the variance:
   !  1) Poission fluctuations in the counts that the detector is predicted
   !     to have received in each channel during this livetime.
   !     I say "predicted" counts since the model count rate is used rather
   !     than the observed count rate.
   !     The variance of this term is the square root of the total number of
   !     counts (not count rate), transformed to count rate.
   !  2) The uncertainty in the background count rate, BACK_CSIG, as input to
   !     MFIT.

   !  There are two contributions to the total predicted count rate, and thus
   !  to item 1):
   !  A) the model predicted count rate from the source, MODEL_CNT_RATE, and
   !  B) the background model count rate, BACK_CRATE, as input to MFIT.

   !  The observed total counts (not count rate) and the observed source counts
   !  (if we could identify which counts were source counts) both must be
   !  non-negative integers.    In contrast, the model source counts can be
   !  any non-negative real number, e.g. 0.5 count, meaning that we predict
   !  a count every other time.   In calculating the data variance
   !  in (MGET_DATA_CR_VARI), we took the larger of the observed counts or
   !  1 count in order not to have zero contribution from Poission fluctuations
   !  to the estimated variance.    Here we don't do that, since model counts
   !  less than 1 are valid.    Instead we take the larger of the predicted
   !  source counts and zero so that the source model never decreases the
   !  variance.   However, this may overestimate the variance in the case that
   !  the background model rate is too large and thus the source model rate
   !  is negative, in which case, the sum of the two rates probably is a better
   !  estimate of the total count rate.   Since in this case the background
   !  model is wrong, lets hope that the estimated uncertainty in the
   !  background model dominates the total uncertainty.

   !  We only do the calculation for:
   !  i) selected detectors, as specified by USE_DET,
   !  ii) selected channel range, as specified by CHAN_RANGE,
   !  iii) channels with data, as specified by LIVE_TIME not equal to zero.
   !  (Note that LIVE_TIME < 0 is allowed: this specifies that the channel
   !  has data suitable for inclusion in plots but that the channel is not
   !  to be used in fits.)

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC ES-66, July 1993.


   use MFIT_parameters


   !  Input arguments:

   !  the number of detectors.   Note that "a detector" = "a DRM", e.g.,
   !  summed data such as MER counts as 1 detector:
   integer (kind=mfit_integer), intent (in) ::NUM_DET
   
   !  the number of data channels:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN

   !  the channel number of the first channel passed:
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET

   !  the energy widths of the channels, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH

   !  the model background count rate, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CRATE

   !  the uncertainly (sigma) on the above, counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: BACK_CSIG

   !   the livetime, seconds:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: LIVE_TIME

   !  the detectors included in the fit:
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET

   !  the channel range to process; may be either FIT_CHAN or ALL_CHAN:
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: CHAN_RANGE

   !  the model source count rate, obtained from the product of the photon
   !  model with the DRM (Detector Response Matrix), units are counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1), intent (in) :: MODEL_CNT_RATE


   !  Output arguments:
   !  the variance of the source signal calculated from the photon model.
   !  units are (counts/s-keV)**2:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (out) :: MODEL_CR_VARI


   !  Internal variables:
   integer (kind=mfit_integer) :: ICHAN
   integer (kind=mfit_integer) :: LDET


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN                               ! used det?

         DO ICHAN=CHAN_RANGE (0, LDET) - CHAN_OFFSET (LDET),   &
                  CHAN_RANGE (1, LDET) - CHAN_OFFSET (LDET)

            IF (LIVE_TIME (ICHAN, LDET) .NE. 0.0) THEN       ! have data?

               MODEL_CR_VARI (ICHAN, LDET) = BACK_CSIG (ICHAN, LDET)**2 +    &
                 ( MAX (MODEL_CNT_RATE (ICHAN, 0, LDET), 0.) + BACK_CRATE (ICHAN, LDET) ) /  (ABS (LIVE_TIME (ICHAN, LDET)) *   &
                            CHAN_WIDTH (ICHAN, LDET) )

            ELSE
               MODEL_CR_VARI (ICHAN, LDET) = 0.0
            END IF

         END DO

      END IF
   END DO


   RETURN

   END SUBROUTINE MGET_MODEL_CR_VARI


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   FUNCTION M_RES_FRAC (NUM_DET, LDET, RES_FRAC_AT511, RES_EXP, ENERGY)

   real (kind=mfit_real) :: M_RES_FRAC


   !  This function evaluates the fractional resolution, i.e. resolution FWHM /
   !  energy.     The resolution is evaluated for the LDETth detector at
   !  energy ENERGY.    The fraction resolution is represented by a power law:
   !  fraction resolution = RES_FRAC_AT511 * (ENERGY / 511.) ** RES_EXP.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  M. S. Briggs, UAH / MSFC ES-66, 11 July 1993.


   ! Input variables:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), intent (in) :: LDET
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_FRAC_AT511
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_EXP
   real (kind=mfit_real), intent (in) :: ENERGY

   ! Output is via function return.

   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   M_RES_FRAC = RES_FRAC_AT511 (LDET) * (ENERGY / 511.) ** RES_EXP (LDET)


   RETURN

   END FUNCTION M_RES_FRAC


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE MPHOT_EVAL_LIST  ( NUM_DET, NUM_CHAN, NUM_EBINS, PHOT_ENERGY, PHOT_WIDTH,     &
                 CHAN_ENERGY, CHAN_WIDTH, RES_FRAC_AT511, RES_EXP, PHOT_EVAL, NUME_TO_USE, CHAN_EVAL,       &
                 CHAN_NUME_TO_USE, DIFF_EVAL, DIFF_NUME_TO_USE, NUM_DIFF )

   !  This subroutine generates lists of energies for which the photon model
   !  is to be evaluated: energies and how many of the energies to use.
   !  There are three such pairs of lists:
   !    A) PHOT_EVAL, NUME_TO_USE,
   !    B) CHAN_EVAL, CHAN_NUME_TO_USE,
   !    C) DIFF_EVAL, DIFF_NUME_TO_USE.

   !  A) The following discussion applies to PHOT_EVAL & NUME_TO_USE, which are
   !  use in evaluating the model in the fitting process:
   !  The energy list is based upon PHOT_ENERGY & PHOT_WIDTH with finer
   !  energy spacing to allow averaging of the models across a photon input
   !  bin.  The number of energies to use ranges from 1 to 7 per photon input
   !  bin.  The goal is to have the energies space closer than 0.5 * detector
   !  resolution (FWHM).   The number of energies per bin can be 1, 3 or 7 and
   !  is selected to be the smallest that satisifies the 0.5 * detector
   !  resolution requirement. Of course, for very wide bins, even 7
   !  evaluations per bin may be insufficient to meet the requirement.

   !  The energy list strategy:
   !  (1) center of bin,
   !  (2) & (3) offset from center by 1/3 of width,
   !  (4) & (5) offset from center by 1/6 of width,
   !  (6) & (7) bin edges.

   !  For continuum models: average the function (if NUME_TO_USE > 1):
   !  If we just use 1 per bin, then the spacing is the bin widths,
   !  If we use per bin, then the spacing is 1/3 of the bin width,
   !  If we use 7 per bin, then the spacing is 1/6 of the bin width and
   !  adjacent bins will both evaluate the model at their common edge (a
   !  slight inefficiency overwhelmed by the other advantages of this system;
   !  also, rarely will two adjacent bins both be evaluated for 7 energies).
   !  For line models that automatically average the line over the bin
   !  (e.g. Gaussian for which error function is used), we only need know
   !  the bin edges, which are given by (6) and (7).

   !  B) CHAN_EVAL & CHAN_NUME_TO_USE are used to evaluate the average of the
   !  photon model over the data channels, CHAN_ENERGY & CHAN_WIDTH.  These
   !  are used to calculate certain outputs.   All elements of CHAN_NUME_TO_USE
   !  are set to 7.

   !  C) DIFF_EVAL & DIFF_NUME_TO_USE are used to calculate the "differential"
   !  model curve w/o averaging, e.g. for plotting.  All elements of
   !  DIFF_NUME_TO_USE are set to 1.  We thus only use elements (1, ....),
   !  (6, ...) and (7, ....) of DIFF_EVAL.    Elements (1, ....) are used for
   !  continuum terms while the last two are used for line terms.    We space
   !  the elements (1,...) six per channel plus one at the right edge shared
   !  with the next channel (or extra for the last channel).   We now space
   !  these energies as subdivisions of the channels rather than photon input
   !  bins to allow the resulting model function values to be used in the
   !  calculation of nu-F-nu.  We create the elements (6,....) and (7,...) so
   !  that they specify the edges whoose centers are given by (1,....).

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC Code ES-66, 18 June 1993.


   use MFIT_parameters


   !  Input arguments:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_CHAN
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_ENERGY
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_ENERGY
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_FRAC_AT511
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_EXP

   !  Output arguments:
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (out) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (out) :: NUME_TO_USE
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: CHAN_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: NUM_DET-1), intent (out) :: CHAN_NUME_TO_USE
   real (kind=mfit_real), dimension (7, 0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1), intent (out) :: DIFF_EVAL
   integer (kind=mfit_integer), dimension (0: 6 * maxval (NUM_CHAN), 0: NUM_DET-1), intent (out) :: DIFF_NUME_TO_USE
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (out) :: NUM_DIFF

   !  Internal variables:
   integer (kind=mfit_integer) :: LDET
   integer (kind=mfit_integer) :: PBIN, ICHAN, DPLOT
   real (kind=mfit_real) :: RES_FWHM


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
   !     A) Generate the list of energies for evaluating the fit:
   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

   DO LDET=0, NUM_DET - 1
      DO PBIN=0, NUM_EBINS (LDET) - 1

         PHOT_EVAL (1, PBIN, LDET) = PHOT_ENERGY (PBIN, LDET)
         PHOT_EVAL (2, PBIN, LDET) = PHOT_ENERGY (PBIN, LDET) - 0.33333 * PHOT_WIDTH (PBIN, LDET)
         PHOT_EVAL (3, PBIN, LDET) = PHOT_ENERGY (PBIN, LDET) + 0.33333 * PHOT_WIDTH (PBIN, LDET)
         PHOT_EVAL (4, PBIN, LDET) = PHOT_ENERGY (PBIN, LDET) - 0.16667 * PHOT_WIDTH (PBIN, LDET)
         PHOT_EVAL (5, PBIN, LDET) = PHOT_ENERGY (PBIN, LDET) + 0.16667 * PHOT_WIDTH (PBIN, LDET)
         PHOT_EVAL (6, PBIN, LDET) = PHOT_ENERGY (PBIN, LDET) - 0.5     * PHOT_WIDTH (PBIN, LDET)
         PHOT_EVAL (7, PBIN, LDET) = PHOT_ENERGY (PBIN, LDET) + 0.5     * PHOT_WIDTH (PBIN, LDET)

      END DO
   END DO


   !  Decide how many energies per bin to use:

   DO LDET=0, NUM_DET - 1
      DO PBIN=0, NUM_EBINS (LDET) - 1

         RES_FWHM = PHOT_ENERGY (PBIN, LDET) * M_RES_FRAC (NUM_DET, LDET, RES_FRAC_AT511, RES_EXP, PHOT_ENERGY (PBIN, LDET))

        ! start with 1:
         NUME_TO_USE (PBIN, LDET) = 1

         ! cf bin width to 0.5 * resolution fwhm: if fail, try 3:
         IF (PHOT_WIDTH (PBIN, LDET) .GE. RES_FWHM / 2.)  NUME_TO_USE (PBIN, LDET) = 3

         ! if we do 3 evals per bin (above), we have divided bin into thirds:
         ! see if this is good enough by comparing 1/3 bin width with fwhm / 2:
         IF (PHOT_WIDTH (PBIN, LDET) / 3. .GE. RES_FWHM / 2.)  NUME_TO_USE (PBIN, LDET) = 7

      END DO
   END DO


   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
   !     B) Generate the list of energies for calculating the model for the chan:
   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

   DO LDET=0, NUM_DET - 1
      DO ICHAN=0, NUM_CHAN (LDET) - 1

         CHAN_EVAL (1, ICHAN, LDET) = CHAN_ENERGY (ICHAN, LDET)
         CHAN_EVAL (2, ICHAN, LDET) = CHAN_ENERGY (ICHAN, LDET) - 0.33333 * CHAN_WIDTH (ICHAN, LDET)
         CHAN_EVAL (3, ICHAN, LDET) = CHAN_ENERGY (ICHAN, LDET) + 0.33333 * CHAN_WIDTH (ICHAN, LDET)
         CHAN_EVAL (4, ICHAN, LDET) = CHAN_ENERGY (ICHAN, LDET) - 0.16667 * CHAN_WIDTH (ICHAN, LDET)
         CHAN_EVAL (5, ICHAN, LDET) = CHAN_ENERGY (ICHAN, LDET) + 0.16667 * CHAN_WIDTH (ICHAN, LDET)
         CHAN_EVAL (6, ICHAN, LDET) = CHAN_ENERGY (ICHAN, LDET) - 0.5     * CHAN_WIDTH (ICHAN, LDET)
         CHAN_EVAL (7, ICHAN, LDET) = CHAN_ENERGY (ICHAN, LDET) + 0.5     * CHAN_WIDTH (ICHAN, LDET)

      END DO
   END DO


   !  Always use 7 energies per channel:

   DO LDET=0, NUM_DET - 1
      DO ICHAN=0, NUM_CHAN (LDET) - 1
         CHAN_NUME_TO_USE (ICHAN, LDET) = 7
      END DO
   END DO


   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
   !     C) Generate the list of energies for the "differential" model curve:
   !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

   !  Generate the elements (1,...):

   DO LDET=0, NUM_DET - 1
      DPLOT = 0
      DO ICHAN=0, NUM_CHAN (LDET) - 1

         DIFF_EVAL (1, DPLOT, LDET) = CHAN_EVAL (6, ICHAN, LDET)
         DPLOT = DPLOT + 1
         DIFF_EVAL (1, DPLOT, LDET) = CHAN_EVAL (2, ICHAN, LDET)
         DPLOT = DPLOT + 1
         DIFF_EVAL (1, DPLOT, LDET) = CHAN_EVAL (4, ICHAN, LDET)
         DPLOT = DPLOT + 1
         DIFF_EVAL (1, DPLOT, LDET) = CHAN_EVAL (1, ICHAN, LDET)
         DPLOT = DPLOT + 1
         DIFF_EVAL (1, DPLOT, LDET) = CHAN_EVAL (5, ICHAN, LDET)
         DPLOT = DPLOT + 1
         DIFF_EVAL (1, DPLOT, LDET) = CHAN_EVAL (3, ICHAN, LDET)
         DPLOT = DPLOT + 1

      END DO
      DIFF_EVAL (1, DPLOT, LDET) = CHAN_EVAL (7, NUM_CHAN (LDET) - 1, LDET)
      NUM_DIFF (LDET) = DPLOT + 1
   END DO


   !  Generate the elements (6,....) and (7,...):

   DO LDET=0, NUM_DET - 1
      DO DPLOT=1, NUM_DIFF (LDET) - 1

         DIFF_EVAL (6, DPLOT, LDET) = DIFF_EVAL (1, DPLOT, LDET) -   &
                0.5 * (DIFF_EVAL (1, DPLOT, LDET) - DIFF_EVAL (1, DPLOT-1, LDET))

         END DO

         DIFF_EVAL (6, 0, LDET) = DIFF_EVAL (1, 0, LDET) - 0.5 * (DIFF_EVAL (1, 1, LDET) - DIFF_EVAL (1, 0, LDET))

   END DO


   DO LDET=0, NUM_DET - 1
      DO DPLOT=0, NUM_DIFF (LDET) - 2

         DIFF_EVAL (7, DPLOT, LDET) = DIFF_EVAL (1, DPLOT, LDET) +     &
                0.5 * (DIFF_EVAL (1, DPLOT+1, LDET) - DIFF_EVAL (1, DPLOT, LDET))

      END DO

      DIFF_EVAL (7, NUM_DIFF (LDET) - 1, LDET) =    &
                       DIFF_EVAL (1, NUM_DIFF (LDET) - 1, LDET) +   &
                0.5 * (DIFF_EVAL (1, NUM_DIFF (LDET) - 1, LDET) -   &
                       DIFF_EVAL (1, NUM_DIFF (LDET) - 2, LDET))

   END DO


   !  Always use 1 energy:

   DO LDET=0, NUM_DET - 1
      DO DPLOT=0, NUM_DIFF (LDET) - 1
         DIFF_NUME_TO_USE (DPLOT, LDET) = 1
      END DO
   END DO


   RETURN

   END SUBROUTINE MPHOT_EVAL_LIST


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE MPHOT_EVAL_OUT  ( NUM_DET, NUM_EBINS, RES_FRAC_AT511, RES_EXP, PHOT_EVAL, NUME_TO_USE,   &
               PHOT_ENERGY, PHOT_WIDTH, DebugMask, MFIT_VERSION )

   !  This subroutine outputs information about the energies at which the
   !  model function is evaluatated during the fitting process.   The model
   !  function is evaluated 1, 3 or 7 times per DRM photon input bin (not
   !  channel), based upon the bin width vs the detector's resolution.

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, July 1993,  UAH / ES-66.


   use MFIT_parameters


   !  Input variables:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: NUM_EBINS
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_FRAC_AT511
   real (kind=mfit_real), dimension (0: num_det-1), intent (in) :: RES_EXP
   real (kind=mfit_real), dimension (7, 0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: PHOT_EVAL
   integer (kind=mfit_integer), dimension (0: maxval (NUM_EBINS) -1, 0: NUM_DET-1), intent (in) :: NUME_TO_USE
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_ENERGY
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH
   integer (kind=mfit_integer), intent (in) :: DebugMask
   CHARACTER (len=VERSION_LEN), intent (in) :: MFIT_VERSION


   !   Output: solely by writing to units 6 & MODULE_PARAM_FILE_UNIT:


   !   Internal variables:
   integer (kind=mfit_integer) :: LDET, PBIN, NUNIT
   logical :: UnitOpen


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   NUNIT = MODULE_PARAM_FILE_UNIT
   call Open_DebugOutput_File  ( DebugMask, MFIT_VERSION )


   WRITE (NUNIT, 1010)
1010 FORMAT ( //  &
       '## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##'/   &
       'Dump of energies at which photon model is evaluated during the fitting'/   &
       'process.    Continuum models are evaluated at 1, 3 or 7 energies (as' /    &
       'listed below) per photon bin (not channel).    The number of energies' /   &
       'per bin is choosen based upon the detector resolution.' /                  &
       'Additive/subtractive Gaussian lines are evaluated using the error' /       &
       'function and the photon bin edges.' // )


   DO LDET=0, NUM_DET - 1
      WRITE (NUNIT, 1020) LDET, RES_FRAC_AT511 (LDET), RES_EXP (LDET)
1020  FORMAT ( // ' ## ## Information for the', I4, 'th detector: ' /         &
        'Fractional resolution at 511 keV:', F7.4, ' and exponent:', F8.4 //  &
        'bin #  left edge   right edge         width    resolution FWHM' /    &
        '# of energies & the energies' / )

      DO PBIN=0, NUM_EBINS (LDET) - 1
         WRITE (NUNIT, 1030)  PBIN, PHOT_EVAL (6, PBIN, LDET), PHOT_EVAL (7, PBIN, LDET), PHOT_WIDTH (PBIN, LDET),   &
                      PHOT_ENERGY (PBIN, LDET) * M_RES_FRAC (NUM_DET, LDET, RES_FRAC_AT511, RES_EXP, PHOT_ENERGY (PBIN, LDET))
 1030    FORMAT ( / 1X, I5, 1PG11.5E1, 2X, 1PG11.5E1, 4X, 1PG11.5E1, 4X, 1PG11.5E1)

         IF (NUME_TO_USE (PBIN, LDET) .EQ. 1) THEN
            WRITE (NUNIT, 1040) NUME_TO_USE (PBIN, LDET), PHOT_EVAL (1, PBIN, LDET)
         END IF

         IF (NUME_TO_USE (PBIN, LDET) .EQ. 3) THEN
            WRITE (NUNIT, 1040) NUME_TO_USE (PBIN, LDET), PHOT_EVAL (6, PBIN, LDET),   &
                      PHOT_EVAL (1, PBIN, LDET), PHOT_EVAL (7, PBIN, LDET)
         END IF

         IF (NUME_TO_USE (PBIN, LDET) .EQ. 7) THEN
            WRITE (NUNIT, 1040) NUME_TO_USE (PBIN, LDET), PHOT_EVAL (6, PBIN, LDET),   &
                            PHOT_EVAL (2, PBIN, LDET), PHOT_EVAL (4, PBIN, LDET), PHOT_EVAL (1, PBIN, LDET),    &
                            PHOT_EVAL (5, PBIN, LDET), PHOT_EVAL (3, PBIN, LDET), PHOT_EVAL (7, PBIN, LDET)
         END IF

1040     FORMAT (1X, I1, 1X, 7(1X, 1PG10.5E1))

         END DO
      END DO

   WRITE (NUNIT, 1050)
1050 FORMAT ( / ' End of energy listing' /   &
       '## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##' / )


#ifndef __G95__
   inquire ( unit=nunit, opened=UnitOpen )
   if ( UnitOpen )  flush (nunit)
#endif


   RETURN

   END SUBROUTINE MPHOT_EVAL_OUT


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   SUBROUTINE MPHOTS_TO_CNTS  ( NUM_DET, NUM_CHAN, NUM_EBINS, CHAN_WIDTH, USE_DET, CHAN_OFFSET, FIT_CHAN,    &
         FIRST_PHOT_BIN, LAST_PHOT_BIN, FIRST_NONZERO, TERM_USED,NUM_ADD_TERMS, PHOT_WIDTH, DRM,             &
         MODEL_PHOT_RATE_WS, MODEL_CNT_RATE )

   !  Converts model photon rates to model prediction count rates via the DRM.
   !
   !  For speed, do calculation only for indices for which it will be used:
   !  1) only do for detectors included in fit (USE_DET),
   !  2) only do terms selected by the user (TERM_USED=1).    Note that if
   !     JTERM=0 is to be processed, TERM_USED (0) must = 1,
   !  3) only do for fit channel range,
   !  4) only use elements of DRM requested by user (x_BIN_CALC)
   !     (i.e., calling routine has figured out for which bins the model
   !     is nonzero),
   !  5) only use nonzero elements of DRM (FIRST_NONZERO).

   !  Initial translation from Fortran 77 to Fortran 95 by Michael S. Briggs, 2009 April 4--6.

   !  Michael S. Briggs, UAH / MSFC Code ES-62, 28 September 1992.


   use MFIT_parameters


   !                    **** Input arguments: ****

   !  the number of detectors.   Note that "a detector" = "a DRM", e.g.,
   !  summed data such as MER counts as 1 detector:
   integer (kind=mfit_integer), intent (in) :: NUM_DET
   
   !  the number of data channels:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_CHAN

   !  the number of energy bins on the input side of the DRM.  The photon
   !  model is evaluated for these bins:
   integer (kind=mfit_integer), dimension (0: NUM_DET-1), intent (in) :: NUM_EBINS

   !  the energy widths of each channel, keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: CHAN_WIDTH

   !  the detectors to be processed:
   logical (kind=mfit_logical), dimension (0: num_det-1), intent (in) :: USE_DET

   !  the channel number of the first channel passed:
   integer (kind=mfit_integer), dimension (0: num_det-1), intent (in) :: CHAN_OFFSET

   !  the range of channels included in the fit:
   integer (kind=mfit_integer), dimension (0:1, 0: num_det-1), intent (in) :: FIT_CHAN

   !  the range of photon bins of the DRM that to be used.   May be smaller
   !  than entire range because of zero portions of the photon model:
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: FIRST_PHOT_BIN
   integer (kind=mfit_integer), dimension (0: MAX_TERMS, 0: num_det-1), intent (in) :: LAST_PHOT_BIN

   !  for each channel and detector, the smallest DRM photon bin which is non-zero:
   integer (kind=mfit_integer), dimension (0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: FIRST_NONZERO

   !  the model terms to be processed:
   integer (kind=mfit_integer), dimension (0: MAX_TERMS), intent (in) :: TERM_USED

   !  the number of additive terms.    Usually we don't need to calculate the
   !  count model for the multiplicative terms.   In the case that we do (the
   !  count derivatives), we pass in NUM_TERMS_AVAIL in this argument.
   integer (kind=mfit_integer), intent (in ) :: NUM_ADD_TERMS

   !  the widths of the photon bins, keV:
   real (kind=mfit_real), dimension(0: maxval (NUM_EBINS) -1, 0: num_det-1), intent (in) :: PHOT_WIDTH

   !  the DRM, detector response matrix:
   !  Converts photons/s-cm^2-bin to counts/s-chan-detector so units are
   !  counts-cm^2-bin / photons-chan-detector, or, using only "official" units,
   !  the DRM has units cm^2.   "Chan" are data channels and "bin" are the
   !  input or photons bins.
   !  Logical dimensions are # of photon bins X # of channels X # of detectors:
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: maxval (NUM_CHAN) -1, 0: num_det-1), intent (in) :: DRM


   !                   ****Input/Output argument: ****
   !  the photon model rate, units on input are photons/s-cm2-keV.
   !  on output the values have been modified to units photons/s-cm2-bin, which
   !  destroys the values:
   real (kind=mfit_real), dimension (0: maxval (NUM_EBINS) -1, 0: MAX_TERMS, 0: num_det-1), intent (inout) :: MODEL_PHOT_RATE_WS


   !                    **** Output arguments: ****
   !  the model count rate, units counts/s-keV:
   real (kind=mfit_real), dimension (0: maxval (NUM_CHAN) -1, 0: MAX_TERMS, 0: num_det-1), intent (out) :: MODEL_CNT_RATE


   !  Internal variables:

   integer (kind=mfit_integer) :: JTERM
   integer (kind=mfit_integer) :: PBIN
   integer (kind=mfit_integer) :: ICHAN
   integer (kind=mfit_integer) :: LDET


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   !  zero output since we sum into it:

   MODEL_CNT_RATE = 0.0


   !  M_PHOTONS_MODEL returns MODEL_PHOT_RATE_WS with units photons/s-cm2-keV,
   !  we now multiply by bin width to get photons/s-cm2-bin:

   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN
         DO JTERM=0, NUM_ADD_TERMS
            IF (TERM_USED (JTERM) .EQ. 1) THEN
               DO PBIN=FIRST_PHOT_BIN (JTERM,LDET), LAST_PHOT_BIN(JTERM,LDET)

                  MODEL_PHOT_RATE_WS (PBIN, JTERM, LDET) =   &
                  MODEL_PHOT_RATE_WS (PBIN, JTERM, LDET) * PHOT_WIDTH (PBIN, LDET)

               END DO
            END IF
         END DO
      END IF
   END DO


   !  We now multiply (for each term) the vector of photon fluxes in each
   !  energy bin by the DRM to obtain the predicted count rate in each bin:
   !  Multiplying by the DRM causes:
   !  photons/s-cm2-bin --> counts/s-chan-detector.
   !  We use the term "channels" for the count rate data, and the term "bins"
   !  for the input photon energy bins used to make the DRM.

   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN
         DO JTERM=0, NUM_ADD_TERMS
            IF (TERM_USED (JTERM) .EQ. 1) THEN
               DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET),   &
                        FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)
                  DO PBIN=MAX (FIRST_PHOT_BIN (JTERM, LDET), FIRST_NONZERO (ICHAN, LDET)), LAST_PHOT_BIN(JTERM,LDET)

                     MODEL_CNT_RATE (ICHAN, JTERM, LDET) =    &
                     MODEL_CNT_RATE (ICHAN, JTERM, LDET) + MODEL_PHOT_RATE_WS (PBIN, JTERM, LDET) * DRM (PBIN, ICHAN, LDET)

                  END DO
               END DO
            END IF
         END DO
      END IF
   END DO


   !  Now divide by channel width to get counts per sec-keV-det rather than counts per sec-chan-det:

   DO LDET=0, NUM_DET - 1
      IF (USE_DET (LDET)) THEN
         DO JTERM=0, NUM_ADD_TERMS
            IF (TERM_USED (JTERM) .EQ. 1) THEN
               DO ICHAN=FIT_CHAN (0, LDET) - CHAN_OFFSET (LDET),   &
                        FIT_CHAN (1, LDET) - CHAN_OFFSET (LDET)

                  MODEL_CNT_RATE (ICHAN, JTERM, LDET) =  &
                  MODEL_CNT_RATE (ICHAN, JTERM, LDET) / CHAN_WIDTH (ICHAN, LDET)

               END DO
            END IF
         END DO
      END IF
   END DO


   RETURN

   END SUBROUTINE MPHOTS_TO_CNTS


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   !  Test whether any element of a matrix is an illegal float.
   !  "illegal" mean NaN, +infinity or -infinity.   The test is performed
   !  by call GSL function gsl_finite.
   !  This is used to validate the alpha matrix after it has been calculated
   !  from the data -- we return from MFIT with a more "direct" error if somehow
   !  this matrix is invalid, rather than with an indirect error about a failed fit.

   !  Someday the Fortran 2003 IEEE Floating-point Exception Handling features
   !  could be used instead, but these have not been widely implemented yet.

   !  Output is solely via function return:  FALSE = ok, TRUE = ERROR.

   function M_validate_matrix  ( N, matrix )


   use MFIT_parameters

   use GSL_interfaces


   !  Input arguments:
   logical (kind=mfit_logical) :: M_validate_matrix

   integer (kind=mfit_integer), intent (in) :: N
   real (kind=mfit_DoubleReal), dimension (:, :), intent (in) :: matrix


   !  Internal variables:

   integer (kind=mfit_integer) :: i, j


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   M_validate_matrix = .FALSE.

   do j=1, N
      do i=1, N

         if ( gsl_finite ( matrix (i, j) )  == 0 )  M_validate_matrix = .TRUE.

      end do
   end do


   end function M_validate_matrix


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   !  Test whether any element of a vector is an illegal float.
   !  "illegal" mean NaN, +infinity or -infinity.   The test is performed
   !  by call GSL function gsl_finite.
   !  This is used to validate the beta vector after it has been calculated
   !  from the data -- we return from MFIT with a more "direct" error if somehow
   !  this vector is invalid, rather than with an indirect error about a failed fit.

   !  Someday the Fortran 2003 IEEE Floating-point Exception Handling features
   !  could be used instead, but these have not been widely implemented yet.

   !  Output is solely via function return:  FALSE = ok, TRUE = ERROR.

   function M_validate_vector  ( N, vector )


   use MFIT_parameters

   use GSL_interfaces


   !  Input arguments:
   logical (kind=mfit_logical) :: M_validate_vector

   integer (kind=mfit_integer), intent (in) :: N
   real (kind=mfit_DoubleReal), dimension (:), intent (in) :: vector


   !  Internal variables:

   integer (kind=mfit_integer) :: i


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   M_validate_vector = .FALSE.

   do i=1, N

      if ( gsl_finite ( vector (i) )  == 0 )  M_validate_vector = .TRUE.

   end do


   end function M_validate_vector


! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^! ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^


   !  Output the condition number of a square matrix.
   !  For this method to be correct, the matrix must be symmetric.
   !  (The singular value decomposition method is more general.)
   !  Michael S. Briggs, 2009 May 2, UAH/NSSTC.


   subroutine Output_Condition_Number  ( caller, Logical_Dim, Physical_Dim, matrix )


   use MFIT_parameters

   use GSL_interfaces


   !  Input arguments:

   character (len=*), intent (in) :: caller
   integer (kind=mfit_integer), intent (in) :: Logical_Dim
   integer (kind=mfit_integer), intent (in) :: Physical_Dim
   real (kind=mfit_DoubleReal), dimension (1:Physical_Dim, 1:Physical_Dim), intent (in) :: matrix


   !  Internal variables:

   integer (kind=mfit_integer) :: status
   real (kind=mfit_DoubleReal), dimension (Logical_Dim) :: eigenvalues
   real (kind=mfit_DoubleReal) :: Small_eigenvalue, Large_eigenvalue


   ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   status = eigenvalues_symm_matrix_w_gsl  ( Logical_Dim, Physical_Dim, MATRIX, eigenvalues )
   
   if ( status /= 0 ) then
   
      write ( 6, 1000 )  trim (caller)
      1000  format ( / A, ": failed to determine eigenvalues of matrix.  Singular matrix !?" / )
   
   else

      eigenvalues = abs (eigenvalues)
      Small_eigenvalue = minval (eigenvalues)
      Large_eigenvalue = maxval (eigenvalues)
   
      write ( 6, 1010, advance='no' )  trim (caller), Small_eigenvalue, Large_eigenvalue
      1010  format ( / A, ": Min & Max eigenavlues:", 1pg14.6, " & ", 1pg14.6 )
   
      if ( Small_eigenvalue > 0.0  .AND.  gsl_finite (Small_eigenvalue) == 1  .AND.  gsl_finite (Large_eigenvalue) == 1 ) then
         write ( 6, 1020 )  log10 (Large_eigenvalue / Small_eigenvalue)
         1020  format ( " ==> log10 Condition Number:", 1pg13.5 / )
      else
         write ( 6, 1030 )
         1030  format  ( " ==> singular matrix !!" / )
      end if
      
   end if


   end subroutine Output_Condition_Number



end module MFIT_MODULE

! #################################################################################################
