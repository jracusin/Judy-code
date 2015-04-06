
! #################################################################################################


!  Fortran 95 interfaces to C routines in the GNU Scientific Library (GSL) used by MFIT.
!  The interfaces were written based on GSL & GSL Reference Manual version 1.12 of 15 December 2008.

!  Also interfaces to C routines that call GSL routines -- in some cases MFIT "indirectly" calls
!  the GNU Scientific Library -- intermediate C routines are used to provide a simpler interface.

!  Michael S. Briggs, 2009 April 8, UAH / NSSTC.
!  rev. 2009 May 3 by MSB.


module GSL_interfaces


   use iso_c_binding

   implicit none


   ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_CompErrorFunction   ! Complementary Error Function

      function gsl_sf_erfc  ( x )  bind ( C, name="gsl_sf_erfc" )

         import

         real (kind=c_double) :: gsl_sf_erfc
         real (kind=c_double), VALUE, intent (in) :: x

      end function gsl_sf_erfc

   end interface GSL_CompErrorFunction


   ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_Test_Float   ! tests floating point for legality

      function gsl_finite  ( x )  bind ( C, name="gsl_finite" )

         import

         integer (kind=c_int32_t) :: gsl_finite
         real (kind=c_double), VALUE, intent (in) :: x

      end function gsl_finite

   end interface GSL_Test_Float


   ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_LogFactorial   !  Logarithm of the factorial: log (n!)

      function gsl_sf_lnfact  ( n )  bind ( C, name="gsl_sf_lnfact" )

         import

         real (kind=c_double) :: gsl_sf_lnfact

         !  In the GSL, the argument "n" is typed as "unsigned int".
         !  The Fortran 2003 ISO C Binding doesn't have an unsigned int type so
         !  we use a signed int.   These will agree for values about 2**31 = 2,147,483,648,
         !  which is plenty large enough for our usage.
         !  Tested to work to 2147483647 -- same answers as Numerical Recipes routine log_factorial.

         integer (kind=c_int), VALUE, intent (in) :: n

      end function gsl_sf_lnfact

   end interface GSL_LogFactorial


   ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_CummulativeChiSq_Prob   !  ChisSq cumulative probability distribution from 0 to x
                                         !  x is ChiSq value, nu is Degrees-of-Freedom

      function gsl_cdf_chisq_P  ( x,  nu )  bind ( C, name="gsl_cdf_chisq_P" )

         import

         real (kind=c_double) :: gsl_cdf_chisq_P

         real (kind=c_double), VALUE, intent (in) :: x
         real (kind=c_double), VALUE, intent (in) :: nu

      end function gsl_cdf_chisq_P

   end interface GSL_CummulativeChiSq_Prob


   ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_CummulativeChiSq_Prob_Upper   !  ChisSq cumulative probability distribution from x to infinity
                                               !  x is ChiSq value, nu is Degrees-of-Freedom

      function gsl_cdf_chisq_Q  ( x,  nu )  bind ( C, name="gsl_cdf_chisq_Q" )

         import

         real (kind=c_double) :: gsl_cdf_chisq_Q

         real (kind=c_double), VALUE, intent (in) :: x
         real (kind=c_double), VALUE, intent (in) :: nu

      end function gsl_cdf_chisq_Q

   end interface GSL_CummulativeChiSq_Prob_Upper


   ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_Integrate

      !  This interface isn't directly to a GSL routine, but rather to some interface C-code
      !  that provides a simpler interface.    That C-code calls GSL routine gsl_integration_qags
      !  to perform the integration.

      function integrate_using_gsl  ( a, b, epsabs, epsrel, integrand_ptr, result, abserr )     &
            bind ( C, name="integrate_using_gsl" )

         import

         !  Function return indicates success (zero) or failure (non-zero):
         integer (kind=c_int32_t) :: integrate_using_gsl

         real (kind=c_double), VALUE, intent (in) :: a        ! lower integration limit
         real (kind=c_double), VALUE, intent (in) :: b        ! upper integration limit
         real (kind=c_double), VALUE, intent (in) :: epsabs     ! absolute error request
         real (kind=c_double), VALUE, intent (in) :: epsrel     ! relative error request
         type (c_funptr), intent (in), VALUE :: integrand_ptr
         real (kind=c_double), intent (out) :: result         ! estimate of the integral
         real (kind=c_double), intent (out) :: abserr         ! estimate of absolute error of result

      end function integrate_using_gsl

   end interface GSL_Integrate


   !  apparently one can't have a function and a subroutine in the same interface block, so:
   interface GSL_Integrate_Helper

      subroutine integrate_gsl_free_mem  ()  bind ( C, name="integrate_gsl_free_mem" )

      import

      end subroutine integrate_gsl_free_mem

   end interface GSL_Integrate_Helper


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_Matrix_Invert

      !  This interface isn't directly to a GSL routine, but rather to some interface C-code
      !  that provides a simpler interface to invert a matrix.    That C-code calls several
      !  GSL routines to invert a matrix.   The method is LU decompostion.

      !  The Fortran Matrices are declared with size ( Physical_Dim, Not Applicable ).
      !  The logical sizes are ( Logical_Dim, Logical_Dim ).


      function invert_matrix_with_gsl  ( Logical_Dim, Physical_Dim, Matrix, Inverse_Matrix )     &
            bind ( C, name="invert_matrix_with_gsl" )

         import

         !  Function return indicates success (zero) or failure (non-zero):
         integer (kind=c_int32_t) :: invert_matrix_with_gsl

         integer (kind=c_int32_t), VALUE, intent (in) :: Logical_Dim
         integer (kind=c_int32_t), VALUE, intent (in) :: Physical_Dim
         real (kind=c_double), dimension (1:Physical_Dim, 1:Physical_Dim), intent (in) :: Matrix
         real (kind=c_double), dimension (1:Physical_Dim, 1:Physical_Dim), intent (out) :: Inverse_Matrix

      end function invert_matrix_with_gsl

   end interface GSL_Matrix_Invert


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_Linear_System_Solve

      !  This interface isn't directly to a GSL routine, but rather to some interface C-code
      !  that provides a simpler interface to solve a linear system Ax = b for x, where A is a matrix
      !  and x and b are vectors.    That C-code calls several GSL routines to perform the solution.
      !  The method is LU decompostion.

      !  The Fortran Matrix is declared with size ( Physical_Dim, Not Applicable ).
      !  The logical size is ( Logical_Dim, Logical_Dim ).

      function solve_linear_system_with_gsl  ( Logical_Dim, Physical_Dim, Matrix_A, Vector_b, Vector_x )      &
            bind ( C, name="solve_linear_system_with_gsl" )

         import

         !  Function return indicates success (zero) or failure (non-zero):
         integer (kind=c_int32_t) :: solve_linear_system_with_gsl

         integer (kind=c_int32_t), VALUE, intent (in) :: Logical_Dim
         integer (kind=c_int32_t), VALUE, intent (in) :: Physical_Dim
         real (kind=c_double), dimension (1:Physical_Dim, 1:Physical_Dim), intent (in) :: Matrix_A
         real (kind=c_double), dimension (1:Physical_Dim), intent (in) :: Vector_b
         real (kind=c_double), dimension (1:Physical_Dim), intent (out) :: Vector_x

      end function solve_linear_system_with_gsl

   end interface GSL_Linear_System_Solve


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


   interface GSL_Eigenvalues_Solve

      !  Obtains the eigenvalues of a real SYMMETRIC matrix
      !  This interface isn't directly to a GSL routine, but rather to some interface C-code
      !  that provides a simpler interface.   The actual work is done by GSL routines.

      !  The Fortran Matrix is declared with size ( Physical_Dim, Not Applicable ).
      !  The logical size is ( Logical_Dim, Logical_Dim ).

      function eigenvalues_symm_matrix_w_gsl  ( Logical_Dim, Physical_Dim, Matrix, eigenvalues )      &
            bind ( C, name="eigenvalues_symm_matrix_w_gsl" )

         import

         !  Function return indicates success (zero) or failure (non-zero):
         integer (kind=c_int32_t) :: eigenvalues_symm_matrix_w_gsl

         integer (kind=c_int32_t), VALUE, intent (in) :: Logical_Dim
         integer (kind=c_int32_t), VALUE, intent (in) :: Physical_Dim
         real (kind=c_double), dimension (1:Physical_Dim, 1:Physical_Dim), intent (in) :: Matrix
         real (kind=c_double), dimension (1:Physical_Dim), intent (out) :: eigenvalues

      end function eigenvalues_symm_matrix_w_gsl

   end interface GSL_Eigenvalues_Solve


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


end module GSL_interfaces
