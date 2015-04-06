

//  Provides interface / glue routines between Fortran routines and
//  routines of the GNU Scientific Library (GSL).   The C routines of this file
//  are intended to be easier to call from Fortran, at least for my applications.
//  Michael S. Briggs, UAH/NSSTC, 2009 April 28.

//  Routines provided by this file / externally visible:
//  integrate_using_gsl
//  integrate_gsl_free_memory
//  invert_matrix_with_gsl
//  solve_linear_system_with_gsl
//  eigenvalues_symm_matrix_w_gsl


#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include <gsl/gsl_matrix.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_eigen.h>


//  file-scope variables:

static  gsl_integration_workspace * gsl_workspace_ptr = NULL;



//  #defines:

//  200 is probably enough....
#define WORKSPACE_SIZE   800



//  prototypes local to this file:

static void  matrix_Fortran_to_gsl ( 

   // input arguments:
   const int32_t  Logical_Dim,
   const int32_t  Physical_Dim,
   const double * const restrict Fortran_Matrix_ptr,
   
   // output argument:
   gsl_matrix * const restrict gsl_matrix_ptr
   
);

static void  matrix_gsl_to_Fortran ( 

   // input arguments:
   const int32_t  Logical_Dim,
   const int32_t  Physical_Dim,
   const gsl_matrix * const restrict gsl_matrix_ptr,
   
   // output argument:
   double * const restrict Fortran_Matrix_ptr
   
);


// *****************************************************************************************
// *****************************************************************************************

//  Provides an interface between GSL numeric integration routine gsl_integration_qags and Fortran.
//  Michael S. Briggs, 2009 April 11, rev. 2009 May 2.

//  When you are done with all numeric integration in a program you should
//  free the workspace memory by calling integrate_gsl_free_memory.

//  Function return indicates success (zero) or failure (non-zero).

//  To keep the code from becoming deeply nested, it has multiple returns -- a simple 
//  "return (non-zero-value);" where ever a non-recoverable error might be detected.


int32_t  integrate_using_gsl (

   // input arguments:
   const double a,
   const double b,
   const double epsabs,
   const double epsrel,
   
   // function to integrate:
   double (* const integrand_ptr)  (double x, void * params),
   
   // output arguments:
   double * const result_ptr,
   double * const abserr_ptr

) {

   size_t limit = WORKSPACE_SIZE;

   int status;

   gsl_function FUNC;


   //  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   if ( gsl_workspace_ptr == NULL ) {

      gsl_workspace_ptr = gsl_integration_workspace_alloc ( limit );
      if ( gsl_workspace_ptr == NULL )  return (1001);

   }  // need to allocate workspace?


   FUNC.function = integrand_ptr;
   FUNC.params = NULL;

   status = gsl_integration_qags ( &FUNC, a, b, epsabs, epsrel, limit, gsl_workspace_ptr, result_ptr, abserr_ptr );
   if ( status != 0 )  return (1002);


   return (0);


}  //  integrate_using_gsl ()



// *****************************************************************************************
// *****************************************************************************************


void  integrate_gsl_free_memory  ()  {

   gsl_integration_workspace_free  ( gsl_workspace_ptr );
   gsl_workspace_ptr = NULL;

   return;

}  //  integrate_gsl_free_memory ()


// *****************************************************************************************
// *****************************************************************************************


//  Inverts square matrix Fortran_Matrix.
//  Michael S. Briggs, 2009 April 26, rev. 2009 May 2.

//  The input matrix is not altered, instead the inverse is output in a separate matrix.

//  The method is LU decomposition.  The vector & matrix routines of Chap. 8 are needed 
//  because the Linear Algebra routines of Chap. 13 expect to receive their vector and 
//  matrix arguments in the GSL vector/matrix structures described in Chap. 8.  The 
//  routines of Chap. 8 are used to move the data to/from the Fortran matrices and
//  GSL matrix structures.  (There is also permutation structure declared (Chap. 9),
//  but it's use is more implicit / transparent.)

//  In the C routines matrix_Fortran_to_gsl and  matrix_gsl_to_Fortran of this file, 
//  the Fortran matrices are treated as pointers, with explicit calculation of the offsets 
//  of matrix elements.   For this offset calculation, it is necessary to know the declared 
//  (aka "physical") dimensions of the Fortran arrays,  and not just the sizes currently 
//  in use (aka "logical").

//  The Fortran Matrices are declared with size Physical_Dim X Physical_Dim,
//  while the used sizes are Logical_Dim X Logical_Dim.   The sizes have to be passed
//  to this routine so that the indexing can be done correctly.
//  This routine assumes that both input and output matrices have the same dimensions --
//  physical and (obviously) logical -- this is a requirement.

//  Function return indicates success (zero) or failure (non-zero).

//  To keep the code from becoming deeply nested, it has multiple returns -- a simple 
//  "return (non-zero-value);" where ever a non-recoverable error might be detected.


int32_t  invert_matrix_with_gsl (

   // input arguments:
   const int32_t  Logical_Dim,
   const int32_t  Physical_Dim,
   const double * const Fortran_Matrix_ptr,
   
   // output argument:
   double * const Fortran_INV_Matrix_ptr

) {

   gsl_matrix * gsl_matrix_ptr;
   gsl_matrix * gsl_INV_matrix_ptr;
   gsl_permutation * gsl_permutation_ptr;
   
   int status;
   int sign;
   
   
   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   //  Allocate GSL structures:
   gsl_matrix_ptr = gsl_matrix_alloc  ( Logical_Dim, Logical_Dim );
   gsl_INV_matrix_ptr = gsl_matrix_alloc  ( Logical_Dim, Logical_Dim );
   gsl_permutation_ptr = gsl_permutation_alloc  ( Logical_Dim );
   if ( gsl_matrix_ptr == NULL  ||  gsl_INV_matrix_ptr == NULL  ||  gsl_permutation_ptr == NULL )  return (1011);

   
   //  Copy the the Fortran array to the GSL matrix structure:
   matrix_Fortran_to_gsl  ( Logical_Dim, Physical_Dim, Fortran_Matrix_ptr, gsl_matrix_ptr );

   //  Perform the LU decompostion:
   status = gsl_linalg_LU_decomp  ( gsl_matrix_ptr, gsl_permutation_ptr, &sign );
   if ( status != 0 )  return (1012);
   
   //  Invert the matrix!
   status = gsl_linalg_LU_invert  ( gsl_matrix_ptr, gsl_permutation_ptr, gsl_INV_matrix_ptr );
   if ( status != 0 )  return (1013);

   //  Copy the solution (matrix inverse) from the GSL structure to the Fortran output matrix:
   matrix_gsl_to_Fortran  ( Logical_Dim, Physical_Dim, gsl_INV_matrix_ptr, Fortran_INV_Matrix_ptr );


   //  And free the GSL structures:
   gsl_matrix_free  ( gsl_matrix_ptr );  
   gsl_matrix_free  ( gsl_INV_matrix_ptr );
   gsl_permutation_free  ( gsl_permutation_ptr );
   
   
   return (0);

}  //  invert_matrix_with_gsl ()


// *****************************************************************************************
// *****************************************************************************************


//  This routine solves the linear system Ax = b for x, where A is a matrix and x and b
//  are vectors.  
//  Michael S. Briggs, 2009 April 28, rev. 2009 May 2.

//  The method is LU decomposition of A.  Then the linear equations simplify and
//  can be solved by forward and back substitution.  These methods are implemented in
//  GSL subroutines.   The complication is that the GSL routines require that the
//  matrices and vectors be in particular GSL structures, whilst this routine receives
//  them as pointers to simple Fortran matrices and vectors, so we transfer the input
//  to GSL structures, and the solution from a GSL vector structure to a Fortran vector.

//  So the vector & matrix routines of Chap. 8 of the GSL manual are used to convert
//  between the Fortran arrays/vectors and GSL structures.   The actual solution is done
//  using linear algebra routines from Chap. 13.    (There is also permutation structure 
//  declared (Chap. 9), but it's use is more implicit / transparent.)

//  In the C routines matrix_Fortran_to_gsl and  matrix_gsl_to_Fortran of this file, 
//  the Fortran matrices are treated as pointers, with explicit calculation of the offsets 
//  of matrix elements.   For this offset calculation, it is necessary to know the declared 
//  (aka "physical") dimensions of the Fortran arrays,  and not just the sizes currently 
//  in use (aka "logical").

//  The Fortran array is declared with size Physical_Dim X Physical_Dim,
//  while the used size is Logical_Dim X Logical_Dim.   The sizes have to be passed
//  to this routine so that the indexing can be done correctly.
//  The vectors are less critical; obviously in Fortran they must have sizes >= Logical_Dim.

//  Function return indicates success (zero) or failure (non-zero).

//  To keep the code from becoming deeply nested, it has multiple returns -- a simple 
//  "return (non-zero-value);" where ever a non-recoverable error might be detected.


int32_t  solve_linear_system_with_gsl (

   // input arguments:
   const int32_t  Logical_Dim,
   const int32_t  Physical_Dim,
   const double * const Fortran_Matrix_A_ptr,
   const double * const Fortran_Vector_b_ptr,
   
   // output argument:
   double * const Fortran_Vector_x_ptr

) {

   gsl_matrix * gsl_matrix_A_ptr;
   gsl_matrix * gsl_matrix_A2_ptr;
   gsl_vector * gsl_vector_b_ptr;
   gsl_vector * gsl_vector_x_ptr;
   gsl_vector * gsl_vector_residual_ptr;
   gsl_permutation * gsl_permutation_ptr;
   
   int  status;
   int  sign;
   int  i;
   
   
   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   

   //  Allocate GSL structures:
   gsl_matrix_A_ptr = gsl_matrix_alloc  ( Logical_Dim, Logical_Dim );
   gsl_matrix_A2_ptr = gsl_matrix_alloc  ( Logical_Dim, Logical_Dim );
   gsl_vector_b_ptr = gsl_vector_alloc  ( Logical_Dim );
   gsl_vector_x_ptr = gsl_vector_alloc  ( Logical_Dim );
   gsl_vector_residual_ptr = gsl_vector_alloc  ( Logical_Dim );
   gsl_permutation_ptr = gsl_permutation_alloc  ( Logical_Dim );
   
   if ( gsl_matrix_A_ptr == NULL  ||  gsl_matrix_A2_ptr == NULL  ||  gsl_vector_b_ptr == NULL  ||
        gsl_vector_x_ptr == NULL  ||  gsl_vector_residual_ptr == NULL  ||  gsl_permutation_ptr == NULL )  return (1021);

   //  Copy the the Fortran array to the GSL matrix structure; make a duplicate.
   matrix_Fortran_to_gsl  ( Logical_Dim, Physical_Dim, Fortran_Matrix_A_ptr, gsl_matrix_A_ptr );
   status = gsl_matrix_memcpy  ( gsl_matrix_A2_ptr, gsl_matrix_A_ptr );
   if ( status != 0 )  return (1022);

   //  Perform the LU decompostion:
   status = gsl_linalg_LU_decomp  ( gsl_matrix_A_ptr, gsl_permutation_ptr, &sign );
   if ( status != 0 )  return (1023);

   //  Copy the Fortran vector "b" into a GSL vector structure:
   for ( i=0;  i < Logical_Dim;  i++ )  gsl_vector_set  ( gsl_vector_b_ptr, i, Fortran_Vector_b_ptr [i] );
   
   //  Solve the linear system !
   status = gsl_linalg_LU_solve  ( gsl_matrix_A_ptr, gsl_permutation_ptr, gsl_vector_b_ptr, gsl_vector_x_ptr );
   if ( status != 0 )  return (1024);
   
   //  "polish" the solution by doing one step of iterative improvement.
   //   "A" is now the LU decomposition of the input matrix, while A2 remains a copy of the matrix.
   status = gsl_linalg_LU_refine  ( gsl_matrix_A2_ptr, gsl_matrix_A_ptr, gsl_permutation_ptr, gsl_vector_b_ptr, 
                  gsl_vector_x_ptr, gsl_vector_residual_ptr );
   if ( status != 0 )  return (1025);

   //  Copy the vector solution "x" from the GSL structure to the Fortran output vector (1D array):
   for ( i=0;  i < Logical_Dim;  i++ )  Fortran_Vector_x_ptr [i] = gsl_vector_get  ( gsl_vector_x_ptr, i );
   

   //  And free the GSL structures:
   gsl_matrix_free  ( gsl_matrix_A_ptr ); 
   gsl_matrix_free  ( gsl_matrix_A2_ptr );
   gsl_vector_free  ( gsl_vector_b_ptr );
   gsl_vector_free  ( gsl_vector_x_ptr );
   gsl_vector_free  ( gsl_vector_residual_ptr );
   gsl_permutation_free  ( gsl_permutation_ptr );
   
   
   return (0);

}  //  solve_linear_system_with_gsl ()


// *****************************************************************************************
// *****************************************************************************************


//  This routine obtains the eigenvalues of a real symmetric matrix.
//  Michael S. Briggs, 2009 April 28, rev. 2009 May 2.

//  This subroutine calls a GSL subroutine to calculate the eigenvalues.  The complication
//  is that the linear algebra GSL routine requires that the matrices and vectors be in 
//  particular GSL structures, whilst this routine receives them as pointers to simple 
//  Fortran matrices and vectors, so we transfer the input to GSL structures, and the 
//  solution from a GSL vector structure to a Fortran vector.

//  So the vector & matrix routines of Chap. 8 of the GSL manual are used to convert
//  between the Fortran arrays/vectors and GSL structures.   The actual solution is done
//  using linear algebra routines from Chap. 13. 

//  In the C routines matrix_Fortran_to_gsl and  matrix_gsl_to_Fortran of this file, 
//  the Fortran matrices are treated as pointers, with explicit calculation of the offsets 
//  of matrix elements.   For this offset calculation, it is necessary to know the declared 
//  (aka "physical") dimensions of the Fortran arrays,  and not just the sizes currently 
//  in use (aka "logical").

//  The Fortran array is declared with size Physical_Dim X Physical_Dim,
//  while the used size is Logical_Dim X Logical_Dim.   The sizes have to be passed
//  to this routine so that the indexing can be done correctly.
//  The vectors are less critical; obviously in Fortran they must have sizes >= Logical_Dim.

//  Function return indicates success (zero) or failure (non-zero).

//  To keep the code from becoming deeply nested, it has multiple returns -- a simple 
//  "return (non-zero-value);" where ever a non-recoverable error might be detected.


int32_t  eigenvalues_symm_matrix_w_gsl ( 

   // input arguments:
   const int32_t  Logical_Dim,
   const int32_t  Physical_Dim,
   const double * const Fortran_Matrix_ptr,
   
   // output argument:
   double * const Fortran_Vector_ptr

) {

   gsl_matrix * gsl_matrix_ptr;
   gsl_vector * gsl_vector_ptr;
   
   gsl_eigen_symm_workspace * gsl_eigen_workspace_ptr;
   
   int  status;
   int  i;
   
   
   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   

   //  Allocate GSL structures:
   gsl_matrix_ptr = gsl_matrix_alloc  ( Logical_Dim, Logical_Dim );
   gsl_vector_ptr = gsl_vector_alloc  ( Logical_Dim );
   gsl_eigen_workspace_ptr = gsl_eigen_symm_alloc  ( Logical_Dim );
   if ( gsl_matrix_ptr == NULL  ||  gsl_vector_ptr == NULL  ||  gsl_eigen_workspace_ptr == NULL )  return (1031);
   

   //  Copy the the Fortran array to the GSL matrix structure:
   matrix_Fortran_to_gsl  ( Logical_Dim, Physical_Dim, Fortran_Matrix_ptr, gsl_matrix_ptr );
   
   //  Solve for the eigenvalues:
   status = gsl_eigen_symm  ( gsl_matrix_ptr, gsl_vector_ptr, gsl_eigen_workspace_ptr );
   if ( status != 0 )  return (1032);

   //  Copy the eigenvalues from the GSL vector structure to the Fortran output vector (1D array):
   for ( i=0;  i < Logical_Dim;  i++ )  Fortran_Vector_ptr [i] = gsl_vector_get  ( gsl_vector_ptr, i );
   

   //  And free the GSL structures:
   gsl_matrix_free  ( gsl_matrix_ptr ); 
   gsl_vector_free  ( gsl_vector_ptr );
   gsl_eigen_symm_free  ( gsl_eigen_workspace_ptr );
   
   
   return (0);

}  //  eigenvalues_symm_matrix_w_gsl ()


// *****************************************************************************************
// *****************************************************************************************


//  This routine is internal to this file and does not have external visibility.

//  This routine transfers the elements of a Fortran array to a GSL matrix structure.
//  Michael S. Briggs, 2009 April 26, rev. 2009 May 2.

//  The Fortran array is "seen" by C as a pointer.
//  The Fortran array is declared with size Physical_Dim X Physical_Dim,
//  while the used size is Logical_Dim X Logical_Dim.
//  (The simpler gsl_matrix_view_array can't be used when Physical_Dim != Logical_Dim,
//   because in that case the array elements aren't completely contiguous.)

void  matrix_Fortran_to_gsl ( 

   // input arguments:
   const int32_t  Logical_Dim,
   const int32_t  Physical_Dim,
   const double * const restrict Fortran_Matrix_ptr,
   
   // output argument:
   gsl_matrix * const restrict gsl_matrix_ptr
   
) {

   int i, j;


   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   for ( i=0; i < Logical_Dim; i++ ) {
      for ( j=0; j < Logical_Dim; j++ ) {
      
         gsl_matrix_set  ( gsl_matrix_ptr, i, j, Fortran_Matrix_ptr [i + j*Physical_Dim] );
      
      }  // j 
   }  // i


   return;

}  // matrix_Fortran_to_gsl ()


// *****************************************************************************************
// *****************************************************************************************


//  This routine is internal to this file and does not have external visibility.

//  This routine transfers the elements of a GSL matrix structure to a Fortran array.
//  Michael S. Briggs, 2009 April 26, rev. 2009 May 2.

//  The Fortran array is "seen" by C as a pointer.
//  The Fortran array is declared with size Physical_Dim X Physical_Dim,
//  while the used size is Logical_Dim X Logical_Dim.

void  matrix_gsl_to_Fortran ( 

   // input arguments:
   const int32_t  Logical_Dim,
   const int32_t  Physical_Dim,
   const gsl_matrix * const restrict gsl_matrix_ptr,
   
   // output argument:
   double * const restrict Fortran_Matrix_ptr
   
) {

   int i, j;
   
   
   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   for ( i=0; i < Logical_Dim; i++ ) {
      for ( j=0; j < Logical_Dim; j++ ) {
      
         Fortran_Matrix_ptr [i + j*Physical_Dim] = gsl_matrix_get  ( gsl_matrix_ptr, i, j );
      
      }  // j 
   }  // i


   return;

}  // matrix_gsl_to_Fortran ()

