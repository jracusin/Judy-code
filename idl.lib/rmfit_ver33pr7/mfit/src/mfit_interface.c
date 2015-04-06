
/******************************************************************************
** This routine serves as an interface to the FORTRAN MFIT code.
**
** 0) It is called by C-routine mfit (file mfit_idl.c), which is called by IDL.
**    C-routine mfit does the work of obtaining C-pointers to
**    the IDL variables; C-routine mfit then calls this routine with the
**    addresses as pointer arguments.
**    (Somehow the linker is distinguishing between two routines with the name
**    "mfit", one in C, the other in Fortran 95 -- maybe underscores....)
**
** 1) It calls the entry point to the MFIT code, "mfit" of file mfit.F90.
**    Note that the format of this call is compiler dependent.
**    Compilers frequently generate different symbols for routine names; some
**    add trailing underscore(s), some prepend underscore(s), etc.
**    gcc adds a single trailing underscore.
**
** 2) mfit is now mfit.F90, translated to Fortran 95.  We are using the
**    Fortran 2003 ISO C Binding facility to interface this C routine to
**    to the Fortran subroutine mfit.  This is more portable (across compilers
**    and operating systems) than previous meshings of C and Fortran.
**    There may still be some issues because IDL may not always declare
**    variables with types consistent with the ISO C Binding.   In such cases
**    this routine will need to convert between the variables/types received
**    from IDL to variabes/types included in the ISO C Binding, so that
**    the variables will be suitable for calling a Fortran 95 (2003) routine
**    using the ISO C Binding.
**
** 3) Currently the need to type-convert variables occurs for:
**    a) the logical type, for which the rmfit IDL code currently uses 4-bytes, 
**       while the ISO C Binding provides the _Bool type, for which gcc uses 1-byte,
**    b) strings, which the Fortran provides as a vector of 1-character elements,
**       null terminated -- this is consistent with a C-string, while IDL Strings
**       are a structure.   Below IDL-provided funtions are used to copy the
**       Fortran-provided C-string into the IDL structure.
**
** 4) It receives the arguments that should be publically known and passes
**    them to MFIT.
**
** 5) It reserves storage for additional (non-public) MFIT arguments in
**    two classes:
**        a) arrays for which we use this C routine to dynamically allocate
**           storage at run time,
**        b) variables that we might want to add to the argument list of this
**           routine, thereby making them know outside of the MFIT package.
**    NOTE: the need for 5) has diminshed with the conversion of MFIT to Fortran 95,
**    since Fortran 95 can create arrays at sizes known only at runtime.
**    The reason that arrays are still dynamically created in this routine is
**    for reason 3a).
**
** 6) This is also a convenient place to disable the default error handler for the 
**    GNU Scientific Library -- see additional comments below.
**
******************************************************************************/

//  This routine was probably originally written by Bob Mallozzi.
//  Changes by Michael S. Briggs, Univ. of Alabama in Huntsville & NSSTC.

//  Revised by MSB 2009 May 18: revised above comments describing this routine.

//  Revised by MSB 2009 early May: added string output argument with MFIT version.

//  Revised by MSB 2008 Nov 17: added additional arguments: ptr_enable_undetermine,
//  ptr_undetermine_priority, ptr_undetermine_rel_req, ptr_undetermine_abs_req,
//  and ptr_highest_ud_priority.

//  Revised by MSB 2008 Oct 31: added additional argument fit_mode.


//  The pre-processor symbol MFIT_DEBUG should be set on the compile command
//  line with the "D" option, but if not, set the value to 0.
//  Below the value of MFIT_DEBUG is copied to the argument variable DebugMask.

#ifndef MFIT_DEBUG
#define MFIT_DEBUG 0
#endif


#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "export.h"

#include <gsl/gsl_errno.h>

#include "mfit_cparams.h"



//   ***   prototypes:

// provided below in this file -- we want file-scope visibility so "static":
static  void * mfit_alloc  ( void *pointer, long int bytes, int call_num );


//   The Fortran routines "mfit", accessed via the Fortran iso_c_binding -- see mfit.f95:
extern void mfit  (/* add prototype! */);


//   ***   typedefs

//  4-byte floating point
typedef float IDL_FLOAT;

//  IDL_LONG is provided by export.h -- 4-byte signed integer

/*  ** MFIT datatypes */


/*  **  now 1 byte for gcc for Fortran interoperability with C  */
typedef _Bool LOGICAL;



//  *****************************************************************************


void  mfit_interface (

   IDL_LONG  *ptr_fit_mode,
   IDL_LONG  *ptr_num_det,
   IDL_LONG  *ptr_num_chan,
   IDL_LONG  *ptr_num_ebins,
   IDL_FLOAT *ptr_chan_energy,
   IDL_FLOAT *ptr_chan_width,
   IDL_FLOAT *ptr_obs_crate,
   IDL_FLOAT *ptr_back_crate,
   IDL_FLOAT *ptr_back_csig,
   IDL_FLOAT *ptr_live_time,
   IDL_FLOAT *ptr_phot_energy,
   IDL_FLOAT *ptr_phot_width,
   IDL_FLOAT *ptr_drm,
   IDL_FLOAT *ptr_res_frac_at511,
   IDL_FLOAT *ptr_res_exp,
   IDL_LONG  *ptr_param_vary_INTEGER,     // logical, but received as 4-byte int
   IDL_LONG  *ptr_fit_chan,
   IDL_LONG  *ptr_use_det_INTEGER,     // logical, but received as 4-byte int
   IDL_LONG  *ptr_term_used,
   IDL_LONG  *ptr_num_terms_avail,
   IDL_LONG  *ptr_num_add_terms,
   IDL_LONG  *ptr_nt_add_line,
   IDL_LONG  *ptr_num_param_of_term,
   IDL_FLOAT *ptr_rel_change_limit,
   IDL_FLOAT *ptr_abs_change_limit,
   IDL_FLOAT *ptr_rel_converg,
   IDL_FLOAT *ptr_abs_converg,
   IDL_LONG  *ptr_max_tries,
   IDL_LONG  *ptr_enable_undetermine_INTEGER,     // logical, but received as 4-byte int
   IDL_LONG  *ptr_undetermine_priority,
   IDL_FLOAT *ptr_undetermine_rel_req,
   IDL_FLOAT *ptr_undetermine_abs_req,
   IDL_LONG  *ptr_highest_ud_priority,
   IDL_LONG  *ptr_chan_offset,
   IDL_FLOAT *ptr_param,
   IDL_LONG  *ptr_fit_err,
   IDL_FLOAT *ptr_model_chisq,
   IDL_LONG  *ptr_dof,
   IDL_LONG  *ptr_num_vary,
   IDL_FLOAT *ptr_param_uncer,
   IDL_FLOAT *ptr_alpha,
   IDL_FLOAT *ptr_covar,
   IDL_FLOAT *ptr_model_cnt_rate,
   IDL_FLOAT *ptr_phot_obs_rate,
   IDL_FLOAT *ptr_phot_obs_sig,
   IDL_FLOAT *ptr_nu_f_nu_data,
   IDL_FLOAT *ptr_nu_f_nu_sig,
   IDL_FLOAT *ptr_nu_f_nu_model,
   IDL_FLOAT *ptr_model_energy,
   IDL_FLOAT *ptr_model_phot_rate,
   IDL_FLOAT *ptr_model_phot_rate_bychan,
   IDL_FLOAT *ptr_model_cr_vari,
   IDL_FLOAT *ptr_data_cr_vari,
   IDL_STRING *ptr_mfit_version_IDL_String
   
) {


   static _Bool  FirstCall = true;      // static for value that persists across calls
   
   gsl_error_handler_t * gsl_error_handler_ptr;


   //  Create a variable that receives a compile-line "D" option
   //  (undefined MFIT_DEBUG will convert to 0, which is what we want):
   IDL_LONG DebugMask = MFIT_DEBUG;


   /*
   ** Additional variables of mfit.F90

   ** These are created here, in C, so that they can be dynamically sized
   ** at run time.
   */

   char mfit_version_C_string [VERSION_LEN+1];


   //  Fix up the logicals -- received from IDL via mfit_idl.c as 4-byte logicals,
   //  but we must pass them to Fortran as 1-byte (in gcc) c_bool type, in order
   //  to satisfy a Fortran 95 compiler when using the ISO C Binding capability.
   //  In order to receive these items in 4-byte variables, above they were
   //  declared as "IDL_LONG".   Here we create new variables of type LOGICAL
   //  (which is c_bool) and copy the information.

   LOGICAL *ptr_param_vary          = NULL;
   LOGICAL *ptr_use_det             = NULL;
   LOGICAL enable_undetermine;

   int i, j;


   //  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *


   //  Turn off the current error handler for the GNU Scientific Library (GSL) without providing
   //  any replacement-- since this is the first action w.r.t. GSL error handling, the error handler 
   //  that we are disabling is the default GNU Scientific Library (GSL) Error Handler.
   //  We don't want to use the default Error Handler, which is intended as simple placeholder
   //  for code being developed, because it aborts execution.   Instead of using an error handler 
   //  we will check in MFIT (either in gsl_interface_routines.c or mfit.F90) the status return of
   //  every GSL function that provides a status return.
   
   if (FirstCall) {
      gsl_error_handler_ptr = gsl_set_error_handler_off ();
      FirstCall = false;
   }


   //  Copy from the 4-byte ints that the current rmfit IDL code uses as logical variables
   //  into the 1-byte C _Bool variables that we will pass to Fortran 95 MFIT using
   //  the ISO C Binding:

   ptr_param_vary = (LOGICAL *) mfit_alloc (ptr_param_vary, MAX_TERMS * MAX_PPT * sizeof (LOGICAL), 201);

   ptr_use_det = (LOGICAL *) mfit_alloc (ptr_use_det, (*ptr_num_det) * sizeof (LOGICAL), 202);


   for ( i=0;  i<MAX_TERMS;  i++ ) {
      for ( j=0;  j<MAX_PPT;  j++ ) {
         ptr_param_vary [i*MAX_PPT + j] = ptr_param_vary_INTEGER [i*MAX_PPT + j];
      }
   }

   for ( i=0;  i<*ptr_num_det;  i++ ) {
      ptr_use_det [i] = ptr_use_det_INTEGER [i];
   }

   enable_undetermine = *ptr_enable_undetermine_INTEGER;


   //  Finally, call the Fortran Code:

   mfit (

     &DebugMask,                 ptr_fit_mode,
     ptr_num_det,                ptr_num_chan,
     ptr_num_ebins,
     ptr_chan_energy,            ptr_chan_width,        ptr_obs_crate,
     ptr_back_crate,             ptr_back_csig,         ptr_live_time,
     ptr_phot_energy,            ptr_phot_width,        ptr_drm,
     ptr_res_frac_at511,         ptr_res_exp,
     ptr_fit_chan,               ptr_use_det,           ptr_term_used,
     ptr_num_terms_avail,        ptr_num_add_terms,     ptr_nt_add_line,
     ptr_num_param_of_term,      ptr_rel_change_limit,  ptr_abs_change_limit,
     ptr_rel_converg,            ptr_abs_converg,       ptr_max_tries,
     &enable_undetermine,        ptr_undetermine_priority, ptr_undetermine_rel_req,
     ptr_undetermine_abs_req,    ptr_highest_ud_priority,  ptr_param_vary,
     ptr_chan_offset,            ptr_param,             ptr_fit_err,
     ptr_model_chisq,            ptr_dof,               ptr_num_vary,          
     ptr_param_uncer,            ptr_alpha,
     ptr_covar,                  ptr_model_cnt_rate,    ptr_phot_obs_rate,
     ptr_phot_obs_sig,           ptr_nu_f_nu_data,      ptr_nu_f_nu_sig,
     ptr_nu_f_nu_model,          ptr_model_energy,      ptr_model_phot_rate,
     ptr_model_phot_rate_bychan, ptr_model_cr_vari,     ptr_data_cr_vari,
     mfit_version_C_string

   );


   //  Copy back "inout" and "out" arguments of mfit that need to have their types changed.
   //  No need to copy back "in" arguments since they can't have been altered.
   
   //  1-byte logicals in Fortran MFIT to 4-byte integers in IDL:

   for ( i=0;  i<MAX_TERMS;  i++ ) {
      for ( j=0;  j<MAX_PPT;  j++ ) {
         ptr_param_vary_INTEGER [i*MAX_PPT + j] = ptr_param_vary [i*MAX_PPT + j];
      }
   }

   *ptr_enable_undetermine_INTEGER = enable_undetermine;

   free (ptr_param_vary);            // 201
   free (ptr_use_det);               // 202
   
   
   //  Convert C string (received from Fortran output argument via ISO C Binding)
   //  to a IDL String structure:
   //  Step 1 is to delete existing the contents of the IDL_STRING structure,
   //  using IDL_StrDelete, thereby freeing any currently allocated memory.
   //  Step 2 to use IDL_StrStore to copy the null-terminated C string
   //  to the IDL String structure.
   
   IDL_StrDelete  ( ptr_mfit_version_IDL_String, 1 );
   
   IDL_StrStore  ( ptr_mfit_version_IDL_String, mfit_version_C_string );


   return;


}  // void mfit_interface ();


//  *****************************************************************************


void * mfit_alloc ( void *pointer, long int bytes, int call_num ) {

   if (pointer != NULL) {

     /*
      printf ("mfit_alloc: Deallocating pointer: %p, bytes %ld, call_num %d\n", pointer, bytes, call_num);
      */

      free (pointer);
   }

   pointer = (void *) malloc (bytes);
   
   if (pointer == NULL) {

      fprintf (stderr, "mfit_alloc: FAILED to allocate %ld bytes in call %d\n", bytes, call_num );
      exit (1);

   }


   /*
   printf ("mfit_alloc: Allocating pointer: %p, bytes %ld, call_num %d\n",
   pointer, bytes, call_num);
   */

   return pointer;

}  // void *mfit_alloc ();
