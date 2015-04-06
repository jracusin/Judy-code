
//  This file contains two externally visible routines:
//  "IDL_Load" and "mfit".

//  During the startup phase of IDL, IDL determines that the mfit DLM
//  exists by finding the file mfit.dlm on its DLM path.
//  When routine mfit is called from IDL rmfit, IDL loads the shareable
//  library mfit.so and calls the function "IDL_Load" to determine
//  the characterisics of "mfit".   It then calls routine "mfit" of
//  this file.   All further mfit calls from IDL directly call "mfit".
//  See Chapter 15, "Adding System Routines", of the IDL "External
//  Development Guide".

//  Routine "mfit" serves as the first step of the interface to the 
//  Fortran 95 MFIT.
//  (This routine is called "mfit", and so is the Fortran 95 routine --
//  somehow the linker is distinguishing them ... maybe underscores...)

//  The primary work of this routine is to call IDL_VarGetData
//  to obtain obtain a C-pointer for each variable passed
//  by IDL (which is otherwise contained in an IDL structure).
//  This routine also performs some data-validity checks --
//  IDL_VarGetData also returns the number of elements in arrays,
//  and this routine checks that each array has the expected number
//  of elements.   (However, this routine does not check that the
//  calling IDL routine is using the expected types for the variables.)

//  This routine then calls mfit_interface.c, which performs some
//  additional interface transformations and finally calls the Fortran 95 MFIT.

//  Above comments by Michael S. Briggs, 2009 May 18 & 24.


//  This routine was probably originally written by Bob Mallozzi.
//  Changes by Michael S. Briggs, Univ. of Alabama in Huntsville & NSSTC.


//  Revised 2009 May 24 by MSB:
//  program self-consistency check: compare number of calls to IDL_VarGetData
//  to value of MFIT_INTERFACE_NPARAMS.    Improve comments.

//  Revised 2009 May 22 by MSB & RDP: fix several array size checks.

//  Revised 2009 May 19 by MSB:
//  deleted arguments ptr_MaxChan and ptr_MaxEbins as permitted by changes
//  of May 16th.   Argument count now 54.

//  Revised 2009 May 16 by MSB:
//  Calculate the maximum number of channels (CalcMaxChan) and the maximum
//  number of energy bins (CalcMaxEbins) from the arrays of number of 
//  channels and energy bins using new function CMaxVal_of_vector.
//  Eventually this will allow deleting arguments ptr_MaxChan and ptr_MaxEbins.

//  Revised 2009 May 6 by MSB:
//  Add mfit_version as additional argument.  Argument count now 56.

//  Revised 2009 May 4 by MSB: 
//  Removed two arguments, have_model and have_fit, added argument alpha.
//  Net change in argument count is -1 to argument count of 55.

//  Revised 2008 Oct 31 by MSB: added additional argument fit_mode;
//  increase MFIT_INTERFACE_NPARAMS by 1 to 51.

//  Revised 2008 Oct 2 by Michael S. Briggs:
//  1) rearrange code to be more maintainable,
//  2) to handle the different types of IDL_LONG and
//  IDL_MEMINT on 32-bit and 64-bit machines.

//  Revised 2008 Sept 6 & 7 by Michael S. Briggs to test values of some scalers
//  input by IDL and the number of elements of all arrays created by IDL.
//  The response to an error is to print via printf to standard io --
//  the terminal window.  This checking is only done under unix type operating
//  systems because MS Windows doesn't have a terminal indow under IDL-- this
//  is checked via the #define symbol IDL_OS_HAS_TTYS from idl_export.h --
//  this isn't supposed to be a public symbol, so there is a small risk that
//  this symbol might disappear.
//  However, this code does not check the types of the input arguments !



#include <stdio.h>

#include <stdbool.h>

#include "export.h"

#include "mfit_cparams.h"




//  Number of input parameters to mfit_interface.c and to mfit_FinePhotModel_C --
//  Also maintain the two pairs of values in mfit.dlm to agree with this value !!

#define MFIT_INTERFACE_NPARAMS     54
#define MFIT_FINEPHOTMODEL_NPARAMS  8



//  function prototypes:


extern  void  mfit_interface ();

//   The Fortran routine "mfit_FinePhotModel", accessed via the Fortran iso_c_binding -- see mfit.f95:
extern void mfit_FinePhotModel  (/* add prototype! */);


//  "static" to be local to this file.

static  unsigned int  CMaxVal_of_vector (

   IDL_LONG * vector_ptr,
   IDL_LONG * vec_len_ptr

);



/* Handy macros */


#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))


typedef float IDL_FLOAT;


//  Step 1: setup macro symbol FORMAT_IDL_LONG to be used when
//  outputing a variable of type IDL_LONG:

//  The following is based on the logic in IDL's file export.h for
//  defining the #define macro symbol IDL_LONG.
//  That file creates the macro symbol IDL_SIZEOF_C_LONG to indicate
//  the number of bytes of a C-language long int, i.e., to identify
//  whether the machine has a 32 or 64-bit word.
//  In all cases IDL_LONG is 4-bytes -- on a 32-bit machine it is
//  typedef'd by IDL in export.h as "long int" and this code defines
//  the (correct) format specifier as "ld", while on a 64-bit machine
//  IDL_LONG is typedef's by IDL in export.h as "int", so this
//  code defines the format specifier as "d".

#if IDL_SIZEOF_C_LONG == 8
   // in this case IDL_LONG is typedef'd to be "int", so we want
   // the format specifier "d":
   #define FORMAT_IDL_LONG  "d"
#elif IDL_SIZEOF_C_LONG == 4
   // in this case IDL_LONG is typedef'd to be "long" [int], so we want
   // the format specifier "ld":
   #define FORMAT_IDL_LONG  "ld"
#else
   #warning "unexpected value of IDL_SIZEOF_C_LONG"
   // IDL_SIZEOF_C_LONG is undefined, give a default value to the
   // format specifier -- it will probably work, but may generate
   // compiler warnings:
   #define FORMAT_IDL_LONG  "ld"
#endif


//  Step 2: setup macro symbol FORMAT_IDL_MEMINT to be used when
//  outputing a variable of type IDL_MEMINT:

#if IDL_SIZEOF_C_PTR == 8
   // for 64-bit machines, IDL_MEMINT is typedef'd to be  IDL_LONG64
   #define FORMAT_IDL_MEMINT "lld"
#elif IDL_SIZEOF_C_PTR == 4
   // for 32-bit machines, IDL_MEMINT is typedef'd to be IDL_LONG
   // so we have to go through the logic of what IDL_LONG means.....
   // see above....
   #if IDL_SIZEOF_C_LONG == 8
      #define FORMAT_IDL_MEMINT "lld"
   #elif IDL_SIZEOF_C_LONG == 4
      #define FORMAT_IDL_MEMINT "ld"
   #else
      #define FORMAT_IDL_MEMINT "ld"
   #endif
#else
   #warning "unexpected value of IDL_SIZEOF_C_PTR "
   // IDL_SIZEOF_C_PTR is undefined, give a default value to the
   // format specifier -- it will probably work, but may generate
   // compiler warnings:
   #define FORMAT_IDL_MEMINT "lld"
#endif



//  *****************************************************************************


//  ***  Implementation of the MFIT IDL procedure:

static  void  mfit  ( int argc, IDL_VPTR *argv ) {

   static _Bool  FirstCall = true;      // static for value that persists across calls

   unsigned long int i;

   IDL_MEMINT num_elem [MFIT_INTERFACE_NPARAMS];

   IDL_LONG  *ptr_fit_mode;
   IDL_LONG  *ptr_num_det;
   IDL_LONG  *ptr_num_chan;
   IDL_LONG  *ptr_num_ebins;
   IDL_FLOAT *ptr_chan_energy;
   IDL_FLOAT *ptr_chan_width;
   IDL_FLOAT *ptr_obs_crate;
   IDL_FLOAT *ptr_back_crate;
   IDL_FLOAT *ptr_back_csig;
   IDL_FLOAT *ptr_live_time;
   IDL_FLOAT *ptr_phot_energy;
   IDL_FLOAT *ptr_phot_width;
   IDL_FLOAT *ptr_drm;
   IDL_FLOAT *ptr_res_frac_at511;
   IDL_FLOAT *ptr_res_exp;
   IDL_LONG  *ptr_param_vary;
   IDL_LONG  *ptr_fit_chan;
   IDL_LONG  *ptr_use_det;
   IDL_LONG  *ptr_term_used;
   IDL_LONG  *ptr_num_terms_avail;
   IDL_LONG  *ptr_num_add_terms;
   IDL_LONG  *ptr_nt_add_line;
   IDL_LONG  *ptr_num_param_of_term;
   IDL_FLOAT *ptr_rel_change_limit;
   IDL_FLOAT *ptr_abs_change_limit;
   IDL_FLOAT *ptr_rel_converg;
   IDL_FLOAT *ptr_abs_converg;
   IDL_LONG  *ptr_max_tries;
   IDL_LONG  *ptr_enable_undetermine;
   IDL_LONG  *ptr_undetermine_priority;
   IDL_FLOAT *ptr_undetermine_rel_req;
   IDL_FLOAT *ptr_undetermine_abs_req;
   IDL_LONG  *ptr_highest_ud_priority;
   IDL_LONG  *ptr_chan_offset;
   IDL_FLOAT *ptr_param;
   IDL_LONG  *ptr_fit_err;
   IDL_FLOAT *ptr_model_chisq;
   IDL_LONG  *ptr_dof;
   IDL_LONG  *ptr_num_vary;
   IDL_FLOAT *ptr_param_uncer;
   IDL_FLOAT *ptr_alpha;
   IDL_FLOAT *ptr_covar;
   IDL_FLOAT *ptr_model_cnt_rate;
   IDL_FLOAT *ptr_phot_obs_rate;
   IDL_FLOAT *ptr_phot_obs_sig;
   IDL_FLOAT *ptr_nu_f_nu_data;
   IDL_FLOAT *ptr_nu_f_nu_sig;
   IDL_FLOAT *ptr_nu_f_nu_model;
   IDL_FLOAT *ptr_model_energy;
   IDL_FLOAT *ptr_model_phot_rate;
   IDL_FLOAT *ptr_model_phot_rate_bychan;
   IDL_FLOAT *ptr_model_cr_vari;
   IDL_FLOAT *ptr_data_cr_vari;
   IDL_STRING *ptr_mfit_version_IDL_String;
   
   IDL_LONG  CalcMaxChan;
   IDL_LONG  CalcMaxEbins;


   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


   //  Print a startup message -- but only print if the compiler-line "D" option 
   //  MFIT_DEBUG is non-zero (undefined MFIT_DEBUG will convert to 0, which is
   //  what we want), and if the Operating System has Terminal Output capability.
   //  Also only print on the first call to this routine.

   #if MFIT_DEBUG != 0
      #ifdef IDL_OS_HAS_TTYS
         #if IDL_SIZEOF_C_LONG == 8
            if (FirstCall)  printf ( "\n\n *** mfit_idl checking arguments in a 64-bit executable.\n\n" );
         #elif IDL_SIZEOF_C_LONG == 4
            if (FirstCall)  printf ( "\n\n *** mfit_idl checking arguments in a 32-bit executable.\n\n" );
         #else
            if (FirstCall)  printf ( "\n\n *** mfit_idl checking arguments -- unidentified word length.\n\n" );
         #endif
      #endif   //  OS has terminal output ?
   #endif   // MFIT_DEBUG non-zero?

   FirstCall = false;



   //  Obtain C-pointers for each of the variables, using IDL_VarGetData
   //  to extract the addresses from the IDL structures:
   //  Also, where possible, check that the arrays as provided by IDL
   //  have the expected number of elements:
   //  (One or two other checks of the values of scalers are also made.)

   i = 0;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_fit_mode,               TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( *ptr_fit_mode < 1  ||  *ptr_fit_mode > 3 )
         printf ( "\nmfit_idl: unexpected fit_mode: %"FORMAT_IDL_LONG"\n", *ptr_fit_mode );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_num_det,                TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_num_chan,               TRUE );
   
   // From the 1-D array "num_chan", obtain the maximum number of channels over the detectors:
   CalcMaxChan = CMaxVal_of_vector  ( ptr_num_chan, ptr_num_det );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_num_ebins,              TRUE );
   
   // From the 1-D array "num_ebins", obtain the maximum number of Ebins over the detectors:
   CalcMaxEbins = CMaxVal_of_vector  ( ptr_num_ebins, ptr_num_det );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_chan_energy,            TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: chan_energy size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_chan_width,             TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: chan_width size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_obs_crate,              TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: obs_crate size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_back_crate,             TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: back_crate size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_back_csig,              TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: back_csig size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_live_time,               TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: live_time size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_phot_energy,             TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxEbins * *ptr_num_det )
         printf ( "\nmfit_idl: phot_energy size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_phot_width,              TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxEbins * *ptr_num_det )
         printf ( "\nmfit_idl: phot_width size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_drm,                     TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxEbins * CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: drm size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_res_frac_at511,          TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != *ptr_num_det )
         printf ( "\nmfit_idl: res_frac_at511 size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_res_exp,                 TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != *ptr_num_det )
         printf ( "\nmfit_idl: res_exp size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_param_vary,              TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS * MAX_PPT )
         printf ( "\nmfit_idl: param_vary size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_fit_chan,                TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != 2 * *ptr_num_det )
         printf ( "\nmfit_idl: fit_chan size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_use_det,                 TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != *ptr_num_det )
         printf ( "\nmfit_idl: use_det size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_term_used,               TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS+1 )
         printf ( "\nmfit_idl: term_used size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_num_terms_avail,         TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_num_add_terms,           TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_nt_add_line,             TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_num_param_of_term,       TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS )
         printf ( "\nmfit_idl: num_param_of_term size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_rel_change_limit,        TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS * MAX_PPT )
         printf ( "\nmfit_idl: rel_change_limit size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_abs_change_limit,        TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS * MAX_PPT )
         printf ( "\nmfit_idl: abs_change_limit size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_rel_converg,             TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_abs_converg,             TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_max_tries,               TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_enable_undetermine,      TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_undetermine_priority,    TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS * MAX_PPT )
         printf ( "\nmfit_idl: undetermine_priority size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_undetermine_rel_req,     TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS * MAX_PPT )
         printf ( "\nmfit_idl: undetermine_rel_req size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_undetermine_abs_req,     TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS * MAX_PPT )
         printf ( "\nmfit_idl: undetermine_abs_req size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_highest_ud_priority,     TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_chan_offset,             TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != *ptr_num_det )
         printf ( "\nmfit_idl: chan_offset size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_param,                   TRUE );
   #ifdef IDL_OS_HAS_TTYS
       if ( num_elem[i] != MAX_TERMS * MAX_PPT )
          printf ( "\nmfit_idl: param size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_fit_err,                 TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_model_chisq,             TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_dof,                     TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_num_vary,                TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_param_uncer,             TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS * MAX_PPT )
         printf ( "\nmfit_idl: param_uncer size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_alpha,                   TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_PARAM * MAX_PARAM )
         printf ( "\nmfit_idl: alpha size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_covar,                   TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_PARAM * MAX_PARAM )
         printf ( "\nmfit_idl: covar size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_model_cnt_rate,          TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * (MAX_TERMS+1) * *ptr_num_det )
         printf ( "\nmfit_idl: model_cnt_rate size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_phot_obs_rate,           TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: phot_obs_rate size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_phot_obs_sig,            TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: phot_obs_sig size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_nu_f_nu_data,            TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: nu_f_nu_data size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_nu_f_nu_sig,             TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: nu_f_nu_sig size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_nu_f_nu_model,           TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != (6 * CalcMaxChan + 1) * *ptr_num_det )
         printf ( "\nmfit_idl: nu_f_nu_model wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_model_energy,            TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != (6 * CalcMaxChan + 1) * *ptr_num_det )
         printf ( "\nmfit_idl: model_energy wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_model_phot_rate,         TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != (6 * CalcMaxChan + 1) * (MAX_TERMS+1) * *ptr_num_det )
         printf ( "\nmfit_idl: model_phot_rate wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_model_phot_rate_bychan,  TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: model_phot_rate_bychan wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_model_cr_vari,           TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: model_cr_vari wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_data_cr_vari,            TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != CalcMaxChan * *ptr_num_det )
         printf ( "\nmfit_idl: data_cr_vari wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif
   
   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_mfit_version_IDL_String, TRUE );


   //  A final check -- of program self-consistency -- the variable "i" has counted the
   //  the number of arguments that we have processed with IDL_VarGetData -- this count
   //  should match MFIT_INTERFACE_NPARAMS:
   
   i++;   //  to change start of count from 0 to 1
   #ifdef IDL_OS_HAS_TTYS
      if ( i != MFIT_INTERFACE_NPARAMS )
         printf ( "\nmfit_idl: Expected number of arguments is %d, but processed %ld args!!\n", MFIT_INTERFACE_NPARAMS, i );
   #endif


   // Call the C-routine mfit_interface which is the next stage in calling the Fortran MFIT:

   mfit_interface (
      ptr_fit_mode,
      ptr_num_det,              ptr_num_chan,            ptr_num_ebins,
      ptr_chan_energy,
      ptr_chan_width,           ptr_obs_crate,           ptr_back_crate,
      ptr_back_csig,            ptr_live_time,           ptr_phot_energy,
      ptr_phot_width,           ptr_drm,                 ptr_res_frac_at511,
      ptr_res_exp,              ptr_param_vary,          ptr_fit_chan,
      ptr_use_det,              ptr_term_used,           ptr_num_terms_avail,
      ptr_num_add_terms,        ptr_nt_add_line,         ptr_num_param_of_term,
      ptr_rel_change_limit,     ptr_abs_change_limit,    ptr_rel_converg,
      ptr_abs_converg,          ptr_max_tries,           ptr_enable_undetermine,
      ptr_undetermine_priority, ptr_undetermine_rel_req, ptr_undetermine_abs_req,
      ptr_highest_ud_priority,  ptr_chan_offset,
      ptr_param,                ptr_fit_err,             ptr_model_chisq, 
      ptr_dof,                  ptr_num_vary,            ptr_param_uncer,
      ptr_alpha,                ptr_covar,
      ptr_model_cnt_rate,       ptr_phot_obs_rate,       ptr_phot_obs_sig,
      ptr_nu_f_nu_data,         ptr_nu_f_nu_sig,         ptr_nu_f_nu_model,
      ptr_model_energy,         ptr_model_phot_rate,     ptr_model_phot_rate_bychan,
      ptr_model_cr_vari,        ptr_data_cr_vari,        ptr_mfit_version_IDL_String
   );


   return;
   
   
}    //  mfit ()



//  *****************************************************************************


static  void  mfit_FinePhotModel_C  ( int argc, IDL_VPTR *argv ) {

   static _Bool  FirstCall = true;      // static for value that persists across calls

   unsigned long int i;

   IDL_MEMINT num_elem [MFIT_INTERFACE_NPARAMS];
   
   IDL_LONG  *ptr_NumEnergies;
   IDL_FLOAT *ptr_Energies;
   IDL_FLOAT *ptr_param;
   IDL_LONG  *ptr_term_used;
   IDL_LONG  *ptr_num_terms_avail;
   IDL_LONG  *ptr_num_add_terms;
   IDL_FLOAT *ptr_PhotonFlux;
   IDL_LONG  *ptr_ErrNum;


   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   
   //  Print a startup message for this routine -- but only print if the compiler-line "D"
   //  option MFIT_DEBUG is non-zero (undefined MFIT_DEBUG will convert to 0, which is
   //  what we want), and if the Operating System has Terminal Output capability.
   //  Also only print on the first call to this routine.

   #if MFIT_DEBUG != 0
      #ifdef IDL_OS_HAS_TTYS

         if (FirstCall)  printf ( "\n\n *** mfit_FinePhotModel_C checking arguments.\n\n" );

      #endif   //  OS has terminal output ?
   #endif   // MFIT_DEBUG non-zero?

   FirstCall = false;
   
   
   i = 0;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_NumEnergies,             TRUE );
   
   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_Energies,               TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != *ptr_NumEnergies )
         printf ( "\nmfit_FinePhotModel_C: Energies size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif
   
   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_param,                   TRUE );
   #ifdef IDL_OS_HAS_TTYS
       if ( num_elem[i] != MAX_TERMS * MAX_PPT )
          printf ( "\nmfit_FinePhotModel_C: param size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif
   
   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_term_used,               TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != MAX_TERMS+1 )
         printf ( "\nmfit_FinePhotModel_C: term_used size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_num_terms_avail,         TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_num_add_terms,           TRUE );

   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i],  (char **) &ptr_PhotonFlux,             TRUE );
   #ifdef IDL_OS_HAS_TTYS
      if ( num_elem[i] != *ptr_NumEnergies )
         printf ( "\nmfit_FinePhotModel_C: PhotonFlux size wrong: %"FORMAT_IDL_MEMINT"\n", num_elem[i] );
   #endif
      
   i++;
   IDL_VarGetData  ( argv[i], &num_elem[i], (char **) &ptr_ErrNum,                  TRUE );
   
   
   //  A final check -- of program self-consistency -- the variable "i" has counted the
   //  the number of arguments that we have processed with IDL_VarGetData -- this count
   //  should match MFIT_INTERFACE_NPARAMS:
   
   i++;   //  to change start of count from 0 to 1
   #ifdef IDL_OS_HAS_TTYS
      if ( i != MFIT_FINEPHOTMODEL_NPARAMS )
         printf ( "\nmfit_FinePhotModel_C: Expected number of arguments is %d, but processed %ld args!!\n", MFIT_FINEPHOTMODEL_NPARAMS, i );
   #endif
   
   
   //  Directly call the Fortran routine -- no need for a C interface between this routine and the Fortran routine
   //  because there is no extra work to do.
   
   mfit_FinePhotModel (
      ptr_NumEnergies,  ptr_Energies,  ptr_param,  ptr_term_used,  ptr_num_terms_avail,  ptr_num_add_terms,
      ptr_PhotonFlux,  ptr_ErrNum   
   );   
   
   return;
   
   
}    //  mfit_FinePhotModel_C ()


//  *****************************************************************************


int IDL_Load  ( void ) {

    /*
    ** This table contains information on the functions and procedures
    ** that make up the MFIT DLM. The information contained in this
    ** table must be identical to that contained in mfit.dlm.
    ** See comments at the top of the file for more info.
    */

   //  IDL_FUN_RET should be IDL_SYSRTN_GENERIC according to the IDL documentation,
   //  but that isn't recognized.

   static IDL_SYSFUN_DEF2 procedure_addr_ONE [] = {
        { { (IDL_FUN_RET) mfit }, "MFIT",
      MFIT_INTERFACE_NPARAMS, MFIT_INTERFACE_NPARAMS, 0, 0 }
   };
   
   static IDL_SYSFUN_DEF2 procedure_addr_TWO [] = {
        { { (IDL_FUN_RET) mfit_FinePhotModel_C }, "MFIT_FINEPHOTMODEL_C",
      MFIT_FINEPHOTMODEL_NPARAMS, MFIT_FINEPHOTMODEL_NPARAMS, 0, 0 }
   };



   /*
    ** Register the routines. The routines must be specified exactly the same
    ** as in mfit.dlm, and be in increasing lexical order.
    */


   return 
       IDL_SysRtnAdd  ( procedure_addr_ONE, FALSE, ARRLEN (procedure_addr_ONE) )  &&
       IDL_SysRtnAdd  ( procedure_addr_TWO, FALSE, ARRLEN (procedure_addr_TWO) );


}   //  IDL_Load ()



//  *****************************************************************************


//  Function to find the maximum value in a vector.   Vector is assumed to
//  contain positive integers, so starting with a maximum value of zero will
//  find the true maximum.
//  "static" to be local to this file.

static unsigned int  CMaxVal_of_vector (

   IDL_LONG * vector_ptr,    // pointer to vector of length *vec_len_ptr
   IDL_LONG * vec_len_ptr

) {

   int  i;
   int  MaxValue;
   
   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
   
   
   MaxValue = 0;
   
   for ( i=0; i < *vec_len_ptr;  i++ ) {
   
      if ( vector_ptr [i] > MaxValue )  MaxValue = vector_ptr [i];
   
   }
   
   return MaxValue;
   
}  // CMaxVal_of_vector

