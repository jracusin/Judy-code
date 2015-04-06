! #################################################################################################


module MFIT_kinds

   !  Use C-types for any type that is used in the argument list of mfit.
   !  This is because mfit is designed to be called from IDL (=C) using
   !  the iso_c_binding.

   use iso_c_binding, only : c_int32_t, c_float, c_double, c_bool


   integer, parameter :: mfit_integer = c_int32_t

   integer, parameter :: mfit_real = c_float

   integer, parameter :: mfit_DoubleReal = c_double

   integer, parameter :: mfit_logical = c_bool


end module MFIT_kinds


! #################################################################################################
