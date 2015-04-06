# This little routine uses the tclreadline package to present a prompt to the user with command recall capability.
# This code is inspired by $HEADAS/../spectral/scripts/xs_tclreadline.tcl

proc interact {} {
   while 1 {
      set cmd [::tclreadline::readline read "Type c to continue> "]
      
      if {$cmd eq "c" } break
      catch {uplevel 1 $cmd} result
      if {[string length $result]} {puts $result}
   }
}

