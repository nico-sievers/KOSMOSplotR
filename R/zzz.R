.onAttach=function(libname, pkgname) {
  packageStartupMessage("Welcome to KOSMOSplotR!")
  KOSMOSselect("Kiel")
  message("UPDATE: Attention dear users! Unfortunately there was an error in my calculation of the linear mixed model that produces the stats information displayed in the plot. All regression plots over more than one sampling day created before this update contain incorrect p and R^2 values and need to be revisited. If the stats argument of the timeline plot was used, this is equally affected, and the code for that is not fixed yet, so please don't use the option for now. Please ask me for further details!")
}
