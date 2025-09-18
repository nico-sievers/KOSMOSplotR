.onAttach=function(libname, pkgname) {
  packageStartupMessage("\nWelcome to KOSMOSplotR!\n")
  KOSMOSselect()
  message("\nUPDATE: Attention dear users! Unfortunately there was an error in my calculation of the linear mixed model that produces the stats information displayed in the plot. All regression plots over more than one sampling day created before update 2.5.0 (28th of January 2025) contain incorrect p and R^2 values and need to be revisited. If the stats argument of the timeline plot was used, this is equally affected. Please ask me for further details!\n")
}
