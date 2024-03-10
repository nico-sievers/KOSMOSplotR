library(testthat)             # load testthat package
library(KOSMOSplotsR)         # load our package


KOSMOStimeplot(KOSMOStestdata,"Parameter")
KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord")
KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord",ignore = 1)
KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord",ylabel = "Parameter [fantasy unit]",xlabel = "Timeline - Experiment day")
#KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord",stats.show = T)
