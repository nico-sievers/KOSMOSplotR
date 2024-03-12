library(testthat)
library(KOSMOSplotR)

KOSMOStimeplot()
KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord")
KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord",ignore = 1)
KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord",ylabel = "Parameter [fantasy unit]",xlabel = "Timeline - Experiment day")
#KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord",stats.show = T)


KOSMOSregplot()
KOSMOSregplot(day = c(3,4,5))
KOSMOSregplot(ignore = 5)
KOSMOSregplot(daylabellocation = "bottom")
