#########
# setting up

library(KOSMOSplotR)

KOSMOSselect("GC")


load("data_prelim.rda")
View(data_prelim)



#########
# timeline plot

KOSMOStimeplot(data_prelim)
?KOSMOStimeplot

KOSMOStimeplot(dataset=data_prelim, parameter="ChlaProxyRaw",
               ylabel="Chlorophyll A Proxy",
               main="Flow cytometry preview",
               headspace=0.5)

KOSMOStimeplot(dataset=data_prelim, treatmentgroups_sidebyside=T, showControlsBothTimes=F)

svg("ChlaProxy_plot.svg", width=8, height=4.5)
KOSMOStimeplot(data_prelim, ylabel="Chlorophyll A Proxy")
dev.off()

# cut for brevity: subsetting datasets



#########
# data organising

data_organised = KOSMOSadjustColumnnames(data_prelim)
View(data_organised)

write.csv(data_organised, file="data_organised.csv")



#########
# regression plot

load("KOSMOS_2023_Helgoland_sediment-data-combined.rda")
KOSMOSselect("helgo")

KOSMOSregplot(sed, "Total POC [µmol/L]", days=11:15)

KOSMOSregplot(sed, "Total POC [µmol/L]", days=11:15,
              independent="CO2 [µatm]")



#########
# regressions rapid overview

KOSMOSdailyRegressionWrapper("POC_regression_wrapper.svg", sed, "Total POC [µmol/L]")