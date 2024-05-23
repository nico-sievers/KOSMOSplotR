library(testthat)
library(KOSMOSplotR)

try(KOSMOSselect(experiment="kosm"))
try(KOSMOSselect(experiment="bla"))
KOSMOSselect(experiment="helg")
KOSMOSselect()

KOSMOStimeplot()
KOSMOStimeplot(dataset=KOSMOStestdata,
               parameter=dimnames(KOSMOStestdata)[[2]][ncol(KOSMOStestdata)],
               ylabel="Parameter [fantasy unit]",xlabel="Timeline - Experiment day",
               control="Fjord",baseline=FALSE,treatment.abline=TRUE,
               exclude_meso=FALSE,exclude_day=FALSE,
               startat0=TRUE,headspace=0,includeThisInYlimit=FALSE,ylimit=FALSE,
               xlimit=FALSE,
               axis.ticks="xy",axis.values="xy",
               stats.show=FALSE,stats.days=FALSE,stats.exclude_meso=FALSE,
               stats.digits=FALSE,stats.location="bottom",
               stats.meanlabel=c("below","above"),stats.doublespecial=FALSE,
               copepod.draw=FALSE,copepod.position="top",
               new.plot=TRUE)
#KOSMOStimeplot(control=F)
KOSMOStimeplot(exclude_meso=c(1,2),treatment.abline=F,exclude_day=c(3,5),startat0=F,headspace=0.25,includeThisInYlimit=40,axis.ticks="x",axis.values="")
KOSMOStimeplot(xlimit=c(0,5),ylimit=c(c(5,40)))
#KOSMOStimeplot(stats.show=T)
KOSMOStimeplot(stats.show=T,stats.days=c(3,5),stats.exclude_meso=c(1,2),stats.digits=6,stats.location="top",stats.meanlabel=c("below","above"))

KOSMOSregplot()
KOSMOSregplot(dataset=KOSMOStestdata,
              parameter=dimnames(KOSMOStestdata)[[2]][ncol(KOSMOStestdata)],
              days=FALSE,exclude_meso=FALSE,
              ylabel="Parameter [fantasy unit]",xlabel="default",
              startat0=TRUE,headspace=0.3,includeThisInYlimit=FALSE,
              ylimit=FALSE,
              axis.ticks="xy",axis.values="xy",
              statsblocklocation="topleft",daylabellocation="topright",
              new.plot=TRUE)
KOSMOSregplot(days=c(3,4,5),exclude_meso=5,startat0=F,headspace=0.25,includeThisInYlimit=40,axis.ticks="x",axis.values="",statsblocklocation="center",daylabellocation="bottomright")
KOSMOSregplot(ylimit=c(c(5,40)))

