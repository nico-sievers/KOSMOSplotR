library(readxl)
library(KOSMOSplotR)

path="../colour schemes/examples/"


KOSMOSselect("ber")
dataset=read_excel(paste0(path,"Data table CC Bergen2022_JS_20231215.xlsx"),sheet="CC_data")
KOSMOSadjustColumnames(dataset)
#dataset=KOSMOSadjustColumnames(dataset)
KOSMOStimeplot(dataset,"TA [umol/Kg]",ylimit=c(2000,2800))
KOSMOStimeplot(dataset,"TA [umol/Kg]",ylimit=c(2000,2800),stats.show=T)
KOSMOSregplot(dataset,"TA [umol/Kg]")

KOSMOSselect("ber")
dataset=read_excel(paste0(path,"unsuitable/Inorg-Nutrients_KOSMOS_Bergen2022_JulianeTammen_20231114.xlsx"),sheet="Mastertable_mean_values")
KOSMOSadjustColumnames(dataset)
KOSMOStimeplot(dataset,"TA [umol/Kg]")

KOSMOSselect("ber")
dataset=read_excel(paste0(path,"HPLC Pigments_KOSMOS_Bergen2022_Kittu_Xin_20240214-SemiFinal.xlsx"),sheet=2)
KOSMOSadjustColumnames(dataset)
names(dataset)[1]="meso"
KOSMOSadjustColumnames(dataset)
#dataset=KOSMOSadjustColumnames(dataset)
KOSMOStimeplot(dataset,"Chl_a")
#KOSMOStimeplot(dataset,"Chl_a",stats.show=T)
#KOSMOSregplot(dataset,"Chl_a")

KOSMOSselect("helg")
dataset=read_excel(paste0(path,"Inorg_Nutrients_RETAKE_Helgoland2023_JulianeTammen_20231101.xlsx"),sheet="mean_QC")
KOSMOSadjustColumnames(dataset)
names(dataset)[3]="dtotalalkalinity"
names(dataset)[4]="dillu"
#names(dataset)[6]="treatmentmesocosm"
dataset$treatmeso=paste(dataset$Mesocosm,dataset$dtotalalkalinity,dataset$dillu)
dataset=dataset[dataset$Sampling_depth=="integrated",]
KOSMOSadjustColumnames(dataset)
KOSMOStimeplot(dataset,"Si_µM")
KOSMOSregplot(dataset,"Si_µM",startat0=F)

KOSMOSselect("kie")
dataset=read_excel(paste0(path,"KOSMOS_Kiel_Fjord_Spring_2024_BSi_updated_25032024.xlsx"),sheet=3)
KOSMOSadjustColumnames(dataset)
KOSMOStimeplot(dataset,"BSi (µmol/L)")
KOSMOSregplot(dataset,"BSi (µmol/L)")

KOSMOSselect("kie")
dataset=read_excel(paste0(path,"KOSMOS_Kiel_spring_2024_data-template_Lana.xlsx"),sheet=2)
KOSMOSadjustColumnames(dataset)
names(dataset)[4]="dtotalalkalinity"
dataset$treatmeso=paste(dataset$Mesocosm,dataset$dtotalalkalinity,dataset$Mineral)
KOSMOSadjustColumnames(dataset)
KOSMOStimeplot(dataset,"GP",control="F")
KOSMOSregplot(dataset,"GP")

KOSMOSselect("kie")
dataset=read_excel(paste0(path,"KOSMOS_Kiel_spring_Chlorophyll_a_25_03_2024_JT .xlsx"),sheet=4)
KOSMOSadjustColumnames(dataset)
KOSMOStimeplot(dataset)
KOSMOSregplot(dataset,startat0=F)

KOSMOSselect("hel")
dataset=read_excel(paste0(path,"POP_RETAKE_Helgoland2023_JulianeTammen_20231101.xlsx"),sheet="QC_data")
names(dataset)[3]="treatmeso"
KOSMOSadjustColumnames(dataset)
#KOSMOStimeplot(dataset,control=F)
#KOSMOSregplot(dataset,startat0=F)
