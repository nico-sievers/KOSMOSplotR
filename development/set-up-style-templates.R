################
# Kiel spring

mesoarray=paste0("M",c(3,7,11,1,5,9,6,10,4,8,12,2))

minerallist=c(rep("Ca(OH)2",6),rep("Mg(OH)2",6))
TAlist=rep(seq(0,750,150),2)
mesolist=paste0(TAlist," / ",minerallist," / ",mesoarray)

blues=colorRampPalette(c("lightblue1","#000052"))
greens=colorRampPalette(c("lightgreen","#005200"))
colourlist=c(blues(6),greens(6))

ltylist=c(rep("solid",6),rep("dashed",6))
shapelist=rep(c(15,25,23,19,17,42),2)
#par("font"=11) # or 12 apparently

KOSMOS2024KielStyletable=data.frame(cbind(mesolist,colourlist,ltylist,shapelist))
KOSMOS2024KielStyletable[,"shapelist"]=as.numeric(KOSMOS2024KielStyletable[,"shapelist"])

save(KOSMOS2024KielStyletable,file="./data/KOSMOS2024KielStyletable.rda")
rm(mesoarray,minerallist,TAlist,mesolist,blues,greens,colourlist,ltylist,shapelist)


#######################################
### Bergen

mesoarray=paste0("M",c(5,1,9,7,3,6,10,2,4,8))
minerallist=c(rep("Ca",5),rep("Si",5))
TAlist=rep(seq(0,600,150),2)
mesolist=paste0(minerallist," ",TAlist," / ",mesoarray)

colourlist=c("#98c6e4", "#59a5de", "#1f7fd2", "#0f569c", "#053061", "#a6d96a", "#66bd63", "#1a9850", "#006837", "#004625")
shapelist=c(rep(21,5),rep(23,5))
ltylist=c(rep("solid",5),rep("dashed",5))

KOSMOS2022BergenStyletable=data.frame(cbind(mesolist,colourlist,ltylist,shapelist))
KOSMOS2022BergenStyletable[,"shapelist"]=as.numeric(KOSMOS2022BergenStyletable[,"shapelist"])

save(KOSMOS2022BergenStyletable,file="./data/KOSMOS2022BergenStyletable.rda")
rm(mesoarray,minerallist,TAlist,mesolist,colourlist,ltylist,shapelist)


##############################################
# Helgoland

mesoarray=paste0("M",c(6,10,4,8,12,2,9,7,1,3,5,11))
minerallist=c(rep("Del",6),rep("Imm",6))
TAlist=rep(seq(0,1250,250),2)
mesolist=paste0(minerallist," ",TAlist," / ",mesoarray)

colourlist=c("#A6D96A" , "#83DD8F","#1ABD63", "#008E4A", "#007200", "#1B4332","#90E0EF","#00B4D8" ,"#4393C2", "#0077B6", "#0466C8", "#03045E")
shapelist=rep(c(25,23,22,21,24,42),2) # 8 instead?
ltylist=c(rep("dashed",6),rep("solid",6))

KOSMOS2023HelgolandStyletable=data.frame(cbind(mesolist,colourlist,ltylist,shapelist))
KOSMOS2023HelgolandStyletable[,"shapelist"]=as.numeric(KOSMOS2023HelgolandStyletable[,"shapelist"])

save(KOSMOS2023HelgolandStyletable,file="./data/KOSMOS2023HelgolandStyletable.rda")
rm(mesoarray,minerallist,TAlist,mesolist,colourlist,ltylist,shapelist)


###############################################
# current

KOSMOScurrentStyletable=KOSMOS2024KielStyletable
save(KOSMOScurrentStyletable,file="./data/KOSMOScurrentStyletable.rda")


##############################################
# controls and stuff

KOSMOScurrentStatscols=c(KOSMOScurrentStyletable[5,"colourlist"],KOSMOScurrentStyletable[11,"colourlist"])
save(KOSMOScurrentStatscols,file="./data/KOSMOScurrentStatscols.rda")

# KOSMOScurrentStatslty=c(KOSMOScurrentStyletable[5,"ltylist"],KOSMOScurrentStyletable[11,"ltylist"])
# save(KOSMOScurrentStatslty,file="./data/KOSMOScurrentStatslty.rda")
#
# KOSMOScurrentControlcol="darkgrey"
# save(KOSMOScurrentControlcol,file="./data/KOSMOScurrentControlcol.rda")
#
# KOSMOScurrentControllty="dotted"
# save(KOSMOScurrentControllty,file="./data/KOSMOScurrentControllty.rda")



###################################
# and the column name variable
KOSMOScurrentCategoricalVar="Mineral"
save(KOSMOScurrentCategoricalVar,file="./data/KOSMOScurrentCategoricalVar.rda")


###################################
# columnnames table
KOSMOScolumntable=matrix(c("day","Day",
                           "meso","Mesocosm",
                           KOSMOScurrentCategoricalVar,KOSMOScurrentCategoricalVar,
                           "(?=.*d)(?=.*ta)","Delta_TA",
                           "(?=.*treat)(?=.*meso)","Treat_Meso"),
                         ncol=2,byrow=T)
