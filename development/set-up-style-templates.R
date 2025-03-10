### Now the new one for giulia for OAE-PIIP

OAEPIIPStyletable=KOSMOS2024KielStyletable
OAEPIIPStyletable=OAEPIIPStyletable[1:9,]

names(OAEPIIPStyletable)[2:4]=c("Label","Treatments","Replicate")

OAEPIIPStyletable$Mesocosm=paste0("M",1:9)
treatments=rep(c("Control","NonEq","Eq"),each=3)
OAEPIIPStyletable$Label=paste(treatments,1:3,sep="_")
OAEPIIPStyletable$Treatments=treatments
OAEPIIPStyletable$Replicate=rep(1:3,3) #c("A","B","C")

OAEPIIPStyletable
save(OAEPIIPStyletable,file="data/OAEPIIPStyletable.rda")



# here im retrospectively adding in the other columns to be added to the user's data frame

######

#############################
candidate=KOSMOS2024KielStyletable
candidate=KOSMOS2024KielQuartzSideExperimentStyletable
Treat_Meso=candidate$mesolist
tmp=strsplit(Treat_Meso," / ")
tmp1=lapply(tmp,"[",1)
tmp2=lapply(tmp,"[",2)
tmp3=lapply(tmp,"[",3)
Mesocosm=as.numeric(sub(pattern="m",replacement="",tmp3,ignore.case=T))
Mineral=unlist(tmp2)
Delta_TA=as.numeric(tmp1)
tmp0=data.frame(Mesocosm,Mineral,Delta_TA,Treat_Meso,stringsAsFactors=F)
candidate=merge(tmp0,candidate,by.x="Treat_Meso",by.y="mesolist")[,c(2:4,1,5:7)]
candidate
KOSMOS2024KielStyletable=candidate
save(KOSMOS2024KielStyletable,file="./data/KOSMOS2024KielStyletable.rda")
save(KOSMOS2024KielQuartzSideExperimentStyletable,file="./data/KOSMOS2024KielQuartzSideExperimentStyletable.rda")
#############################
candidate=KOSMOS2022BergenStyletable
Treat_Meso=candidate$mesolist
tmp=strsplit(Treat_Meso," ")
tmp1=lapply(tmp,"[",1)
tmp2=lapply(tmp,"[",2)
tmp3=lapply(tmp,"[",4)
Mesocosm=as.numeric(sub(pattern="m",replacement="",tmp3,ignore.case=T))
Mineral=unlist(tmp1)
Delta_TA=as.numeric(tmp2)
tmp0=data.frame(Mesocosm,Mineral,Delta_TA,Treat_Meso,stringsAsFactors=F)
candidate=merge(tmp0,candidate,by.x="Treat_Meso",by.y="mesolist")[,c(2:4,1,5:7)]
candidate
KOSMOS2022BergenStyletable=candidate
save(KOSMOS2022BergenStyletable,file="./data/KOSMOS2022BergenStyletable.rda")
#############################
candidate=KOSMOS2023HelgolandStyletable
Treat_Meso=candidate$mesolist
tmp=strsplit(Treat_Meso," ")
tmp1=lapply(tmp,"[",1)
tmp2=lapply(tmp,"[",2)
tmp3=lapply(tmp,"[",4)
Mesocosm=as.numeric(sub(pattern="m",replacement="",tmp3,ignore.case=T))
Dilution=unlist(tmp1)
Delta_TA=as.numeric(tmp2)
tmp0=data.frame(Mesocosm,Dilution,Delta_TA,Treat_Meso,stringsAsFactors=F)
candidate=merge(tmp0,candidate,by.x="Treat_Meso",by.y="mesolist")[,c(2:4,1,5:7)]
candidate
KOSMOS2023HelgolandStyletable=candidate
save(KOSMOS2023HelgolandStyletable,file="./data/KOSMOS2023HelgolandStyletable.rda")
#############################

######



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



#################################################
# quartz kiel side experiment

#KOSMOS2024KielQuartzSideExperimentStyletable=KOSMOS2024KielQuartzSideExperimentStyletable[1:6,]
#KOSMOS2024KielQuartzSideExperimentStyletable$mesolist=c("350 / SiO2 / M3","140 / SiO2 / M4","0 / SiO2 / M6","70 / SiO2 / M7","280 / SiO2 / M10","210 / SiO2 / M11")
KOSMOS2024KielQuartzSideExperimentStyletable$mesolist=c("45 / SiO2 / M3","18 / SiO2 / M4","0 / SiO2 / M6","9 / SiO2 / M7","36 / SiO2 / M10","27 / SiO2 / M11")
KOSMOS2024KielQuartzSideExperimentStyletable$mesolist=sort(KOSMOS2024KielQuartzSideExperimentStyletable$mesolist)
KOSMOS2024KielQuartzSideExperimentStyletable$mesolist=KOSMOS2024KielQuartzSideExperimentStyletable$mesolist[c(1,6,2:5)]

#col=KOSMOS2024KielQuartzSideExperimentStyletable$colourlist
#barplot(1:8, col = c("saddlebrown", "sienna", "chocolate4", "deeppink4", "brown", "brown4", "rosybrown4", "tan4"))
#col=colorRampPalette(c("moccasin","#E7B690","darkred"));col=col(6)
#col=colorRampPalette(c("moccasin","#D99A7A","deeppink4"));col=col(6)
#plot(1:6,rep(0,6),col=col,bg=col,pch=KOSMOS2024KielQuartzSideExperimentStyletable$shapelist,cex=3)
# colourblind optimised by chatGPT:
col=c("#FFD8A8","#F4A883","#E17671","#CC4C6C","#AD285B","#800037")

KOSMOS2024KielQuartzSideExperimentStyletable$colourlist=col

save(KOSMOS2024KielQuartzSideExperimentStyletable,file="data/KOSMOS2024KielQuartzSideExperimentStyletable.rda")




############################################
# create legend for any style

KOSMOScurrentStyletable=rbind(KOSMOSplotR::KOSMOS2024KielQuartzSideExperimentStyletable,KOSMOSplotR::KOSMOS2024KielQuartzSideExperimentStyletable,KOSMOSplotR::KOSMOS2024KielQuartzSideExperimentStyletable)

data=FlowCytometry[FlowCytometry$Set=="Synechococcus" & FlowCytometry$Day %in% 1:3,1:9]
data=data[order(data$Day,data$Treat_Meso,decreasing=T),]
data$Count=rep(1:6,3)

ggplot(data,aes(Day,Count,col=Treat_Meso,fill=Treat_Meso,shape=Treat_Meso)) +
  geom_point(shape=rev(KOSMOScurrentStyletable$shapelist)) +
  geom_line(linetype=KOSMOScurrentStyletable$ltylist) +
  scale_color_discrete(type=KOSMOScurrentStyletable$colourlist) +
  scale_fill_discrete(type=KOSMOScurrentStyletable$colourlist) +
  theme_minimal() +
  theme(panel.grid=element_blank())
  #scale_shape_discrete(name=NULL,KOSMOScurrentStyletable$shapelist)
KOSMOScurrentStyletable$colourlist
