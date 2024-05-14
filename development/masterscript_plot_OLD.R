#######################################################
#SETUP

#library(readxl)
#library(png)

#to make all plots square (change back when done with these plots)
#par(pty = "s")

#load all the colour and style info
#source(paste0(path,"Scripts/labels+colours.R"))
#source("labels+colours.R")

#load the cute copepod image
#copepic=readPNG("D:/GEOMAR/Masterarbeit/Scripts/copepics/copepod_sharpedge_blue_noBG.png",native = T)


#######################################################


### debugging

#dataset=CTD
#dataset=POM
#dataset=BSi
#dataset=Photophysiology
#dataset=ZoopBiov
#dataset=test
#dataset=FlowCytometry
#dataset=thisset

#parameter="Total biovolume"
#parameter="Chl_A [µg/l]"
#parameter="Concentration"
#parameter="02 sat %"
#parameter="µg N / Litre"
#parameter="Absorption-blank"
#parameter="Fo"
#parameter="pH_Tc"
#parameter="NO3"
#parameter="Concentration [n/Î¼l]"


#ylabel=parameter;xlabel="Experiment day";control=FALSE;ignore=FALSE;baseline=FALSE;ylimit=FALSE;includeThisInYlimit=FALSE;startat0=FALSE;headspace=0;xlimit=FALSE;stats.show=FALSE;stats.days=FALSE;stats.ignore=FALSE;stats.digits=FALSE;stats.location="bottom";copepod.draw=FALSE;copepod.bottom=FALSE;new.plot=TRUE#day=max(dataset$Day);ylimit=FALSE;ignore=FALSE;statsblocklocation="topleft";headspace=0.15;daylabellocation="topright";stats.doublespecial=F;axis.tick="xy";axis.show="xy";treatment.abline=F
#ylabel=parameter;day=FALSE;ignore=FALSE;ylimit=FALSE;includeThisInYlimit=FALSE;startat0=FALSE;xlabel="default";statsblocklocation="topleft";daylabellocation="topright";headspace=0;new.plot=TRUE;axis.tick="xy";axis.show="xy"


#control="Harbour"
#control="Fjord"

#ignore="Fjord"

#meso=mesos[1]


### lineplot ###
# controlcol="darkgrey"
# globalcex=0.8
# plot(1,1,col="white",pch=4)
# abline(h=1,col=controlcol,lwd=1.9*globalcex,lty="longdash")
# lines(0.5:1.5,c(1,1),
#       col=controlcol,
#       lwd=1.5)
# points(1,1,
#          col=controlcol,
#          pch=4,
#          cex=1.5*globalcex)
# abline(h=baselinemean,col=controlcol,lwd=1.9*globalcex,lty="longdash")


# format p values
format_p_values_plot <- function(p_value) {
  if (is.na(p_value)) {
    formatted_value=NaN
  } else if (0.0001 < p_value & p_value < 0.001) {
    closest_larger_decimal <- 10^ceiling(log10(p_value))
    formatted_value=sprintf("< %.3f", closest_larger_decimal)
  } else if (p_value < 0.0001) {
    exponent <- ceiling(log10(p_value))
    formatted_value=bquote(paste("< ",10^.(exponent)))
  } else {
    formatted_value=sprintf("= %.3f", p_value)
  }
  return(formatted_value)
}


#format_p_values_plot(NA)

GC22timeplot=function(dataset,parameter,ylabel=parameter,xlabel="Experiment day",
                      control=FALSE,baseline=FALSE,ignore=FALSE,
                      stats.show=FALSE,stats.days=FALSE,stats.ignore=FALSE,
                      stats.digits=FALSE,stats.location="bottom",
                      stats.meanlabel=c("below","above"),stats.doublespecial=FALSE,
                      xlimit=FALSE,
                      ylimit=FALSE,includeThisInYlimit=FALSE,startat0=FALSE,headspace=0,
                      axis.tick="xy",axis.show="xy",
                      copepod.draw=FALSE,copepod.position="top",treatment.abline=FALSE,
                      new.plot=TRUE,
                      ...){

  dataset=as.data.frame(dataset[,c("Day","Mesocosm","Mineral","Delta_TA","Treat_Meso",parameter)])
  dataset$Day=as.integer(dataset$Day)
  dataset=dataset[order(dataset$Day),]
  #dataset$Mesocosm=as.integer(dataset$Mesocosm)
  #dataset$Mineral=as.factor(dataset$Mineral)


  datasetwithall=dataset
  if(!is.logical(ignore)){
    dataset=dataset[!((dataset$Mesocosm %in% ignore) | (dataset$Treat_Meso %in% ignore) |
                        (dataset$Mesocosm %in% baseline) | (dataset$Treat_Meso %in% baseline)),]
  }

  if(!is.logical(baseline)){
    days=unique(dataset$Day[!(dataset$Treat_Meso %in% baseline | dataset$Mesocosm %in% baseline)])
    #days=days$Day
  }else{
    days=unique(dataset$Day)
  }

  if(is.logical(ylimit) && ylimit==FALSE){
    if(!is.logical(includeThisInYlimit)){
      yrange=c(unlist(dataset[,parameter],use.names = FALSE),includeThisInYlimit)
    } else {
      yrange=dataset[,parameter]
    }
    yrange=yrange[!is.na(yrange)]
    ##ymax=max(yrange)+headspace*(max(yrange)-min(yrange))
    if(startat0){
      ylimit=c(0,max(yrange))
    } else {
      ylimit=c(min(yrange),max(yrange))
    }
    ylimit[2]=ylimit[2]+headspace*(ylimit[2]-ylimit[1])
  }

  if(is.logical(xlimit) && xlimit==FALSE){
    #xlimit=c(1,max(days))
    xlimit=c(min(days),max(days))
  }
  xbuffer=0.2
  xlimit[1]=xlimit[1]-xbuffer
  xlimit[2]=xlimit[2]+xbuffer
  if(stats.show){
    xlimit[2]=xlimit[2]+1
  }
  if(stats.doublespecial){
    xlimit[1]=xlimit[1]-1
  }

  if(new.plot){
    #par(pty = "s")
    plot(x=-1,y=0,
         #xlim=c(min(days),max(days)),
         xlim=xlimit,
         ylim=ylimit,
         xlab="",ylab="",xaxt="n",yaxt="n",...)

    if(grepl("x",axis.tick)){
        xticklabels=F
      if(grepl("x",axis.show)){
        xticklabels=paste("T",days,sep="")
        title(xlab=xlabel, line=2.3)
      }
      axis(1,at=days,labels=xticklabels)
    }
    if(grepl("y",axis.tick)){
      yticklabels=F
      if(grepl("y",axis.show)){
        yticklabels=T
        title(ylab=ylabel, line=2.3)
      }
      axis(2,labels=yticklabels)
    }
  }
  globalcex=par(no.readonly = T)$cex

  if(copepod.draw){
    #calc position of copepod image
    #copewidth=c(1.75,2.75)
    copewidth=c(1.75,NA)
    coperelativewidth=0.15#(2.75-1.75)/(9-1)
    copeangle=-66
    copeanglerad=copeangle*pi/180
    #coperelativewidth=(copewidth[2]-copewidth[1])/(xlimit[2]-xlimit[1])
    copewidth[2]=copewidth[1]+(xlimit[2]-xlimit[1])*coperelativewidth
    coperelativeheight=689/800*coperelativewidth
    #calc how much it needs to be shifted in y-direction because of the rotation
    originangle=atan2(coperelativeheight/2,coperelativewidth/2)
    centredistance=sqrt((coperelativewidth/2)^2+(coperelativeheight/2)^2)
    if(copepod.position=="centre"){
      ycentre=mean(ylimit)
      shiftYby=(coperelativeheight/2+centredistance*-sin(copeanglerad+originangle))*(ylimit[2]-ylimit[1])
      #thats how i did it before: shiftYby=0*abs(copeylim[2]-copeylim[1])
      copelowerheight=ycentre-(coperelativeheight*(ylimit[2]-ylimit[1]))/2+shiftYby
      copeheight=c(copelowerheight,copelowerheight+coperelativeheight*(ylimit[2]-ylimit[1]))
    }else{
      if(copepod.position=="bottom"){
        copeylim=ylimit[c(2,1)]
        negativate=-1
      }else if(copepod.position=="top"){
        copeylim=ylimit
        negativate=1
      }
    shiftYby=(coperelativeheight/2+centredistance*-sin(copeanglerad+originangle))*(copeylim[2]-copeylim[1])*negativate
    #thats how i did it before: shiftYby=0*abs(copeylim[2]-copeylim[1])
    copeouterheight=+0*(copeylim[2]-copeylim[1])+copeylim[2]+shiftYby
    copeinnerheight=copeouterheight-(coperelativeheight*(copeylim[2]-copeylim[1]))
    copeheight=sort(c(copeinnerheight,copeouterheight))#+shiftYby
    }
    #draw it
    rasterImage(copepic,copewidth[1],copeheight[1],copewidth[2],copeheight[2],copeangle)
  }

  #abline the treatment day
  if(treatment.abline){
    ### XXX move this value
    abline(v=2,col=statscols[1],lty="longdash",lwd=0.75)
  }

  #draw the baseline control line
  if(!is.logical(baseline)){
    usr=par("usr")
    clip(usr[1],max(days)+0.3,usr[3],usr[4])
    baselinemean=mean(unlist(datasetwithall[(datasetwithall$Treat_Meso %in% baseline) | (datasetwithall$Mesocosm %in% baseline),parameter]))
    abline(h=baselinemean,col=controlcol,lwd=1.9*globalcex,lty="longdash")
    do.call(clip,as.list(usr))
  }

  #draw lines of control
  if(any(dataset$Treat_Meso==control)){
    ycontrol=dataset[dataset$Treat_Meso==control,]
    lines(days,ycontrol[[parameter]],
          col=controlcol,
          lwd=1.5)
  }
  mesos=unique(dataset$Treat_Meso)
  #draw lines
  for(meso in mesos){
    if(meso==control){}
    else{
      data_meso=dataset[dataset$Treat_Meso==meso,]
      data_meso=data_meso[[parameter]]
      style=styletable[styletable[,"mesolist"]==meso,c("colourlist","ltylist","shapelist")]
      lines(days,data_meso,
            col=style[["colourlist"]],
            lty=style[["ltylist"]],
            lwd=1.5)
    }
  }

  #draw shapes of control
  if(any(dataset$Treat_Meso==control)){
    ycontrol=dataset[dataset$Treat_Meso==control,]
    points(days,ycontrol[[parameter]],
           col=controlcol,
           bg=controlcol,
           pch=4,
           cex=1.5)
  }
  #draw shapes
  for(meso in mesos){
    if(meso==control){}
    else{
      data_meso=dataset[dataset$Treat_Meso==meso,]
      data_meso=data_meso[[parameter]]
      style=styletable[styletable[,"mesolist"]==meso,c("colourlist","ltylist","shapelist")]
      points(days,data_meso,
             col=style[["colourlist"]],
             bg=style[["colourlist"]],
             pch=style[["shapelist"]],
             cex=1.5)
    }
  }

  if(stats.show){
    if(is.logical(stats.days) && stats.days==FALSE){
      statsdays=max(days)
    }else{
      statsdays=stats.days
    }
    #if(stats.doublespecial){
    #  statsdays=min(days)
    #}

    statsdata=dataset[dataset$Day %in% statsdays & dataset$Treat_Meso!=control & !((dataset$Treat_Meso %in% stats.ignore) | (dataset$Mesocosm %in% stats.ignore)),]
    statsmatch=unlist(statsdata[statsdata$Mineral=="match",parameter])
    statsmis=unlist(statsdata[statsdata$Mineral=="mismatch",parameter])
    interactionmodel=lm(statsdata[[parameter]]~statsdata$Delta_TA*statsdata$Mineral)
    aimp=anova(interactionmodel)$`Pr(>F)`
    statsmeans=c(mean(statsmatch,na.rm = T),mean(statsmis,na.rm = T))

    statsx=xlimit[2]-0.35
    if(stats.doublespecial){
      statsx=xlimit[1]+0.35
    }

    #lines(rep(statsx,2),statsmeans)
    statslineoffset=0.35
    lines(c(statsx-statslineoffset,statsx+statslineoffset),rep(statsmeans[1],2),
          col=styletable[nrow(styletable)/2,"colourlist"],
          lwd=4.3*globalcex)
    lines(c(statsx-statslineoffset,statsx+statslineoffset),rep(statsmeans[2],2),
          col=styletable[nrow(styletable),"colourlist"],
          lwd=4.3*globalcex)

    statsmeans=sort(statsmeans)
    roundto=3
    if(is.logical(stats.digits) && stats.digits==FALSE){
      signifdigits=roundto#+1
    }else{
      signifdigits=stats.digits
    }
    nwanteddigits=max(nchar(sapply(strsplit(format(signif(statsmeans,signifdigits)),"\\."), `[`, 2)))
    if(is.na(nwanteddigits)){nwanteddigits=0}
    #nwanteddigits=max(nchar(sub('.*\\.','',format(signif(statsmeans,signifdigits)))))
    statsmeantexts=format(round(statsmeans,digits=nwanteddigits),
                          nsmall=nwanteddigits,trim = T)

    i=stats.meanlabel=="below"
    text(statsx,statsmeans[i],paste("\n",statsmeantexts[i],sep=""),adj=c(0.5,0.6),cex=0.7)
    i=stats.meanlabel=="above"
    text(statsx,statsmeans[i],paste(statsmeantexts[i],"\n",sep=""),adj=c(0.5,0.4),cex=0.7)

    if(length(statsdays)>1){
      #ptext=paste("Mean of T",paste(statsdays[c(1,length(statsdays))],collapse="-"),sep="")
      ptext=paste("T",paste(statsdays[c(1,length(statsdays))],collapse="-"),sep="")
    }else{
      ptext=paste("T",statsdays,sep="")
    }

    p_value=aimp[2]
    if (0.0001 < p_value & p_value < 0.001) {
      closest_larger_decimal <- 10^ceiling(log10(p_value))
      statstext=paste(ptext," Mineral p ",sprintf("< %.3f", closest_larger_decimal),sep="")
    } else if (p_value < 0.0001) {
      exponent <- ceiling(log10(p_value))
      statstext=bquote(paste(.(ptext)," Mineral p < ",10^.(exponent),sep=""))
    } else {
      statstext=paste(ptext," Mineral p ",sprintf("= %.3f", p_value),sep="")
    }

    if(stats.location=="bottom"){
      text(statsx+0,ylimit[1],statstext,adj=c(0,0.5),srt=90,cex=0.7)
    }else if(stats.location=="centre"){
      text(statsx+0,mean(statsmeans),statstext,adj=c(0.5,0.5),srt=90,cex=0.7)
    }else if(stats.location=="top"){
      text(statsx+0,ylimit[2],statstext,adj=c(1,0.5),srt=90,cex=0.7)
    }
  }
}



### regplot ###

GC22regplot=function(dataset,parameter,ylabel=parameter,xlabel="default",
                     day=FALSE,ignore=FALSE,
                     ylimit=FALSE,includeThisInYlimit=FALSE,startat0=FALSE,
                     statsblocklocation="topleft",daylabellocation="topright",
                     headspace=0,axis.tick="xy",axis.show="xy",new.plot=TRUE,...){

  dataset$Day=as.integer(dataset$Day)
  dataset=dataset[order(dataset$Day),]

  if(is.logical(day)){
    day=max(dataset$Day)
  }

  dataset=dataset[(dataset$Day %in% day) & !is.na(dataset$Delta_TA),c("Mesocosm","Mineral","Delta_TA","Treat_Meso",parameter)]
  dataset$Mineral=as.factor(dataset$Mineral)

  if(!is.logical(ignore)){
    dataset=dataset[!((dataset$Mesocosm %in% ignore) | (dataset$Treat_Meso %in% ignore)),-1]
  }

  mesos=unique(dataset$Treat_Meso)
  Delta_TA=unique(dataset$Delta_TA)


  if(is.logical(ylimit) && ylimit==FALSE){
    yrange=NULL
    i=0
    for(meso in mesos){
      i=i+1
      data_meso=dataset[dataset$Treat_Meso==meso,]
      yrange[i]=mean(na.rm=T,data_meso[[parameter]])
    }
    if(!is.logical(includeThisInYlimit)){
      #yrange=c(unlist(dataset[,parameter],use.names = FALSE),includeThisInYlimit)
      yrange=c(yrange,includeThisInYlimit)
    }
    yrange=yrange[!is.na(yrange)]
    ##ymax=max(yrange,na.rm = T)+headspace*(max(yrange,na.rm = T)-min(yrange,na.rm = T))
    if(startat0){
      ylimit=c(0,max(yrange))
    } else {
      ylimit=c(min(yrange),max(yrange))
    }
    ylimit[2]=ylimit[2]+headspace*(ylimit[2]-ylimit[1])
  }

  if(xlabel=="default"){xlabel="Added alkalinity"}
  if(new.plot){
    par(pty = "s")
    plot(x=0,y=0,col="white",
         xlim=c(min(Delta_TA)-0.1,max(Delta_TA)+0.1),
         ylim=ylimit,
         xlab="",ylab="",xaxt="n",yaxt="n",...)
    if(grepl("x",axis.tick)){
      xticklabels=F
      if(grepl("x",axis.show)){
        xticklabels=T
        title(xlab=xlabel, line=2.3)
      }
      axis(1,at=Delta_TA,labels=xticklabels)
    }
    if(grepl("y",axis.tick)){
      yticklabels=F
      if(grepl("y",axis.show)){
        yticklabels=T
        title(ylab=ylabel, line=2.3)
      }
      axis(2,labels=yticklabels)
    }
  }

  #stats from here
  interactionmodel=lm(dataset[[parameter]]~dataset$Delta_TA*dataset$Mineral)

  slopematch=interactionmodel$coefficients[["dataset$Delta_TA"]]
  interceptmatch=interactionmodel$coefficients[["(Intercept)"]]
  abline(interceptmatch,slopematch,col=statscols[1],lwd=2.5,lty=statslty)

  slopemismatch=slopematch+interactionmodel$coefficients[["dataset$Delta_TA:dataset$MineralMg(OH)2"]]
  interceptmismatch=interceptmatch+interactionmodel$coefficients[["dataset$MineralMg(OH)2"]]
  abline(interceptmismatch,slopemismatch,col=statscols[2],lwd=2.5,lty=statslty)

  sim=summary(interactionmodel)
  aimp=anova(interactionmodel)$`Pr(>F)`
  roundto=3
  statsblock=legend(x=statsblocklocation,
                    legend=rep("",4),
                    text.width = strwidth(expression(paste("Delta_TA \u00D7 Mineral    p = 0.000")),cex = 0.75),
                    cex=0.75,bty="n",
                    x.intersp=0)
  text(statsblock$rect$left, statsblock$text$y[1:3], pos=4, cex=0.75,
       c("Delta_TA","Mineral",expression(paste("Delta_TA \u00D7 Mineral"))))
  text(statsblock$rect$left, statsblock$text$y[4], pos=4, cex=0.75,
       expression(paste("Adj. ",italic(R)^2)))

  x=statsblock$rect$left+statsblock$rect$w
  y=statsblock$text$y
  text(x-strwidth("0.000"),y,pos=2,cex=0.75,c("p ","p ","p ",""))
  for(i in 1:3){text(x-strwidth(" = 0.000"),y[i],format_p_values_plot(aimp[i]),pos=4,cex=0.75)}
  text(x-strwidth(" = 0.000"),y[4],paste("= ",format(round(sim$adj.r.squared,roundto),nsmall=3),sep=""),pos=4,cex=0.75)

  if(length(day)>1){
    legendtext=paste("Mean of T",paste(day[c(1,length(day))],collapse="-"),sep="")
    #legendtext=paste("T",paste(day[c(1,length(day))],collapse="-"),sep="")
  }else{
    legendtext=paste("T",day,sep="")
  }
  legend(x=daylabellocation,legend=legendtext,bty="n",x.intersp=0)

  for(meso in mesos){
    data_meso=dataset[dataset$Treat_Meso==meso,]
    #data_meso=data_meso[[parameter]]
    style=styletable[styletable[,"mesolist"]==meso,c("colourlist","shapelist")]
    points(mean(na.rm=T,data_meso$Delta_TA),mean(na.rm=T,data_meso[[parameter]]),
          col=style[["colourlist"]],
          bg=style[["colourlist"]],
          pch=style[["shapelist"]],
          cex=1.5)
  }
}


### Effectsize calculator


# dataset=POM
# dataset=Sediment
# parameter="µg C / Litre"
# parameter="Faecal Pellets [L]"
# ignore="M1"
# baselinedays=head(sort(unique(dataset$Day)),1)
# effectdays=tail(sort(unique(dataset$Day)),1)

GC22effectsize=function(dataset,parameter,baselinedays=head(sort(unique(dataset$Day)),1),
                        effectdays=tail(sort(unique(dataset$Day)),1),ignore=FALSE,show.datasets=F){

  startdata=dataset[((dataset$Day %in% baselinedays) & dataset$Mesocosm!=ignore
                     & dataset$Treat_Meso!=ignore),
                    c("Day","Mesocosm","Mineral","Treat_Meso",parameter)]
  startdata=startdata[order(startdata$Treat_Meso),]
  matchstart=unlist(startdata[startdata$Mineral=="match" & !is.na(startdata$Mineral),parameter])
  misstart=unlist(startdata[startdata$Mineral=="mismatch" & !is.na(startdata$Mineral),parameter])

  enddata=dataset[((dataset$Day %in% effectdays) & dataset$Mesocosm!=ignore
                     & dataset$Treat_Meso!=ignore),
                    c("Mesocosm","Mineral","Treat_Meso",parameter)]
  enddata=enddata[order(enddata$Treat_Meso),]
  matchend=unlist(enddata[enddata$Mineral=="match" & !is.na(enddata$Mineral),parameter])
  misend=unlist(enddata[enddata$Mineral=="mismatch" & !is.na(enddata$Mineral),parameter])

  if(show.datasets){
    print(startdata,n = 100)
    print(enddata,n = 100)
  }

  baselinedaynames=paste("t",paste(baselinedays,collapse="+"),sep="")
  effectdaynames=paste("t",paste(effectdays,collapse="+"),sep="")

  tabledata=signif(c(mean(na.rm=T,matchstart),mean(na.rm=T,misstart),mean(na.rm=T,misstart)/mean(na.rm=T,matchstart),mean(na.rm=T,matchstart)/mean(na.rm=T,misstart),
                     sd(matchstart),sd(misstart),NA,NA,
                     mean(na.rm=T,matchend),mean(na.rm=T,misend),mean(na.rm=T,misend)/mean(na.rm=T,matchend),mean(na.rm=T,matchend)/mean(na.rm=T,misend),
                     sd(matchend),sd(misend),NA,NA,

                     mean(na.rm=T,matchend)/mean(na.rm=T,matchstart),mean(na.rm=T,misend)/mean(na.rm=T,misstart),NA,NA,

                     mean(na.rm=T,matchend/matchstart),mean(na.rm=T,misend/misstart),NA,NA,
                     sd(matchend/matchstart),sd(misend/misstart),NA,NA



                     # NA,
                     #
                     # NA,
                     #
                     # mean(misend)/mean(matchend),#NA,
                     # sd(misstart/matchstart),sd(misend/matchend),sd(misend)/sd(matchend),#NA,
                     # NA,
                     # sd(matchstart/misstart),sd(matchend/misend),NA
                     ),
                   3)
  dimnameslist=list(c("match","mismatch","mis/match","match/mis"),
                    c(paste(baselinedaynames,"mean"),paste(baselinedaynames,"sd"),
                      paste(effectdaynames,"mean"),paste(effectdaynames,"sd"),
                      "effectsize from means","effectsize per meso","sd from the latter")
                    )

  print(parameter)
  matrix(tabledata,nrow=4,dimnames=dimnameslist)
}

GC22dailyaverage=function(dataset,parameter,ignore=FALSE){

  means=c(NULL)
  days=unique(dataset$Day)

  for(day in days){

    daydata=dataset[((dataset$Day==day) & !(dataset$Mesocosm %in% ignore)
                       & !(dataset$Treat_Meso %in% ignore)),
                      c("Day","Mesocosm","Mineral","Treat_Meso",parameter)]
    daydata=daydata[order(daydata$Treat_Meso),]
    match=unlist(daydata[daydata$Mineral=="match" & !is.na(daydata$Mineral),parameter])
    mis=unlist(daydata[daydata$Mineral=="mismatch" & !is.na(daydata$Mineral),parameter])

    means[length(means)+1]=mean(na.rm=T,match)
    means[length(means)+1]=mean(na.rm=T,mis)
    means[length(means)+1]=mean(na.rm=T,mis)/mean(na.rm=T,match)
    means[length(means)+1]=mean(na.rm=T,match)/mean(na.rm=T,mis)
    means[length(means)+1]=sd(na.rm=T,match)
    means[length(means)+1]=sd(na.rm=T,mis)
    means[length(means)+1]=NA
    means[length(means)+1]=NA
  }

  means=signif(means,digits=3)
  print(parameter)
  matrix(data = means,nrow=4,
         dimnames=list(c("match","mismatch","mis/match","match/mis"),
                       paste(paste("t",sort(rep(days,2)),sep=""),rep(c("mean","sd"),length(days)))
                       )
         )
}

