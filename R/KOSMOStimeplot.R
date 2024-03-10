#' @title KOSMOStimeplot
#'
#' @description Creates a timeline plot over the sampled days with a line per mesocosm. It works on an excel datasheet following the common KOSMOS layout, assuming a continuous independent variable and a categorical variable with two factors. The current version is limited to work with the KOSMOS Kiel spring 2024 campaign.
#'
#' @param dataset A data set object following the common KOSMOS layout
#' @param parameter The column name of the parameter to be plotted
#' @param ylabel The y-axis label, defaults to the same value as \code{parameter}
#'
#' @return directly outputs the plot, either to the plot window or another graphics device (e.g. a .png file) if a connection has been made
# @examples
# data(toydata)
# output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
# @importFrom dplyr "%>%"


KOSMOStimeplot=function(dataset,parameter,ylabel=parameter,xlabel="Experiment day",
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