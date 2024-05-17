#' @title Function to plot a timeline graph across sampling days
#'
#' @description Creates a timeline plot over the sampled days with a line per mesocosm. It works on an excel datasheet following the common KOSMOS layout, assuming a continuous independent variable and a categorical variable with two factors. The current version is limited to work with the KOSMOS Kiel spring 2024 campaign.
#'
#' @param dataset A data set object following the common KOSMOS layout, i.e. loaded from the standard excel data sheet. If left empty, an example dataset \code{KOSMOStestdata} will be plotted to showcase the function. Check \code{View(KOSMOStestdata)} to compare the required data structure.
#' @param parameter The column name of the response variable to be plotted given as a string. Defaults to the last column in the data table.
#' @param ylabel The y-axis label to be printed. Defaults to the same value as \code{parameter}.
#' @param xlabel The x-axis label to be printed. Defaults to \code{"Experiment day"}.
#' @param control A sample that stands out of the experimental design, such as a harbour or fjord sample, and shall be plotted in a separate style. Name the identifier from the "Mesocosm" or "Treat_Meso" column. Defaults to "Fjord".
#' @param baseline \code{(currently unavailable)}
#' @param treatment.abline Should treatment additions be marked with vertical lines? \code{TRUE} or \code{False}. Defaults to \code{TRUE}, which means "yes".
#' @param exclude_meso,exclude_day List one or multiple mesocosm or day numbers, respectively, to exclude those from the plot, i.e. \code{c(1,3,10)}.
#' @param startat0 Should the y-axis start at 0? Can be \code{TRUE} (the default) or \code{False}.
#' @param headspace More space needed above the data lines to include additional features such as labels? \code{headspace} enlarges the y-axis range by the given factor (i.e. \code{0.25}) by setting the upper axis limit to \code{125\%} of the original value. Defaults to \code{0}.
#' @param includeThisInYlimit Set this to any value you want included in the range of the y-axis. If the value anyway falls within the range nothing will change, otherwise the lower or upper end of the Y-axis will be shifted to accommodate it. Can be useful if you wish display certain thresholds or reference values.
#' @param ylimit Set a fixed range for the y-axis following the pattern \code{c("lower end", "upper end")}, i.e. \code{c(1,3)}. This overwrites \code{startat0}, \code{headspace}, and \code{includeThisInYlimit}. If set to \code{FALSE} (the default), the range will be defined based on the range of data values.
#' @param xlimit Set a fixed range for the x-axis following the pattern \code{c("lower end", "upper end")}.  If set to \code{FALSE} (the default), the range will include all sampling days for which there is data in the table.
#' @param axis.tick,axis.show These options control whether axis ticks and/or labels are displayed. Each can be set to \code{NA} ("show for none"), \code{"x"} ("show for only the x-axis"), \code{"y"} ("show for only the y-axis"), or \code{"xy"} (show for both; the default option). If only \code{axis.tick} is set for an axis the tick marks appear without labels, if both \code{axis.tick} and \code{axis.label} labels are printed next to the ticks.
# @param axis.show \code{(will be made available with the next update)}
#' @param stats.show Choose whether a linear model shall be calculated and the mean values and p-value for the categorical variable displayed (\code{TRUE} or \code{TRUE}, the default).
#' @param stats.days Data from which day or days should be included in the stats analysis? If more than one day is selected, a mean value of y across those days is calculated per mesocosm. Supply an integer (\code{7}) or vector containing the first and last day (\code{c(5,9)}). If set to \code{FALSE} (the default), the last sampling day is plotted.
#' @param stats.exclude_meso List one or multiple mesocosms to exclude those from the stats analysis, i.e. \code{c(1,3,10)}. Mesocosms excluded from the plot via \code{exclude_meso} are anyway excluded.
#' @param stats.digits The number of digits of the displayed values.
#' @param stats.location Position of the stats text given as \code{"top"}, \code{"centre"}, or \code{"bottom"} (the default)
#' @param stats.meanlabel Indicate whether the label of a factor level's mean value should be displayed \code{above} or \code{below} the mark, in the format \code{c([lower value],[higher value])}. Defaults to \code{c("below","above")}.
#' @param stats.doublespecial \code{(don}'\code{t ask...)}
#' @param copepod.draw \code{(outdated, unavailable)}
#' @param copepod.position \code{(outdated, unavailable)}
#' @param new.plot If set to \code{FALSE}, the plot will be plotted on-top of an existing, open plot rather than creating a new one. One can use this option to plot data sets on-top of each other, or to set up a plot window independently of the limitations of this function before adding in the data lines. Created for experimentation primarily, his option is rather unstable and prone to unexpected results.
#' @param ... Some arguments of the base-R \code{plot}-function can be passed on, such as setting a title or the aspect ratio, as well as further graphics parameters. Please check [base::plot()] and [graphics::par()].
#'
#' @return Directly outputs the plot, either to the plot window or another graphics device (e.g. a \code{.png} file) if a connection has been opened.
#'
#' @examples
#' KOSMOStimeplot()
#'
#' @export
#' @importFrom graphics abline axis clip lines par points rasterImage text title
#' @importFrom stats anova lm

# for debugging
# dataset=KOSMOStestdata;parameter=dimnames(dataset)[[2]][ncol(dataset)]
# ylabel=parameter;xlabel="Experiment day";control="Fjord";baseline=FALSE;treatment.abline=TRUE;exclude_meso=FALSE;exclude_day=FALSE;startat0=TRUE;headspace=0;includeThisInYlimit=FALSE;ylimit=FALSE;xlimit=FALSE;axis.tick="xy";axis.show="xy";stats.show=FALSE;stats.days=FALSE;stats.exclude_meso=FALSE;stats.digits=FALSE;stats.location="bottom";stats.meanlabel=c("below","above");stats.doublespecial=FALSE;copepod.draw=FALSE;copepod.position="top";new.plot=TRUE



KOSMOStimeplot=function(dataset=KOSMOStestdata,
                        parameter=dimnames(dataset)[[2]][ncol(dataset)],
                        ylabel=parameter,xlabel="Experiment day",
                        control="Fjord",baseline=FALSE,treatment.abline=TRUE,
                        exclude_meso=FALSE,exclude_day=FALSE,
                        startat0=TRUE,headspace=0,includeThisInYlimit=FALSE,ylimit=FALSE,
                        xlimit=FALSE,
                        axis.tick="xy",axis.show="xy",
                        stats.show=FALSE,stats.days=FALSE,stats.exclude_meso=FALSE,
                        stats.digits=FALSE,stats.location="bottom",
                        stats.meanlabel=c("below","above"),stats.doublespecial=FALSE,
                        copepod.draw=FALSE,copepod.position="top",
                        new.plot=TRUE,
                        ...){


  if(stats.show){
    required_columns=c("Day","Mesocosm",KOSMOScurrentCategoricalVar,"Delta_TA","Treat_Meso")
  } else {
    required_columns=c("Day","Mesocosm","Treat_Meso")
  }

  #dataset=as.data.frame(dataset)
  dataset=KOSMOSadjustColumnames(dataset,required_columns)

  dataset=dataset[,c(required_columns,parameter)]
  dataset$Day=as.integer(dataset$Day)
  dataset=dataset[order(dataset$Day),]
  #dataset$Mesocosm=as.integer(dataset$Mesocosm)


  datasetwithall=dataset
#  if(!is.logical(exclude_meso)){
    dataset=dataset[!((dataset$Day %in% exclude_day) | (dataset$Mesocosm %in% exclude_meso) | (dataset$Treat_Meso %in% exclude_meso) | (dataset$Mesocosm %in% baseline) | (dataset$Treat_Meso %in% baseline)),]
#  }

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
    plot(x=-1,y=0,col="white",
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
    # #calc position of copepod image
    # #copewidth=c(1.75,2.75)
    # copewidth=c(1.75,NA)
    # coperelativewidth=0.15#(2.75-1.75)/(9-1)
    # copeangle=-66
    # copeanglerad=copeangle*pi/180
    # #coperelativewidth=(copewidth[2]-copewidth[1])/(xlimit[2]-xlimit[1])
    # copewidth[2]=copewidth[1]+(xlimit[2]-xlimit[1])*coperelativewidth
    # coperelativeheight=689/800*coperelativewidth
    # #calc how much it needs to be shifted in y-direction because of the rotation
    # originangle=atan2(coperelativeheight/2,coperelativewidth/2)
    # centredistance=sqrt((coperelativewidth/2)^2+(coperelativeheight/2)^2)
    # if(copepod.position=="centre"){
    #   ycentre=mean(ylimit)
    #   shiftYby=(coperelativeheight/2+centredistance*-sin(copeanglerad+originangle))*(ylimit[2]-ylimit[1])
    #   #thats how i did it before: shiftYby=0*abs(copeylim[2]-copeylim[1])
    #   copelowerheight=ycentre-(coperelativeheight*(ylimit[2]-ylimit[1]))/2+shiftYby
    #   copeheight=c(copelowerheight,copelowerheight+coperelativeheight*(ylimit[2]-ylimit[1]))
    # }else{
    #   if(copepod.position=="bottom"){
    #     copeylim=ylimit[c(2,1)]
    #     negativate=-1
    #   }else if(copepod.position=="top"){
    #     copeylim=ylimit
    #     negativate=1
    #   }
    #   shiftYby=(coperelativeheight/2+centredistance*-sin(copeanglerad+originangle))*(copeylim[2]-copeylim[1])*negativate
    #   #thats how i did it before: shiftYby=0*abs(copeylim[2]-copeylim[1])
    #   copeouterheight=+0*(copeylim[2]-copeylim[1])+copeylim[2]+shiftYby
    #   copeinnerheight=copeouterheight-(coperelativeheight*(copeylim[2]-copeylim[1]))
    #   copeheight=sort(c(copeinnerheight,copeouterheight))#+shiftYby
    # }
    # #draw it
    # rasterImage(copepic,copewidth[1],copeheight[1],copewidth[2],copeheight[2],copeangle)
  }

  #abline the treatment day
  if(treatment.abline){
    ### XXX move this value
    abline(v=c(4,6),col=KOSMOSdesignfeatures[["treatmentablinecol"]],lty=KOSMOSdesignfeatures[["treatmentablinelty"]],lwd=0.75)
  }

  #draw the baseline control line
  if(!is.logical(baseline)){
    # usr=par("usr")
    # clip(usr[1],max(days)+0.3,usr[3],usr[4])
    # baselinemean=mean(unlist(datasetwithall[(datasetwithall$Treat_Meso %in% baseline) | (datasetwithall$Mesocosm %in% baseline),parameter]))
    # abline(h=baselinemean,col=KOSMOScurrentControlcol,lwd=1.9*globalcex,lty="longdash")
    # do.call(clip,as.list(usr))
  }

  drawcontrol=T
  if(is.logical(control)){
    if(!control){drawcontrol=F}
  }

  #draw lines of control
  if(drawcontrol){
    if(any(dataset$Treat_Meso==control | dataset$Mesocosm==control)){
      foundcontrol=T
      ycontrol=dataset[dataset$Treat_Meso==control | dataset$Mesocosm==control,]
      lines(ycontrol$Day,ycontrol[[parameter]],
            col=KOSMOSdesignfeatures[["controlcol"]],
            lty=KOSMOSdesignfeatures[["controllty"]],
            lwd=1.5)
      if(length(ycontrol$Day)>length(days)){warning("More than one data point per sampling day was plotted for the control!")}
    } else {
      foundcontrol=F
      warning(paste0("No control under the name of '",control,"' found in the data set!\nSet 'control = FALSE' if you don't wish to plot one."))
    }
  }

  mesos=unique(dataset$Treat_Meso[dataset$Treat_Meso!=control & dataset$Mesocosm!=control])

  #draw lines
  usedstyles=rep(NA,length(mesos)) # workaround to avoid double-use of style entries
  stylefailcounter=0 # for reporting non-matching styles
  multipleentriescheck=F # for warning if there is more than one data point per day and meso
  for(meso in mesos){
    data_meso=dataset[dataset$Treat_Meso==meso,]
    data_meso=data_meso[!is.na(data_meso[,parameter]),]
    tmp_days=data_meso$Day
    if(length(tmp_days)>length(days)){multipleentriescheck=T}
    data_meso=data_meso[[parameter]]

    # increase the chance of finding the right style info from the template
    tmp=unlist(strsplit(meso, " |-|/"))
    tmp=tmp[tmp!=""]
    tmp=sub("(","\\(",tmp,fixed=T)
    tmp=sub(")","\\)",tmp,fixed=T)
    tmp=paste("(?=.*",tmp,")",sep="",collapse="")
    whichstyle=grep(tmp,KOSMOScurrentStyletable[,"mesolist"],perl=T,ignore.case=T)
    style=KOSMOScurrentStyletable[whichstyle,c("colourlist","ltylist","shapelist")]
    lines(tmp_days,data_meso,
          col=style[["colourlist"]],
          lty=style[["ltylist"]],
          lwd=1.5)

    # take note if you can't match the style for something
    if(nrow(style)==0){stylefailcounter=stylefailcounter+1} else {
    # workaround to avoid double-use of style entries
    #if(is.numeric(whichstyle)){
      if(!is.na(usedstyles[whichstyle])){
        stop(paste0("Nico's algorithm accidentally assigns the same style from the template to at least two different mesocosm identifiers, namely '",usedstyles[whichstyle],"' and '",meso,"'. Please ask Nico to have a look at this!"))
      } else {
        usedstyles[whichstyle]=meso
      }
    }
  }
  # report if some style wasn't matched
  if(stylefailcounter>1){warning(paste0(stylefailcounter," mesocosm identifiers in dataset$Treat_Meso (or equivalent) could not be matched to an entry in the style template!\nmake sure the column contains a string of the added alkalinity, the ",KOSMOScurrentCategoricalVar,", and the mesocosm number, in any order, separated by either a whitespace, '-', or '/'."))} else if(stylefailcounter==1){warning("One mesocosm identifier in dataset$Treat_Meso (or equivalent) could not be matched to an entry in the style template!\nThis could be the control if it wasn't recognised correctly.")}
  # report if multiple entries per meso and day
  if(multipleentriescheck){warning("More than one data point per sampling day and meso was plotted!")}


  #change font for plotting the symbold only
  origfont=par("font")
  par("font"=11)

  #draw shapes of control
  if(drawcontrol){
    if(foundcontrol){
      points(days,ycontrol[[parameter]],
             col=KOSMOSdesignfeatures[["controlcol"]],
             bg=KOSMOSdesignfeatures[["controlcol"]],
             pch=KOSMOSdesignfeatures[["controlshape"]],
             cex=1.5)
    }
  }
  #draw shapes
  for(meso in mesos){
    if(meso==control){}
    else{
      data_meso=dataset[dataset$Treat_Meso==meso,]
      data_meso=data_meso[!is.na(data_meso[,parameter]),]
      tmp_days=data_meso$Day
      data_meso=data_meso[[parameter]]

      # increase the chance of finding the right style info from the template
      tmp=unlist(strsplit(meso, " |-|/"))
      tmp=tmp[tmp!=""]
      tmp=sub("(","\\(",tmp,fixed=T)
      tmp=sub(")","\\)",tmp,fixed=T)
      tmp=paste("(?=.*",tmp,")",sep="",collapse="")

      style=KOSMOScurrentStyletable[grep(tmp,KOSMOScurrentStyletable[,"mesolist"],perl=T,ignore.case=T),c("colourlist","ltylist","shapelist")]
      points(tmp_days,data_meso,
             col=style[["colourlist"]],
             bg=style[["colourlist"]],
             pch=style[["shapelist"]],
             cex=1.5)
    }
  }

  #change back the font
  par("font"=origfont)

  if(stats.show){
    if(is.logical(stats.days) && stats.days==FALSE){
      statsdays=max(days)
    }else{
      statsdays=stats.days[1]:stats.days[2]
    }
    #if(stats.doublespecial){
    #  statsdays=min(days)
    #}

    statsdata=dataset[dataset$Day %in% statsdays & dataset$Treat_Meso!=control & !((dataset$Treat_Meso %in% stats.exclude_meso) | (dataset$Mesocosm %in% stats.exclude_meso)),]
    statsone=unlist(statsdata[statsdata[[KOSMOScurrentCategoricalVar]]==unique(statsdata[1,KOSMOScurrentCategoricalVar]),parameter])
    statstwo=unlist(statsdata[statsdata[[KOSMOScurrentCategoricalVar]]==unique(statsdata[2,KOSMOScurrentCategoricalVar]),parameter])
    interactionmodel=lm(statsdata[[parameter]]~statsdata$Delta_TA*statsdata[[KOSMOScurrentCategoricalVar]])
    aimp=anova(interactionmodel)$`Pr(>F)`
    statsmeans=c(mean(statsone,na.rm = T),mean(statstwo,na.rm = T))

    statsx=xlimit[2]-0.35
    if(stats.doublespecial){
      statsx=xlimit[1]+0.35
    }

    #lines(rep(statsx,2),statsmeans)
    statslineoffset=0.35
    lines(c(statsx-statslineoffset,statsx+statslineoffset),rep(statsmeans[1],2),
          col=KOSMOScurrentStatscols[1],
          lwd=4.3*globalcex)
    lines(c(statsx-statslineoffset,statsx+statslineoffset),rep(statsmeans[2],2),
          col=KOSMOScurrentStatscols[2],
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
    if(is.na(p_value) | is.nan(p_value)){
    #if(!is.numeric(p_value)){
      statstext=paste(ptext,KOSMOScurrentCategoricalVar,"p is",p_value)
    } else if (0.0001 < p_value & p_value < 0.001) {
      closest_larger_decimal <- 10^ceiling(log10(p_value))
      statstext=paste0(ptext," ",KOSMOScurrentCategoricalVar," p ",sprintf("< %.3f", closest_larger_decimal))
    } else if (p_value < 0.0001) {
      exponent <- ceiling(log10(p_value))
      statstext=bquote(paste(.(ptext)," ",.(KOSMOScurrentCategoricalVar)," p < ",10^.(exponent),sep=""))
    } else {
      statstext=paste0(ptext," ",KOSMOScurrentCategoricalVar," p ",sprintf("= %.3f", p_value))
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
