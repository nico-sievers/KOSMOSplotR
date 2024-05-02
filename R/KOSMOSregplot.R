#' @title Function to plot a regression line per treatment group
#'
#' @description Creates a regression plot over the alkalinity gradient with a line per treatment group. It works on an excel datasheet following the common KOSMOS layout, assuming a continuous independent variable and a categorical variable with two factors. The current version is limited to work with the KOSMOS Kiel spring 2024 campaign.
#'
#' @param dataset A data set object following the common KOSMOS layout, i.e. loaded from the standard excel data sheet. If left empty, an example dataset \code{KOSMOStestdata} will be plotted to showcase the function. Check \code{View(KOSMOStestdata)} to compare the required data structure.
#' @param parameter The column name of the response variable to be plotted given as a string. Defaults to the last column in the data table.
#' @param days Data from which day or days should be plotted and included in the regression analysis? If more than one day is selected, a mean value of y across those days is calculated per mesocosm. Supply an integer (\code{7}) or vector containing the first and last day (\code{c(5,9)}). If set to \code{FALSE} (the default), the last sampling day is plotted.
#' @param exclude_meso List one or multiple mesocosm numbers to exclude those from the plot and the regression analysis, i.e. \code{c(1,3,10)}. Consider the implications of an unbalanced design for the linear model!
#' @param ylabel The y-axis label to be printed. Defaults to the same value as \code{parameter}.
#' @param xlabel The x-axis label to be printed. Currently defaults to \code{"Added alkalinity"}.
# @param control A sample that stands out of the experimental design, such as a harbour or fjord sample, and shall be plotted in a separate style. Name the identifier from the "Mesocosm" or "Treat_Meso" column. Defaults to "Fjord"
#' @param startat0 Should the y-axis start at 0? Can be \code{TRUE} (the default) or \code{False}.
#' @param headspace More space needed above the data lines to accommodate the little stats table (see \code{statsblocklocation}), or to include additional features such as labels? \code{headspace} enlarges the y-axis range by the given factor (i.e. \code{0.3}, the dafault) by setting the upper axis limit to \code{130\%} of the original value.
#' @param includeThisInYlimit Set this to any value you want included in the range of the y-axis. If the value anyway falls within the range nothing will change, otherwise the lower or upper end of the Y-axis will be shifted to accommodate it. Can be useful if you wish display certain thresholds or reference values.
#' @param ylimit Set a fixed range for the y-axis following the pattern \code{c("lower end", "upper end")}, i.e. \code{c(1,3)}. This overwrites \code{startat0}, \code{headspace}, and \code{includeThisInYlimit}. If set to \code{FALSE} (the default), the range will be defined based on the range of data values.
#' @param axis.tick,axis.show These options control whether axis ticks and/or labels are displayed. Each can be set to \code{NA} ("show for none"), \code{"x"} ("show for only the x-axis"), \code{"y"} ("show for only the y-axis"), or \code{"xy"} (show for both; the default option). If only \code{axis.tick} is set for an axis the tick marks appear without labels, if both \code{axis.tick} and \code{axis.label} labels are printed next to the ticks.
# @param axis.show \code{(will be made available with the next update)}
#' @param statsblocklocation,daylabellocation These define where in the plot the little stats table and the label for the displayed sampling day(s) are placed so that they don't collide with the data. The values default to \code{topleft} and \code{topright}, and each can be replaced with any of \code{left, right, top, bottom,} a combination of those, or \code{center} (see [graphics::legend()]). Consider using these options in combination with \code{headspace} to create a clean-looking arrangement.
#' @param new.plot If set to \code{FALSE}, the plot will be plotted on-top of an existing, open plot rather than creating a new one. One can use this option to plot data sets on-top of each other, or to set up a plot window independently of the limitations of this function before adding in the data lines. Created for experimentation primarily, his option is rather unstable and prone to unexpected results.
#' @param ... Some arguments of the base-R \code{plot}-function can be passed on, such as setting a title or the aspect ratio, as well as further graphics parameters. Please check [base::plot()] and [graphics::par()].
#'
#' @return Directly outputs the plot, either to the plot window or another graphics device (e.g. a \code{.png} file) if a connection has been opened.
#'
#' @examples
#' KOSMOSregplot()
#'
#' @export
#' @importFrom graphics abline axis legend lines par points strwidth text title
#' @importFrom stats anova lm

# for debugging
#dataset=KOSMOStestdata;parameter=dimnames(dataset)[[2]][ncol(dataset)];days=FALSE;exclude_meso=FALSE;ylabel=parameter;xlabel="default";startat0=TRUE;headspace=0.3;includeThisInYlimit=FALSE;ylimit=FALSE;axis.tick="xy";axis.show="xy";statsblocklocation="topleft";daylabellocation="topright";new.plot=TRUE

KOSMOSregplot=function(dataset=KOSMOStestdata,
                       parameter=dimnames(dataset)[[2]][ncol(dataset)],
                       days=FALSE,exclude_meso=FALSE,
                       ylabel=parameter,xlabel="default",
                       startat0=TRUE,headspace=0.3,includeThisInYlimit=FALSE,
                       ylimit=FALSE,
                       axis.tick="xy",axis.show="xy",
                       statsblocklocation="topleft",daylabellocation="topright",
                       new.plot=TRUE,...){

  dataset=KOSMOSadjustColumnames(dataset)

  dataset$Day=as.integer(gsub("\\D", "", as.character(dataset$Day)))
  dataset=dataset[order(dataset$Day),]

  if(is.logical(days)){
    days=max(dataset$Day)
  } else if(is.numeric(days) & length(days)==2){
    days=days[1]:days[2]
  }

  dataset$Delta_TA=suppressWarnings(as.numeric(dataset$Delta_TA))
  dataset=dataset[(dataset$Day %in% days) & !is.na(dataset$Delta_TA) & dataset$Delta_TA!="NA",c("Mesocosm",KOSMOScurrentCategoricalVar,"Delta_TA","Treat_Meso",parameter)]
  dataset[,KOSMOScurrentCategoricalVar]=as.factor(dataset[[KOSMOScurrentCategoricalVar]])

  if(!is.logical(exclude_meso)){
    dataset=dataset[!((dataset$Mesocosm %in% exclude_meso) | (dataset$Treat_Meso %in% exclude_meso)),-1]
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
    #par(pty = "s")
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
  interactionmodel=lm(dataset[[parameter]]~dataset$Delta_TA*dataset[[KOSMOScurrentCategoricalVar]])

  slopelvlA=interactionmodel$coefficients[["dataset$Delta_TA"]]
  interceptlvlA=interactionmodel$coefficients[["(Intercept)"]]
  abline(interceptlvlA,slopelvlA,col=KOSMOScurrentStatscols[1],lwd=2.5,lty=KOSMOSdesignfeatures[["statslty"]])

  slopelvlB=slopelvlA+interactionmodel$coefficients[[4]]
  interceptlvlB=interceptlvlA+interactionmodel$coefficients[[3]]
  abline(interceptlvlB,slopelvlB,col=KOSMOScurrentStatscols[2],lwd=2.5,lty=KOSMOSdesignfeatures[["statslty"]])

  sim=summary(interactionmodel)
  aimp=anova(interactionmodel)$`Pr(>F)`
  roundto=3
  statsblock=legend(x=statsblocklocation,legend=rep("",4),
                    text.width = strwidth(bquote(paste("Delta TA \u00D7 ",.(KOSMOScurrentCategoricalVar),"    p = 0.000")),cex = 0.75),
                    cex=0.75,bty="n",x.intersp=0)
  text(statsblock$rect$left, statsblock$text$y[c(1,2)], pos=4, cex=0.75,
       c("Delta TA",KOSMOScurrentCategoricalVar))
  text(statsblock$rect$left, statsblock$text$y[3], pos=4, cex=0.75,
       bquote(paste("Delta TA \u00D7 ",.(KOSMOScurrentCategoricalVar))))
  text(statsblock$rect$left, statsblock$text$y[4], pos=4, cex=0.75,
       expression(paste("Adj. ",italic(R)^2)))

  x=statsblock$rect$left+statsblock$rect$w
  y=statsblock$text$y
  text(x-strwidth("0.000"),y,pos=2,cex=0.75,c("p ","p ","p ",""))
  for(i in 1:3){text(x-strwidth(" = 0.000"),y[i],KOSMOSformatPvalues(aimp[i]),pos=4,cex=0.75)}
  text(x-strwidth(" = 0.000"),y[4],paste("= ",format(round(sim$adj.r.squared,roundto),nsmall=3),sep=""),pos=4,cex=0.75)

  if(length(days)>1){
    legendtext=paste("Mean of T",paste(days[c(1,length(days))],collapse="-"),sep="")
    #legendtext=paste("T",paste(days[c(1,length(days))],collapse="-"),sep="")
  }else{
    legendtext=paste("T",days,sep="")
  }
  legend(x=daylabellocation,legend=legendtext,bty="n",x.intersp=0)

  # draw the shapes
  origfont=par("font")
  par("font"=11)

  usedstyles=rep(NA,length(mesos)) # workaround to avoid double-use of style entries
  stylefailcounter=0 # for reporting non-matching styles

  for(meso in mesos){
    data_meso=dataset[dataset$Treat_Meso==meso,]

    # increase the chance of finding the right style info from the template
    tmp=unlist(strsplit(meso, " |-|/"))
    tmp=tmp[tmp!=""]
    tmp=sub("(","\\(",tmp,fixed=T)
    tmp=sub(")","\\)",tmp,fixed=T)
    tmp=paste("(?=.*",tmp,")",sep="",collapse="")

    whichstyle=grep(tmp,KOSMOScurrentStyletable[,"mesolist"],perl=T,ignore.case=T)
    style=KOSMOScurrentStyletable[whichstyle,c("colourlist","ltylist","shapelist")]
    points(mean(na.rm=T,data_meso$Delta_TA),mean(na.rm=T,data_meso[[parameter]]),
           col=style[["colourlist"]],
           bg=style[["colourlist"]],
           pch=style[["shapelist"]],
           cex=1.5)

    # take note if you can't match the style for something
    if(nrow(style)==0){stylefailcounter=stylefailcounter+1}

    # workaround to avoid double-use of style entries
    if(!is.na(usedstyles[whichstyle])){
      stop(paste0("Nico's algorithm accidentally assigns the same style from the template to at least two different mesocosm identifiers, namely '",usedstyles[whichstyle],"' and '",meso,"'. Please ask Nico to have a look at this!"))
    } else {
      usedstyles[whichstyle]=meso
    }
  }
  # report if some style wasn't matched
  if(stylefailcounter>1){warning(paste0(stylefailcounter," mesocosm identifiers in dataset$Treat_Meso (or equivalent) could not be matched to an entry in the style template!\nmake sure the column contains a string of the added alkalinity, the ",KOSMOScurrentCategoricalVar,", and the mesocosm number, in any order, separated by either a whitespace, '-', or '/'."))} else if(stylefailcounter==1){warning("One mesocosm identifier in dataset$Treat_Meso (or equivalent) could not be matched to an entry in the style template!\nThis could be the control data, and if so, can be ignored.")}

  par("font"=origfont)
}

