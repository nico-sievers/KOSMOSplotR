#' @title Function to plot a regression line per treatment group
#'
#' @description Creates a regression plot over the alkalinity gradient with a line per mineral treatment group. It works on an excel datasheet following the common KOSMOS layout, assuming a continuous independent variable and a categorical variable with two factors. The current version is limited to work with the KOSMOS Kiel spring 2024 campaign.
#'
#' @param dataset A data set object following the common KOSMOS layout, i.e. loaded from the standard excel data sheet. If left empty, an example dataset \code{KOSMOStestdata} will be plotted to showcase the function. Check \code{View(KOSMOStestdata)} to compare the required data structure.
#' @param parameter The column name of the response variable to be plotted given as a string. Defaults to the last column in the data table.
#' @param day Data from which day or days should be plotted and included in the regression analysis? If more than one day is selected, a mean value of y across those days is calculated per mesocosm. Supply an integer (\code{7}) or vector (\code{c(5,7,9)}). If set to \code{FALSE} (the default), the last sampling day is plotted.
#' @param ignore List one or multiple mesocosm numbers to exclude those from the plot and the regression analysis, i.e. \code{c(1,3,10)}. Consider the implications of an unbalanced design in linear regression!
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


KOSMOSregplot=function(dataset=KOSMOStestdata,
                       parameter=dimnames(dataset)[[2]][ncol(dataset)],
                       day=FALSE,ignore=FALSE,
                       ylabel=parameter,xlabel="default",
                       startat0=TRUE,headspace=0.3,includeThisInYlimit=FALSE,
                       ylimit=FALSE,
                       axis.tick="xy",axis.show="xy",
                       statsblocklocation="topleft",daylabellocation="topright",
                       new.plot=TRUE,...){

  dataset$Day=as.integer(dataset$Day)
  dataset=dataset[order(dataset$Day),]

  if(is.logical(day)){
    day=max(dataset$Day)
  }

#  dataset=dataset[(dataset$Day %in% day) & !is.na(dataset$Delta_TA),c("Mesocosm","Mineral","Delta_TA","Treat_Meso",parameter)]
  dataset=dataset[(dataset$Day %in% day) & !is.na(dataset$Delta_TA) & dataset$Delta_TA!="NA",c("Mesocosm","Mineral","Delta_TA","Treat_Meso",parameter)]
  dataset$Delta_TA=as.numeric(dataset$Delta_TA)
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
  interactionmodel=lm(dataset[[parameter]]~dataset$Delta_TA*dataset$Mineral)

  slopematch=interactionmodel$coefficients[["dataset$Delta_TA"]]
  interceptmatch=interactionmodel$coefficients[["(Intercept)"]]
  abline(interceptmatch,slopematch,col=KOSMOScurrentStatscols[1],lwd=2.5,lty=KOSMOScurrentStatslty)

  slopemismatch=slopematch+interactionmodel$coefficients[["dataset$Delta_TA:dataset$MineralMg(OH)2"]]
  interceptmismatch=interceptmatch+interactionmodel$coefficients[["dataset$MineralMg(OH)2"]]
  abline(interceptmismatch,slopemismatch,col=KOSMOScurrentStatscols[2],lwd=2.5,lty=KOSMOScurrentStatslty)

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
  for(i in 1:3){text(x-strwidth(" = 0.000"),y[i],KOSMOSformatPvalues(aimp[i]),pos=4,cex=0.75)}
  text(x-strwidth(" = 0.000"),y[4],paste("= ",format(round(sim$adj.r.squared,roundto),nsmall=3),sep=""),pos=4,cex=0.75)

  if(length(day)>1){
    legendtext=paste("Mean of T",paste(day[c(1,length(day))],collapse="-"),sep="")
    #legendtext=paste("T",paste(day[c(1,length(day))],collapse="-"),sep="")
  }else{
    legendtext=paste("T",day,sep="")
  }
  legend(x=daylabellocation,legend=legendtext,bty="n",x.intersp=0)

  par("font"=11)
  for(meso in mesos){
    data_meso=dataset[dataset$Treat_Meso==meso,]
    #data_meso=data_meso[[parameter]]
    style=KOSMOScurrentStyletable[KOSMOScurrentStyletable[,"mesolist"]==meso,c("colourlist","shapelist")]
    points(mean(na.rm=T,data_meso$Delta_TA),mean(na.rm=T,data_meso[[parameter]]),
           col=style[["colourlist"]],
           bg=style[["colourlist"]],
           pch=style[["shapelist"]],
           cex=1.5)
  }
  par("font"=1)
}

