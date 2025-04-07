#' @title Function to plot a regression line per treatment group
#'
#' @description Creates a regression plot over the alkalinity gradient with a line per treatment group. It works on an excel datasheet roughly following the common KOSMOS layout, assuming a continuous independent variable and a categorical variable with two factors.
#'
#'@details  Warning: This function currently does not warn you should there be more than one data point per mesocosm and day in the table. Best check your data by plotting a \code{KOSMOStimelineplot()} first!
#'
#' @param dataset A data set object following the common KOSMOS layout, i.e. loaded from the standard excel data sheet. If left empty, an example dataset \code{KOSMOStestdata} will be plotted to showcase the function. Check \code{View(KOSMOStestdata)} to compare the required data structure.
#' @param parameter The column name of the response variable to be plotted given as a string. Defaults to the last column in the data table.
#' @param days Data from which day or days should be plotted and included in the regression analysis? If more than one day is selected, a mean value of y across those days is calculated per mesocosm. Supply an integer (\code{7}), or a vector containing the first and last day to be included in the calculation (\code{c(5,9)}). Note that if the vector is longer than two elements, only the first and last element are used and the rest ignored. If set to \code{FALSE} (the default), the very last sampling day in the data set is plotted. This parameter does not allow for example to exclude a specific day within that range - if you wish to do so please use the \code{subset_data} parameter on the \code{Day}-column to remove misfitting data points!
#' @param independent Choose the independent variable over which to calculate and plot the regression. By default this will be \code{KOSMOScurrentContinuousVar}. Either, supply a column name where the variable is stored (an average over time will be created similar to how \code{parameter} is handled; see\code{independent_days}), or supply a speciic value for each mesocosm directly. To do the latter, supply a data frame object of the structure \code{independent=data.frame(Mesocosm=1:12,`Your categorical variable`=c(..., ...))} (Note that the name of the second column will be used for labeling and that the data frame function sometimes alters your given names, so consider setting \code{data.frame(..., check.names = FALSE)}). Name the Mesocosms numerically so that they can be matched to the precessed data!
#' @param independent_days \code{independent_days} Select the range of days over which to average the chosen independent variable by supplying a numeric vector (not a start and end point like for \code{days}). If set to \code{NULL} (the default) it will adopt the value of \code{days}, and it will be ignored if \code{independent} is a data frame of manually supplied values. Note that you hereby can select differing ranges for the independent and dependent variable to be averaged over, and be aware of the implications!
#' @param subset_data Subset the data by including or excluding rows that have given values in a specified column. If set to \code{FALSE} (the default), no sub-setting is performed. To subset the data table by columns \code{'columnA'} and \code{'columnB'}, supply the following syntax: \code{subset_data = list( columnA = c("value1","value2") , columnB = c("value A","value B"))}, where the column name is the name of the element of the list and the element is a vector (or single value) that marks all rows you wish to include. Alternatively, values can be excluded by adding the prefix \code{"not_"} to a column name, such as \code{subset_data = list( not_columnA = c("value1","value2"))}. Here, rows with \code{value1} and \code{value2} are dropped while all other rows remain. Note that \code{exclude_meso} is a more convenient parameter to exclude mesocosms from the plot.
#' @param exclude_meso List one or multiple mesocosm numbers to exclude those from the plot and the regression analysis, i.e. \code{c(1,3,10)}. Consider the implications of an unbalanced design for the linear model!
#' @param ylabel The y-axis label to be printed. Defaults to the same value as \code{parameter}.
#' @param xlabel The x-axis label to be printed. Currently defaults to \code{"Added alkalinity"}.
# @param control A sample that stands out of the experimental design, such as a harbour or fjord sample, and shall be plotted in a separate style. Name the identifier from the "Mesocosm" or "Treat_Meso" column. Defaults to "Fjord"
#' @param startat0 Should the y-axis start at 0? Can be \code{TRUE} or \code{False} (the default), which sets it to the lowest value in the data (which may be negative, therefore consider whether \code{includeThisInYlimit=0} is the more suitable option for you).
#' @param headspace More space needed above the data lines to accommodate the little stats table (see \code{statsblocklocation}), or to include additional features such as labels? \code{headspace} enlarges the y-axis range by the given factor, i.e. \code{0.3}, by setting the upper axis limit to \code{130\%} of the original value. The default value is dependent on the number of columns in the stats info box.
#' @param includeThisInYlimit Set this to any value you want included in the range of the y-axis. If the value anyway falls within the range nothing will change, otherwise the lower or upper end of the Y-axis will be shifted to accommodate it. Can be useful if you wish display certain thresholds or reference values, or make sure that zero is always displayed (the default).
#' @param ylimit Set a fixed range for the y-axis following the pattern \code{c("lower end", "upper end")}, i.e. \code{c(1,3)}. This overwrites \code{startat0}, \code{headspace}, and \code{includeThisInYlimit}. If set to \code{FALSE} (the default), the range will be defined based on the range of data values.
#' @param axis.ticks,axis.values These options control whether axis ticks and/or labels are displayed. Each can be set to \code{NA} ("show for none"), \code{"x"} ("show for only the x-axis"), \code{"y"} ("show for only the y-axis"), or \code{"xy"} (show for both; the default option). If only \code{axis.ticks} is set for an axis the tick marks appear without labels, if both \code{axis.ticks} and \code{axis.label} labels are printed next to the ticks.
# @param axis.values \code{(will be made available with the next update)}
#' @param statsblocklocation,daylabellocation These define where in the plot the little stats table and the label for the displayed sampling day(s) are placed so that they don't collide with the data. The values default to \code{topleft} and \code{topright}, and each can be replaced with any of \code{left, right, top, bottom,} a combination of those, or \code{center} (see [graphics::legend()]). Consider using these options in combination with \code{headspace} to create a clean-looking arrangement.
#' @param new.plot If set to \code{FALSE}, the plot will be plotted on-top of an existing, open plot rather than creating a new one. One can use this option to plot data sets on-top of each other, or to set up a plot window independently of the limitations of this function before adding in the data lines. Created for experimentation primarily, his option is rather unstable and prone to unexpected results.
#' @param ... Some arguments of the base-R \code{plot}-function can be passed on, such as setting a title or the aspect ratio, as well as further graphics parameters. Please check [base::plot()] and [graphics::par()].
#'
#' @return Directly outputs the plot, either to the plot window or another graphics device (e.g. a \code{.png} file) if a connection has been opened.
#'
# @examples
# KOSMOSregplot()
#'
#' @export
#' @importFrom graphics abline axis legend lines par points strwidth text title
#' @importFrom stats anova lm
#' @importFrom dplyr %>% filter group_by summarise

# for debugging
#library(KOSMOSplotR)
#library(dplyr);dataset=KOSMOStestdata
#parameter=names(dataset)[ncol(dataset)];days=FALSE;subset_data=FALSE;exclude_meso=FALSE;ylabel=parameter;xlabel="default";startat0=TRUE;headspace=0.3;includeThisInYlimit=FALSE;ylimit=FALSE;axis.ticks="xy";axis.values="xy";statsblocklocation="topleft";daylabellocation="topright";new.plot=TRUE;independent=KOSMOScurrentContinuousVar;independent_days=NULL

# library(readxl);dataset=read_excel("../../KOSMOS_2024_Kiel_Quartz-experiment_FlowCytometry/KOSMOS_Kiel_2024_Quartz-side-experiment_FlowCytometry_Sievers_R.xlsx",sheet="Main table");parameter="Count"

#library(readxl);dataset=read_excel("../../KOSMOS_2024_autumn_Kiel_FlowCytometry/KOSMOS_Kiel_autumn_2024_FlowCytometry_Sievers_R.xlsx",sheet="Main table");parameter="Count"

#library(readxl);dataset=read_excel("H:/KOSMOS_2024_Kiel_spring_FlowCytometry/KOSMOS_Kiel_spring_2024_FlowCytometry_Sievers_R.xlsx",sheet="Main table");parameter="Count";days=c(9,11)
#subset_data=list(Settings="large",Set="Cryptophytes")

#KOSMOSselect("helgo");load("../../LOCAL KOSMOS_2023_Helgoland_Sediment/Nico analysis/KOSMOS_2023_Helgoland_sediment-data-combined.rda");dataset=sed



KOSMOSregplot=function(dataset=KOSMOStestdata,
                       parameter=names(dataset)[ncol(dataset)],
                       days=FALSE,
                       independent=KOSMOScurrentContinuousVar,independent_days=NULL,
                       subset_data=FALSE,exclude_meso=FALSE,
                       ylabel=parameter,xlabel="default",
                       startat0=FALSE,headspace="default",includeThisInYlimit=0,
                       ylimit=FALSE,
                       axis.ticks="xy",axis.values="xy",
                       statsblocklocation="topleft",daylabellocation="topright",
                       new.plot=TRUE,...){

  # potentially subset the dataset according to the user parameter
  if(is.list(subset_data) & !is.null(subset_data)){
    not_operator="^not_"
    if(!all(sub(not_operator,"",names(subset_data)) %in% names(dataset))){
      stop("Not all column names given to 'subset_data' where found in the data table!")
    }
    # iterate through given column names
    for(i in names(subset_data)){
      # if it starts in a "!", remove those entries rather than keeping them!
      if(grepl(not_operator,i)){
        dataset=dataset[!(dataset[[sub(not_operator,"",i)]] %in% subset_data[[i]]),]
      } else {
        dataset=dataset[dataset[[i]] %in% subset_data[[i]],]
      }
    }
  }

  dataset=KOSMOSadjustColumnnames(dataset)

  dataset$Day=as.integer(gsub("\\D", "", as.character(dataset$Day)))
  #dataset=dataset[order(dataset$Day),]

  if(is.logical(days)){
    days=max(dataset$Day,na.rm=T)
  } else if(is.numeric(days) & length(days)==2){
    days=days[1]:days[2]
  }
  days=sort(days)
  if(is.null(independent_days)){independent_days=days}

  # set the procedure of how to get the independent variable calculated
  calculate_independent=T
  if(is.data.frame(independent) & isTRUE(nrow(independent)>0) & isTRUE(ncol(independent)==2)){
    calculate_independent=F
    independent_days=NULL
    independent_name=names(independent)[2]
    dataset=merge(dataset,independent,all=T)
  } else if(is.character(independent) & independent %in% names(dataset)){
    independent_name=independent
    independent=NULL
  } else {
    warning("Couldn't process the requested independent variable - Using '",KOSMOScurrentContinuousVar,"' instead!")
    independent_name=KOSMOScurrentContinuousVar
    independent=NULL
  }
  # find the adequate labels
  if(independent_name %in% KOSMOScolumntable$Names){
    independent_shortlabel=KOSMOScolumntable$Shortlabel[KOSMOScolumntable$Names==independent_name]
    independent_longlabel=KOSMOScolumntable$Longlabel[KOSMOScolumntable$Names==independent_name]
  # if(independent_name==KOSMOScurrentContinuousVar){
  #   independent_shortlabel=KOSMOScolumntable$Shortlabel[KOSMOScolumntable$Names==KOSMOScurrentContinuousVar]
  #   independent_longlabel=KOSMOScolumntable$Longlabel[KOSMOScolumntable$Names==KOSMOScurrentContinuousVar]
    } else {
    independent_shortlabel=independent_name
    independent_longlabel=paste0('"',independent_name,'"')#sprintf('"%s"',independent_name)
  }

  dataset[[independent_name]]=suppressWarnings(as.numeric(dataset[[independent_name]]))
  dataset=dataset[!is.na(dataset[[independent_name]]) & dataset[[independent_name]]!="NA" & !is.na(dataset[[parameter]]),c("Day","Mesocosm",KOSMOScurrentCategoricalVar,independent_name,"Treat_Meso",parameter)]
  dataset[,KOSMOScurrentCategoricalVar]=as.factor(dataset[[KOSMOScurrentCategoricalVar]])
  if(!is.logical(exclude_meso)){
    dataset=dataset[!((dataset$Mesocosm %in% exclude_meso) | (dataset$Treat_Meso %in% exclude_meso)),]
  }

  # dataset_alldays=dataset
  # dataset=dataset[(dataset$Day %in% days),]

  # now get averages between the days
  dataset_values=dataset %>%
    filter(Day %in% days) %>%
    group_by(Mesocosm,get(KOSMOScurrentCategoricalVar),Treat_Meso) %>%
    summarise(average=mean(get(parameter)))
  names(dataset_values)[names(dataset_values)=="average"]=parameter
  names(dataset_values)[names(dataset_values)=="get(KOSMOScurrentCategoricalVar)"]=KOSMOScurrentCategoricalVar

  if(calculate_independent){
    dataset_independent=dataset %>%
      filter(Day %in% independent_days) %>%
      group_by(Mesocosm,get(KOSMOScurrentCategoricalVar),Treat_Meso) %>%
      summarise(independent=mean(get(independent_name)))
    names(dataset_independent)[names(dataset_independent)=="independent"]=independent_name
    names(dataset_independent)[names(dataset_independent)=="get(KOSMOScurrentCategoricalVar)"]=KOSMOScurrentCategoricalVar
  } else {
    dataset_independent=independent
  }
  dataset=merge(dataset_values,dataset_independent,all.x=T)

  if(nrow(dataset)==0){stop("There is no data entries to plot. There either is missing values in the first place, or the chosen day range, subsetting, and / or excluded datapoints cause the data frame to end up empty.")
  } else if(nrow(dataset)!=nrow(KOSMOScurrentStyletable)){message("After averaging across sampling days, there is not one data point per mesocosm. Either there is a data point missing or something went wrong in the subsetting of the data set or the averaging calculation! Check the summarised data frame below:")
    print(dataset)
  }

  mesos=unique(dataset$Treat_Meso)
  contvar=unique(dataset[[independent_name]])
  categories=levels(dataset[[KOSMOScurrentCategoricalVar]])

  if(is.logical(ylimit) && ylimit==FALSE){
    yrange=NULL
    i=0
    for(meso in mesos){
      i=i+1
      data_meso=dataset[dataset$Treat_Meso==meso,]
      yrange[i]=mean(na.rm=T,data_meso[[parameter]])
    }
    if(!is.logical(includeThisInYlimit)){
      yrange=c(yrange,includeThisInYlimit)
    }
    yrange=yrange[!is.na(yrange)]
    if(startat0){
      ylimit=c(0,max(yrange))
    } else {
      ylimit=c(min(yrange),max(yrange))
    }
    if(length(categories)==1){headspace=0.15}else{headspace=0.3}
    ylimit[2]=ylimit[2]+headspace*abs(ylimit[2]-ylimit[1])
  }

  if(xlabel=="default"){xlabel=independent_longlabel}
  if(new.plot){
    xenlargment=0.08*abs(max(contvar)-min(contvar))
    plot(x=0,y=0,col="white",
         xlim=c(min(contvar)-xenlargment,max(contvar)+xenlargment),
         ylim=ylimit,
         xlab="",ylab="",xaxt="n",yaxt="n",...)
    if(grepl("x",axis.ticks)){
      xticklabels=F
      if(grepl("x",axis.values)){
        xticklabels=T
        title(xlab=parse(text=xlabel), line=2.3)
      }
      if(independent_name==KOSMOScurrentContinuousVar){
        axis(1,at=contvar,labels=xticklabels)
      } else {
        axis(1,labels=xticklabels)
      }
    }
    if(grepl("y",axis.ticks)){
      yticklabels=F
      if(grepl("y",axis.values)){
        yticklabels=T
        title(ylab=ylabel, line=2.3)
      }
      axis(2,labels=yticklabels)
    }
  }

  #stats from here

  # calculate the models and determine required space based on the number of categories
  if(length(categories)>1){
    interactionmodel=lm(dataset[[parameter]]~dataset[[independent_name]]*dataset[[KOSMOScurrentCategoricalVar]])
    statstablelength=4
    longesttextwidth=strwidth(bquote(paste(.(independent_shortlabel)," \u00D7 ",.(KOSMOScurrentCategoricalVar),"    p = 0.000")),cex = 0.75)
  }else{
    interactionmodel=lm(dataset[[parameter]]~dataset[[independent_name]])
    statstablelength=2
    longesttextwidth=strwidth(bquote(paste(.(independent_shortlabel),"    p = 0.000")),cex = 0.75)
  }

  # draw the lines in the plot
  for(i in 1:length(categories)){
    intercept=sum(interactionmodel$coefficients[(1:i)*2-1])
    slope=sum(interactionmodel$coefficients[(1:i)*2])
    abline(intercept,slope,col=KOSMOScurrentStatscols[i],lwd=2.5,lty=KOSMOSdesignfeatures[["statslty"]])
  }

  sim=summary(interactionmodel)
  aimp=anova(interactionmodel)$`Pr(>F)`
  roundto=3

  # draw an invisible table
  statsblock=legend(x=statsblocklocation,legend=rep("",statstablelength),text.width = longesttextwidth,cex=0.75,bty="n",x.intersp=0)
  x=statsblock$rect$left
  y=statsblock$text$y

  # write the factors from the model
  text(statsblock$rect$left, y[1], pos=4, cex=0.75,independent_shortlabel)
  if(length(categories)>1){
    text(statsblock$rect$left, y[2], pos=4, cex=0.75,KOSMOScurrentCategoricalVar)
    text(statsblock$rect$left, y[3], pos=4, cex=0.75,bquote(paste(.(independent_shortlabel)," \u00D7 ",.(KOSMOScurrentCategoricalVar))))
  }
  text(statsblock$rect$left, y[statstablelength], pos=4, cex=0.75,expression(paste("Adj. ",italic(R)^2)))

  # write "p = " OLD external
  # text(x+statsblock$rect$w-strwidth("0.000"),y[1:(statstablelength-1)],pos=2,cex=0.75,rep("p ",statstablelength-1))

  # write the p-values
  for(i in 1:(statstablelength-1)){
    this_p=aimp[i]
    if(this_p<=0.05){font=2}else{font=1}
    text(x+statsblock$rect$w-strwidth("..MM"),y[i],pos=2,cex=0.75,"p ",font=font)
    text(x+statsblock$rect$w-strwidth("= 0.000"),y[i],KOSMOSformatPvalues(this_p),pos=4,cex=0.75,font=font)
    }

  # write the R-squared
  this_R2=round(sim$adj.r.squared,roundto)
  if(this_R2>=0.5){font=2}else{font=1}
  text(x+statsblock$rect$w-strwidth("= 0.000"),y[statstablelength],paste("= ",format(this_R2,nsmall=3),sep=""),pos=4,cex=0.75,font=font)

  # and note down the day
  if(length(days)>1){
    legendtext=paste("Mean of T",paste(days[c(1,length(days))],collapse="-"),sep="")
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

    whichstyle=grep(tmp,KOSMOScurrentStyletable[,"Treat_Meso"],perl=T,ignore.case=T)
    style=KOSMOScurrentStyletable[whichstyle,c("colourlist","ltylist","shapelist")]
    points(data_meso[[independent_name]],data_meso[[parameter]],
           col=style[["colourlist"]],
           bg=style[["colourlist"]],
           pch=style[["shapelist"]],
           cex=1.5)

    if(length(data_meso[[parameter]])>1){
      warning("More than one data point for ",meso," is in the summary table after averaging across sampling days - someting went quite wrong there!")
    }

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

