#' @title Function to plot a timeline graph across sampling days
#'
#' @description Creates a timeline plot over the sampled days with a line per mesocosm. It works on an excel datasheet roughly following the common KOSMOS layout, assuming a continuous independent variable and a categorical variable with two factors.
#'
#' @param dataset A data set object following the common KOSMOS layout, i.e. loaded from the standard excel data sheet. If left empty, an example dataset \code{KOSMOStestdata} will be plotted to showcase the function. Check \code{View(KOSMOStestdata)} to compare the required data structure.
#' @param parameter The column name of the response variable to be plotted given as a string. Defaults to the last column in the data table.
#' @param subset_data Subset the data by including or excluding rows that have given values in a specified column. If set to \code{FALSE} (the default), no sub-setting is performed. To subset the data table by columns \code{'columnA'} and \code{'columnB'}, supply the following syntax: \code{subset_data = list( columnA = c("value1","value2") , columnB = c("value A","value B"))}, where the column name is the name of the element of the list and the element is a vector (or single value) that marks all rows you wish to include. Alternatively, values can be excluded by adding the prefix \code{"not_"} to a column name, such as \code{subset_data = list( not_columnA = c("value1","value2"))}. Here, rows with \code{value1} and \code{value2} are dropped while all other rows remain. Note that \code{exclude_meso} and \code{exclude_day}, as well as \code{xlimit}, are more convenient parameters to exclude sampling days and mesocosms from the plot.
#' @param exclude_meso,exclude_day List one or multiple mesocosm or day numbers, respectively, to exclude those from the plot, i.e. \code{c(1,3,10)}.
#' @param control A sample that stands out of the experimental design, such as a harbour or fjord sample, and shall be plotted in a separate style. Name the identifier from the \code{"Mesocosm"} or \code{"Treat_Meso"} column. Defaults to \code{"Fjord"}.
#' @param treatmentgroups_sidebyside Choose whether the two treatment groups defined by the categorical factor shall be plotted in one graph (\code{"FALSE"}, the default) or side-by-side in separate plots (\code{"TRUE"}). This option is still under development and might cause issues!
#' @param showControlsBothTimes If (\code{treatmentgroups_sidebyside="TRUE"}), by default, those control-mesocosms without added alkalinity are plotted in all panels (\code{"TRUE"}), rather than just with their group (\code{"FALSE"}).
#' @param ylabel The y-axis label to be printed. Defaults to the same value as \code{parameter}.
#' @param xlabel The x-axis label to be printed. Defaults to \code{"Experiment day"}.
#' @param startat0 Should the y-axis start at 0? Can be \code{TRUE} or \code{False} (the default), which sets it to the lowest value in the data (which may be negative, therefore consider whether \code{includeThisInYlimit=0} is the more suitable option for you).
#' @param headspace More space needed above the data lines to include additional features such as labels? \code{headspace} enlarges the y-axis range by the given factor (i.e. \code{0.25}) by setting the upper axis limit to \code{125\%} of the original value. Defaults to \code{0}.
#' @param includeThisInYlimit Set this to any value you want included in the range of the y-axis. If the value anyway falls within the range nothing will change, otherwise the lower or upper end of the Y-axis will be shifted to accommodate it. Can be useful if you wish display certain thresholds or reference values, or make sure that zero is always displayed (the default).
#' @param excludeThisFromYlimit Just opposite to above, here you can exclude some data from the axis limits calculation. For example, if you see a strong outlier that causes the rest of the data to be hard to see, you can list it here by its \code{Mesocosm} or \code{Treat_Meso} identifier (also if it is the control you want to exclude). Different to \code{exclude_meso} or \code{exclude_day}, the data will still be plotted but can exceed the axis boundary, so that it is clear that some data is not shown, rather than hiding it completely.
#' @param ylimit Set a fixed range for the y-axis following the pattern \code{c("lower end", "upper end")}, i.e. \code{c(1,3)}. This overwrites \code{startat0}, \code{headspace}, and \code{includeThisInYlimit}. If set to \code{FALSE} (the default), the range will be defined based on the range of data values.
#' @param xlimit Set a fixed range for the x-axis following the pattern \code{c("lower end", "upper end")}.  If set to \code{FALSE} (the default), the range will include all sampling days for which there is data in the table.
#' @param treatment.abline Should treatment additions be marked with vertical lines? \code{TRUE} (the default) or \code{False}.
#' @param cleaning.abline Should the days of inside-cleaning of the mesocosms be marked with vertical lines? These could for example have impacted sediment parameters. \code{TRUE} or \code{False} (the default).
#' @param axis.ticks,axis.values These options control whether axis ticks and/or labels are displayed. Each can be set to \code{NA} ("show for none"), \code{"x"} ("show for only the x-axis"), \code{"y"} ("show for only the y-axis"), or \code{"xy"} (show for both; the default option). If only \code{axis.ticks} is set for an axis the tick marks appear without labels, if both \code{axis.ticks} and \code{axis.values} labels are printed next to the ticks.
# @param axis.values \code{(will be made available with the next update)}
#' @param stats.show Choose whether a linear model shall be calculated and the mean values and p-value for the categorical variable displayed (\code{TRUE} or \code{TRUE}, the default). Note that this currently only works if the categorical variable has at least two levels!
#' @param stats.days Data from which day or days should be included in the stats analysis? If more than one day is selected, a mean value of y across those days is calculated per mesocosm. Supply an integer (\code{7}) or vector containing the first and last day (\code{c(5,9)}). If set to \code{FALSE} (the default), the last sampling day is plotted.
#' @param stats.exclude_meso List one or multiple mesocosms to exclude those from the stats analysis, i.e. \code{c(1,3,10)}. Mesocosms excluded from the plot via \code{exclude_meso} are anyway excluded.
#' @param stats.digits The number of digits of the displayed values.
#' @param stats.location Position of the stats text given as \code{"top"}, \code{"centre"}, or \code{"bottom"} (the default)
#' @param stats.meanlabel Indicate whether the label of a factor level's mean value should be displayed \code{above} or \code{below} the mark, in the format \code{c([lower value],[higher value])}. Defaults to \code{c("below","above")}.
#' @param stats.doublespecial \code{(don}'\code{t ask...)}
#' @param copepod.draw \code{(outdated, unavailable)}
#' @param copepod.position \code{(outdated, unavailable)}
#' @param new.plot If set to \code{FALSE}, the plot will be plotted on-top of an existing, open plot rather than creating a new one. One can use this option to plot data sets on-top of each other, or to set up a plot window independently of the limitations of this function before adding in the data lines. Created for experimentation primarily, his option is rather unstable and prone to unexpected results.
#' @param baseline \code{(currently unavailable)}
#' @param ... Some arguments of the base-R \code{plot}-function can be passed on, such as setting a title or the aspect ratio, as well as further graphics parameters. Please check [base::plot()] and [graphics::par()].
#'
#' @return Directly outputs the plot, either to the plot window or another graphics device (e.g. a \code{.png} file) if a connection has been opened.
#'
# @examples
# KOSMOStimeplot()
#'
#' @export
#' @importFrom graphics abline axis clip lines par points rasterImage text title
#' @importFrom stats anova lm

# for debugging
# dataset=KOSMOStestdata;parameter=dimnames(dataset)[[2]][ncol(dataset)]
# ylabel=parameter;xlabel="Experiment day";subset_data=FALSE;control="Fjord";baseline=FALSE;treatment.abline=TRUE;exclude_meso=FALSE;exclude_day=FALSE;treatmentgroups_sidebyside=FALSE;startat0=TRUE;headspace=0;includeThisInYlimit=0;excludeThisFromYlimit=F;ylimit=FALSE;xlimit=FALSE;axis.ticks="xy";axis.values="xy";stats.show=FALSE;stats.days=FALSE;stats.exclude_meso=FALSE;stats.digits=FALSE;stats.location="bottom";stats.meanlabel=c("below","above");stats.doublespecial=FALSE;copepod.draw=FALSE;copepod.position="top";new.plot=TRUE;excludeThisFromYlimit;cleaning.abline=F


KOSMOStimeplot=function(dataset=KOSMOStestdata,
                        parameter=tail(names(dataset),1),
                        subset_data=FALSE,exclude_meso=FALSE,exclude_day=FALSE,
                        control="Fjord",
                        treatmentgroups_sidebyside=FALSE,showControlsBothTimes=TRUE,
                        ylabel=parameter,xlabel="Experiment day",
                        startat0=FALSE,headspace=0,includeThisInYlimit=0,excludeThisFromYlimit=FALSE,ylimit=FALSE,
                        xlimit=FALSE,
                        treatment.abline=TRUE,cleaning.abline=FALSE,
                        axis.ticks="xy",axis.values="xy",
                        stats.show=FALSE,stats.days=FALSE,stats.exclude_meso=FALSE,
                        stats.digits=FALSE,stats.location="bottom",
                        stats.meanlabel=c("below","above"),stats.doublespecial=FALSE,
                        copepod.draw=FALSE,copepod.position="top",
                        new.plot=TRUE,baseline=FALSE,
                        ...){

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

  if(stats.show | (treatmentgroups_sidebyside & showControlsBothTimes)){
    required_columns=c("Day","Mesocosm",KOSMOScurrentCategoricalVar,KOSMOScurrentContinuousVar,"Treat_Meso")
  } else if (treatmentgroups_sidebyside){
    required_columns=c("Day","Mesocosm",KOSMOScurrentCategoricalVar,"Treat_Meso")
  } else {
    required_columns=c("Day","Mesocosm","Treat_Meso")
  }

  #dataset=as.data.frame(dataset)
  dataset=KOSMOSadjustColumnnames(dataset,required_columns)

  dataset=dataset[,c(required_columns,parameter)]
  dataset$Day=as.integer(dataset$Day)
  dataset=dataset[order(dataset$Day),]
  #dataset$Mesocosm=as.integer(dataset$Mesocosm)

  # get rid of all excluded mesos and days and the baseline, but keep a backup
  datasetwithall=dataset
  dataset=dataset[!((dataset$Day %in% exclude_day) | (dataset$Mesocosm %in% exclude_meso) | (dataset$Treat_Meso %in% exclude_meso) | (dataset$Mesocosm %in% baseline) | (dataset$Treat_Meso %in% baseline)),]

  # if side-by-side plotting is on
  if(treatmentgroups_sidebyside){
    categories=unique(dataset[[KOSMOScurrentCategoricalVar]])
    categories=categories[!is.na(categories) & categories!="NA" & categories!="" & !is.null(categories)]
    if(length(categories)!=2){warning(paste0("The number of categories found in 'dataset$",KOSMOScurrentCategoricalVar,"' is not two, therefore the algorithm might be confused with splitting the data up into side-by-side panels."))}
    par(mfrow=c(1,length(categories)))
    categories_inverted=rev(categories)
    } else {
      categories=NA
      categories_inverted=NA
    }

  # when assigning days to be plotted kick all empty lines if i.e. future sampling days are already prepared in the sheet
  usedays=dataset[!is.na(dataset[,parameter]),]
  if(!is.logical(baseline)){
    days=unique(usedays$Day[!(usedays$Treat_Meso %in% baseline | usedays$Mesocosm %in% baseline)])
    #days=days$Day
  }else{
    days=unique(usedays$Day)
  }

  if(is.logical(ylimit) && ylimit==FALSE){

    if(!is.logical(excludeThisFromYlimit)){
      yrange=unlist(dataset[(!(dataset$Mesocosm %in% excludeThisFromYlimit) & !(dataset$Treat_Meso %in% excludeThisFromYlimit)),parameter],use.names=F)
    } else {
      yrange=unlist(dataset[,parameter],use.names=F)
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

    ylimit[2]=ylimit[2]+headspace*abs(ylimit[2]-ylimit[1])
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


  ######################## plotting

  # if the user choses to plot side-by-side, the do everything below twice - i think
  categorycounter=0;for(thiscategory in categories){
    categorycounter=categorycounter+1

    # this is only needed so that there is no error in case KOSMOScurrentContinuousVar is not included
    if(treatmentgroups_sidebyside & showControlsBothTimes){
      thisisothercategory=(dataset[[KOSMOScurrentCategoricalVar]]==categories_inverted[categorycounter] & dataset[[KOSMOScurrentContinuousVar]]!=0)
    } else {
      thisisothercategory=(dataset[[KOSMOScurrentCategoricalVar]]==categories_inverted[categorycounter])
    }
#    if(!is.null(thisisothercategory)){
    if(length(thisisothercategory)>0){
      thisisothercategory[is.na(thisisothercategory)]=F
      # after finding those entries definitely belonging to the other category, remove those from the data frame to keep those from the current category plus any control or further extraordinary data.
      thiscategorydataset=dataset[!thisisothercategory,]
    } else {thiscategorydataset=dataset}

    if(new.plot){
      #par(pty = "s")
      plot(x=-1,y=0,col="white",
           #xlim=c(min(days),max(days)),
           xlim=xlimit,
           ylim=ylimit,
           xlab="",ylab="",xaxt="n",yaxt="n",...)
#xlab="",ylab="",xaxt="n",yaxt="n")

      if(grepl("x",axis.ticks)){
        xticklabels=F
        if(grepl("x",axis.values)){
          xticklabels=paste("T",days,sep="")
          title(xlab=xlabel, line=2.3)
        }
        axis(1,at=days,labels=xticklabels)
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
    globalcex=par(no.readonly = T)$cex

    if(treatmentgroups_sidebyside){title(main=thiscategory,cex.main=0.7)}

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

    #abline the treatment days
    if(treatment.abline){
      abline(v=KOSMOScurrentTreatmentSchedule[1,],col=KOSMOScurrentTreatmentSchedule[2,],lty=KOSMOSdesignfeatures[["treatmentablinelty"]],lwd=0.75)
    }
    #abline the cleaning days
    if(cleaning.abline){
      abline(v=KOSMOScurrentCleaningSchedule[1,],col=KOSMOScurrentCleaningSchedule[2,],lty=KOSMOSdesignfeatures[["treatmentablinelty"]],lwd=0.6)
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
        if(length(ycontrol$Day)>length(days)){warning(paste0("More data points than one per sampling day was plotted for the control. Could there be a duplicate entry at ",paste("T",setdiff(unique(ycontrol$Day),days),sep="",collapse=", "),"?"))}
        if(length(unique(ycontrol$Day))<length(days)){warning(paste0("Missing data point(s) in the control: ",paste("T",setdiff(days,unique(ycontrol$Day)),sep="",collapse=", ")))}
      } else {
        foundcontrol=F
        warning(paste0("No control under the name of '",control,"' found in the data set!\nSet 'control = FALSE' if you don't wish to plot one."))
      }
    }

    mesos=unique(thiscategorydataset$Treat_Meso[thiscategorydataset$Treat_Meso!=control & thiscategorydataset$Mesocosm!=control])

    #draw lines
    usedstyles=rep(NA,length(mesos)) # workaround to avoid double-use of style entries
    stylefailcounter=0 # for reporting non-matching styles
    multipleentriescheck=F # for warning if there is more than one data point per day and meso
    missingdatapoints=NULL
    for(meso in mesos){
      data_meso=thiscategorydataset[thiscategorydataset$Treat_Meso==meso,]
      data_meso=data_meso[!is.na(data_meso[,parameter]),]
      tmp_days=data_meso$Day
      if(length(tmp_days)>length(days)){multipleentriescheck=T}
      data_meso=data_meso[[parameter]]
      if(length(unique(tmp_days))<length(days)){missingdatapoints[length(missingdatapoints)+1]=paste0(meso,": ",paste("T",setdiff(days,unique(tmp_days)),sep="",collapse=", "))} # collect missing data points

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
          stop(paste0("Nico's algorithm accidentally assigned the same style from the template to at least two different mesocosm identifiers, namely '",usedstyles[whichstyle],"' and '",meso,"'. Please ask Nico to have a look at this!"))
        } else {
          usedstyles[whichstyle]=meso
        }
      }
    }
    # report if some style wasn't matched
    if(stylefailcounter>1){warning(paste0(stylefailcounter," mesocosm identifiers in dataset$Treat_Meso (or equivalent) could not be matched to an entry in the style template!\nmake sure the column contains a string of the added alkalinity, the ",KOSMOScurrentCategoricalVar,", and the mesocosm number, in any order, separated by either a whitespace, '-', or '/'."))} else if(stylefailcounter==1){warning("One mesocosm identifier in dataset$Treat_Meso (or equivalent) could not be matched to an entry in the style template!\nThis could be the control if it wasn't recognised correctly or if you decided not to plot it.")}
    # report if multiple entries per meso and day
    if(multipleentriescheck){warning("More than one data point per sampling day and meso was plotted! Please check for duplicate entries")}
    # report missing data points
    if(length(missingdatapoints)>0){warning(paste0("Missing data point(s) in the set:\n",paste(missingdatapoints,sep="",collapse="\n")))}



    #change font for plotting the symbol only
    origfont=par("font")
    par("font"=11)

    #draw shapes of control
    if(drawcontrol){
      if(foundcontrol){
        points(ycontrol$Day,ycontrol[[parameter]],
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
        data_meso=thiscategorydataset[thiscategorydataset$Treat_Meso==meso,]
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
      statsone=unlist(statsdata[statsdata[[KOSMOScurrentCategoricalVar]]==unique(statsdata[[KOSMOScurrentCategoricalVar]][1]),parameter])
      statstwo=unlist(statsdata[statsdata[[KOSMOScurrentCategoricalVar]]==unique(statsdata[[KOSMOScurrentCategoricalVar]][2]),parameter])
      interactionmodel=lm(statsdata[[parameter]]~statsdata[[KOSMOScurrentContinuousVar]]*statsdata[[KOSMOScurrentCategoricalVar]])
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


  # if side-by-side plotting is on return the two-plot layout back to normal
  if(treatmentgroups_sidebyside){
    par(mfrow=c(1,1))
  }
}
