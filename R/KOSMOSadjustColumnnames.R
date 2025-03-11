#' @title Function to adjust data tables in a way that even slightly mismatching column names can be plotted correctly within this package.
#'
#' @description Called by either the user or the plotting functions directly, this tool tries to guess the names of the required columns and thereby trys to make the package work even on data tables that deviate from the official template sheet.
#'
#' @param dataset A data set object roughly following the common KOSMOS layout for which the column names shall be adjusted to fit the template.
#' @param required_columns Vector of those columns to be matched, all others are left untouched. Defaults to all potentially required columns for the available plot functions. This argument is supplied by the plotting function if this function is called automatically, so that only strictly necessary ones are marked as required.
#' @param minimal_requirement Vector of those columns that essential for plotting KOSMOS data and can't be derived from others.
#'
#' @return The same data set with (potentially) modified column names.
#'
# @examples
# KOSMOSadjustColumnnames(KOSMOStestdata)
#' @export
#' @importFrom dplyr select


KOSMOSadjustColumnnames=function(dataset=KOSMOStestdata,required_columns=c("Day","Mesocosm",KOSMOScurrentCategoricalVar,KOSMOScurrentContinuousVar,"Treat_Meso"),minimal_requirement=c("Day","Mesocosm")){

  # run the function that does all of the pattern searches, then work based on its output
  tmp=KOSMOSguessColumnnames(dataset,required_columns,minimal_requirement)

  if(tmp[[1]]=="perfect"){
    message("\n\nAll essential columns were found by names, let's go ahead!")
  } else {
    # if it wasn't a perfect fit, jump into fixing action

    # is there anything to rename?
    if((length(tmp[[3]])+length(tmp[[4]]))>0){
      names(dataset)[names(dataset) %in% tmp[[4]]]=tmp[[3]]

      message("They were interpreted as:")
      message(paste(tmp[[3]],collapse=" | "))
      message("Please check whether this assumption is correct!")
    }

    if(tmp[[1]]=="need addition"){ # if some are missing, they will now be added in based on the style sheet

      # fuck meso wird vorher renamed, also ist dieses ganze mitschleppen des orig namens bullshit
      styletmp=KOSMOScurrentStyletable[c("Mesocosm",tmp[[6]])]
      names(styletmp)[1]="tmp_meso"
      styletmp$tmp_meso=as.character(styletmp$tmp_meso)
      dataset$tmp_meso=#[,tmp[[5]]]=
        as.character(gsub("m","",dataset[[tmp[[5]]]]))
      dataset=merge(styletmp,dataset,all.y=T,by="tmp_meso")
      #dataset=dataset[,-c("tmp_meso_clean")]
      #whereisday=which(names(dataset)=="Day")
      dataset=dataset %>%
        select(Day,Mesocosm,everything()) %>%
        select(-tmp_meso)

      message("\nNot all essential column were found by name, so they were created from the style template and added to your data set:")
      message(paste(tmp[[6]],collapse=" | "))
      message("Please check the treatment assigment!")

    }
    message("\nThese are the updated column names:")
    print(names(dataset))
  }
  message("")
  return(dataset)
}
