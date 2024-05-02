#' @title Function to adjust data tables in a way that even slightly mismatching column names can be plotted correctly within this package.
#'
#' @description Called by either the user or the plotting functions directly, this tool tries to guess the names of the required columns and thereby trys to make the package work even on data tables that deviate from the official template sheet.
#'
#' @param dataset A data set object roughly following the common KOSMOS layout for which the column names shall be adjusted to fit the template.
#' @param required_columns Vector of those columns to be matched, all others are left untouched. Defaults to all potentially required columns for the available plot functions. This argument is supplied by the plotting function if this function is called automatically, so that only strictly necessary ones are marked as required.
#'
#' @return The same data set with (potentially) modified column names.
#'
#' @examples
#' KOSMOSadjustColumnames(KOSMOStestdata)
#' @export

KOSMOSadjustColumnames=function(dataset,required_columns=c("Day","Mesocosm",KOSMOScurrentCategoricalVar,"Delta_TA","Treat_Meso")){

  tmp=KOSMOSguessColumnames(dataset,required_columns)
  if(tmp[[1]]!="perfect"){
    if(length(tmp[[2]])==nrow(tmp[[3]])){
      names(dataset)[tmp[[2]]]=tmp[[3]]$Names
      message("They were interpreted as:")
      message(paste(tmp[[3]]$Names,collapse=" | "))
    }
  }
  return(dataset)

}
