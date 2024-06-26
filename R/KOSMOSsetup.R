#' @title Set up the KOSMOSplotR package to suit your needs
#'
#' @description Select the campaign you are working on to automatically apply the suitable style template.
#'
#' @param experiment Select the experiment that the data to be plotted stems from. This then pre-selects the style template. Choose from "\code{KOSMOS2024Kiel}" (the default), "\code{KOSMOS2023Helgoland}", or "\code{KOSMOS2022Bergen}". Unique sharthands such as "\code{kiel}" will be recognised. Note that you can manually adjust the style template further by overwriting \code{KOSMOScurrentStyletable} in your environment.

#' @return Returns \code{KOSMOScurrentStyletable} as the style template for the selected experiment, \code{KOSMOScurrentStatscols} as the second-darkest colour from each treatment group for ablining the linear model, and \code{KOSMOScurrentCategoricalVar} as the categorical variable studies in the experiment.
#'
#' @seealso To overwrite any of these values manually simply assign a new value to them: \code{KOSMOScurrenStyletable[6,2]="red"}. There are further general design features of the plots (such as colour and symbol of the control) saved in \code{KOSMOSdesignfeatures}, that you can access and overwrite if needed.
#'
#' @examples
#' KOSMOSselect("kiel")
#'
#' KOSMOScurrentStyletable
#' KOSMOScurrentCategoricalVar
#' KOSMOScurrentStatscols
#'
#' @export


KOSMOSselect=function(experiment="kiel"){
  options=c("KOSMOS2024Kiel","KOSMOS2023Helgoland","KOSMOS2022Bergen")
  catvars=c("Mineral","Dilution","Mineral")
  chosen=grepl(experiment,options,ignore.case=T)
  nchosen=sum(chosen)
  if(nchosen>1){
    stop("This shorthand is not unique!")
  } else if(nchosen==1){
    assign("KOSMOScurrentStyletable",get(paste0(options,"Styletable")[chosen]),envir=.GlobalEnv)
    assign("KOSMOScurrentCategoricalVar",catvars[chosen],envir=.GlobalEnv)
    nmesos=nrow(KOSMOScurrentStyletable)
    assign("KOSMOScurrentStatscols",KOSMOScurrentStyletable[c(nmesos/2-1,nmesos-1),"colourlist"],envir=.GlobalEnv)
    message(paste0("Style template '",options[chosen],"' selected."))
  } else {
    stop("No matching style template found!")
  }
}
