#' @title Set up the KOSMOSplotR package to suit your needs
#'
#' @description Select the campaign you are working on to automatically apply the suitable style template.
#'
#' @param experiment Select the experiment that the data to be plotted stems from. This then pre-selects the style template. Choose from "\code{KOSMOS2024Kiel}" (the default), "\code{KOSMOS2023Helgoland}", or "\code{KOSMOS2022Bergen}". Unique sharthands such as "\code{kiel}" will be recognised. Note that you can manually adjust the style template further by overwriting \code{KOSMOScurrentStyletable} in your environment.

#  "\code{KOSMOS2024KielQuartzSideExperiment}",


#' @return Returns \code{KOSMOScurrentStyletable} as the style template for the selected experiment, \code{KOSMOScurrentStatscols} as the second-darkest colour from each treatment group for ablining the linear model, and \code{KOSMOScurrentCategoricalVar} as the categorical variable studies in the experiment.
#'
#' @seealso To overwrite any of these values manually simply assign a new value to them: \code{KOSMOScurrenStyletable[6,2]="red"}. There are further general design features of the plots (such as colour and symbol of the control) saved in \code{KOSMOSdesignfeatures}, that you can access and overwrite if needed.
#'
# @examples
# KOSMOSselect("kiel")
#
# KOSMOScurrentStyletable
# KOSMOScurrentCategoricalVar
# KOSMOScurrentStatscols
#'
#' @export


KOSMOSselect=function(experiment="kiel"){
  options=c("KOSMOS2024KielQuartzSideExperiment","KOSMOS2024Kiel","KOSMOS2023Helgoland","KOSMOS2022Bergen")
  catvars=c("Mineral","Mineral","Dilution","Mineral")
  convars=c("Mass added [mg L-1]","Delta_TA","Delta_TA","Delta_TA")
  treatments=list(matrix(c(6,"lightblue3",10.5,"red"),nrow=2),
                  matrix(c(4,"red",6,"red"),nrow=2),
                  matrix(c(1.5,"white"),nrow=2),
                  matrix(c(1.5,"white"),nrow=2))
  numcategories=c(1,2,2,2)
  chosen=grepl(experiment,options,ignore.case=T)
  # because the kiel ones are not truly unique make a negative selection of the side experiment one if there is no quartz or side experiment in the query
  if(chosen[1] & !grepl("quar|side",experiment,ignore.case=T)){chosen[1]=F}

  nchosen=sum(chosen)
  selected=which(chosen)
  if(nchosen>1){
    stop("This shorthand is not unique!")
  } else if(nchosen==1){
    assign("KOSMOScurrentStyletable",get(paste0(options,"Styletable")[chosen]),envir=.GlobalEnv)
    assign("KOSMOScurrentCategoricalVar",catvars[chosen],envir=.GlobalEnv)
    assign("KOSMOScurrentContinuousVar",convars[chosen],envir=.GlobalEnv)
    assign("KOSMOScurrentTreatmentSchedule",treatments[[selected]],envir=.GlobalEnv)
    nmesos=nrow(KOSMOScurrentStyletable)
    if(numcategories[chosen]==2){assign("KOSMOScurrentStatscols",KOSMOScurrentStyletable[c(nmesos/2-1,nmesos-1),"colourlist"],envir=.GlobalEnv)}
    else if(numcategories[chosen]==1){assign("KOSMOScurrentStatscols",KOSMOScurrentStyletable[nmesos-1,"colourlist"],envir=.GlobalEnv)}
    message(paste0("Style template '",options[chosen],"' selected."))
  } else {
    stop("No matching style template found!")
  }
}
