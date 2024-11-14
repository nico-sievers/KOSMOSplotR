#' @title Function to adjust data tables in a way that even slightly mismatching column names can be plotted correctly within this package.
#'
#' @description Called by the plotting functions directly, this tool tries to guess the names of the required columns.
#'
#' @param dataset A data set object roughly following the common KOSMOS layout for which the column names shall be checked.
#' @param required_columns Vector of those columns to be matched, all others are left untouched. Defaults to all potentially required columns for the available plot functions. This argument is supplied by the plotting function if this function is called automatically, so that only strictly necessary ones are marked as required.
#'
#' @return Returns an info value on how well the found columns matched the original, and the column numbers of the matches, and a boiled down version of the \code{KOSMOScolumntable} for internal use by \code{KOSMOSadjustColumnnames}.
#'
# @examples
# KOSMOSguessColumnnames(KOSMOStestdata)
#'
#' @export
#' @importFrom utils head tail

KOSMOSguessColumnnames=function(dataset=KOSMOStestdata,
                               required_columns=c("Day","Mesocosm",KOSMOScurrentCategoricalVar,KOSMOScurrentContinuousVar,"Treat_Meso")){

  tmpcolumntable=KOSMOScolumntable[KOSMOScolumntable$Names %in% required_columns,]
  # this searches for exact matches ### THIS DOES ALWAYS PRINT, SO SOMETHING DOESN'T WORK
  tmp=unlist(lapply(tmpcolumntable$Names,grep,names(dataset)))
  if(length(tmp)==nrow(tmpcolumntable)){
    fit="perfect"
  } else {
    # this searches for patterns now
    tmp=lapply(tmpcolumntable$Patterns,grep,names(dataset),ignore.case=T,perl=T)
    nottmp=lapply(tmpcolumntable$NotMatch,grep,names(dataset),ignore.case=T,perl=T)
    tmp=Map(setdiff,tmp,nottmp)
    tmp=unique(unlist(lapply(tmp,head,1)))


    message("The following columns were identified:")
    message(paste(names(dataset)[tmp],collapse=" | "))
    fit="suggested"
    if(length(tmp)!=nrow(tmpcolumntable)){
      stop("Couldn't identify all essential columns in the data set!\nRequiring an equivalent of:\n",paste(tmpcolumntable$Names,collapse=" | "))
    }
  }
  return(list(fit,tmp,tmpcolumntable))

}




# # Sample data
# entries <- c("mesoabc", "treatmeso", "mesotreat", "treatmesoabc", "xyzmesotreat", "abc")
#
# # Using grep with regular expression
# result <- grep("(?<!treat)meso", entries, perl = TRUE, value = TRUE)
#
# print(result)
#
#
#
#
#
# # Sample data
# entries <- c("meso", "mesotreat", "treatmentmeso", "meso and treat", "mesotherapy", "treatment")
#
# # Use grep with adjusted regular expression
# result <- grep("meso(?!.*treat)|(?<!treat.*)meso", entries, perl = TRUE, value = TRUE)
#
# # Output the result
# print(result)
#
#
#
# # Use grep with adjusted regular expression
# result <- grep("(?=.*meso)(?!.*treat)", entries, perl = TRUE, value = TRUE)
#
# # Output the result
# print(result)
#
#
#
#
#
# # Sample data
# entries <- c("meso", "mesotreat", "treatmentmeso", "meso and treat", "mesotherapy", "treatment", "Treat_Meso", "Treatment_Mesocosm", "Mesocosm","meso_treat")
#
# # Use grep with adjusted regular expression
# result <- grep("\\bmeso\\b(?!.*\\btreat\\b)", entries, perl = TRUE, value = TRUE)
#
# # Use grep with adjusted regular expression
# result <- grep("(?=.*meso)(?!.*\\btreat\\b)", entries, perl = TRUE, value = TRUE)
#
# # Use grep with adjusted regular expression
# result <- grep("\\bmeso\\b(?!\\w*treat\\w*)", entries, perl = TRUE, value = TRUE)
#
# # Use grep with adjusted regular expression
# result <- grep("(?<!\\bmeso)treat|meso(?!\\btreat\\b)", entries, perl = TRUE, value = TRUE)
#
# # Use grep with adjusted regular expression
# result <- grep("\\bmeso(?!.*\\btreat\\b)", entries, perl = TRUE, value = TRUE)
#
# # Output the result
# print(result)
