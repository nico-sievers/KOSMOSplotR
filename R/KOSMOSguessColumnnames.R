#' @title Function to adjust data tables in a way that even slightly mismatching column names can be plotted correctly within this package.
#'
#' @description Called by the plotting functions directly, this tool tries to guess the names of the required columns.
#'
#' @param dataset A data set object roughly following the common KOSMOS layout for which the column names shall be checked.
#' @param required_columns Vector of those columns to be matched, all others are left untouched. Defaults to all potentially required columns for the available plot functions. This argument is supplied by the plotting function if this function is called automatically, so that only strictly necessary ones are marked as required.
#' @param minimal_requirement Vector of those columns that essential for plotting KOSMOS data and can't be derived from others.
#'
#' @return Returns an info value on how well the found columns matched the original, and the column numbers of the matches, and a boiled down version of the \code{KOSMOScolumntable} for internal use by \code{KOSMOSadjustColumnnames}.
#'
# @examples
# KOSMOSguessColumnnames(KOSMOStestdata)
#'
#' @export
#' @importFrom dplyr first

KOSMOSguessColumnnames=function(dataset=KOSMOStestdata,
                               required_columns=c("Day","Mesocosm",KOSMOScurrentCategoricalVar,KOSMOScurrentContinuousVar,"Treat_Meso"),
                               minimal_requirement=c("Day","Mesocosm")){

  # set export variables as empty
  fit=NA
  perfect_matches_columns=character(0)
  #guessed_columns=character(0)
  #guessed_columns_positions=NA
  #create_columns=character(0)
  guessed_columns_ideal_names=character(0)
  guessed_columns_current_names=character(0)
  #essentials_position=character(0)
  meso_position=character(0)
  missing_cols_guessed=character(0)
  # prepare error message
  failed="\nCouldn't identify all essential columns in the data set!\nRequiring at least an equivalent of 'Day' and 'Mesocosm'."

  # get the info on expected columns from the input and the table of campaign-specific names
  tmpcolumntable=KOSMOScolumntable[KOSMOScolumntable$Names %in% required_columns,]

  # which of those don't exact matches in the data set?
  missing_cols_perfect=setdiff(tmpcolumntable$Names,names(dataset))
  perfect_matches_columns=setdiff(required_columns,missing_cols_perfect) # report these later
  if(length(missing_cols_perfect) == 0){
    fit="perfect" # if all are there exactly as expected

  } else { # no perfect match, but still hope

    # this searches for patterns now to find those with small naming differences
    yestmp=lapply(tmpcolumntable$Patterns,grep,names(dataset),ignore.case=T,perl=T) # this is the match pattern
    nottmp=lapply(tmpcolumntable$NotMatch,grep,names(dataset),ignore.case=T,perl=T) # this is the patters to exclude
    tmp=Map(setdiff,yestmp,nottmp) # they ore brought together for the final decision which columns match and which dont
    guessed_columns_ideal_names_position=unlist(lapply(tmp,function(x) length(x)>0))
    guessed_columns_positions=unique(unlist(lapply(tmp,first))) # here i always use the first match of each, assuming that later occurrences are something different. also i exclude duplicates, so if two patterns were matched on the same column name thats also excluded here

    guessed_columns_current_names=setdiff(names(dataset)[guessed_columns_positions],required_columns)
    guessed_columns_ideal_names_full=tmpcolumntable$Names[guessed_columns_ideal_names_position]
    missing_cols_guessed=setdiff(tmpcolumntable$Names,guessed_columns_ideal_names_full) # which ones were not sucessfully guessed?
    guessed_columns_ideal_names=setdiff(guessed_columns_ideal_names_full,perfect_matches_columns) # remove those that were also a perfect match anyway. report these later

    if(length(guessed_columns_ideal_names_full) >= 2){ # only continue if you matched at least two

      create_columns=setdiff(required_columns,guessed_columns_ideal_names_full) # report these later

      if(length(guessed_columns_ideal_names)>0){
        # if you guessed some that are not anyway perfect fits
      message("\n\nWhen scanning for the required columns, the following ones were identified to have slighlty different Names:")
      message(paste(guessed_columns_current_names,collapse=" | "))
      }

      if(length(missing_cols_guessed) == 0){
        fit="guessed all" # if all were found by the pattern search

      } else if(all(!(minimal_requirement %in% missing_cols_guessed))){
        # if at least Day and Mesocosm are there. the rest can be calculated

        # this repeats the pattern search for only meso
        tmpkeycolumntable=KOSMOScolumntable[KOSMOScolumntable$Names %in% minimal_requirement,] # get the info on expected columns from the input and the table of campaign-specific names
        # # day first
        # yestmpday=grep(tmpkeycolumntable$Patterns[1],names(dataset),ignore.case=T,perl=T) # this is the match pattern
        # nottmpday=grep(tmpkeycolumntable$NotMatch[1],names(dataset),ignore.case=T,perl=T) # this is the patters to exclude
        # tmpday=setdiff(yestmpday,nottmpday) # they are brought together for the final decision which columns match and which dont
        # meso next
        yestmpmeso=grep(tmpkeycolumntable$Patterns[2],names(dataset),ignore.case=T,perl=T) # this is the match pattern
        nottmpmeso=grep(tmpkeycolumntable$NotMatch[2],names(dataset),ignore.case=T,perl=T) # this is the patters to exclude
        meso_position=first(setdiff(yestmpmeso,nottmpmeso)) # they ore brought together for the final decision which column matches and which doesnt

        fit="need addition"
        #essentials_position=c(tmpday,tmpmeso) # pass on the key columns

      } else {
        # this triggers if there are guessed matches, but Day and Meso are not among them
        stop(failed)
      }
    } else {
      # this triggers if we have less than two guessed matches
      stop(failed)
    }
  }
  return(list(fit,perfect_matches_columns,guessed_columns_ideal_names,guessed_columns_current_names,meso_position,missing_cols_guessed))
}



if(T){
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
}
