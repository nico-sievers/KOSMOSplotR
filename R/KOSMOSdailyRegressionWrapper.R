# stupid git bug

#' @title Plot a regression for each sampling day quickly
#'
#' @description Create a file containing a panel of regression plots for each (or a selection of) sampling days automatically. This function is still quite raw and will always create an .svg file.
#'
#' @param filename The path and name as which to save the file. This needs to be an \code{.svg} always.
#' @param dataset A data set object following the common KOSMOS layout, i.e. loaded from the standard excel data sheet. If left empty, an example dataset \code{KOSMOStestdata} will be plotted to showcase the function. Check \code{View(KOSMOStestdata)} to compare the required data structure.
#' @param subset_data Subset the data by including or excluding rows that have given values in a specified column. If set to \code{FALSE} (the default), no sub-setting is performed. To subset the data table by columns \code{'columnA'} and \code{'columnB'}, supply the following syntax: \code{subset_data = list( columnA = c("value1","value2") , columnB = c("value A","value B"))}, where the column name is the name of the element of the list and the element is a vector (or single value) that marks all rows you wish to include. Alternatively, values can be excluded by adding the prefix \code{"not_"} to a column name, such as \code{subset_data = list( not_columnA = c("value1","value2"))}. Here, rows with \code{value1} and \code{value2} are dropped while all other rows remain. Note that \code{exclude_meso} and \code{exclude_day}, as well as \code{xlimit}, are more convenient parameters to exclude sampling days and mesocosms from the plot and can be passed on to \code{KOSMOSregplot()}.
#' @param parameter The column name of the response variable to be plotted given as a string. Defaults to the last column in the data table.
#' @param days Select a vector of sampling days to include in the plot pane. By default, all will be included.
#' @param sync_y_axis If this is set to \code{TRUE} all sub-plots will share the same y-axis for better comparison, at the risk of overseeing signal in sampling days with smaller values. By default, each plot will have the best suitable axis range calculated independently. Note that if this is set headspace of 35 percent is currently hard-coded.
#' @param target_ratio Select the target aspect ratio for the panel grid. The algorith will try to create a rectangle slightly wider but as close to that as possible to that. By default, it will be \code{16:9} to fit common powerpoint slides.
#' @param tolerance The percentage by which a gapless panel may diverge from the target ratio to still be preferred over a panel that contains empty spaces. The default is \code{0.5}.
#' @param ... forward any other arguments of \code{KOSMOSregplot} to customise the individual plots further.
#'
#' @importFrom grDevices svg dev.off
#' @importFrom stats na.omit
#' @importFrom dplyr last
#'
#' @export


KOSMOSdailyRegressionWrapper=function(filename="output.svg",dataset=KOSMOStestdata,subset_data=FALSE,parameter=last(names(dataset)),days=NULL,sync_y_axis=FALSE,target_ratio=NULL,tolerance=0.5,...) {

  message("\n\nPlotting ",filename)


  ### prepare the dataset

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

  # get rid of all lines where there is no data for this parameter, so that these days are not considered in the paneling
  dataset=dataset[!is.na(dataset[[parameter]]),]
  # also kick the control so it is not used in the min/max calculations
  dataset=dataset[dataset$Mesocosm!=KOSMOScurrentControl,]

  if(nrow(dataset)==0){warning("No datapoints in the selected range, no plot created.")}
  else{

  days_available=sort(na.omit(unique(dataset$Day)))
  if (is.null(days)) {
    # Default: Use all available days if `days` is not specified
    days=days_available
  } else {
    # subset for the user's pre-selection
    days=days[days %in% days_available]  # Ensure only valid days are used
  }
  num_days=length(days)

  # determine the maximum axis range needed to accomodate all data points if requested by the user
  if(sync_y_axis){
    yrange=c(min(dataset[[parameter]],na.rm=T),max(dataset[[parameter]],na.rm=T)*1.35)
  } else {
    yrange=F
  }


  ### prepare the panel layout

  # set default ratio
  if(is.null(target_ratio)){target_ratio=16/9}


  # define a helper function
  find_perfect_grid <- function(num_days, target_ratio) {
    # List all factor pairs of num_days
    factor_pairs <- list()

    #for (i in sqrt(num_days):1) {
    for (i in num_days:1) {
      if (num_days %% i == 0) {
        factor_pairs[[length(factor_pairs) + 1]] <- c(i, num_days / i)
      }
    }

    good_pairs <- list()
    # Check if any factor pair has an aspect ratio close enough to target_ratio
    i=0;for (pair in factor_pairs) {
      cols <- pair[1]
      rows <- pair[2]
      aspect_ratio <- cols / rows

      # Check if the aspect ratio is within 33% of the target ratio
      this_ratio_rating=abs(aspect_ratio - target_ratio)/target_ratio
      if (this_ratio_rating <= tolerance) {
        i=i+1
        good_pairs[[i]]=c(cols,rows,this_ratio_rating) # save the pair if it matches the ratio tolerance
      }
    }

    if(length(good_pairs)){
      ratings=unlist(lapply(good_pairs,"[",3))
      winner=which(ratings==min(ratings))
      return(good_pairs[[winner]][1:2])
    } else {
      return(NULL)  # Return NULL if no matching grid is found
    }
  }


  # Try finding a perfect grid (columns x rows) for num_days with a close aspect ratio to target_ratio
  perfect_grid <- find_perfect_grid(num_days, target_ratio=target_ratio)

  if (!is.null(perfect_grid)) {
    best_cols <- perfect_grid[1]
    best_rows <- perfect_grid[2]
    message("Using perfect grid layout: ", best_cols, " columns x ", best_rows, " rows")

  } else {

    # Determine optimal layout for the given aspect ratio
    best_cols=ceiling(sqrt(num_days) * (target_ratio))  # Start with a guess
    best_rows=ceiling(num_days / best_cols)

    # Ensure width/height ratio is close to the target raio
    while ((best_cols / best_rows < target_ratio || best_cols * best_rows < num_days)) {
      best_cols=best_cols + 1
      best_rows=ceiling(num_days / best_cols)
    }
    message("Using flexible layout: ",best_cols," columns x ",best_rows," rows for ",num_days," plots")
  }

  plot_size=3  # Adjust if needed
  svg_width=best_cols * plot_size
  svg_height=best_rows * plot_size
  message("Final SVG size: ", svg_width, " x ", svg_height, " inches\n")


  ### create the file and plot

  # Open a direct SVG device
  svg(filename, width = svg_width, height = svg_height)

  # Set up panel layout
  par(mfrow=c(best_rows,best_cols),pty="s",mar=c(3.1,4.0,1.6,0.6)) # Square plots, reduced margins

  # Generate plots directly inside the SVG
  suppressMessages(for(i in days){
    try(KOSMOSregplot(dataset=dataset,parameter=parameter,days=i,ylimit=yrange,...))
  })

  # Close the SVG device
  dev.off()
  # message("Plot saved to ",filename,"\n")
  }
}
