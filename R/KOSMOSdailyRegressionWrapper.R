#' @title Plot a regression for each sampling day quickly
#'
#' @description Create a file containing a panel of regression plots for each (or a selection of) sampling days automatically. This function is still quite raw and will always create a .svg file.
#'
#' @param filename The path and name as which to save the file. This needs to be an \code{.svg} always.
#' @param dataset A data set object following the common KOSMOS layout, i.e. loaded from the standard excel data sheet. If left empty, an example dataset \code{KOSMOStestdata} will be plotted to showcase the function. Check \code{View(KOSMOStestdata)} to compare the required data structure.
#' @param parameter The column name of the response variable to be plotted given as a string. Defaults to the last column in the data table.
#' @param days Select a vector of sampling days to include in the plot pane. By default, all will be included.
#' @param target_ratio Select the target aspect ratio for the panel grid. The algorith will try to create a rectangle slightly wider but as close to that as possible to that. By default, it will be \code{16:9} to fit common powerpoint slides.
#' @param ... forward any other arguments of \code{KOSMOSregplot} to customise the individual plots further.
#'
#' @importFrom grDevices svg dev.off
#' @importFrom stats na.omit
#'
#' @export


KOSMOSdailyRegressionWrapper=function(filename="output.svg",dataset=KOSMOStestdata,parameter=last(names(dataset)),days=NULL,target_ratio=NULL,...) {

  message("\n\nPlotting ",filename)

  # set default ratio
  if(is.null(target_ratio)){target_ratio=16/9}

  # get rid of all lines where there is no data for this parameter, so that these days are not considered in the paneling
  dataset=dataset[!is.na(dataset[[parameter]]),]

  days_available=sort(na.omit(unique(dataset$Day)))
  if (is.null(days)) {
    # Default: Use all available days if `days` is not specified
    days=days_available
  } else {
    # subset for the user's pre-selection
    days=days[days %in% days_available]  # Ensure only valid days are used
  }
  num_days=length(days)

  # Determine optimal layout for 16:9 aspect ratio
  best_cols=ceiling(sqrt(num_days) * (target_ratio))  # Start with a 16:9 guess
  best_rows=ceiling(num_days / best_cols)

  # Ensure width/height ratio is close to 16:9
  while ((best_cols / best_rows < target_ratio || best_cols * best_rows < num_days)) {
    best_cols=best_cols + 1
    best_rows=ceiling(num_days / best_cols)
  }
  message("Using layout: ",best_cols," columns x ",best_rows," rows for ",num_days," plots")

  plot_size=3  # Adjust if needed
  svg_width=best_cols * plot_size
  svg_height=best_rows * plot_size
  message("Final SVG size: ", svg_width, " x ", svg_height, " inches\n")

  # Open a direct SVG device
  svg(filename, width = svg_width, height = svg_height)

  # Set up panel layout
  par(mfrow=c(best_rows,best_cols),pty="s",mar=c(3.1,4.0,1.6,0.6)) # Square plots, reduced margins

  # Generate plots directly inside the SVG
  suppressMessages(for(i in days){
    KOSMOSregplot(dataset=dataset,parameter=parameter,days=i,...)
  })

  # Close the SVG device
  dev.off()
  # message("Plot saved to ",filename,"\n")
}
