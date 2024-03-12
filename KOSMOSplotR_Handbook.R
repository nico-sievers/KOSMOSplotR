#####################################################
#                                                   #
#                    KOSMOSplotR                    #
#                                                   #
#####################################################
#                                                   #
# An R package for plotting from KOSMOS data sheets #
#                                                   #
#                  by Nico Sievers                  #
#                                                   #
#             first released 12/03/2024             #
#                                                   #
#####################################################


### Intro

# This file describes how to install and get started
# on working with the KOSMOSplotR R package. The
# package (currently) provides two functions for
# plotting time line graphs and regression plots
# directly from data sets in the format commonly used
# during KOSMOS campaigns. It automatically
# implements the current colour scheme of the ongoing
# campaign. The design mirrors that of the plots you
# have seen me use for my master's thesis and the
# flow cytometry update. The package is supposed to
# facilitate quick and easy data exploration and
# automated batch export of plots in a uniform
# layout.


### Disclaimer

# This package is the result of my constant
# development of R scripts for the analysis and
# plotting of my own data. It certainly does not yet
# fulfill the quality expectations of an
# established R package. Please keep an eye out for
# errors and unexpected results, notify me of
# any bugs encountered during usage, support me with
# suggestions, and ask for help should you encounter
# difficulties. Please do not distribute this code
# beyond the team of the current KOSMOS Kiel spring
# 2024 campaign.


### Installation

# Currently, the package is not available on CRAN
# but only on GitHub. Downloading and installing it
# on your machine therefore requires multiple steps,
# as shown below. Installing GitHub packages in R
# unfortunately is prone to complications, therefore
# please check the following points carefully before
# proceeding:
#
# - Update R-Studio (help -> check for updates)
#   and R to the current versions. For R, this is
#   4.3.3, which you need to download directly from
#   www.r-project.org.
#
# - Also, update all of your currently installed
#   packages via
    update.packages()
#
#  -> These suggestions may sound like
#     commonplaces, but they really matter for this
#     installation!
#
# - If possible on your computer start R-Studio
#   with administrator privileges and in a fresh
#   session with empty workspace.


# Begin the installation:

# First, the package "devtools" is needed as it
# contains the function to download packages from
# GitHub:

install.packages("devtools")

# This might take several minutes. Be sure that
# the installation was successful and did not
# prompt any error before continuing!

# Next, install my package!

devtools::install_github("nico-sievers/KOSMOSplotR")

# Check for any error messages, then, if the
# installation was successful, load the package
# and perform a first test:

library(KOSMOSplotR)
KOSMOStimeplot()

# Did it plot an example timeline plot? Fabulous!
# If not, please head over to my office...

# Before continuing with scripting - one more
# installation! GitHub packages don't update with
# other packages downloaded from CRAN. The following
# setup will ensure that updates I release for my
# package are installed on your computer:

devtools::install_github("hrbrmstr/dtupdate")

# During installation, you might be asked to update
# certain other packages if you didn't do so before.
# Select "update all" by typing the respective number
# into the console and hitting enter. Again, check
# for errors before testing:

dtupdate::github_update()

# This will list all packages installed from GitHub
# with their current and latest version numbers.
# Check that "KOSMOSplotR" is on the list. If any
# package is not up-to-date, run
dtupdate::github_update(T,F)
# to update them. Should that cause an error try
# alternatively
dtupdate::github_update(T)
# and enter the number of the package to update.

# Important: To ensure that you stay up-to-date with
# my patches (which there might be quite a few of
# especially in the beginning), include this line of
# code in the top section of every script that uses
# my package and run it at least once per day!
dtupdate::github_update(T,F)



#############
### Usage ###
#############


# Load the package into the workspace, e.g. at the
# beginning of your R script. don't forget the update
# routine!

dtupdate::github_update(T,F)
library(KOSMOSplotR)


# Find the documentation of the package at
help(package="KOSMOSplotR")
# and that of the two central functions at
?KOSMOStimeplot()
# and
?KOSMOSregplot()
# There, I detailed all parameters and options.


# Below is a list of all current parameters.
# please read their explanation in the documentation!

KOSMOStimeplot(
  dataset = KOSMOStestdata,
  parameter = "Parameter",
  ylabel = "Parameter",
  xlabel = "Experiment day",
  control = "Fjord",
  treatment.abline = TRUE,
  ignore = FALSE,
  startat0 = TRUE,
  headspace = 0,
  includeThisInYlimit = FALSE,
  ylimit = FALSE,
  xlimit = FALSE,
  axis.tick = "xy",
  axis.show = "xy",
  stats.show = FALSE,
  stats.days = FALSE,
  stats.ignore = FALSE,
  stats.digits = FALSE,
  stats.location = "bottom",
  stats.meanlabel = c("below", "above"),
  stats.doublespecial = FALSE,
  new.plot = TRUE)

KOSMOSregplot(
  dataset = KOSMOStestdata,
  parameter = "Parameter",
  day = 7,
  ignore = FALSE,
  ylabel = "Parameter",
  xlabel = "Added alkalinity",
  startat0 = TRUE,
  headspace = 0.3,
  includeThisInYlimit = FALSE,
  ylimit = FALSE,
  axis.tick = "xy",
  axis.show = "xy",
  statsblocklocation = "topleft",
  daylabellocation = "topright",
  new.plot = TRUE)


# Run the functions without parameters to see example
# plots based an artificial test data set I included:
KOSMOStimeplot()
KOSMOSregplot()


########################################
### Minimal/typical working examples ###
########################################

# check for updates and load the package
dtupdate::github_update(T,F)
library(KOSMOSplotR)

# load (and potentially install this package to read
# excel files
library(readxl)

# Load your excel data sheet
mydata = read_excel(path = "C:/path/to/my/Data.xlsx",
                    sheet = "Main table")


# (1) If your data set contains exactly one entry per
# day and mesocosm, plot ahead!

KOSMOStimeplot(dataset = mydata, parameter = "Parameter 1", treatment.abline = FALSE)

KOSMOStimeplot(dataset = mydata, parameter = "Other parameter", ignore = c(2,6), startat0 = FALSE)

KOSMOSregplot(dataset = mydata, parameter = "Other parameter", day = c(5,7))


# (2) If your data set contains multiple entries per
# day and mesocosm, such as "Populations" defined via
# flow cytometry, consider looping through them for
# rapid batch plotting:

# iterate through the populations according to the
# respective column
for(population in unique(mydata$population)){

  # subset the data set
  thispopulation = mydata[mydata$population==mydata,]

  # plot this population from the subset data
  KOSMOStimeplot(dataset = mydata, parameter = "Parameter 1")

  # set a title to label each plot with the
  # population
  title(main = population)
}


# (3) Print a plot directly to a file:

# open a connection, e.g. to a .png file
png(units = "in", res = 200,
    "C:/print/here/myplot.png",
    width = 3.4, height = 3.74)

# set up the layout of the plotting window
par(mar = c(3.1, 4.0, 1.6, 0.6), pty = "s", cex = 0.8)

# commit the plot
KOSMOSregplot(dataset = mydata, parameter = "Other parameter")

# close the connection and thereby create the file
dev.off()
