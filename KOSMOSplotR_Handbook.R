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
# implements the current colour scheme of the selected
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
# difficulties. Please consider crediting the package
# as
citation("KOSMOSplotR")
# should you use it for your work.


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
KOSMOStimeplot(main="just a quick testplot")

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
# my package and run it before you plot ahead!
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
# and that of the four central functions at
?KOSMOStimeplot()
?KOSMOSregplot()
?KOSMOSadjustColumnames()
# and
?KOSMOSselect()
# There, I detailed all parameters and options.

# Run the plotting functions without parameters to see example
# plots based on an artificial test data set I included:
KOSMOStimeplot()
KOSMOSregplot()


########################################
### Minimal/typical working examples ###
########################################

# check for updates and load the package
dtupdate::github_update(T,F)
library(KOSMOSplotR)

# load (and potentially install) the readxl
# package to read excel files
library(readxl)

# Load your excel KOSMOS data sheet
mydata = read_excel(path = "C:/path/to/my/Data.xlsx",
                    sheet = "Main table")


### Select the campaign ###
# that your data stems from to load the correct
# style template:
KOSMOSselect(experiment="kiel")
# Currently, choose between 'KOSMOS2024Kiel',
# 'KOSMOS2023Helgoland', and 'KOSMOS2022Bergen'.
# You can use shorthands! See the documentation
# of
?KOSMOSselect()
# to see which variables it sets and how you can
# overwrite them manually to design the plot to
# your needs, and how to change further aspects
# such as the controls and abline lines globally.


### Match the colmun names ###
# My scripts require the supplied data to match
# the data sheet template of the respective
# campaign, but has functions to handle minor
# mismatches. You can first of all go ahead and try
# plotting and only come back here if there is an
# issue reported. If you want to be on the save
# side or encountered issues, run
mydata=KOSMOSadjustColumnames(mydata)
# to have the script modify and report on your
# columnnames.


### Now, to the actual plotting! ###

### (1) If your data set contains exactly one entry per
# day and mesocosm, plot ahead!

KOSMOStimeplot(dataset = mydata, parameter = "Parameter 1")
KOSMOStimeplot(dataset = mydata, parameter = "Parameter 1", exclude_meso = 2, exclude_day = c(1,33), control = "Fjord", treatmentgroups_sidebyside = TRUE, ylabel = "Parameter [unit]", startat0 = FALSE, headspace = 0.25, axis.show = "x", treatment.abline = FALSE, stats.show = TRUE, stats.days = c(7,15), stats.meanlabel = c("above","above"))

KOSMOSregplot(dataset = mydata, parameter = "Parameter 1")
KOSMOSregplot(dataset = mydata, parameter = "Parameter 1", days = c(5,7), exclude_meso = c(2,5), xlabel = "OAE", statsblocklocation = "bottomleft", daylabellocation = "bottomright")

# In the function documentations you will find all
# parameters described in detail, so that you can
# customise the plots to your needs.
# In short, You can subset the data; exclude certain
# mesocosms or days; adjust the axes range, labels,
# or hide them, and control the location of stats
# reports.
# For the timeline plot you can furthermore:
# Choose a control like a fjord sample; choose to plot
# all treatments in one or split them into two panels;
# choose to highlight the time point of treatment
# addition; and calculate and display certain statistics.


### (2) If your data set contains multiple entries per
# day and mesocosm, such as e.g. species defined
# via Microscopy, you should define how to subset
# the data:

KOSMOStimeplot(dataset = mydata, parameter = "Parameter 1", subset_data = list(Species="E. hux",Size_class=c("small","medium")))

# consider looping through them for rapid batch
# plotting:

for(thispopulation in unique(mydata$Population)){

  KOSMOStimeplot(dataset = mydata, parameter = "Parameter 1", subset_data = list(Population=thispopulation))
  # set a title to label each plot with the
  # population
  title(main = thispopulation)
}


### (3) Print a plot directly to a file:

# open a connection, e.g. to a .png file
png(units = "in", res = 200,
    "C:/print/here/myplot.png",
    width = 3.4, height = 3.74)

# set up the layout of the plotting window
par(mar = c(3.1, 4.0, 1.6, 0.6),
    pty = "s", cex = 0.8)

# commit the plot
KOSMOSregplot(dataset = mydata, parameter = "Parameter 1")

# close the connection and thereby create the file
dev.off()


#########################################
# That's it with the short introduction.
# I hope it all works fine for you and
# please let me know if not!
#
# P.S.: I have further tools and
# functions in a package called "R2HU",
# mostly surrounding Flow Cytometry, but
# also for some general data handling
# tasks. Feel free to contact me and we
# can have a look whether I have (or can
# create something) to make your data-
# processing-life easier.
########################################
