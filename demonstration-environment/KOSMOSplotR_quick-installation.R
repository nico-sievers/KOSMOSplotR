# update R, then update all your packages
update.packages()

# install devtools first, needed to install from github
install.packages("devtools")

# download my package
devtools::install_github("nico-sievers/KOSMOSplotR")

# load it and test!
library(KOSMOSplotR)
KOSMOStimeplot()


# install another package just to be able to receive my updates
devtools::install_github("hrbrmstr/dtupdate")

# check whether you are up to date
dtupdate::github_update()
# force updates - DO THIS EVERY SCRIPTING DAY PLEASE (because I fix issues all the time)
dtupdate::github_update(T,F)