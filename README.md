# TN_Ecoflows
Ecological flow analyses for predicted fish species richness responses to surface water withdrawal scenarios

## Software
If R (version 3.3 or greater) is not already installed on your computer, download and install the latest binary distribution from the Comprehensive R Archive Network (CRAN). R can also be run from RStudio, which is a nice interface for working with R and provides a console. In order to use RStudio, R must be installed first. The latest version of RStudio is available here. R and RStudio should be accessible from the start menu, a desktop shortcut, or via command shell. Assuming you installed R on a 64-bit operating system (OS), two versions of R are made available, that is, a 32-bit and 64-bit version. The 64-bit version of R (R x64) is preferred. In order to add the USGS Geological Survey R Archive Network, or the GRAN, as a repository from which to download USGS R packages;

rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
write('\noptions(repos=c(getOption(\'repos\'),
    CRAN=\'https://cloud.r-project.org\',
    USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)
