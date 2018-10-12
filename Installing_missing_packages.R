# Installing the packages you do not have
# Julian Wittische 
# October 2018

# Please update your packages regularly, I am just auto-installing missing packages here:

list.of.packages <- c("gtools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if("gtools" %in% new.packages){
  install.packages("gtools")
}

if("devtools" %in% new.packages){
  install.packages("devtools")
}

# if("ResistanceGA" %in% new.packages){
#  devtools::install_github("wpeterman/ResistanceGA", build_vignettes = TRUE)
# }

gc()