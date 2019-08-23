# Installing the packages you do not have
# Julian Wittische 
# July 2019

# Please update your packages regularly, I am just auto-installing missing packages here:

list.of.packages <- c("gtools","vegan", "adespatial", "ggplot2", "reshape2", "cowplot", "extrafont")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if("gtools" %in% new.packages){
  install.packages("gtools")
}

if("vegan" %in% new.packages){
  install.packages("vegan")
}

if("adespatial" %in% new.packages){
  install.packages("adespatial")
}

if("ggplot2" %in% new.packages){
  install.packages("ggplot2")
}

if("reshape2" %in% new.packages){
  install.packages("reshape2")
}

if("cowplot" %in% new.packages){
  install.packages("cowplot")
}

if("extrafont" %in% new.packages){
  install.packages("extrafont")
}
