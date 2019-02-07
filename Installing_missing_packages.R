# Installing the packages you do not have
# Julian Wittische 
# February 2019

# Please update your packages regularly, I am just auto-installing missing packages here:

list.of.packages <- c("gtools","vegan")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if("gtools" %in% new.packages){
  install.packages("gtools")
}

if("vegan" %in% new.packages){
  install.packages("vegan")
}
