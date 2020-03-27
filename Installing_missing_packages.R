# Installing the packages you do not have
# Julian Wittische 
# July 2019

# Please update your packages regularly, I am just auto-installing missing packages here:

list.of.packages <- c("gtools",
                      "vegan",
                      "adespatial",
                      "ggplot2",
                      "reshape2",
                      "cowplot",
                      "extrafont",
                      "adegenet",
                      "pegas",
                      "hierfstat",
                      "colorspace",
                      "Rmisc",
                      "poppr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
new.packages
install.packages(new.packages)
