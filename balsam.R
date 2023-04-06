library(readxl)
library(adegenet)
library(tidyr)

bal <- as.data.frame(read_excel("Data/Input genalex IMP_cleaned.xlsx", sheet="Sheet2"))

bal2011 <- bal[which(bal$year==2011), ]
bal2017 <- bal[which(bal$year==2017), ]

bal2011$pop <- substr(bal2011$pop, start = 1, stop = 2)
bal2017$pop <- substr(bal2017$pop, start = 1, stop = 2)

bal_genind_2011 <- bal2011[,-c(1,2,21)]
bal_genind_2017 <- bal2017[,-c(1,2,21)]
  
bal_genind_temp <- bal_genind_2011[,1:9]

for (i in 1:9){
  bal_genind_temp[i] <- unite(cbind(bal_genind_2011[2*i-1], bal_genind_2011[2*i]),
  col="A")
}
bal_genind_2011 <- bal_genind_temp

*bal_genind_2011 <- df2genind(bal_genind_2011, ind.names=bal2011$sample, ncode=3,
                             pop=bal2011$pop, ploidy=2, type="codom")


