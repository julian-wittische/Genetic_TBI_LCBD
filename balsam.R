library(readxl)
library(adegenet)
library(tidyr)

bal <- as.data.frame(read_excel("Data/Input genalex IMP_cleaned.xlsx", sheet="Sheet2"))
bal[bal==0] <- NA

bal$A2[bal$A2==306] <- NA
bal[bal$A2.2==306] <- NA
bal$A2[bal$A2==334] <- NA
bal$A2.2[bal$A2.2==334] <- NA
bal$A21[bal$A21==350] <- NA
bal$A21.2[bal$A21.2==350] <- NA
bal$R101[bal$R101==96] <- NA
bal$R101.2[bal$R101.2==96] <- NA
bal$R104[bal$R104==126] <- NA
bal$R104.2[bal$R104.2==126] <- NA
bal$R104[bal$R104==128] <- NA
bal$R104.2[bal$R104.2==128] <- NA
bal$R210[bal$R210==125] <- NA
bal$R210.2[bal$R210.2==125] <- NA
bal$R213[bal$R213==136] <- NA
bal$R213.2[bal$R213.2==136] <- NA
bal$R213[bal$R213==148] <- NA
bal$R213.2[bal$R213.2==148] <- NA
bal$R240[bal$R240==151] <- NA
bal$R240.2[bal$R240.2==151] <- NA

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
colnames(bal_genind_temp) <- colnames(bal_genind_2011)[seq(1,18,2)]
bal_genind_2011 <- bal_genind_temp
bal_genind_2011$A2

bal_genind_2011 <- df2genind(bal_genind_2011,sep="_",NA.char="NA_NA",
                             ind.names=bal2011$sample, ncode=3,
                             pop=bal2011$pop, ploidy=2, type="codom")

bal_genind_temp2 <- bal_genind_2017[,1:9]

for (i in 1:9){
  bal_genind_temp2[i] <- unite(cbind(bal_genind_2017[2*i-1],
                                    bal_genind_2017[2*i]),
                              col="A")
}
colnames(bal_genind_temp2) <- colnames(bal_genind_2017)[seq(1,18,2)]
bal_genind_2017 <- bal_genind_temp2

bal_genind_2017 <- df2genind(bal_genind_2017,sep="_",NA.char="NA_NA",
                             ind.names=bal2017$sample, ncode=3,
                             pop=bal2017$pop, ploidy=2, type="codom")

TGI2(bal_genind_2011, bal_genind_2017, nperm=99)

