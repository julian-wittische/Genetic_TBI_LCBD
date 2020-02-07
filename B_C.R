library(adegenet)
library(dplyr)

x <- glSim(100, 10, n.snp.struc=0, ploid=2, k=2)
x.mat <- as.matrix(x) # x is a genlight object
x.mat[x.mat == 0] <- "1/1" # homozygote reference
x.mat[x.mat == 1] <- "1/2" # heterozygote
x.mat[x.mat == 2] <- "2/2" # homozygote alternate
x.gid <- df2genind(x.mat, sep = "/", ploidy = 2)
x.gid@pop <- as.factor(sample(1:2, 100, replace = TRUE))
x.genpop <- genind2genpop(x.gid)
x.genpop@tab[,seq(1, 20, 2)] <- sample(1:100, 10)
x.genpop@tab[,seq(2, 20, 2)] <- 100 - x.genpop@tab[,seq(1, 20, 2)]

dist.genpop(x.genpop, method = 4)

B <- x.genpop
B@tab[,seq(1, 20, 2)] <- ifelse(B@tab[, B@tab[1,] > B@tab[2,]])
dist.genpop(B, method = 4)

data(H3N2)
H3N2
#find pairs
H2001 <- H3N2[H3N2@other$epid == 2001]
H2002 <- H3N2[H3N2@other$epid == 2002]
H2003 <- H3N2[H3N2@other$epid == 2003]
H2004 <- H3N2[H3N2@other$epid == 2004]
H2005 <- H3N2[H3N2@other$epid == 2005]
H2006 <- H3N2[H3N2@other$epid == 2006]

table(H2001@other$x$country)
table(H2002@other$x$country)
table(H2003@other$x$country)
table(H2004@other$x$country)
table(H2005@other$x$country)
table(H2006@other$x$country)

H0203 <- H3N2[H3N2@other$epid == 2002 | H3N2@other$epid == 2003| H3N2@other$epid == 2001]
H0506 <- H3N2[H3N2@other$epid == 2005 | H3N2@other$epid == 2006]
table(H0203@other$x$country)
table(H0506@other$x$country)

scand <- c("Denmark", "Sweden", "Norway", "Finland", "Iceland") #Scandinavia
west <- c("France", "Germany", "Ireland",  "Netherlands", "United Kingdom", "Austria", "Czech Republic") #Western Europe
south <- c("Italy", "Greece", "Croatia", "Turkey") #Southern Europe
northam <- c("USA", "Canada")

H0203@other$x$country[H0203@other$x$country %in% scand] <- "Scandinavia" 
H0203@other$x$country[H0203@other$x$country %in% west] <- "Western Europe"
H0203@other$x$country[H0203@other$x$country %in% south] <- "Southern Europe"
H0203@other$x$country[H0203@other$x$country %in% northam] <- "North America"

H0506@other$x$country[H0506@other$x$country %in% scand] <- "Scandinavia" 
H0506@other$x$country[H0506@other$x$country %in% west] <- "Western Europe"
H0506@other$x$country[H0506@other$x$country %in% south] <- "Southern Europe"
H0506@other$x$country[H0506@other$x$country %in% northam] <- "North America"

H0203_final <- H0203[H0203@other$x$country %in% c("China", "Japan", "North America", "South Korea", "Southern Europe",
                                                  "Taiwan", "Western Europe")]

H0506_final <- H0506[H0506@other$x$country %in% c("China", "Japan", "North America", "South Korea", "Southern Europe",
                                                  "Taiwan", "Western Europe")]

table(H0203_final@other$x$country)
table(H0506_final@other$x$country)

H0203_final@pop <- as.factor(H0203_final@other$x$country)
H0506_final@pop <- as.factor(H0506_final@other$x$country)

H0203_final_pop <- genind2genpop(H0203_final)
H0506_final_pop <- genind2genpop(H0506_final)

H0203_final_pop
H0506_final_pop

testH3N2 <- TBIgenJW_test(H0203_final_pop, H0506_final_pop, nperm=999, method = 4)

# intersect(H2004@other$xy$x, H2005@other$xy$x)
# H2003pair <- H2003[H2003@other$xy$x %in% intersect(H2003@other$xy$x, H2004@other$xy$x)]
# H2003pair@pop <- as.factor(round(H2003pair@other$xy$x))
# table(H2003pair@pop)
# 
# H2004pair <- H2004[H2004@other$xy$x %in% intersect(H2004@other$xy$x, H2005@other$xy$x)]
# H2004pair@pop <- as.factor(round(H2004pair@other$xy$x))
# H2005pair <- H2005[H2005@other$xy$x %in% intersect(H2004@other$xy$x, H2005@other$xy$x)]
# H2005pair@pop <- as.factor(round(H2005pair@other$xy$x))
