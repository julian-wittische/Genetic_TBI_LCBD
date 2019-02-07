af1 <- CD2TBI(0, 150)
af2 <- CD2TBI(0, 151)
af3 <- CD2TBI(0, 152)
af1_s <- vegan::decostand(af1, method="norm")
af2_s <- vegan::decostand(af2, method="norm")
af3_s <- vegan::decostand(af3, method="norm")
af_c <- rbind(af1_s[1,], af2_s[1,], af3_s[1,])
af_c

af1_g <- CD2GSTUDIO(0, 150)
af2_g <- CD2GSTUDIO(0, 151)
af3_g <- CD2GSTUDIO(0, 152)
af_g_c <- cbind(c(rep("t1", nrow(af1_g[af1_g$Subpopulation==1,])),
                  rep("t2", nrow(af2_g[af2_g$Subpopulation==1,])),
                  rep("t3", nrow(af3_g[af3_g$Subpopulation==1,]))),
                rbind(af1_g[af1_g$Subpopulation==1,],
                      af2_g[af2_g$Subpopulation==1,],
                      af3_g[af3_g$Subpopulation==1,]))
colnames(af_g_c)[1] <- "FakeStratum"
head(af_g_c)

gstud <- gstudio::dist_cavalli(af_g_c, stratum="FakeStratum")
legendre <- as.matrix(dist(af_c))
# Why is not the same? Not because of constant (2/pi) used for gene substitution unit.

# Third method: adegenet

library(adegenet)
af1_a <- df2genind(af1_g[,18:ncol(af1_g)], sep=":", ind.names = af1_g$ID, pop=paste("1_", af1_g$Subpopulation, sep=""))
af2_a <- df2genind(af2_g[,18:ncol(af1_g)], sep=":", ind.names = af2_g$ID, pop=paste("2_", af2_g$Subpopulation, sep=""))
af3_a <- df2genind(af3_g[,18:ncol(af1_g)], sep=":", ind.names = af3_g$ID, pop=paste("3_", af3_g$Subpopulation, sep=""))

af_a_c <- repool(af1_a[af1_a@pop=="1_1",],
                af2_a[af2_a$pop=="2_1",],
                af3_a[af3_a$pop=="3_1",])
af_a_c_genpop <- genind2genpop(af_a_c)
ade <- as.matrix(dist.genpop(af_a_c_genpop, method=2))

library(hierfstat)
#af_h_c <- genind2hierfstat(af_a_c) # does not work, not my fault
af_h_c <- genind2df(af_a_c)
af_h_c[] <- lapply(af_h_c, function(x) gsub("[A]", 1, x))
af_h_c[] <- lapply(af_h_c, function(x) gsub("[B]", 4, x))
af_h_c[,2:ncol(af_h_c)] <- lapply(af_h_c[,2:ncol(af_h_c)], as.numeric)
hier <- as.matrix(genet.dist(af_h_c, method="Dch"))

# Comparison:
hier/max(hier)
ade/max(ade)
gstud/max(gstud)
legendre/max(legendre)

# Manual:
cs_dist <- 2*sqrt(2)/pi*sqrt(1-sum(af1[1,]*af2[1,]))

