# SBW
# Data from Jeremy Larroque

load("SBW_1213_JW.RData")

sbw2012_genind <- dat3[dat3@other$year==2012]
sbw2013_genind <- dat3[dat3@other$year==2013]

pop(sbw2012_genind) <- regmatches(pop(sbw2012_genind), regexpr("T[[:digit:]]+", pop(sbw2012_genind)))
pop(sbw2013_genind) <- regmatches(pop(sbw2013_genind), regexpr("T[[:digit:]]+", pop(sbw2013_genind)))

sbw2012_genpop <- genind2genpop(sbw2012_genind)
sbw2013_genpop <- genind2genpop(sbw2013_genind)

sbw2012_genpop@tab  <- sbw2012_genpop@tab[order(row.names(sbw2012_genpop@tab)),]
sbw2013_genpop@tab  <- sbw2013_genpop@tab[order(row.names(sbw2013_genpop@tab)),]

# Check
rownames(sbw2012_genpop@tab)
rownames(sbw2013_genpop@tab)

SBW_results <- TBIgenJW_test(sbw2012_genpop, sbw2013_genpop)
SBW_results

# sep12 <- seppop(sbw2012_genind) 
# mean.hobs12 <- do.call("c", lapply(sep12, function(x) mean(summary(x)$Hobs))) 
# mean.hobs12[is.nan(mean.hobs12)] <- NA 
# barplot(mean.hobs12)
# 
# # CHANGER L'ORDRE POUR LE FAIRE RESPECTER
# sep13 <- seppop(sbw2013_genind)
# mean.hobs13 <- do.call("c", lapply(sep13, function(x) mean(summary(x)$Hobs))) 
# mean.hobs13[is.nan(mean.hobs13)] <- NA 
# barplot(mean.hobs13) 