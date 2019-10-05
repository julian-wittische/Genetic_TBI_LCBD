# SBW
# Data from Jeremy Larroque
getwd()
load("SBW_1213_JW.RData")
dat3
sbw2012 <- dat3[dat3@other$year==2012]
sbw2013 <- dat3[dat3@other$year==2013]

pop(sbw2012) <- regmatches(pop(sbw2012), regexpr("T[[:digit:]]+", pop(sbw2012)))
pop(sbw2013) <- regmatches(pop(sbw2013), regexpr("T[[:digit:]]+", pop(sbw2013)))

sbw2012 <- genind2genpop(sbw2012)
sbw2013 <- genind2genpop(sbw2013)

sbw2012@tab[,1]
sbw2013@tab

TBIgenJW_test(sbw2012, sbw2013)

