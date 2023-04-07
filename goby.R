
genepop.to.genind <- function(name, quiet = TRUE, ncode = 3) {
  if (requireNamespace("adegenet")) {
    tempfile <- file(name)
    tmp <- readLines(tempfile)
    writeLines(tmp, "tempgenepop.gen")
    ind <- adegenet::read.genepop("tempgenepop.gen", quiet = quiet, ncode = ncode)
    unlink("tempgenepop.gen")
    return(ind)
  } else {
    return(0)
  }
}

goby <- genepop.to.genind("Data/GobyCombined.genepop2.txt")

table(goby@pop)

goby_first <- goby
goby_second <- goby

first <- c("ERL06", "BIG06", "MCD06", "PUD06", "VRG06", "ELK06", "EEL06", "WHS06")

second <- c("ERL10", "ERL11", "BIG10", "BIG11", "MCD10", "MCD11", "PUD10", "VRG10", "ELK11a",
            "ELK11b", "EEL10", "SAL11a", "SAL11b")

goby_first <- goby_first[goby_first@pop %in% first]
goby_first@pop

goby_second <- goby_second[goby_second@pop %in% second]
goby_second@pop

goby_first@pop <- as.factor(gsub("[[:lower:][:digit:]]","", goby_first@pop))
goby_second@pop <- as.factor(gsub("[[:lower:][:digit:]]","", goby_second@pop))
levels(goby_second@pop) <- c(levels(goby_second@pop), "WHS")
goby_second@pop[which(goby_second@pop=="SAL")] <- "WHS"
goby_second@pop <- droplevels(goby_second@pop)

goby_first_genpop <- genind2genpop(goby_first)
goby_second_genpop <- genind2genpop(goby_second)

# goby_test <- TBIgenJW_test(goby_first_genpop, goby_second_genpop, nperm = 99)
# goby_test
# 
# goby_test_notgenpop <- TGI(goby_first, goby_second, nperm = 999, method = 3)
# goby_test_notgenpop

goby_test_notgenpop.randmethod3 <- TGI2(goby_first, goby_second, nperm = 999, method = 3)
goby_test_notgenpop.randmethod3

goby_test_notgenpop.randmethod4 <- TGI2(goby_first, goby_second, nperm = 999, method = 4)
goby_test_notgenpop.randmethod4

goby_test_notgenpop.randmethod4 <- TGI2(goby_first, goby_second, nperm = 9999, method = 4)
goby_test_notgenpop.randmethod4