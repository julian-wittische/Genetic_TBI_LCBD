poly <- read.genepop("doi_10.5061_dryad.r90j0__v1/Pygospio_genotype_data (2).gen")

first <- c("FIA2008", "FIA2009", "FIF2008", "FIF2009", "DKR2009", "DKV2008", "DKV2009", "DKH2008",
           "NET2009", "UK2009")

second <- c("FIA2010", "FIF2010", "DKR2010", "DKV2010", "DKH2010", "NET2010", "NET2011", "UK2010")

poly0809 <- poly[poly@pop %in%  first]
poly1011 <- poly[poly@pop %in%  second]

poly0809@pop <- as.factor(gsub("[[:digit:]]","", poly0809@pop))
poly1011@pop <- as.factor(gsub("[[:digit:]]","", poly1011@pop))
     
poly0809_genpop <- genind2genpop(poly0809)
poly1011_genpop <- genind2genpop(poly1011)

poly0809_genpop@tab  <- poly0809_genpop@tab[order(row.names(poly0809_genpop@tab)),]
poly1011_genpop@tab  <- poly1011_genpop@tab[order(row.names(poly1011_genpop@tab)),]

# Check
rownames(poly0809_genpop@tab)
rownames(poly1011_genpop@tab)


POLY_results <- TBIgenJW_test(poly0809_genpop, poly1011_genpop)
POLY_results 
