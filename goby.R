# genepop.to.genind

#' Imports a \code{.txt} file in \code{GenePop} format into an object of type \code{genind}

#' 
#' The main work is done by the function \code{adegenet::read.genepop}. However, that function requires text files with an extension of \code{.gen}, whereas such files usually have extension \code{.txt}. The sole purpose of this function is to work around the \dQuote{.gen} requirement.
#' 
#' @param name the name of a file in \code{GenePop} format
#' @param quiet whether a conversion message should be printed
#' @param ncode Set to the number of characters per allele name
#' 
#' @return an object of class \code{genind}
#' 
#' @export
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

goby <- genepop.to.genind("GobyCombined.genepop2.txt")

table(goby@pop)

goby_first <- goby
goby_first[goby_first@pop %in% c("TS99-48", "ERL99", "ERL06", "STN90-19", "STN06", "BIG06", " MCD99-40",
                                 "MCD06", "ELK06" )]

c("TS99-48", "ERL99", "ERL06", "STN90-19", "STN06", "BIG06", " MCD99-40", "MCD06", "ELK06" )
#lonely: AWP09, AWR10, "GAN06", PND06, JAC06,  JAC10, ROK10, WDC06, "WHS06", 

goby_second <- goby
goby_second[goby_second@pop %in% c("TS11", "ERL10", "ERL11", "STN06", "STN10 ", "STN11", "BIG10","BIG11",
                                   "MCD10", "MCD11", "ELK11a", "ELK11b")]

