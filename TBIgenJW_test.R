TBIgenJW_test <- function (mat1, mat2, method = "chord", 
          nperm = 99, replace = FALSE, test.t.perm = FALSE,  seed. = NULL, clock = FALSE) {

##### dissim function to compute dissimilarities  
  dissim <- function(mat1, mat2, n, method) {
    vecD = vector(mode = "numeric", length = n)
    if (any(method == c("chord"))) {
      for (i in 1:n) vecD[i] <- dist(rbind(mat1[i, ], mat2[i,]))
      list(vecD = vecD)
    }
  }
  
##### initialization    

    if (!is.null(seed.)) 
      set.seed(seed.)
    epsilon <- sqrt(.Machine$double.eps)
    method <- match.arg(method, c("chord"))
    n = nrow(mat1)
    p = ncol(mat1)
    if ((nrow(mat2) != n) | (ncol(mat2) != p)) 
      stop("The matrices are not of the same size.")

    if (method == "chord") {
      tr <- TRUE
    }
    else {
      tr <- FALSE
    }
    test.B.C <- NA

      BCD <- FALSE
      BCD.mat <- NA
      BCD.summ <- NA
    
    if (tr) 
      tmp <- dissim(decostand(mat1, "norm"), decostand(mat2, 
                                                       "norm"), n, method)
    else tmp <- dissim(mat1, mat2, n, method)
    vecD.ref <- tmp$vecD
    BC <- NA
    
##### permutations
    if (nperm > 0) {
      my.vec <- sample(1:(10 * nperm), size = nperm)
      nGE.D = rep(1, n)
      for (iperm in 1:nperm) {
        set.seed(my.vec[iperm])
        mat1.perm <- apply(mat1, 2, sample, replace = replace)
        set.seed(my.vec[iperm])
        mat2.perm <- apply(mat2, 2, sample, replace = replace)
        if (tr) {
          tmp <- dissim(decostand(mat1.perm, "norm"), 
                        decostand(mat2.perm, "norm"), n, method)
        }
        else {
          tmp <- dissim(mat1.perm, mat2.perm, n, method)
        }
        vecD.perm <- tmp$vecD
        ge <- which(vecD.perm + epsilon >= vecD.ref)
        if (length(ge) > 0) 
          nGE.D[ge] <- nGE.D[ge] + 1
      }
      p.dist <- nGE.D/(nperm + 1)
    }
    
    p.adj <- p.adjust(p.dist, "holm")

  out <- list(TBI = vecD.ref, p.TBI = p.dist, p.adj = p.adj)
  
  class(out) <- "TBI"
  return(out)
}