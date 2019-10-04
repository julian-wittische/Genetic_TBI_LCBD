TBIgenJW_test <- function (mat1, mat2, nperm = 99, replace = FALSE, seed. = NULL, method = 4) {
  #count <- NULL

##### dissim function to compute dissimilarities  
  dissim <- function(mat1, mat2, method ) {
    vecD = vector(mode = "numeric", length = nrow(mat1@tab))
    for (i in 1:nrow(mat1@tab)){
      if (i == 1){
        trick <-2
      } else { 
        trick <- 1
      } 
      temp_genpop <- mat1
      temp_genpop@tab[trick,] <- mat2@tab[i,]
      vecD[i] <- dist.genpop(temp_genpop[c(trick, i),], method=method)
    }
    list(vecD = vecD)
  }
##### initialization    

    if (!is.null(seed.)){
      set.seed(seed.)      
    }

    epsilon <- sqrt(.Machine$double.eps)
    n <- nrow(mat1@tab)
    p <- ncol(mat1@tab)
    if ((nrow(mat2@tab) != n) | (ncol(mat2@tab) != p)){
      # print(nrow(mat1@tab))
      # print(ncol(mat1@tab))
      # print(nrow(mat2@tab))
      print(ncol(mat2@tab))
      print(mat2@tab)
      #count <- count +1
      print(getwd())
      stop("The matrices are not of the same size!")
    }

    tmp <- dissim(mat1, mat2, method)
    vecD.ref <- tmp$vecD
    
    
##### permutations
    if (nperm > 0) {
      my.vec <- sample(1:(10 * nperm), size = nperm)
      nGE.D = rep(1, n)
      
      for (iperm in 1:nperm) {
        
        set.seed(my.vec[iperm])
        mat1.perm <- mat1
        mat1.perm@tab <- apply(mat1.perm@tab, 2, sample, replace = replace)
        
        set.seed(my.vec[iperm])
        mat2.perm <- mat2
        mat2.perm@tab <- apply(mat2.perm@tab, 2, sample, replace = replace)
        
        tmp <- dissim(mat1.perm, mat2.perm, method)
        
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