library(poppr)

TGI2 <- function (mat1, mat2, nperm = 999, replace = FALSE, seed. = NULL, method = 4, correc = "holm", thresh_for_GL = 0.05) {
  
  #### genind to genpop objects
  
  mat1p <- genind2genpop(mat1)
  mat1p <- mat1p[,order(colnames(mat1p@tab))]
  mat2p <- genind2genpop(mat2)
  mat2p <- mat2p[,order(colnames(mat2p@tab))]
  
  ##### Function to compute genetic distances
  
  dissim <- function(mat1p, mat2p, method) {
    dis <- vector(mode = "numeric", length = nrow(mat1p@tab))
    for (i in 1:nrow(mat1p@tab)){
      if (i == 1){
        trick <- 2
      } else { 
        trick <- 1
      } 
      temp_genpop <- mat1p
      temp_genpop@tab[trick,] <- mat2p@tab[i,]
      dis[i] <- dist.genpop(temp_genpop[c(trick, i),], method = method)
    }
    list(dis = dis)
  }
  
  ##### Initialization of seed, tolerance
  
  if (!is.null(seed.)){
    set.seed(seed.)      
  }
  
  epsilon <- sqrt(.Machine$double.eps)
  
  ##### Dimensions check
  
  n <- nrow(mat1p@tab)
  p <- ncol(mat1p@tab)
  if ((nrow(mat2p@tab) != n) | (ncol(mat2p@tab) != p)){
    stop("The matrices are not of the same size!")
  }
  
  ##### Empirical genetic distances
  tmp <- dissim(mat1p, mat2p, method)
  dis.ref <- tmp$dis
  
  ##### Permutations
  if (nperm > 0) {
    my.vec <- sample(1:(10 * nperm), size = nperm)
    outlier.count = rep(1, n)
    
    for (iperm in 1:nperm) {
      
      set.seed(my.vec[iperm])
      mat1.perm <- mat1p
      mat1.perm <- shufflepop(mat1.perm, method=4)
      
      set.seed(my.vec[iperm])
      mat2.perm <- mat2p
      mat2.perm <- shufflepop(mat2.perm, method=4)
      
      tmp <- dissim(mat1.perm, mat2.perm, method)
      
      dis.perm <- tmp$dis
      ge <- which(dis.perm + epsilon >= dis.ref)
      if (length(ge) > 0) { 
        outlier.count[ge] <- outlier.count[ge] + 1
      }
    }
    p.dist <- outlier.count/(nperm + 1)
  }
  
  p.adj <- p.adjust(p.dist, method = correc)
  
  ##### Gain or loss?
  
  n.pop1 <- seppop(mat1)
  n.pop2 <- seppop(mat2)
  
  mean.hexp1 <- do.call("c", lapply(n.pop1, function(x) mean(summary(x)$Hexp)))
  mean.hexp2 <- do.call("c", lapply(n.pop2, function(x) mean(summary(x)$Hexp)))
  
  mean.hexp1[is.nan(mean.hexp1)] <- NA
  mean.hexp2[is.nan(mean.hexp2)] <- NA
  
  simple_diff <- mean.hexp2 - mean.hexp1
  
  output <- list(TBI = dis.ref, p.TBI = p.dist, p.adj = p.adj, gainloss = simple_diff[p.adj < thresh_for_GL])
  
  class(output) <- "TGI"
  
  return(output)
}