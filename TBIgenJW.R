function (mat1, mat2, method = "%difference", pa.tr = FALSE, 
          nperm = 99, BCD = TRUE, replace = FALSE, test.BC = TRUE, 
          test.t.perm = FALSE, save.BC = FALSE, seed. = NULL, clock = FALSE) 
{
  RuzickaD <- function(vec1, vec2, method = "ruzicka", 
                       BCD = FALSE, ref = TRUE) {
    A <- sum(pmin(vec1, vec2))
    sum.Y <- sum(vec1, vec2)
    if (ref) {
      tmp <- vec1 - vec2
      B <- sum(tmp[tmp > 0])
      C <- -sum(tmp[tmp < 0])
      D <- B + C
    }
    else {
      D <- sum.Y - 2 * A
    }
    if (method == "ruzicka") 
      den <- (sum.Y - A)
    else den <- sum.Y
    if (!BCD) 
      B <- C <- NA
    list(B.den = B/den, C.den = C/den, D = D/den, B = B, 
         C = C)
  }
  dissim <- function(mat1, mat2, n, method, BCD, ref) {
    vecD = vector(mode = "numeric", length = n)
    if (ref & BCD) {
      vecB <- vector(mode = "numeric", length = n)
      vecC <- vector(mode = "numeric", length = n)
      v.B <- vector(mode = "numeric", length = n)
      v.C <- vector(mode = "numeric", length = n)
    }
    else {
      vecB <- vecC <- v.B <- v.C <- NA
    }
    if (any(method == c("euclidean", "chord"))) 
      for (i in 1:n) vecD[i] <- dist(rbind(mat1[i, ], mat2[i, 
                                                           ]))
      if (any(method == c("ruzicka", "%difference"))) {
        for (i in 1:n) {
          tmp <- RuzickaD(mat1[i, ], mat2[i, ], method = method, 
                          BCD = BCD, ref = ref)
          if (ref & BCD) {
            vecB[i] <- tmp$B.den
            vecC[i] <- tmp$C.den
            v.B[i] <- tmp$B
            v.C[i] <- tmp$C
          }
          vecD[i] <- tmp$D
        }
      }
      list(vecB = vecB, vecC = vecC, vecD = vecD, v.B = v.B, 
           v.C = v.C)
  }
  A <- system.time({
    if (!is.null(seed.)) 
      set.seed(seed.)
    epsilon <- sqrt(.Machine$double.eps)
    method <- match.arg(method, c("%difference", "ruzicka", 
                                  "chord", "hellinger", "log.chord", 
                                  "jaccard", "sorensen", "ochiai", 
                                  "euclidean"))
    n = nrow(mat1)
    p = ncol(mat1)
    if ((nrow(mat2) != n) | (ncol(mat2) != p)) 
      stop("The matrices are not of the same size.")
    if (method == "hellinger") {
      mat1 <- sqrt(mat1)
      mat2 <- sqrt(mat2)
      method <- "chord"
    }
    if (method == "log.chord") {
      mat1 <- log1p(mat1)
      mat2 <- log1p(mat2)
      method <- "chord"
    }
    if (method == "jaccard") {
      pa.tr <- TRUE
      method <- "ruzicka"
    }
    if (method == "sorensen") {
      pa.tr <- TRUE
      method <- "%difference"
    }
    if (method == "ochiai") {
      pa.tr <- TRUE
      method <- "chord"
    }
    if (pa.tr) {
      mat1 <- ifelse(mat1 > 0, 1, 0)
      mat2 <- ifelse(mat2 > 0, 1, 0)
    }
    if (method == "chord") {
      tr <- TRUE
    }
    else {
      tr <- FALSE
    }
    test.B.C <- NA
    if ((any(method == c("ruzicka", "%difference"))) & 
        BCD) {
      BCD.mat <- matrix(0, n, 3)
      if (method == "%difference") 
        colnames(BCD.mat) <- c("B/(2A+B+C)", "C/(2A+B+C)", 
                               "D=(B+C)/(2A+B+C)")
      if (method == "ruzicka") 
        colnames(BCD.mat) <- c("B/(A+B+C)", "C/(A+B+C)", 
                               "D=(B+C)/(A+B+C)")
      rownames(BCD.mat) <- paste("Site", 1:n, sep = ".")
      Change <- vector(mode = "character", length = n)
    }
    else {
      BCD <- FALSE
      BCD.mat <- NA
      BCD.summ <- NA
    }
    if (tr) 
      tmp <- dissim(decostand(mat1, "norm"), decostand(mat2, 
                                                       "norm"), n, method, BCD, ref = TRUE)
    else tmp <- dissim(mat1, mat2, n, method, BCD, ref = TRUE)
    vecD.ref <- tmp$vecD
    BC <- NA
    if (BCD) {
      BCD.mat[, 1] <- tmp$vecB
      BCD.mat[, 2] <- tmp$vecC
      BCD.mat[, 3] <- tmp$vecD
      for (i in 1:n) {
        if (tmp$vecB[i] > tmp$vecC[i]) 
          Change[i] <- "-  "
        else if (tmp$vecB[i] < tmp$vecC[i]) 
          Change[i] <- "+  "
        else Change[i] <- "0  "
      }
      BCD.summ <- matrix(NA, 1, 6)
      colnames(BCD.summ) <- c("mean(B/den)", "mean(C/den)", 
                              "mean(D)", "B/(B+C)", "C/(B+C)", 
                              "Change")
      BCD.means <- apply(BCD.mat, 2, mean, na.rm = TRUE)
      BCD.summ[1, 1:3] <- BCD.means
      BCD.summ[1, 4:5] <- BCD.means[1:2]/BCD.means[3]
      BCD.summ <- as.data.frame(BCD.summ)
      if (BCD.summ[1, 1] > BCD.summ[1, 2]) 
        BCD.summ[1, 6] <- "-  "
      else if (BCD.summ[1, 1] < BCD.summ[1, 2]) 
        BCD.summ[1, 6] <- "+  "
      else BCD.summ[1, 6] <- "0  "
      rownames(BCD.summ) <- ""
      BCD.mat <- as.data.frame(BCD.mat)
      BCD.mat <- cbind(BCD.mat, Change)
      if ((n > 4) & test.BC) {
        test.B.C <- matrix(NA, 1, 4)
        rownames(test.B.C) <- "Paired t.test"
        if (test.t.perm) {
          if (nperm < 999) 
            nperm1 <- 999
          else nperm1 <- nperm
          t.res <- tpaired.randtest(tmp$vecC, tmp$vecB, 
                                    nperm = nperm1, alternative = "two.sided", 
                                    silent = TRUE)
          test.B.C[1, ] <- c(t.res$estim, t.res$t.ref, 
                             t.res$p.param, t.res$p.perm)
          p.value <- t.res$p.param
        }
        else {
          t.res <- t.test(tmp$vecC, tmp$vecB, paired = TRUE, 
                          alternative = "two.sided")
          test.B.C[1, ] <- c(t.res$estimate, t.res$statistic, 
                             t.res$p.value, NA)
          p.value <- t.res$p.value
        }
        signif. <- ifelse(p.value > 0.05, " ", 
                          "*")
        test.B.C <- as.data.frame(test.B.C)
        test.B.C <- cbind(test.B.C, signif.)
        colnames(test.B.C) <- c("  mean(C-B)", 
                                "Stat", "p.param", "p.perm", 
                                "  p<=0.05")
      }
      else {
        test.B.C <- NA
      }
      if (save.BC) {
        BC <- cbind(tmp$v.B, tmp$v.C)
        colnames(BC) <- c("B", "C")
        rownames(BC) <- paste("Site", 1:n, sep = ".")
      }
    }
    if (nperm > 0) {
      my.vec <- sample(1:(10 * nperm), size = nperm)
      BCD <- FALSE
      nGE.D = rep(1, n)
      for (iperm in 1:nperm) {
        set.seed(my.vec[iperm])
        mat1.perm <- apply(mat1, 2, sample, replace = replace)
        set.seed(my.vec[iperm])
        mat2.perm <- apply(mat2, 2, sample, replace = replace)
        if (tr) {
          tmp <- dissim(decostand(mat1.perm, "norm"), 
                        decostand(mat2.perm, "norm"), n, method, 
                        BCD, ref = FALSE)
        }
        else {
          tmp <- dissim(mat1.perm, mat2.perm, n, method, 
                        BCD, ref = FALSE)
        }
        vecD.perm <- tmp$vecD
        ge <- which(vecD.perm + epsilon >= vecD.ref)
        if (length(ge) > 0) 
          nGE.D[ge] <- nGE.D[ge] + 1
      }
      p.dist <- nGE.D/(nperm + 1)
    }
    else p.dist <- NA
    p.adj <- p.adjust(p.dist, "holm")
  })
  A[3] <- sprintf("%2f", A[3])
  if (clock) 
    cat("Computation time =", A[3], " sec", "\n")
  out <- list(TBI = vecD.ref, p.TBI = p.dist, p.adj = p.adj, 
              BCD.mat = BCD.mat, BCD.summary = BCD.summ, t.test_B.C = test.B.C, 
              BC = BC)
  class(out) <- "TBI"
  return(out)
}