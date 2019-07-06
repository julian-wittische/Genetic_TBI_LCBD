
comb <-mapply(c, POS[[1]], POS[[2]], SIMPLIFY=FALSE)

# 1 or 2 TP counts as TP
TP_comb <- function(pos_object1, pos_object2){
  temp <- sapply(pos_object1, function(x) sum(c(unlist(x))%in%c(13))) +
    sapply(pos_object2, function(x) sum(c(unlist(x))%in%c(13)))
  temp[temp>=1] <- 1
  return(temp)
}
testTP <- TP_comb(POS[[1]], POS[[2]])

# FP in one only does not count as FP
FP_comb <- function(pos_object1, pos_object2){
  temp <- cbind(sapply(pos_object1, function(x) sum(c(unlist(x))%!in%c(13))),
                sapply(pos_object2, function(x) sum(c(unlist(x))%!in%c(13))))
  temp[temp[,1]==0|temp[,2]==0] <- c(0,0)
  temp <- apply(temp, 1,  ) 
}
