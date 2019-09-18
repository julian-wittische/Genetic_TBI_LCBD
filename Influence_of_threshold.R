thresh <- mapply(rbind, bl1_combine_positions, bm1_combine_positions, bh1_combine_positions,
              il1_combine_positions, im1_combine_positions, ih1_combine_positions,
              bl2_combine_positions, bm2_combine_positions, bh2_combine_positions,
              il2_combine_positions, im2_combine_positions, ih2_combine_positions,
              bl3_combine_positions, bm3_combine_positions, bh3_combine_positions,
              il3_combine_positions, im3_combine_positions, ih3_combine_positions, SIMPLIFY=FALSE)

meansfpr <- sapply(thresh, function(x) mean(x$FPR))
sdfpr <- sapply(thresh, function(x) sd(x$FPR))
meansfnr <- sapply(thresh, function(x) mean(x$FNR))
sdfnr <- sapply(thresh, function(x) sd(x$FNR))

meansfpr
sdfpr
meansfnr
sdfnr
