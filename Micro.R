#TEST <- TBI_test_auto_allp_TBIgenJW_test(path = "E:/Julian_simulations/MICRO_MICRO/output1568926112", scenario = 0,
#                                         nloci = 10, nalleles = 10)
                                         
MICRO <- TEST
m_1 <- lapply(MICRO, function(x) confusion_mat(x[1:30], 1))
m_2 <- lapply(MICRO, function(x) confusion_mat(x[31:60], 2))
m_3 <- lapply(MICRO, function(x) confusion_mat(x[61:90], 3))
m_7 <- lapply(MICRO, function(x) confusion_mat(x[91:120], 7))
m_8 <- lapply(MICRO, function(x) confusion_mat(x[121:150], 8))
m_13 <- lapply(MICRO, function(x) confusion_mat(x[151:180], 13))
m_combine_positions <- mapply(rbind, m_1, m_2, m_3, m_7, m_8, m_13, SIMPLIFY=FALSE)


