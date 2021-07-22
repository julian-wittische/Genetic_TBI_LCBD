library(adegenet)
library(poppr)

?df2genind
df <- data.frame(locus1=c("AA","AA","BB","BB","AB","BB","BB","BB"),
                 locus2=c("CC","CC","CD","DD","DD","CD","DD","DD"),
                 locus3=c("FF","FF","EF","EF","EE","EE","EF","EE"),
                 locus4=c("GH","GG","GG","GH","HH","HH","GH","HH"))
row.names(df) <- .genlab("ind",8)
df_genind <- df2genind(df, pop=as.factor(c("pop1","pop1","pop2","pop2","pop3","pop3","pop4","pop4")),sep="")
df_genpop <- genind2genpop(df_genind)
df_genind
set.seed(1)
df_genind_shu <- shufflepop(df_genind, method=4)
df_genind@tab
df_genind_shu@tab
set.seed(1)
df_genpop_shu <- shufflepop(df_genpop, method=4)
df_genpop@tab
df_genpop_shu@tab