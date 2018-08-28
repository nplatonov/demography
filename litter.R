source("./main.R")
invisible({
  # seed1 <- 179 ## 560 468 786 447 152 335 279 203 649 515 790 829
  # seed2 <- 346  ## (236,441)
   if (TRUE) {
      require(ggplot2)
      prm <- randomize()
      prm$indep.C1 <- 0.01
      str(prm)
      with(prm,{
         mortality <- mortalityTube(max.age=max.age,mortality.cub=mortality.cub
                                   ,mortality.adult=mortality.adult
                                   ,k1=k1d,k2=k2)
         indep.mortality=mortalityIndep(mortality,k1=k1i,k2=k2)
         q()
         age <- seq(max.age)
         da1 <- data.frame(age=age,mortality=mortality
                          ,kind="Dependent")
         da2 <- data.frame(age=age,mortality=c(indep.mortality,tail(mortality,-3))
                          ,kind="Independent")
         print(da1[1:4,])
         print(da2[1:4,])
         comb.mortality <- mortality
         indep.fraction <- c(C0=0.001,C1=indep.C1,C2=0.99)         
         comb.mortality[1:3] <- mortality[1:3]*(1-indep.fraction)+
                                indep.mortality[1:3]*indep.fraction
         da3 <- data.frame(age=age,mortality=comb.mortality
                          ,kind="Combines")
         print(da3[1:4,])
         da <- rbind(da1,da2,da3)
         tube <- ggplot(da,aes(age,mortality,colour=kind))
         tube <- tube+geom_point()+geom_line()
         tube <- tube+xlab("Age")+ylab("Mortality")
         tube <- tube+theme(panel.background=element_rect(fill="#F39C1240"))
         print(tube)
         q()
      })
   }
   if (TRUE) {
      if (TRUE) {
        # lifestory <- simulate(seed1=677,seed2=466)
         lifestory <- simulate(seed1=561,seed2=744)#c(NA,744,484)[2])
      }
      else
         lifestory <- simulate(
                       ,max.age=34
                       ,litter=1.8 # 2.2 # 1.8
                       ,mortality.cub=0.32 ## 0.34 # 0.29
                       ,pregnant=0.64 #0.63
                       ,mortality.adult=0.08
                       ,indep.C1=0.2)
      if (FALSE)
         saveRDS(lifestory,"lifestory.rds")
   }
   else
      lifestory <- readRDS("lifestory.rds")
   if (TRUE) {
      opW <- options(warn=10)
      res <- analyze(lifestory)
      options(opW)
      pdf(width=8,height=6)
      print(res$p6)
      print(res$p5)
      print(res$p1)
      print(res$p2)
      print(res$p3)
      print(res$p4)
   }
})
