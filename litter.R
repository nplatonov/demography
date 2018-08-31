source("./main.R")
invisible({
   if (FALSE) {
      with(randomize(), {
         k1i <- sample(c(2,5,10,15,20),1)
         mortality.cub <- sample(c(0.25,0.35,0.45),1)
         print(data.frame(k1i=k1i,mCOY=mortality.cub,mAdult=mortality.adult))
         mortality <- mortalityTube(max.age=max.age,mortality.cub=mortality.cub
                                   ,mortality.adult=mortality.adult
                                   ,k1d=k1d,k1i=k1i,k2=k2)
         print(mortality,digits=3)
         tube <- mortalityTubePlot(mortality)
         print(tube)
      })
      stop()
   }
   if (!TRUE) {
      if (TRUE) {
         lifestory <- simulate(seed1=NA,seed2=NA)
      }
      else
         lifestory <- simulate(
                       ,max.age=34
                       ,litter=1.8 # 2.2 # 1.8
                       ,mortality.cub=0.32 ## 0.34 # 0.29
                       ,pregnant=0.64 #0.63
                       ,mortality.adult=0.08
                       ,indep.C1=0.2)
      if (!FALSE)
         saveRDS(lifestory,"lifestory.rds")
   }
   else
      lifestory <- readRDS("lifestory.rds")
  # LS <- lifestory[lifestory$epoch==max(lifestory$epoch) & lifestory$season==0,]
  # print(nrow(LS))
  # print(lifestory[lifestory$id=="gdq9gs2e",])
   res <- analyze(lifestory)
   if (!FALSE) {
      pdf(width=3.6,height=2.4)
      with(res,{
        # print(p7+facet_grid(age~.)+p0)
        # print(p7+facet_grid(.~age)+p0)
         print(p8+facet_grid(.~age)+p0)
        # print(p8+facet_grid(age~.)+p0)
         print(p6)
         print(p5)
         print(p1)
         print(p2)
         print(p3)
         print(p4)
         print(p9)
      })
   }
})
warnings()