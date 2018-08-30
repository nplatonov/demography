source("./main.R")
invisible({
  # seed1 <- 179 ## 560 468 786 447 152 335 279 203 649 515 790 829
  # seed2 <- 346  ## (236,441)
   if (TRUE) {
      if (TRUE) {
         lifestory <- simulate(seed1=193,seed2=NA)
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
      })
   }
})
warnings()