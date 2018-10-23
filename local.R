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
   if (TRUE) {
      if ((FALSE)&&(file.exists("simulation.rds")))
         file.remove("simulation.rds")
      toUpdate <- F & file.exists("simulation.rds")
      lifestory <- if (toUpdate) readRDS("simulation.rds") else NULL
      if (TRUE) {
         if (!toUpdate) {
            if (TRUE) {
               curv <- simulate(seed1=795,seed2=130,check=TRUE)
               saveRDS(curv,"checkInputs.rds")
               if (TRUE) {
                  ind <- which(sapply(curv,inherits,"ggplot"))
                  str(curv[-ind])
                  print(names(curv[ind]))
                  if (TRUE) {
                     pdf(width=3.6,height=2.4)
                     for (i in ind) {
                        print(curv[[i]])
                     }
                  }
               }
               q()
            }
            lifestory <- simulate(seed1=795,seed2=130)
            saveRDS(lifestory,"simulation.rds")
         }
         if (TRUE) {
            pdf(width=3.6,height=2.4)
            res <- perturb(lifestory)
            opW <- options(warn=1)
            sapply(res,print)
            options(opW)
           # print("A")
           # print(warnings())
           # q()
         }
      }
      else if (TRUE) {
         lifestory <- simulate(seed1=447,seed2=840)
      }
      else if (!FALSE) { ## MOSJ
         lifestory <- simulate(
                              ,max.age=32
                              ,litter=1.69
                              ,mortality.cub=0.35
                              ,mortality.adult=0.09
                              ,pregnant=0.64
                              ,k1d=10
                              ,k1i=8
                              ,k2=5
                              ,fert=0.70
                              ,indep.fraction=0.45
                              ,seed1=214
                              ,seed2=850
                              )
        ## control: trend value for 2005
        ### Cub production = Natality rate: 0.63
        ### Litter proportion: 0.36 (0yr), 0.13 (1yr)
      }
      else if (TRUE) ## Wiig1998
         lifestory <- simulate(
                              ,max.age=35
                              ,litter=1.83 # 2.2 # 1.8
                              ,mortality.cub=0.39 ## 0.34 # 0.29
                              ,mortality.adult=0.09
                              ,pregnant=0.73
                              ,k1i=8
                              ,fert=0.7
                              ,indep.fraction=0.5
                              ,seed1=NA
                              ,seed2=NA
                              )
      else
         stop("Что дальше?")
   }
   else {
      lifestory <- readRDS("lifestory.rds")
     # LS <- lifestory[lifestory$epoch==max(lifestory$epoch) & lifestory$season==0,]
     # print(nrow(LS))
     # print(lifestory[lifestory$id=="gdq9gs2e",])
     # ursa:::.elapsedTime("============== analysis start ================")
      res <- analyze(lifestory,options="all")
      saveRDS(res,"analysis.rds")
     # ursa:::.elapsedTime("============== analysis finish ================")
      if (!FALSE) {
         pdf(width=3.6,height=2.4)
         with(res,{
           # print(p7+facet_grid(age~.)+p0)
           # print(p7+facet_grid(.~age)+p0)
           # print(p8+facet_grid(age~.)+p0)
            if (!is.null(p6))
               print(p6)
            if (!is.null(p5))
               print(p5)
            if (!is.null(p8))
               print(p8+facet_grid(.~age)+p0)
            if (!is.null(p10))
               print(p10)
            if (!is.null(p1))
               print(p1)
            if (!is.null(p2))
               print(p2)
            if (!is.null(p3))
               print(p3)
            if (!is.null(p4))
               print(p4)
            if (!is.null(p9))
               print(p9)
         })
      }
   }
})
warnings()
