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
      if ((!FALSE)&&(file.exists("lifestory.rds")))
         file.remove("lifestory.rds")
      toUpdate <- file.exists("lifestory.rds")
      lifestory <- if (toUpdate) readRDS("lifestory.rds") else NULL
      if (TRUE) {
         if (!toUpdate) {
            lifestory <- simulate(seed1=NA,seed2=NA)
            q()
         }
         else {
            set.seed(NULL)
            pdf(width=3.6,height=2.4)
            s <- 1.15^(seq(-2,2))
            for (p in rev(c("mortality.adult","mortality.cub"))) {
               res <- data.frame(x=round(lifestory$input[[p]]*s,3)
                                ,mean=NA,sd=NA)
               res2 <- NULL
               arglist <- list(lifestory,NA)
               names(arglist) <- c("",p)
               for (i in sample(seq(nrow(res)))) {
                  arglist[[p]] <- res$x[i]
                  g1 <- do.call("growthRate",arglist)
                 # g1 <- growthRate(lifestory,mortality.adult=res$x[i])
                  res$mean[i] <- mean(g1)
                  res$sd[i] <- sd(g1)
                 # desc <- paste0(res$x[i],"\n",sprintf("%.3f\u00B1%.3f",res$mean[i],res$sd[i]))
                  desc <- paste0(sprintf("%.3f\u00B1%.3f",res$mean[i],res$sd[i]))
                  res2 <- rbind(res2,data.frame(prm=res$x[i],value=g1,desc=desc))
               }
               print(res)
               res2$desc <- factor(res2$desc,levels=unique(res2$desc),ordered=TRUE)
               s1 <- ggplot(res2,aes(prm,value))+
                     geom_violin(fill="yellow",colour="blue")+
                     xlab("Adult Mortality")+ylab("Growth Rate")+
                     facet_grid(.~desc,scales="free")+
                     scale_x_continuous(breaks=res$x)
               print(s1)
            }
            q()
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
      if (!toUpdate)
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
        # print(p8+facet_grid(age~.)+p0)
         print(p6)
         print(p5)
         print(p8+facet_grid(.~age)+p0)
         print(p10)
         print(p1)
         print(p2)
         print(p3)
         print(p4)
         print(p9)
      })
   }
})
warnings()
