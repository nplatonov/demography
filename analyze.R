'analyze' <- function(LS,options="") {
   if (missing(LS))
      return(list(input=NULL,p0=NULL,p1=NULL,p2=NULL,p3=NULL,p4=NULL,p5=NULL
                 ,p6=NULL,p7=NULL,p8=NULL,p9=NULL,p10=NULL))
   isNone <- length(grep("none",options,ignore.case=TRUE))>0
   isAll <- length(options)==0 | nchar(options)==0 |
            length(grep("all",options,ignore.case=TRUE))>0
   isLitter <- isAll | length(grep("litter",options,ignore.case=TRUE))>0 ## p7 p8 p10
   isSurvival <- isAll | length(grep("(surv|mort)",options,ignore.case=TRUE))>0 ## p9
   isStructure <- isAll | length(grep("(struct|peer)",options,ignore.case=TRUE))>0 ## p6
   isGrowthRate <- isAll | length(grep("(growth|popsize)",options,ignore.case=TRUE))>0 ## p5
   isInterbirth <- isAll | length(grep("interbirth",options,ignore.case=TRUE))>0 ## p1
   isDenning <- isAll | length(grep("denning",options,ignore.case=TRUE))>0 ## p2 p3 p4
   print(c(All=isAll,None=isNone,Litter=isLitter,Survival=isSurvival
          ,Structure=isStructure,GrowthRate=isGrowthRate
          ,Interbirth=isInterbirth))
   isShiny <- ("shiny" %in% loadedNamespaces())
   if (isShiny)
      showModal(modalDialog(title = "Analysis in progress","Please wait"
                       ,size="s",easyClose = TRUE,footer = NULL))
   input <- LS$input
   lifestory <- LS$output
   epoch <- sort(unique(lifestory$epoch))
   season <- sort(unique(lifestory$season))
   ns <- length(season)
   max.age <- max(lifestory$age)
   subad.ini <- 3
   cs <- colorScheme()
   p0 <- theme_grey()+
         theme(panel.background=element_rect(fill=cs$bg))+
         theme(strip.background=element_rect(fill=cs$strip))+#,colour="red"))
         theme(legend.margin=margin(t=-1,b=0,unit='char'))
        # theme(legend.key.size=size(1,unit="char"))
   p10 <- p9 <- p8 <- p7 <- p6 <- p5 <- p4 <- p3 <- p2 <- p1 <- NULL
  # pdf("res1.pdf",width=8,height=4)
   res <- NULL
   natalityRate <- NA
   if (isLitter) {
      if (isShiny)
         showNotification(closeButton=TRUE,"Litter size...",id="litter"
                         ,duration=99)
      pop <- lifestory[lifestory$epoch>c(0,max.age)[2] &
                       lifestory$epoch<=max(epoch)-0*4 &
                       lifestory$season==0,]
      res <- NULL
      res3 <- NULL
      if (nrow(pop)) {
         age <- c('0Yr'=0,'1Yr'=1,'2Yr'=2)
         pop3 <- pop[pop$age>3 & pop$sex=="F",]
         epoch3 <- sort(unique(pop3$epoch))
         nFemale <- aggregate(pop3$id,list(epoch=pop3$epoch),length)$x
        # brokenFamily <- c(0.001,input$iC1,0.999) ## == input$indep.fraction
         for (i in seq_along(age)) {
            pop1 <- pop[pop$age==age[i] & !is.na(pop$parent),]
            res2 <- aggregate(pop1$parent,list(epoch=pop1$epoch),function(x) {
               lf <- unname(table(table(x)))
               while (length(lf)<3) lf <- c(lf,0)
               lf
            })
            ind <- match(epoch3,res2$epoch)
            nCOY <- rep(0,length(epoch3))
            nCOY[!is.na(ind)] <- aggregate(pop1$parent,list(epoch=pop1$epoch),length)$x
            nFamily <- rep(0,length(epoch3))
            nFamily[!is.na(ind)] <- apply(res2$x,1,sum)
            L <- nCOY/nFamily ## litter size
            L[is.nan(L)] <- 0
            LP <- nFamily/nFemale ## litter production
            CP <- L*LP ## cub production == natality rate
            if (names(age)[i]=="0Yr")
               natalityRate <- CP
            LP2 <- LP*(1-input$indep.fraction[i])
           # N <- nCOY/nFemale ## N = LS*LP ## natality rate
            da3 <- data.frame(mean=c(mean(L),mean(LP),mean(LP2),mean(CP))
                             ,sd=c(sd(L),sd(LP),sd(LP2),sd(CP))
                             ,row.names=c("Litter Size","Litter Production"
                                         ,"Litter Production by mating"
                                         ,"Natality Rate"))
            if (i==1)
               print(da3[c(1,2,4),],digits=2)
            else if (i==2)
               print(da3[c(2,3),],digits=2)
            else
               print(da3[2,],digits=2)
            lab <- sprintf("%s\n%.2f\u00B1%.2f",names(age[i]),mean(LP),sd(LP))
            if (!(names(age[i]) %in% "2Yr"))
               res3 <- rbind(res3,data.frame(age=lab,epoch=epoch3,value=LP))
            if (names(age[i]) %in% c("1Yr","2Yr")[1]) {
               lab <- sprintf("%s min\n%.2f\u00B1%.2f",names(age[i]),mean(LP2),sd(LP2))
               res3 <- rbind(res3,data.frame(age=lab,epoch=epoch3,value=LP2))
            }
            L <- apply(res2$x,1,function(lf) {
               x <- lf/sum(lf)
               sum(x*seq_along(x))
            })
            res2$x <- t(apply(res2$x,1,function(lf) lf/sum(lf)))
            lab <- sprintf("%s\n%.2f\u00B1%.2f",names(age[i]),mean(L),sd(L))
            res2 <- cbind(data.frame(age=lab,epoch=res2$epoch),res2$x)
            res <- rbind(res,tidyr::gather(res2,cubs,value,-epoch,-age))
         }
         res3$age <- factor(res3$age,levels=unique(res3$age),ordered=TRUE)
         p7 <- ggplot(res,aes(epoch,value,colour=cubs))+geom_line()
         p8 <- ggplot(res,aes(cubs,value))+
               geom_violin(fill=cs$hist,colour=cs$base)+
               xlab("Age-specific litter Size")+ylab("Proportion")
         p10 <- ggplot(res3,aes(age,value))+
               geom_violin(fill=cs$hist,colour=cs$base)+
               xlab("Age")+ylab("Litter Production")+
               scale_x_discrete(breaks=NULL)+
               facet_grid(.~age,scales="free")+
               p0
      }
      if (isShiny)
         removeNotification(id="litter")
     # stop()
   }
   if (isSurvival) {
      if (isShiny)
         showNotification(closeButton=TRUE,"Actual survival",id="survival"
                         ,duration=99)
     # print(tail(lifestory[lifestory$season==9,],100))
     # str(lifestory[lifestory$season==9 & lifestory$epoch==max(lifestory$epoch),])
     # str(lifestory[lifestory$season==0 & lifestory$epoch==max(lifestory$epoch),])
     # str(lifestory[lifestory$season==1 & lifestory$epoch==max(lifestory$epoch),])
      age <- list('0Yr'=1,'1Yr'=2,'2Yr'=3,'Sub-Ad'=4:5,'Adult'=6:99)
      res <- NULL
      ep <- max(lifestory$age)-4
      label <- rep("",length(age))
      toSkip <- FALSE
      opW <- options(warn=10)
      for (i in seq_along(age)) {
         pop0 <- lifestory[lifestory$age %in% (age[[i]]-1) & lifestory$epoch>=ep,]
         pop1 <- lifestory[lifestory$age %in% age[[i]] & lifestory$epoch>=ep,]
         if ((!nrow(pop0))||(!nrow(pop1))) {
            toSkip <- TRUE
            break
         }
         spop0 <- pop0[pop0$season==0,]
         spop1 <- pop1[pop1$season==1,]
         spop9 <- pop1[pop1$season==9,]
         if ((!nrow(spop0))||(!nrow(spop1))||(!nrow(spop9))) {
            toSkip <- TRUE
            break
         }
         s0 <- aggregate(id~epoch,data=spop0,length)
         s1 <- aggregate(id~epoch,data=spop1,length)
         s9 <- aggregate(id~epoch,data=spop9,length)
         s0 <- s0[match(s1$epoch,s0$epoch),]
         surv <- s1$id/s0$id
         surv[surv>=1] <- 1-1e-3
         res2 <- data.frame(Epoch=s0$epoch,Survival=surv#,Age=names(age)[i]
                           ,Label="")
         label[i] <- sprintf("%s\n%.2f\u00B1%.2f",names(age)[i]
                            ,mean(res2$Survival),sd(res2$Survival))
         res2$Label <- label[i]
         res <- rbind(res,res2)
      }
      options(opW)
      if (!toSkip) {
         res$Label <- factor(res$Label,levels=label,ordered=TRUE)
         p9 <- ggplot(res,aes(Label,Survival))+
               geom_violin(fill=cs$hist,colour=cs$base)+
               xlab("Age Structure")+ylab("Actual Survival")+
               scale_x_discrete(breaks=NULL)+
               facet_grid(.~Label,scales="free")+
               p0
      }
     # print(p9+p0);q()
      if (isShiny)
         removeNotification(id="survival")
   }
   if (isStructure) {
      if (isShiny)
         showNotification(closeButton=TRUE,"Population structure",id="structure"
                         ,duration=99)
      epoch <- epoch[(epoch*ns) %% ns == 0 & epoch>=max(lifestory$age)-4]
      pop <- lifestory[lifestory$epoch %in% epoch & lifestory$season==0,]
      if (nrow(pop)) {
         a1 <- aggregate(id~age+epoch,data=pop,length)
         colnames(a1)[ncol(a1)] <- "size"
         p6 <- ggplot(a1,aes(age,size))+
              # geom_violin(aes(group=cut_width(age,1)))+
               geom_boxplot(aes(group=cut_width(age,1)),outlier.alpha=0.5
                           ,colour=cs$base,width=1)+
               p0
      }
      if (isShiny)
         removeNotification(id="structure")
   }
   if (isGrowthRate) {
      if (isShiny)
         showNotification(closeButton=TRUE,"Growth rate",id="growth"
                         ,duration=99)
      res <- NULL
      growth <- NA
      for (s in c(0,1)) {
         season <- ifelse(s==0,"After COY","Before COY")
         pop <- lifestory[which(lifestory$season==s),] ## 0 - with C0, 1 - without C0
         epoch <- sort(unique(pop$epoch))
         res2 <- data.frame(epoch=epoch,size=NA,era="",season=season)
         for (i in seq(nrow(res2))) {
            res2$size[i] <- nrow(pop[pop$epoch==res2$epoch[i],])
         }
         if (s==0) {
            pop1 <- unique(pop[pop$age==0,c("epoch","parent")])
            parent <- aggregate(parent~epoch,data=pop1,length)
            res3 <- with(parent,data.frame(epoch=epoch,size=parent*10
                                          ,era="",season="Dens (x10)"
                                          ))
         }
         for (j in c(1,2)) {
            era <- ifelse(j==1,"Initialization","Implementation")
            if (j==1)
               ind <- which(res2$epoch<=max.age-subad.ini)
            else
               ind <- which(res2$epoch>=max.age-subad.ini)
            res2$era[ind] <- era
            res <- rbind(res,res2[ind,])
            if (s==0) {
               res3$era[ind] <- era
               res <- rbind(res,res3[ind,])
            }
            if ((s==0)&&(j==2)) {
               size <- res2$size[ind]
               growth <- diff(size)/head(size,-1)
            }
         }
      }
      res$era <- factor(res$era)
      res$season <- factor(res$season)
      res$lab <- sprintf("Growth rate = %.3f\u00B1%.3f",mean(growth),sd(growth))
     # p5 <- ggplot(res,aes(epoch,size,colour=era))+facet_grid(season~.)+
     #       geom_line()
      p5 <- ggplot(res,aes(epoch,size))+
            geom_line(aes(colour=era,linetype=season))+
            xlab("Epoch")+ylab("Population Size")+
           # scale_y_contionuos()
            ylim(0,max(res$size))+
           # geom_line(size=2)+
            facet_grid(.~lab)+
            p0+
            theme(legend.position="bottom")+
            theme(legend.title=element_blank())+
            guides(colour=guide_legend(nrow=2))+
            guides(linetype=guide_legend(nrow=2))+
            NULL
      if (isShiny)
         removeNotification(id="growth")
   }
   if (isInterbirth | isDenning) {
     # done <- lifestory$id[lifestory$season==9]
     # print(lifestory[ind,])
     # print(lifestory[lifestory$id=="dphefunx",])
     # ind <- which(epoch>=c(0,max.age)[2] & round(epoch*ns) %% ns == 0)
     # pop <- do.call("rbind",lifestory[ind])
      pop <- lifestory[lifestory$epoch>c(0,max.age)[2] &
                      # lifestory$epoch<=max(epoch)-0*4 &
                       lifestory$season == 0 &
                      # lifestory$id %in% done &
                      1,]
      rownames(pop) <- NULL
      if (nrow(pop)) {
         ind <- which(!is.na(pop$parent) & pop$age==0)
         parent <- unique(pop$id[pop$id %in% pop$parent[ind]])
         parent <- sample(parent)
      }
      else {
         parent <- character()
         if (isInterbirth)
            isInterbirth <- FALSE
         if (isDenning)
            isDenning <- FALSE
      }
   }
   if (isInterbirth) {
      if (isShiny)
         showNotification(closeButton=TRUE,"Interbirth interval",id="interbirth"
                         ,duration=99)
      ind <- which(!is.na(pop$parent) & pop$age==0)
      parent <- unique(pop$id[pop$id %in% pop$parent[ind]])
      parent <- sample(parent)
     # ursa:::.elapsedTime("A1")
      reprod.cycle <- unlist(lapply(parent,function(id) {
         ind2 <- which(pop$parent==id & pop$age==0)
         ta <- as.integer(names(table(pop$epoch[ind2])))
         if (length(ta)<2)
            return(NULL)
         diff(ta)
      }))
    #  ursa:::.elapsedTime("A2")
      print(table(reprod.cycle))
      print(summary(reprod.cycle))
      lab <- sprintf("%.2f\u00B1%.2f",mean(reprod.cycle),sd(reprod.cycle))
      p1 <- ggplot(data.frame(v=reprod.cycle,lab=lab),aes(v,stat(density)))+
            geom_histogram(binwidth=1,fill=cs$hist)+
            geom_vline(xintercept=mean(reprod.cycle),col=cs$line)+
            geom_vline(xintercept=median(reprod.cycle),col=cs$line
                      ,linetype=2)+
            scale_x_continuous(breaks=0:100,minor_breaks=NULL)+
            xlab("Interbirth interval, years")+ylab("")+
            facet_grid(.~lab)+
            p0
      if (isShiny)
         removeNotification(id="interbirth")
   }
   if (isDenning) {
      if (isShiny)
         showNotification(closeButton=TRUE,"Lifespan denning",id="denning"
                         ,duration=99)
     # ursa:::.elapsedTime("B1")
      res <- lapply(parent,function(id) {
         if (length(parent)<3)
            print(pop[pop$id==id,])
         ind2 <- which(pop$parent==id & pop$age==0 &
                       pop$epoch<=max(epoch)-4*1)
         ta <- table(pop$epoch[ind2])
         if (length(parent)<3)
            print(pop[ind2,])
         child <- pop$id[ind2]
         if (length(parent)<3)
            print(child)
         pop2 <- pop[pop$id %in% child & pop$age>subad.ini,,drop=FALSE]
         if (!nrow(pop2))
            adults <- 0
         else {
            if (length(parent)<3)
               print(pop2)
            ad <- aggregate(pop2$age,list(id=pop2$id),max)
            if (length(parent)<3)
               print(ad)
            adults <- nrow(ad)
         }
        # print(ta)
         list(dens=length(ta),cubs=sum(ta),adults=adults)
      })
      if (isShiny)
         removeNotification(id="denning")
      if (isShiny)
         showNotification(closeButton=TRUE,"Lifespan fertililty",id="fertility"
                         ,duration=99)
     # ursa:::.elapsedTime("B2")
      if (length(parent)<3)
         str(res)
      res <- do.call("rbind",lapply(res,as.data.frame))
      rownames(res) <- parent
      if (nrow(res)>2) {
         if (FALSE) {
            dens <- res$dens
            cubs <- res$cubs
            adults <- res$adults
            print(table(dens))
            print(table(cubs))
            print(table(adults))
         }
         print(summary(res))
         dens <- rep("",length(res$dens))
         dens[res$dens==1] <- "single"
         dens[res$dens==0] <- "none"
         dens[res$dens>1] <- "multiple"
         tden <- table(dens)
         tden <- round(100*tden/sum(tden),1)
         print(tden)
         adults <- res$adults
         tadult <- table(adults)
         tadult <- round(100*tadult/sum(tadult),1)
         print(tadult)
         res$dens.lab <- sprintf("%.2f\u00B1%.2f Never=%.2f Once=%.2f"
                                ,mean(res$dens),sd(res$dens)
                                ,tden[2]/100,tden[3]/100)
         res$cubs.lab <- sprintf("%.2f\u00B1%.2f Cub Production = %.2f\u00B1%.2f"
                                ,mean(res$cubs),sd(res$cubs)
                                ,mean(natalityRate),sd(natalityRate))
         res$adults.lab <- sprintf("%.2f\u00B1%.2f",mean(res$adults),sd(res$adults))
         p2 <- ggplot(res,aes(dens,stat(density)))+
               geom_histogram(binwidth=1,fill=cs$hist)+
               geom_vline(xintercept=mean(res$dens),col=cs$line)+
               geom_vline(xintercept=median(res$dens),col=cs$line
                         ,linetype=2)+
               scale_x_continuous(breaks=0:100,minor_breaks=NULL)+
               xlab("Dens during lifespan")+ylab("")+
               facet_grid(.~dens.lab)+
               p0
         p3 <- ggplot(res,aes(cubs,stat(density)))+
               geom_histogram(binwidth=1,fill=cs$hist)+
               geom_vline(xintercept=mean(res$cubs),col=cs$line)+
               geom_vline(xintercept=median(res$cubs),col=cs$line
                         ,linetype=2)+
               scale_x_continuous(breaks=0:100,minor_breaks=NULL)+
               xlab("Cubs during lifespan")+ylab("")+
               facet_grid(.~cubs.lab)+
               p0
         p4 <- ggplot(res,aes(adults,stat(density)))+
               geom_histogram(binwidth=1,fill=cs$hist)+
               geom_vline(xintercept=mean(res$adults),col=cs$line)+
               geom_vline(xintercept=median(res$adults),col=cs$line
                         ,linetype=2)+
               scale_x_continuous(breaks=0:100,minor_breaks=NULL)+
               xlab("Survived cubs during lifespan")+ylab("")+
               facet_grid(.~adults.lab)+
               p0
      }
      if (isShiny)
         removeNotification(id="fertility")
   }
   if (isShiny)
      removeModal()
   list(input=input,p0=p0,p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,p6=p6,p7=p7,p8=p8,p9=p9
       ,p10=p10)
}
