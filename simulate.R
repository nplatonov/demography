'simulate' <- function(history=NULL,max.age=NA,litter=NA,sexratio=NA #,mortality=NA
                      ,init.den=NA,pregnant=NA
                      ,mortality.cub=NA,mortality.adult=NA
                      ,indep.fraction=NA,fertility=NA
                      ,removal.rate=NA,removal.age=NA
                      ,k1d=NA,k1i=NA,k2=NA
                      ,seed1=NA,seed2=NA,seed3=NA,era.length=c(75L,15L),output.size=90
                      ,quiet=FALSE,check=FALSE,...) {
   msg <- "Ready for analysis"
   if (F) {
      list1 <- list(...)
      list2 <- as.list(match.call())
     # print(names(list1))
     # print(names(list2))
      ind <- match(names(list1),names(list2))
      list3 <- list2[-ind]
      str(list3)
   }
   isShiny <- (("shiny" %in% loadedNamespaces())&&(length(shiny::shinyOptions())>0))
   if ((isShiny)&&(!quiet)) {
      showModal(modalDialog(title = "Simulation in progress","Please wait"
                       ,size="s",easyClose = TRUE,footer = NULL))
      on.exit(removeModal())
   }
   eraLength <- rep(era.length,length=2) # EL: max.age(Initialization)+EL(Realization)+EL(Control/Update)
  # set.seed(NULL)
  # if (is.na(seed1)) {
  #    seed1 <- sample(100:999,1)
  #    seed2 <- NA
  # }
  # set.seed(seed1)
   seasonName <- c('0'="dens release",'1'="dens prerelease",'9'="passed away"
                  ,'8'="broken family",'7'="human-caused removal")
   update <- !is.null(history)
   if (FALSE) {
      if (isTRUE(seed3>0))
         init$seed3 <- seed3
      else {
         set.seed(NULL)
         seed3 <- sample(100:999,1)
         print(c('FORCED seed3'=seed3))
         init$seed3 <- seed3
      }
   }
  # set.seed(init$seed2)
   if (update) {
     # str(history)
      init <- history$input
     # set.seed(init$seed3) ## here2 (seed2 or seed3)
   }
   else {
     # print(data.frame(from="simulate",seed1=seed1,seed2=seed2,seed3=seed3))
      init <- randomize(seed1=seed1,seed2=seed2,seed3=seed3)
   }
   prm <- as.list(match.call())[-1]
  # str(prm)
  # str(init)
   init <- updatePrm(init,prm)
  # str(init)
   if (!update)
      set.seed(init$seed2)
   ##~ print(c('prm$seed3'=seed3,'init$seed3'=init$seed3
          ##~ ,'prm$seed2'=seed2,'init$seed2'=init$seed2))
  # cat("init:\n")
   for (pass in names(init)) {
      assign(pass,init[[pass]])
   }
  # str(litterF)
   if ("litterF" %in% names(init))
      rm(litterF) ## NOT IMPLEMENTED but TODO
   if (check) {
      curv <- curveInputs(indep.mortality=indep.mortality
                         ,mortality.cub=mortality.cub
                         ,mortality.adult=mortality.adult
                         ,init.den=init.den
                         ,litter=litter
                         ,broken.C1=indep.fraction[2]
                         ,broken.C2=indep.fraction[3]
                         ,max.age=max.age
                         ,pregnant=pregnant
                         ,sexratio=sexratio
                         ,seed1=seed1
                         ,seed2=seed2
                         ,seed3=seed3
                         ,fertility=fertility
                         ,removal.rate=removal.rate
                         ,removal.age=removal.age
                         ,k1d=k1d
                         ,k1i=k1i
                         ,k2=k2
                         )
      return(curv)
   }
   max.age <- as.integer(round(max.age))
   nepoch <- ifelse(update,eraLength[2],max.age+sum(eraLength))
  # print(data.frame(update=update,nepoch=nepoch))
   input2 <- list(mA=max.age,ltr=litter,sex=sexratio
                 ,dens=init.den,pregn=pregnant
                 ,mCOY=mortality.cub,mAdlt=mortality.adult
                 ,wC1=indep.fraction[2],wC2=indep.fraction[3]
                # ,indep.fraction=indep.fraction
                 ,fert=fertility,remR=removal.rate,remA=removal.age
                 ,k1d=k1d,k1i=k1i,k2=k2,rnd1=seed1,rnd2=seed2,rnd3=seed3)
   input2prn <- input2
  # input2prn$indep.fraction <- NULL
   input2prn$sex <- NULL # 0.5 or close
   input2prn$dens <- NULL # stable (100)
   if (!quiet)
      print(data.frame(input2prn))
  # indep.fraction <- c(C0=0.001,C1=broken.C1,C2=0.99) # 0.27 ## Broken families
  # pregnant <- 0.64 #0.63
   p <- litterFraction(litter)
   age <- c(0,seq(max.age))
   if (devRemoval <- FALSE) {
      p <- removalCurve(as.integer(age[-1]),u=removal.age,plot=TRUE)
      print(p)
      q()
   }
   mortality <- mortalityTube(max.age=max.age,mortality.cub=mortality.cub
                             ,mortality.adult=mortality.adult
                             ,k1d=k1d,k1i=k1i,k2=k2)
   indep.mortality <- mortality$indep
   mortality <- mortality$depend
   if (FALSE) {
      print(c(mortality[1:2],min(mortality)))
      print(k1)
      q()
   }
   subad.ini <- 3 #subad[1]
   age1 <- as.integer(age[-1])
   daM <- data.frame(age=age1,peer=NA)
   daF <- data.frame(age=age1,fert=NA,peer=NA)
   daF$fert <- fertilityCurve(age1,u=fertility)
  # print(daF)
  # q()
   daR <- data.frame(age=age1,vuln=removalCurve(age1,u=removal.age),peer=0)
   if (!update) {
      checkIS <- !TRUE
      if (!checkIS)
         sink("nul")
      res <- initialState(mortality=mortality*0.99,init.den=init.den
                     ,litter=litter
                     ,max.age=max.age,sigma=0,removal.age=c(4,20)
                     ,reprod.age=c(6,max.age-5),sexratio=sexratio
                     ,reprod.cycle=3.2,removal=0*c(0.045,0.03)[2]
                     ,subad=3:max.age,equilibrium=!TRUE,toRound=TRUE)
      if (!checkIS)
         sink()
      litter <- c(res$df1$litter[1:3],1)
      LF <- cbind(do.call("rbind",lapply(litter,litterFraction)),litter)
      dimnames(LF) <- list(c("C0","C1","C2","C3"),c("p1","p2","p3","L"))
      df1 <- res$df1
      peer0 <- round(init.den*litter)
     # print(c('spring 0+'=peer0+sum(df1$peer),'spring 1+'=sum(df1$peer)))
     # print(head(df1,12),digits=3)
      daM$peer <- round(df1$peer[na.omit(match(daM$age,df1$age))]*(1-sexratio))
      daF$peer <- c(round(df1$peer[na.omit(match(daF$age,df1$age))]*sexratio))
      popF <- data.frame(id=NA,epoch=0L,era="",season=0L,born=NA,sex="F"
                        ,age=rep(daF$age,daF$peer),child=0L,parent=NA)
      popM <- data.frame(id=NA,epoch=0L,era="",season=0L,born=NA,sex="M"
                        ,age=rep(daM$age,daM$peer),child=0L,parent=NA)
      pop <- rbind(popM,popF)
      pop$id <- makeID(nrow(pop))
      base <- as.integer(2018-nepoch)
     # pop$epoch <- 0L
      pop$born <- base-pop$age
      seqepoch <- c(0L,seq(nepoch))
   }
   else {
      lifestory <- history$output
      lifestory <- lifestory[lifestory$era=="R",]
      epoch0 <- max(lifestory$epoch)
      if (epoch0<max(lifestory$age)+1L*eraLength[1]) { ## max(lifestory$age)+15L
         return(list(input=init,output=NULL))
      }
      pop <- lifestory[lifestory$epoch==epoch0 & lifestory$season==1,]
      popInclude <- lifestory[lifestory$epoch==epoch0 & lifestory$season==0,]
      pop$epoch <- pop$epoch+1L
     # np <- -pop$child[pop$child<0]
     # str(np)
     # print(table(np))
     # q()
      base <- max(pop$born)
      seqepoch <- c(seq(nepoch))
      LF <- matrix(NA,ncol=4,nrow=4
                  ,dimnames=list(c("C0","C1","C2","C3"),c("p1","p2","p3","L")))
      LF["C0",] <- c(init$litterF,init$litter)
   }
   ns <- 3
   lifestory <- vector("list",ns*(nepoch+1-as.integer(update)))
  # names(lifestory) <- format(seq(ns*(nepoch+1))/ns-0.5)
   lifecut <- 1L
   isProgressBar <- !quiet
   if (isProgressBar) {
      if (!isShiny)
         pb <- tcltk::tkProgressBar(min=0,max=nepoch,title="Demography simulation")
      else {
         pb <- shiny::Progress$new()
         pb$set(message = "", value = 0)
      }
   }
   for (epoch in seqepoch) {
      if (!update) {
        # if (epoch==1) {
        #    set.seed(init$seed2)
        # }
         if (epoch==max.age+1L*eraLength[1]+1L) {
           # print(c(epoch=epoch,'REAL->CONTROL set seed3'=init$seed3))
            set.seed(init$seed3)
         }
      }
      else if ((update)&&(epoch==1L)) {
        # print(c(epoch=epoch,'UPDATE CONTROL set seed3'=init$seed3))
         set.seed(init$seed3)
      }
     # ursa:::.elapsedTime("A")
      verbose <- !quiet & epoch %in% c(tail(seqepoch,1))
      if (isProgressBar) {
         if (!isShiny)
            tcltk::setTkProgressBar(pb,epoch,label=paste("Epoch",epoch))
         else
            pb$inc(epoch/nepoch, detail = paste("Epoch",epoch))
      }
      if (verbose)
         cat("-----------","epoch:",epoch,"-----------","\n")
      pop <- repairFamily(pop)
      lf <- LF["C0",]
      pattern <- grep("^p\\d+",names(lf))
      if ((!update)&&(epoch==0)) {
         np <- roundAmount(init.den*lf[pattern])
         for (i in seq_along(np)) {
            if (np[i]<1)
               next
            ind <- which(pop$sex=="F" & pop$age>subad.ini & !pop$child)
            ind2 <- if (length(ind)==1) ind else sample(ind,np[i])
            pop$child[ind2] <- -i
         }
      }
      else {
         np <- c(p1=0,p2=0,p3=0)
         np2 <- table(-pop$child[pop$child<0])
         if (length(ind <- as.integer(names(np2))))
            np[ind] <- np2
      }
      for (a in subad.ini+c(0,1)[-1]) {
         if (length(ind <- which(pop$age==a))) { ## break C3+ families
            parent <- unique(na.omit(pop$parent[ind]))
            if (length(parent)) {
               pop$parent[ind] <- NA
            }
         }
      }
      pop <- repairFamily(pop)
      peer0 <- sum(np*seq_along(np))
      repeat({
         sexCOY <- sample(rep(c("F","M"),round(c(sexratio,1-sexratio)*(peer0*10)))
                         ,peer0)
         if (length(sexCOY)<2) {
            tCOY <- integer()
            break
         }
         tCOY <- table(sexCOY)
         if (length(table(sexCOY))==2)
            break
      })
      if (sum(tCOY)<3) {
         msg <- "Population extra lost"
         break
      }
      nF <- length(sexCOY[sexCOY=="F"])
      nM <- length(sexCOY[sexCOY=="M"])
      if (verbose)
         print(table(sexCOY))
      newera <- ifelse(update,epoch0+epoch,epoch)
      popF0 <- data.frame(id=NA,epoch=newera,era="",season=0L,born=base+epoch
                         ,sex="F",age=rep(0L,nF),child=0L,parent=NA)
      popM0 <- data.frame(id=NA,epoch=newera,era="",season=0L,born=base+epoch
                         ,sex="M",age=rep(0L,nM),child=0L,parent=NA)
     # print(nrow(pop))
      pop0 <- rbind(popM0,popF0)
      pop0$id <- makeID(nrow(pop0))
      pop <- rbind(pop0,pop)
      ind <- which(pop$child<0)
     # pop3 <- pop
     # ursa:::.elapsedTime("B")
      for (ind2 in sample(ind)) {
         ind3 <- which(pop$age==0 & is.na(pop$parent))
         if (!length(ind3)) {
            if (!isShiny)
               message("Lost children")
            pop$child[ind2] <- 0L
            next
         }
         pop$child[ind2] <- abs(pop$child[ind2])
         if (length(ind3)>=pop$child[ind2]) {
            if (length(ind3)>1)
               ind3 <- sample(ind3,pop$child[ind2])
         }
         else {
            pop$child[ind2] <- length(ind3)
         }
         pop$parent[ind3] <- pop$id[ind2]
      }
     # ursa:::.elapsedTime("C")
      pop$season <- 0L
      lifestory[[lifecut]] <- pop
      lifecut <- lifecut+1L
      pop$age <- pop$age+1L
      ind <- which(pop$age>max.age)
      if (length(ind))
         pop$season[ind] <- 9L
      pop <- repairFamily(pop)
      if (verbose) {
         cat("---------------------------------------------- 20190907\n")
         print(table(pop$season))
         print(c(subad=subad.ini))
         print(head(table(pop$age)))
         
      }
      for (i in head(seq_along(mortality),subad.ini)) { ## break family
         ind1 <- which(pop$id %in% pop$parent[pop$age==i & !is.na(pop$parent)])
         n1 <- length(ind1)
         if (!n1)
            next
         n2 <- roundAmount(unname(indep.fraction[i])*n1)
        # n2 <- roundAmount(ifelse(i==3,1,0)*n1) ## dummy
         if (verbose)
            print(data.frame(i=i,indFraction=indep.fraction[i],n1=n1,n2=n2))
         if (i==1) {
            fr <- mortality[1]*2/12*1/3 ## 2/12: May-June, 1/3 late mate prob
            remate <- roundAmount(fr*n1)
            n2 <- n2+remate
            if (verbose) {
               print(data.frame(parents=n1,broken=n2
                               ,remate_fraction=fr,broken_with_remate=remate))
            }
         }
         if (!n2)
            next
         ind2 <- if ((n1==1)||(n2<1)) ind1 else sample(ind1,n2)
         if (!length(ind2))
            next
         if ((verbose)&&(i==3))
            print(c(i=i,n1=n1,n2=n2,n=length(ind2)))
         if (verbose)
            nchild <- sum(pop$child[ind2])
         pop$child[ind2] <- 0L
         ind3 <- which(pop$parent %in% pop$id[ind2])
         pop$parent[ind3] <- NA
        # pop <- repairFamily(pop)
         if ((verbose)&&(i==-3)) {
            print(nchild)
           # ind6 <- ind1[which(is.na(match(ind1,ind2)))]
           # print(pop[ind6,])
            ind4 <- which(pop$id %in% pop$parent[pop$age==i & !is.na(pop$parent)])
            print(pop[ind4,])
            ind5 <- which(pop$age==i & !is.na(pop$parent))
            print(pop[ind5,])
           # print(pop[ind1,])
            pop <- repairFamily(pop,verbose=TRUE)
            ind5 <- which(pop$age==i & !is.na(pop$parent))
            print(pop[ind5,])
            q()
         }
      }
      if (verbose)
         cat("---------------------------------------------- 20190907\n")
     ## mating
      ind <- which(pop$sex=="F" & !pop$child)
      peer <- table(pop$age[ind])
      daF$peer <- 0
      indF <- match(as.integer(names(peer)),daF$age)
     # daF$peer[match(as.integer(names(peer)),daF$age)] <- as.integer(peer)
      daF$peer[na.omit(indF)] <- as.integer(peer[which(!is.na(indF))])
      ageF <- daF$age[daF$fert>0]
      indS <- which(pop$sex=="F" & pop$age %in% ageF &
                    !pop$child & is.na(pop$parent))
      if (FALSE)
         parents <- init.den
      else if ((!update)&&(epoch<c(2,max.age)[1])) { ## epoch<1 epoch<max.age
         parents <- init.den
         if (!FALSE) {
            available <- roundAmount(sum(daF$peer[daF$fert>0])*pregnant)
           # print(c(available=available,parents=parents))
            if (available<parents)
               parents <- available
         }
      }
      else {
        # parents <- init.den ## temporal assign
         parents <- roundAmount(length(indS)*pregnant)
      }
     # print(parents)
      if ((!FALSE)&&(verbose)) {
        # str(c(na.omit(pop$parent[which(pop$age==3)])))
         P0 <- length(na.omit(unique(pop$parent[which(pop$age==0)])))
         P1 <- length(na.omit(unique(pop$parent[which(pop$age==1)])))
         P2 <- length(na.omit(unique(pop$parent[which(pop$age==2)])))
         P3 <- length(na.omit(unique(pop$parent[which(pop$age==3)])))
         P4 <- length(na.omit(unique(pop$parent[which(pop$age==4)])))
         print(c(P0=P0,P1=P1,P2=P2,P3=P3,P4=P4))
      }
     # indP <- which(pop$child>0)
     # print(pop[pop$parent %in% pop$id,])
      if (verbose) {
         popC <- unique(pop[pop$parent %in% pop$id,c("age","parent")])
         ta <- table(popC$age)
         tc <- c(S=length(indS),F=length(indS)-parents,P=parents,C=sum(ta),ta)
        # cat("-------------------------------\n")
        # print(c(epoch=epoch))
         print(tc)
         print(tc[1:2]/sum(tc[1:2]))
        # cat("-------------------------------\n")
      }
      np <- roundAmount(parents*lf[pattern])
      birth <- vector("list",3)
      names(birth) <- paste0(seq_along(birth),"C")
     # nrep <- round(10*daF$peer*daF$fert) ## deprecated
     # s0 <- with(daF,rep(age,round(100*fert*peer)))
      toBreak <- FALSE
     # ursa:::.elapsedTime("D")
      for (i in rev(seq_along(birth))) {
         b <- rep(NA,np[i])
         for (k in seq_len(np[i])) {
            for (u in c(1,2,3,4,5)) { ## c(1,2,3,4,5)
               if ((u>10000001)&&(!isShiny))
                  print(c(u=u))
               s <- with(daF,round(rep(age,(10^u)*peer*fert)))
               if (length(s))
                  break
            }
            j <- .sample(seq_along(s),1)
            if (!length(j)) {
               toBreak <- TRUE
               break
            }
            ind <- which(daF$age==s[j])
            daF$peer[ind] <- daF$peer[ind]-1
            b[k] <- s[j]
         }
         if (toBreak)
            break
         birth[[i]] <- b
      }
      if (toBreak) {
         message("Not enough fertile females")
        # break
      }
     # ursa:::.elapsedTime("E")
     # print(birth)
     # str(pop[pop$age==0,])
      for (i in sample(seq_along(birth))) { ## i - litter size
         b <- birth[[i]]
         for (j in sample(seq_along(b))) { # b[j] - this is age
            ind <- which(pop$sex=="F" & pop$age==b[j] &
                         !pop$child)# & is.na(pop$parent))
            if (!length(ind)) { ## no free females with such age
               message("no free females with such age")
               print(c(children=i,parent=j,age=b[j],ind=ind,ind3=ind3))
               next ## skip; reduce breeding rate
            }
            if (length(ind)>1)
               ind <- sample(ind,1)
            pop$child[ind] <- -i
           # ind2 <- which(pop$age==0 & is.na(pop$parent))
           # if (length(ind2)>1)
           #    ind2 <- sample(ind2,i)
           # pop$parent[ind2] <- pop$id[ind] ## <- ind
         }
      }
     # ursa:::.elapsedTime("F")
      if (verbose) {
        # a portion wihtouut cubs, a portion with yearlinfs, a portion with 2yrold, a portion with COYs
         ageF <- daF$age[daF$fert>0]
         indS <- which(pop$sex=="F" & pop$age %in% ageF & !pop$child)
         indP <- which(pop$child>0)
         popC <- unique(pop[pop$parent %in% pop$id,c("age","parent")])
        # print(pop[pop$parent %in% pop$id,])
         tc <- c(S=length(indS),table(popC$age))
        # print(c(epoch=epoch))
         print(tc)
        # message(paste("epoch:",epoch))
      }
      if (verbose) {
         popC0 <- pop[!is.na(pop$parent) & pop$age==0,]
         if (FALSE) {
            print(table(popC0$sex))
            print(popC0)
            q()
         }
         F0 <- table(popC0$parent)
            print(cbind(data.frame(LF["C0",,drop=FALSE])
                       ,F=length(F0),C=sum(F0)))
      }
     # p <- unname(table(as.integer(F0)))
     # p <- round(p/sum(p),3)
      for (i in sample(tail(seq_along(mortality),-subad.ini))) { ## mort to adult
         ind1 <- which(pop$age==i)
         n1 <- length(ind1)
         if (!n1)
            next
         n2 <- roundAmount(n1*mortality[i])
         if (!n2)
            next
         ind2 <- if ((length(ind1)>1)&&(n2>0)) sample(ind1,n2) else ind1
         pop$season[ind2] <- 9L
      }
      for (i in head(seq_along(mortality),subad.ini)) { ## mort to independent 
         ind1 <- which(pop$age==i & is.na(pop$parent)) 
         n1 <- length(ind1)
         if (!n1)
            next
         n2 <- roundAmount(n1*unname(indep.mortality[i]))
         if (!n2)
            next
         ind2 <- if ((length(ind1)>1)&&(n2>0)) sample(ind1,n2) else ind1
         if (!length(ind2))
            next
         pop$season[ind2] <- 9L
      }
      for (i in head(seq_along(mortality),subad.ini)) { ## mort to dependent
         ind1 <- which(pop$age==i & !is.na(pop$parent))
         n1 <- length(ind1)
         if (!n1)
            next
         n2 <- roundAmount(n1*unname(mortality[i]))
         if (!n2)
            next
         ind2 <- if ((length(ind1)>1)&&(n2>0)) sample(ind1,n2) else ind1
         if (!length(ind2))
            next
         pop$season[ind2] <- 9L
         pop$parent[ind2] <- NA
      }
      if ((removal.rate>0)&&(epoch>3)) { ## human caused removal
         n0 <- nrow(pop)
         sRatio <- c(F=1,M=2)
         removal <- roundAmount(nrow(pop)*removal.rate*sRatio/sum(sRatio))
        # message(paste("------- epoch:",epoch,"--------"))
         for (sex in c("F","M")) {
           # if ((epoch==40)&&(sex=="F")) {
           #    stop("HERE")
           # }
            popS <- pop[pop$sex==sex & is.na(pop$parent) & !(pop$season %in% c(9L)),]
            peerS <- table(popS$age)
            daR$peer <- 0L
            daR$peer[match(as.integer(names(peerS)),age1)] <- as.integer(peerS)
            lottery <- integer()
            for (i in seq_len(removal[sex])) {
               participant <- with(daR,rep(age,round(10*peer*vuln)))
               if (!length(participant))
                  next
               else if (length(participant)==1)
                  dropped <- participant
               else
                  dropped <- sample(participant,1)
               lottery <- c(lottery,dropped)
               daR$peer[dropped] <- daR$peer[dropped]-1L
            }
            lottery <- table(lottery)
            lotteryAge <- as.integer(names(lottery))
            lottery <- as.integer(lottery)
            fail <- character()
            for (i in seq_along(lottery))
               fail <- c(fail,sample(popS$id[popS$age==lotteryAge[i]],lottery[i]))
            ind <- sort(which(pop$id %in% fail))
            pop$season[ind] <- 7L
           # pop$parent[ind] <- NA
         }
      }
      pop1 <- pop[pop$season %in% c(7L,8L,9L),]
      lifestory[[lifecut]] <- pop1
      lifecut <- lifecut+1L
      pop <- pop[!(pop$season %in% c(7L,9L)),]
      pop <- repairFamily(pop)
      pop$season <- 1L
      lifestory[[lifecut]] <- pop
      lifecut <- lifecut+1L
      pop$epoch <- pop$epoch+1L
      if (F & verbose)
         print(sum(pop$child))
      for (a in c(1,2)) {
         rname <- paste0("C",a)
         pop1 <- pop[pop$age==a,]
         parent <- table(pop1$parent)
         lf <- table(parent)
         if ((a==-3)&&(verbose)) {
            cat("--parent--\n")
            str(parent)
            print(lf)
            cat("----------\n")
         }
         lf <- as.numeric(round(lf/sum(lf),3))
         while(length(lf)<3) lf <- c(lf,0)
        # LF <- rbind(LF,cbind(lf[1],lf[2],lf[3],sum(seq_along(lf)*lf)))
         LF[rname,] <- cbind(lf[1],lf[2],lf[3],sum(seq_along(lf)*lf))
        # rownames(LF)[nrow(LF)] <- paste0("C",a)
         if (verbose) {
            print(cbind(data.frame(LF[rname,,drop=FALSE])
                       ,F=length(parent),C=sum(parent)))
         }
      }
      if (verbose)
         print(nrow(pop))
      size.ls <- c(object.size(lifestory)*2^(-20))
     # print(c(size=size.ls))
      if ((isShiny & size.ls>output.size) | (!isShiny & size.ls>output.size)) {
        # nepoch <- epoch+2
         msg <- "Population extra growth"
         break
      }
      if (toBreak)
         break
     # if (epoch==3)
     #    break
   }
   if (isProgressBar) {
      if (!isShiny)
         close(pb)
      else
         pb$close()
   }
   lifestory <- lifestory[!sapply(lifestory,is.null)]
   if (!length(lifestory)) {
      return(list(input=init,output=NULL))
   }
   liferow <- unname(sapply(lifestory,nrow))
   LS <- pop[rep(1,sum(liferow)),]
   k1 <- 1
   for (i in seq_along(liferow)) {
      k2 <- k1+liferow[i]-1
     # print(c(i=i,r=liferow[i],cr=sum(liferow[seq(i)]),k1=k1,k2=k2))
      LS[k1:k2,] <- lifestory[[i]]
      k1 <- k2+1
   }
   LS$era <- "C"
   if (!update) {
     # print(c('before control'=max.age+1L*eraLength))
      LS$era[LS$epoch<=max.age+1L*eraLength[1]] <- "R"
      LS$era[LS$epoch<=max.age] <- "I"
   }
   else
      LS <- rbind(popInclude,LS)
   rownames(LS) <- NULL
   list(input=init,output=LS,message=msg)
}
