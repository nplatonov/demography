'simulate' <- function(max.age=NA,litter=NA,sexratio=NA #,mortality=NA
                      ,init.den=NA,pregnant=NA
                      ,mortality.cub=NA,mortality.adult=NA
                      ,indep.fraction=NA,fertility=NA,k1d=NA,k1i=NA,k2=NA
                      ,seed1=NA,seed2=NA,...) {
  # set.seed(NULL)
  # if (is.na(seed1)) {
  #    seed1 <- sample(100:999,1)
  #    seed2 <- NA
  # }
  # set.seed(seed1)
   birthSuccess <- TRUE
   seasonName <- c('0'="dens release",'1'="dens prerelize",'9'="passed away"
                  ,'8'="broken family")
   init <- randomize(seed1=seed1,seed2=seed2)
  # print(c(seed1=seed1,seed2=seed2))
   if (is.na(max.age))
      max.age <- init$max.age
   if (is.na(litter))
      litter <- init$litter
   if (is.na(sexratio))
      sexratio <- init$sexratio
   if (is.na(init.den))
      init.den <- init$init.den
   if (is.na(pregnant))
      pregnant <- init$pregnant
   if (is.na(mortality.cub))
      mortality.cub <- init$mortality.cub
   if (is.na(mortality.adult))
      mortality.adult <- init$mortality.adult
   if ((anyNA(indep.fraction))||(length(indep.fraction)!=3)) {
      if ((length(indep.fraction)==1)&&(!is.na(indep.fraction)))
         init$indep.fraction[2] <- indep.fraction
      indep.fraction <- init$indep.fraction
   }
   if (is.na(fertility))
      fertility <- init$fertility
   if (is.na(k1d))
      k1d <- init$k1d
   if (is.na(k1i))
      k1i <- init$k1i
   if (is.na(k2))
      k2 <- init$k2
   if (is.na(seed1))
      seed1 <- init$seed1
   if (is.na(seed2))
      seed2 <- init$seed2
   if (sexratio>1)
      sexratio <- sexratio/100
   nepoch <- max.age+50
   input <- list(max.age=max.age,litter=litter,sex=sexratio
                ,dens=init.den,pregn=pregnant
                ,mCOY=mortality.cub,mAdult=mortality.adult,iC1=indep.fraction[2]
                ,indep.fraction=indep.fraction,fert=fertility,k1d=k1d,k1i=k1i,k2=k2
                ,seed1=seed1,seed2=seed2)
   input2prn <- input
   input2prn$indep.fraction <-NULL
   print(data.frame(input2prn))
  # indep.fraction <- c(C0=0.001,C1=indep.C1,C2=0.99) # 0.27 ## Broken families
  # pregnant <- 0.64 #0.63
   p <- litterFraction(litter)
   L <- sum(seq_along(litter)*litter)
   age <- c(0,seq(max.age))
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
   subad.ini <- 3 #subad[1]
   litter <- c(res$df1$litter[1:3],1)
   LF <- cbind(do.call("rbind",lapply(litter,litterFraction)),litter)
   dimnames(LF) <- list(c("C0","C1","C2","C3"),c("p1","p2","p3","L"))
   df1 <- res$df1
   peer0 <- round(init.den*litter)
  # print(c('spring 0+'=peer0+sum(df1$peer),'spring 1+'=sum(df1$peer)))
  # print(head(df1,12),digits=3)
   age1 <- as.integer(age[-1])
   daM <- data.frame(age=age1,peer=NA)
   daM$peer <- round(df1$peer[na.omit(match(daM$age,df1$age))]*(1-sexratio))
   daF <- data.frame(age=age1,fert=NA,peer=NA)
   daF$fert <- fertilityCurve(age1,d=3,s=9,m=10,u=fertility)
   daF$peer <- c(round(df1$peer[na.omit(match(daF$age,df1$age))]*sexratio))
   if (F & birthSuccess) {
      daM0 <- data.frame(age=0,peer=round((df1$peer[1]+df1$lost[1])*(1-sexratio)))
      daF0 <- data.frame(age=0,fert=0,peer=round((df1$peer[1]+df1$lost[1])*sexratio))
      daM <- rbind(daM0,daM)
      daF <- rbind(daF0,daF)
   }
   popF <- data.frame(id=NA,epoch=0L,season=0L,born=NA,sex="F"
                     ,age=rep(daF$age,daF$peer),child=0L,parent=NA)
   popM <- data.frame(id=NA,epoch=0L,season=0L,born=NA,sex="M"
                     ,age=rep(daM$age,daM$peer),child=0L,parent=NA)
   pop <- rbind(popM,popF)
   pop$id <- makeID(nrow(pop))
   base <- as.integer(2018-nepoch)
  # pop$epoch <- 0L
   pop$born <- base-pop$age
   lifestory <- NULL
   ns <- 3
   lifestory <- vector("list",ns*(nepoch+1))
  # names(lifestory) <- format(seq(ns*(nepoch+1))/ns-0.5)
   lifecut <- 1L
   isProgressBar <- TRUE
   isShiny <- ("shiny" %in% loadedNamespaces())
   if (isProgressBar) {
      if (!isShiny)
         pb <- tcltk::tkProgressBar(min=0,max=nepoch,title="Demography simulation")
      else {
         pb <- shiny::Progress$new()
         pb$set(message = "", value = 0)
      }
   }
   for (epoch in c(0L,seq(nepoch))) {
      verbose <- epoch %in% c(0,nepoch-c(1,0))
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
      if (epoch==0) {
         np <- roundAmount(init.den*lf[pattern])
         for (i in seq_along(np)) {
            if (np[i]<1)
               next
            ind <- which(pop$sex=="F" & pop$age>subad.ini & !pop$child)
            ind2 <- if (length(ind)==1) ind else sample(ind,np[i])
            pop$child[ind2] <- -i
         }
      }
     # verbose <- birthSuccess & epoch %in% c(0,nepoch-c(1,0))
      for (a in subad.ini+c(0,1)[-1]) {
         if (length(ind <- which(pop$age==a))) { ## break C3+ families
            parent <- unique(na.omit(pop$parent[ind]))
            if (length(parent)) {
               pop$parent[ind] <- NA
            }
         }
      }
      pop <- repairFamily(pop)
      if (birthSuccess) {
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
           # message("BREAK: population extra lost")
            break
         }
         nF <- length(sexCOY[sexCOY=="F"])
         nM <- length(sexCOY[sexCOY=="M"])
         if (verbose)
            print(table(sexCOY))
         popF0 <- data.frame(id=NA,epoch=epoch,season=0L,born=base+epoch,sex="F"
                            ,age=rep(0L,nF),child=0L,parent=NA)
         popM0 <- data.frame(id=NA,epoch=epoch,season=0L,born=base+epoch,sex="M"
                            ,age=rep(0L,nM),child=0L,parent=NA)
        # print(nrow(pop))
         pop0 <- rbind(popM0,popF0)
         pop0$id <- makeID(nrow(pop0))
         pop <- rbind(pop0,pop)
         ind <- which(pop$child<0)
        # pop3 <- pop
        # str(c(c=ind))
         for (ind2 in sample(ind)) {
            pop2 <- pop[ind2,]
            ind3 <- which(pop$age==0 & is.na(pop$parent))
            if (!length(ind3)) {
               pop$child[ind2] <- 0L
               next
            }
            pop$child[ind2] <- abs(pop$child[ind2])
            if (length(ind3)>=pop$child[ind2])
               ind3 <- sample(ind3,pop$child[ind2])
            else
               pop$child[ind2] <- length(ind3)
            pop$parent[ind3] <- pop$id[ind2]
         }
      }
      if (!birthSuccess) {
         ind <- which(pop$sex=="F" & !pop$child) ## pri
        # ind <- which(!(pop$id[pop$sex=="F"] %in% pop$parent)) ## alt
         peer <- table(pop$age[ind])
         daF$peer <- 0
         daF$peer[match(as.integer(names(peer)),daF$age)] <- as.integer(peer)
         ageF <- daF$age[daF$fert>0]
         indS <- which(pop$sex=="F" & pop$age %in% ageF & !pop$child) ## pri
        # indS <- which(pop$id %in% pop$parent[pop$parent %in% pop$id]) ## alt
         if (FALSE)
            parents <- init.den
         else if (epoch<c(2,max.age)[1]) { ## epoch<1 epoch<max.age
            parents <- init.den
            if (!FALSE) {
               available <- roundAmount(sum(daF$peer[daF$fert>0])*c(1,pregnant)[2])
              # print(c(available=available,parents=parents))
               if (available<parents)
                  parents <- available
            }
         }
         else {
           # parents <- init.den ## temporal assign
            parents <- roundAmount(length(indS)*pregnant)
         }
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
         peer0 <- sum(np*seq_along(np))
         if ((FALSE)&&((epoch>=max.age)&&(peer0>init.den*14))) {
            message("BREAK: population extra gain")
            break
         }
         if (TRUE) {
            if ((FALSE)&&(epoch<max.age)) {
               dens <- sum(np)
              # print(dens)
               rate <- init.den/dens
               if (rate<0.95) {
                  n1 <- nrow(pop)
                  n2 <- round(n1*rate)
                  print(c(n1=n1,n2=n2))
                  pop <- pop[sample(seq(n1),n2),]
                 # pop <- repairFamily(pop)
               }
              # if (rate>1.05) {
                 # n1 <- 
              # }
            }
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
              # message("BREAK: population extra lost")
               break
            }
         }
         else { ## less randomize
            a <- round(peer0*(1-sexratio))
            sexCOY <- sample(c(rep("F",peer0-a),rep("M",a)))
         }
         nF <- length(sexCOY[sexCOY=="F"])
         nM <- length(sexCOY[sexCOY=="M"])
         if (verbose)
            print(table(sexCOY))
         popF0 <- data.frame(id=NA,epoch=epoch,season=0L,born=base+epoch,sex="F"
                            ,age=rep(0L,nF),child=0L,parent=NA)
         popM0 <- data.frame(id=NA,epoch=epoch,season=0L,born=base+epoch,sex="M"
                            ,age=rep(0L,nM),child=0L,parent=NA)
        # print(nrow(pop))
         pop0 <- rbind(popM0,popF0)
         pop0$id <- makeID(nrow(pop0))
         pop <- rbind(pop0,pop)
        # print(nrow(pop))
         birth <- vector("list",3)
         names(birth) <- paste0(seq_along(birth),"C")
         nrep <- round(10*daF$peer*daF$fert)
        # s0 <- with(daF,rep(age,round(100*fert*peer)))
         peer0 <- daF$peer
         children <- sum(seq_along(np)*np)
         if (children!=nrow(pop0)) {
            print(epoch)
            print(c(expected=children,fact=nrow(pop0)))
            q()
         }
         if (verbose & F) {
            print(daF[daF$fert>0,])
         }
         toBreak <- FALSE
         for (i in rev(seq_along(birth))) {
            b <- rep(NA,np[i])
            for (k in seq_len(np[i])) {
               for (u in c(1,2,3,4,5)) { ## c(1,2,3,4,5)
                  if (u>1)
                     print(c(u=u))
                  s <- with(daF,round(rep(age,(10^u)*peer*fert)))
                  if (length(s))
                     break
               }
               j <- try(sample(seq_along(s),1))
               toBreak <- (inherits(j,"try-error"))
               if (toBreak)
                  break
               ind <- which(daF$age==s[j])
               daF$peer[ind] <- daF$peer[ind]-1
               b[k] <- s[j]
            }
            if (toBreak)
               break
            birth[[i]] <- b
         }
         if (verbose & F)
            str(birth)
         if (toBreak) {
            if (FALSE) {
               print(c(epoch=epoch))
               print(c(litter=i,age=k,children=children,parents=parents))
               print(np)
               print(c(k=k))
               print(u)
               print(s)
               print(daF)
            }
            message("Not ehough fertile females")
            break
         }
        # str(birth)
        # str(pop[pop$age==0,])
         for (i in sample(seq_along(birth))) { ## i - litter size
            b <- birth[[i]]
            for (j in sample(seq_along(b))) { # b[j] - это возраст
               ind <- which(pop$sex=="F" & pop$age==b[j] & !pop$child) ## pri
              # indP <- which(pop$sex=="F" & pop$age==b[j]) ## alt1
              # ind <- indP[!(pop$id[indP] %in% pop$parent)] ## alt2
               if (!length(ind)) { ## no free females with such age
                  message("no free females with such age")
                  print(c(children=i,parent=j,age=b[j],ind=ind,ind3=ind3))
                  next ## skip; reduce breeding rate
               }
               if (length(ind)>1)
                  ind <- sample(ind,1)
               pop$child[ind] <- i
               ind2 <- which(pop$age==0 & is.na(pop$parent))
               if (length(ind2)>1)
                  ind2 <- sample(ind2,i)
               pop$parent[ind2] <- pop$id[ind] ## <- ind
               ind3 <- which(pop$sex=="M" & pop$child>0)
               if (length(ind3)) {
                  cat("===============================================\n")
                  print(which(pop$sex=="F" & pop$age==b[j] & !pop$child))
                  print(c(childs=i,parent=j,age=b[j],ind=ind,ind3=ind3))
                  print(pop[ind,])
                  print(pop[ind3,])
                  cat("===============================================\n")
                  stop("Transgenders are not supported")
               }
            }
         }
         if (verbose) {
           # доля бездетных, доля с годовиками, доля с двухлетками, доля с сеголет.
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
      } ## !birthSuccess
      pop$season <- 0L
      lifestory[[lifecut]] <- pop
      lifecut <- lifecut+1L
      pop$age <- pop$age+1L
      ind <- which(pop$age>max.age)
      if (length(ind))
         pop$season[ind] <- 9L
      pop <- repairFamily(pop)
      for (i in head(seq_along(mortality),subad.ini)) { ## break family
         ind1 <- which(pop$id %in% pop$parent[pop$age==i & !is.na(pop$parent)])
         n1 <- length(ind1)
         if (!n1)
            next
         n2 <- roundAmount(unname(indep.fraction[i])*n1)
         if (!n2)
            next
         ind2 <- if ((n1==1)||(n2<1)) ind1 else sample(ind1,n2)
         if (!length(ind2))
            next
         if ((verbose)&&(i==3))
            print(c(i=i,n1=n1,n2=n2,n=length(ind2)))
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
      if (birthSuccess) { ## mating
         ind <- which(pop$sex=="F" & !pop$child)
         peer <- table(pop$age[ind])
         daF$peer <- 0
         daF$peer[match(as.integer(names(peer)),daF$age)] <- as.integer(peer)
         ageF <- daF$age[daF$fert>0]
         indS <- which(pop$sex=="F" & pop$age %in% ageF &
                       !pop$child & is.na(pop$parent))
         if (FALSE)
            parents <- init.den
         else if (epoch<c(2,max.age)[1]) { ## epoch<1 epoch<max.age
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
         nrep <- round(10*daF$peer*daF$fert)
        # s0 <- with(daF,rep(age,round(100*fert*peer)))
         toBreak <- FALSE
         for (i in rev(seq_along(birth))) {
            b <- rep(NA,np[i])
            for (k in seq_len(np[i])) {
               for (u in c(1,2,3,4,5)) { ## c(1,2,3,4,5)
                  if (u>1)
                     print(c(u=u))
                  s <- with(daF,round(rep(age,(10^u)*peer*fert)))
                  if (length(s))
                     break
               }
               j <- try(sample(seq_along(s),1))
               toBreak <- (inherits(j,"try-error"))
               if (toBreak)
                  break
               ind <- which(daF$age==s[j])
               daF$peer[ind] <- daF$peer[ind]-1
               b[k] <- s[j]
            }
            if (toBreak)
               break
            birth[[i]] <- b
         }
         if (toBreak) {
            message("Not ehough fertile females")
            break
         }
        # print(birth)
        # str(pop[pop$age==0,])
         for (i in sample(seq_along(birth))) { ## i - litter size
            b <- birth[[i]]
            for (j in sample(seq_along(b))) { # b[j] - это возраст
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
         if (verbose) {
           # доля бездетных, доля с годовиками, доля с двухлетками, доля с сеголет.
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
      } ## birthSuccess
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
      pop1 <- pop[pop$season %in% c(8L,9L),]
      lifestory[[lifecut]] <- pop1
      lifecut <- lifecut+1L
      pop <- pop[pop$season!=9L,]
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
      if (size.ls>25) {
        # nepoch <- epoch+2
         break
      }
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
   liferow <- unname(sapply(lifestory,nrow))
   LS <- pop[rep(1,sum(liferow)),]
   k1 <- 1
   for (i in seq_along(liferow)) {
      k2 <- k1+liferow[i]-1
     # print(c(i=i,r=liferow[i],cr=sum(liferow[seq(i)]),k1=k1,k2=k2))
      LS[k1:k2,] <- lifestory[[i]]
      k1 <- k2+1
   }
   rownames(LS) <- NULL
   list(input=input,output=LS)
}
