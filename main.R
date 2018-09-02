options(stringsAsFactors=FALSE)
require(ggplot2)
'fert' <- function(x,d=7,s=7,m=11.5)
{
   r1 <- x-m
   r2 <- (s+d)+(s-d)*tanh(r1)
   y <- exp(-r1*r1/(0.5*r2*r2))
   y
}
'fertilityCurve' <- function(age,d=3,s=9,m=10,u=1,plot=FALSE)
{
   r1 <- age-m
  # d <- d*40/length(age)
   r2 <- (s+d)+(s-d)*tanh(r1)
   fert <- exp(-r1*r1/(0.5*r2*r2))
   ind <- which.max(fert)
   indL <- 1:(ind-1)
   indR <- (ind+1):length(fert)
   fert3 <- fert[age==3]
   fert[indR] <- fert3+fert[indR]*(1-fert3)
   fert <- fert-fert3
   fert[fert<0] <- 0
   if (u<1e-2)
      u <- 1e-2
   fert <- fert^u
   fert <- fert/max(fert)
   fert <- round(fert,3)
   if (!plot) {
     # plot(age,fert,type="b");q()
      return(fert)
   }
   col.base <- "#428BCA"
   col.line <- paste0(col.base,"FF")
   p1 <- ggplot(data.frame(age=age,fert=fert),aes(age,fert))+
      geom_point()+geom_line(colour=col.line)+
      xlab("Age")+ylab("Fertility")+
     # scale_colour_manual(values=c("indianred2","seagreen3","sienna3"))+
     # scale_colour_hue()+
     # guides(colour=guide_legend(title=""))+
     # theme(legend.pos=c(0.25,0.85))+
     # theme(legend.background=element_rect(fill="transparent"))+
     # theme(legend.key=element_rect(fill="transparent",colour="transparent"))+
     # theme(legend.box.background=element_rect(fill="transparent"))+
      theme(panel.background=element_rect(fill=paste0(col.base,"40")))+
      NULL
   p1
}
'repairFamily' <- function(pop,verbose=FALSE) {
   pa <- table(pop$parent)
   if (!length(pa))
      return(pop)
  # str(na.omit(pop$parent))
  # str(unique(na.omit(pop$parent)))
   ind <- match(pop$id,names(pa))
   ind2 <- which(is.na(match(names(pa),pop$id)))
   indC <- c(na.omit(ind))
   indP <- which(!is.na(ind))
   ind3 <- which(pop$age>4 & !is.na(pop$parent))
   if (length(ind3)) {
      print(pop[ind3,])
      q()
   }
   if ((verbose)&&(sample(seq(100),1)==-3)) {
      pop2 <- pop
      pop2$ch2 <- 0
     # pop$child[pop$sex %in% "F"] <- 0
      pop2$ch2[indP] <- pa[indC]
      pop2$id2 <- ""
      pop2$id2[indP] <- names(pa)[indC]
      pop2$loss <- pop2$child-pop2$ch2
      pop2$eq <- pop2$id==pop2$id2
      print(pop2[indP,])
      q()
   }
   pop$child <- 0L
   pop$child[indP] <- as.integer(pa[indC])
  # if (length(ind2))
  #    pop$parent[ind2] <- NA
   indL <- which(!(pop$parent %in% pop$id) & !is.na(pop$parent))
   if (length(indL)) ## lost parents
      pop$parent[indL] <- NA
   pop
}
'makeID' <- function(n=1) {
   if (!FALSE) { ## long
     s <- c(letters,as.character(0:9))
     a <- sapply(seq(n),function(x) paste(sample(s,8,rep=TRUE),collapse=""))
   }
   else {
      a <- basename(sapply(rep("",n),tempfile))
      a <- substr(a,nchar(a)-7,nchar(a))
   }
   a
}
'.argv0.' <- function() {
   arglist <- commandArgs(FALSE)
   if (length(ind <- grep("^--file=.+",arglist,ignore.case=FALSE))==1)
      return(basename(strsplit(arglist[ind],"=")[[1]][2]))
   if (length(ind <- grep("^-f$",arglist,ignore.case=FALSE))==1)
      return(basename(arglist[ind+1L]))
   ""
}
'litterFraction' <- function(litter) {
   L <- litter#-0.02 #round(runif(1,min=1.2,max=3),3)
   if (L<=1)
      return(c(L,0,0))
   i <- 0
   repeat({
      i <- i+1
      p <- dnorm(1:3,mean=L,sd=L^0.75-1) # ,sd=0.3*L
      p <- round(p/sum(p),3)
      s <- sum(p*seq(3))
      d <- s-litter
      if (abs(d)<1e-4)
         break
      L <- L-sign(d)*0.01/sqrt(i)
      if (i>100)
         break
   })
   p <- round(p,3)
  # names(p) <- paste0("p",seq(3))
   p
}
'roundAmount' <- function(amount) {
   if ((all(amount<2))&&(sample(seq(5),1)==1))
      return(ceiling(amount))
   round(amount)
}
'mortalityTube' <- function(max.age,mortality.cub,adult=12,mortality.adult
                           ,k1d,k1i,k2) {
   adult.range <- adult:round(c(0.9,1)[1]*max.age)
   age <- seq(max.age)
   mortality <- rep(NA,length(age))
   mortality[1] <- mortality.cub
   mortality[adult.range] <- mortality.adult
   mortality[length(mortality)] <- 1
   n <- adult
   y1 <- mortality[n]
   y2 <- mortality[1]
   x1 <- 0
   x0l <- age[1]-x1
   x2 <- age[n]-x0l
   a1 <- (y2-y1)/(x2^k1d-x1^k1d)
   b1 <- y1-a1*x1^k1d
  # print(c(k1=k1d,x1=x1,x2=x2,y1=y1,y2=y2,a1=a1,b1=b1,x1k=x1^k1d,x2k=x2^k1d))
   mortality[1:n] <- a1*(age[n:1]-x0l)^k1d+b1
   i1 <- adult
   i2 <- max.age
   x1 <- 0
   x0r <- age[i1]-x1
   x2 <- age[i2]-x0r
   y1 <- mortality[i1]
   y2 <- mortality[i2]
   a2 <- (y2-y1)/(x2^k2-x1^k2)
   b2 <- y1-a2*x1^k2
   mortality[i1:i2] <- a2*(age[i1:i2]-x0r)^k2+b2
  # mortality <- c(a1*((age[n]+1)-x0l)^k1d+b1,mortality)
   indep.mortality <- c(0.999
                       ,mortality[2]*k1i^0.5
                       ,mortality[3]*k1i^0.1
                       ,mortality[4]*k1i^0.05)
   indep.mortality[indep.mortality>0.999] <- 0.999
   mortality[4] <- indep.mortality[4]
   list(depend=mortality,indep=indep.mortality)
}
'mortalityTubePlot' <- function(mortality) {
   indep <- mortality$indep
   depend <- mortality$depend
   age <- seq_along(depend)
   st <- c('indep'="Independent Youngs",'dep'="Dependent Youngs"
          ,'adult'="Adults")
   da1 <- data.frame(age=age,mortality=depend
                    ,status=unname(st["dep"]))
   da2 <- data.frame(age=age,mortality=c(head(indep,3),tail(depend,-3))
                    ,status=unname(st["indep"]))
  # print(da1[1:4,])
  # print(da2[1:4,])
  # comb.mortality <- mortality
  # indep.fraction <- c(C0=0.001,C1=indep.C1,C2=0.99)
  # comb.mortality[1:3] <- mortality[1:3]*(1-indep.fraction)+
  #                        indep.mortality[1:3]*indep.fraction
  # da3 <- data.frame(age=age,mortality=comb.mortality
  #                  ,status="Combines")
  # print(da3[1:4,])
   da4 <- data.frame(age=tail(age,-3),mortality=tail(depend,-3)
                    ,status=unname(st["adult"]))
   da <- rbind(da1,da2,da4)
   da$status <- factor(da$status,levels=st,ordered=TRUE)
   tube <- ggplot(da,aes(age,mortality,colour=status))+
      geom_point()+geom_line()+
      xlab("Age")+ylab("Mortality")+
     # scale_colour_manual(values=c("indianred2","seagreen3","sienna3"))+
      scale_colour_hue()+
      guides(colour=guide_legend(title=""))+
      theme(legend.pos=c(0.25,0.85))+
      theme(legend.background=element_rect(fill="transparent"))+
      theme(legend.key=element_rect(fill="transparent",colour="transparent"))+
     # theme(legend.box.background=element_rect(fill="transparent"))+
      theme(panel.background=element_rect(fill="#428BCA40"))+
      NULL
   tube
}
'initialState' <- function(mortality,init.den,litter,max.age,sigma,removal.age
                          ,reprod.age,sexratio,reprod.cycle,removal,subad
                          ,equilibrium,maxj=1000,toRound=!FALSE) {
   adult <- 6:round(c(0.9,1)[1]*max.age) ## indirect use, for stats
   if (sigma>0.5)
      sigma <- sigma/100
   if ((length(removal.age)==2)&&(diff(range(removal.age))>1))
      removal.age <- removal.age[1]:removal.age[2]
   if ((length(reprod.age)==2)&&(diff(range(reprod.age))>1))
      reprod.age <- reprod.age[1]:reprod.age[2]
   if ((length(subad)==2)&&(diff(range(subad))>1))
      subad <- subad[1]:subad[2]
   if (sexratio>1)
      sexratio <- sexratio/100
   if (noShiny)
      str(list(mortality=mortality,init.den=init.den,litter=litter,max.age=max.age
              ,sigma=sigma,removal.age=removal.age,reprod.age=reprod.age
              ,sexratio=sexratio,reprod.cycle=reprod.cycle,removal=removal
              ,subad=subad,adult=adult,equilibrium=equilibrium
              ))
   age <- seq(max.age)
   subad.ini <- subad[1]
   toBreak <- !equilibrium
   keep.den <- init.den
  # print(c(isShiny=isShiny,noShiny=noShiny))
   if (!toBreak) {
      if (!isShiny)
         pb <- tcltk::tkProgressBar(min=0,max=maxj,title="Initial state")
      else {
         pb <- shiny::Progress$new()
         pb$set(message = "", value = 0)
      }
   }
   for (j in 1:maxj) { #to adjust equilibrium by changing 'reprod.cycle'
      if ((!toBreak)&&(j%%100==0)) {
         if (!isShiny)
            tcltk::setTkProgressBar(pb,j,label=paste("Adjustment step",j))
         else
            pb$inc(j/maxj, detail = paste("Adjustment step",j))
      }
      p0 <- init.den*litter ## borned in dens
      init <- 1
     # for (k in seq(round(removal/6,4),round(removal/4,4),by=1e-4)) {
      for (k in seq(0,0.01,by=0.0001)) {
         peer <- dens <- lost <- rep(NA,length(age)) ## 'peer' - has the same age
         mort.k <- mortality*(1+rnorm(length(mortality),mean=0,sd=sigma))
         mort.k[mort.k<0] <- 0.01
         mort.k[removal.age] <- mort.k[removal.age]+k
         for (i in seq_along(age))
         {
            p <- ifelse(i==1,p0,peer[i-1])
            peer[i] <- p*(1-mort.k[i])
            if (toRound)
               peer[i] <- round(peer[i])
            sc <- if ((age[i]<min(reprod.age)-2)||(age[i]>max(reprod.age))) 0 
                  else if (age[i]<min(reprod.age)-1) 0.3
                  else if (age[i]<min(reprod.age)) 0.6
                  else 1
            dens[i] <- sc*sexratio*peer[i]/reprod.cycle
            if (toRound)
               dens[i] <- round(dens[i])
            lost[i] <- ifelse(age[i]<2,p0-peer[i],peer[i-1]-peer[i])
         }
         peer1 <- sum(peer)-0.5*peer[1]
         if (init) {
            peer0 <- peer1*(1-removal)
            init <- 0
         }
        # print(c(peer0=peer0,peer1=peer1))
         if (peer1<=peer0) {
           # mortality <- mort.k
            break
         }
        # break
      }
     # if (toRound) {
     #    peer <- round(peer)
     # }
     # return(list(char="A",num=12))
      df1 <- data.frame(age=age,mortality=mort.k,peer=peer
                       ,dens=dens,lost=lost
                       ,lostP=100*lost/sum(lost) ## lost in %%
                       ,lostC=cumsum(100*lost/sum(lost)) ## cumul lost in %%
                       ,litter=NA)
      peer1 <- with(df1,sum(peer)-0.5*peer[1])
      peer2 <- with(df1,sum(peer[subad]))
      mort1 <- mean(df1$mortality)
      mort2 <- mean(df1$mortality[adult])
      if (FALSE)
         mort3 <- mean(df1$mortality[subad])
      else {
         mort3 <- sum(df1$peer[subad]*df1$mortality[subad])/sum(df1$peer[subad])
      }
      d1 <- 100*init.den/peer1
      d2 <- 100*init.den/peer2
      dens <- sum(df1$dens)
      lost <- sum(df1$lost)
      df1$litter[1:subad.ini] <- c(litter,df1$peer[1:(subad.ini-1)]/init.den)
      df2 <- data.frame(n=p0,mort.all=mort1,mort.adult=mort2,mort.sub=mort3
                       ,peer1=peer1,peer2=peer2
                       ,d1=d1,d2=d2,dens=dens,lost=lost,cycle=reprod.cycle)
      if (toBreak)
         break
     # rpc <- reprod.cycle
      if (dens-init.den>1e-3)
         reprod.cycle <- reprod.cycle+1/(j*5)
      else if (dens-init.den<1e-3)
         reprod.cycle <- reprod.cycle-1/(j*5)
      else
         toBreak <- TRUE
     # print(c(dens=dens,init.den=init.den,cycle.prev=rpc,cycle=reprod.cycle,d=rpc-reprod.cycle))
      if (reprod.cycle<3)
      {
         if (!isShiny)
            print("A")
         reprod.cycle <- 3.1
         init.den <- if (toRound) init.den-1 else init.den-0.1
         toBreak <- FALSE
      }
      if (!toBreak)
         next
   }
   if (!toBreak) {
      if (!isShiny)
         close(pb)
      else
         pb$close()
   }
   df1$mortality <- mort.k
   if (!isShiny)
      print(c(j=j,k=k))
   df1$peer <- round(df1$peer,3)
   print(df1,digits=3)
   print(df2,digits=4)
   w <- round(with(df1,dens/sum(dens))*1e6)
   d <- NULL
   for (i in seq(nrow(df1)))
      d <- c(d,rep(df1$age[i],w[i]))
   den <- with(df1,data.frame(n=age,d=dens,w=dens/sum(dens)))
   genLength <- weighted.mean(den$n,den$w)
   GL <- c(GL.median=median(d),GL.mean=mean(d),GL.mean.check=genLength)
   print(GL)
   ret <- list(char="A",num=12,df1=df1,df2=df2,GL=GL)
   ##~ df1 <- res$df1
   ##~ df2 <- res$df2
   ##~ GL <- res$GL
   ##~ print(df1,digits=3)
   ##~ print(df2,digits=4)
   ##~ print(GL)
   return(ret)
}
'randomize' <- function(seed1=NA,seed2=NA,verbose=FALSE) {
   if (is.na(seed1)) {
      seed1 <- sample(100:999,1)
      seed2 <- NA
   }
   set.seed(seed1)
   res <- list(seed1=seed1
              ,seed2=NA
              ,max.age=sample(29:40,1)
              ,litter=sample(seq(1.4,2.1,by=0.01),1)
              ,litterF=NA
              ,sexratio=50
              ,init.den=100
              ,pregnant=sample(seq(0.5,0.9,by=0.01),1)
              ,mortality.cub=sample(seq(0.25,0.45,by=0.01),1)
              ,mortality.adult=sample(seq(0.06,0.15,by=0.005),1)
              ,indep.C1=sample(seq(0.05,0.75,by=0.05),1)
              ,fertility=sample(seq(0.1,1.0,by=0.01),1)
              ,k1d=10
              ,k1i=sample(seq(3:18),1)
              ,k2=5
              )
   res$litterF <- litterFraction(res$litter)
   noseed2 <- is.na(seed2)
   if (noseed2) {
      set.seed(NULL)
      seed2 <- sample(100:999,1)
   }
   res$seed2 <- seed2
   set.seed(seed2)
   if (verbose)
      print(as.data.frame(res))
   res
}
'simulate' <- function(max.age=NA,litter=NA,sexratio=NA #,mortality=NA
                      ,init.den=NA,pregnant=NA
                      ,mortality.cub=NA,mortality.adult=NA
                      ,indep.C1=NA,fertility=NA,k1d=NA,k1i=NA,k2=NA
                      ,seed1=NA,seed2=NA,...) {
  # set.seed(NULL)
  # if (is.na(seed1)) {
  #    seed1 <- sample(100:999,1)
  #    seed2 <- NA
  # }
  # set.seed(seed1)
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
   if (is.na(indep.C1))
      indep.C1 <- init$indep.C1
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
                ,mCOY=mortality.cub,mAdult=mortality.adult
                ,iC1=indep.C1,fert=fertility,k1d=k1d,k1i=k1i,k2=k2
                ,seed1=seed1,seed2=seed2)
   print(as.data.frame(input))
   indep.fraction <- c(C0=0.001,C1=indep.C1,C2=0.99) # 0.27 ## Broken families
   indep.mortality <- 0.999
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
                  ,reprod.cycle=3,removal=0*c(0.045,0.03)[2]
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
   if (TRUE) {
      daF <- data.frame(age=age1,fert=NA,peer=NA)
      daF$fert <- fertilityCurve(age1,d=3,s=9,m=10,u=fertility)
   }
   else { ## to deprecate
      daF <- data.frame(age=age1
                     # ,fert=fert(age1,d=7,s=11,m=11.5)
                      ,fert=fert(age1,d=3,s=9,m=10)
                      ,peer=NA)
      if (!FALSE) {
         daF$fert <- daF$fert-daF$fert[daF$age==subad.ini]
         daF$fert[daF$fert<0] <- 0
      }
      else {
         daF$fert <- daF$fert-daF$fert[daF$age==max(reprod.age)]
         daF$fert[1:subad.ini] <- 0
         daF$fert[daF$fert<0] <- 0
         daF$fert[daF$fert>0] <- 1
      }
      daF$fert <- daF$fert/max(daF$fert)
      daF$fert <- round(daF$fert,3)
   }
   daF$peer <- c(round(df1$peer[na.omit(match(daF$age,df1$age))]*sexratio))
   popF <- data.frame(id=NA,epoch=0L,season=0L,born=NA,sex="F"
                     ,age=rep(daF$age,daF$peer),child=0L,parent=NA,omit=FALSE)
   popM <- data.frame(id=NA,epoch=0L,season=0L,born=NA,sex="M"
                     ,age=rep(daM$age,daM$peer),child=0L,parent=NA,omit=FALSE)
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
      pop <- repairFamily(pop)
      verbose <- T & epoch %in% c(nepoch-c(1,0))
      if (isProgressBar) {
         if (!isShiny)
            tcltk::setTkProgressBar(pb,epoch,label=paste("Epoch",epoch))
         else
            pb$inc(epoch/nepoch, detail = paste("Epoch",epoch))
      }
      if (verbose)
         cat("-----------","epoch:",epoch,"-----------","\n")
      for (a in subad.ini+c(0,1)[-1]) {
         if (length(ind <- which(pop$age==a))) { ## break C3+ families
            parent <- unique(na.omit(pop$parent[ind]))
            if (length(parent)) {
               pop$parent[ind] <- NA
            }
         }
      }
      pop <- repairFamily(pop)
      lf <- LF["C0",]
      pattern <- grep("^p\\d+",names(lf))
      ind <- which(pop$sex=="F" & pop$child==0) ## pri
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
      popC <- unique(pop[pop$parent %in% pop$id,c("age","parent")])
     # print(pop[pop$parent %in% pop$id,])
      ta <- table(popC$age)
      tc <- c(S=length(indS),F=length(indS)-parents,P=parents,C=sum(ta),ta)
      if (verbose) {
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
                         ,age=rep(0L,nF),child=0L,parent=NA,omit=FALSE)
      popM0 <- data.frame(id=NA,epoch=epoch,season=0L,born=base+epoch,sex="M"
                         ,age=rep(0L,nM),child=0L,parent=NA,omit=FALSE)
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
            ind <- which(pop$sex=="F" & pop$age==b[j] & pop$child==0) ## pri
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
               print(which(pop$sex=="F" & pop$age==b[j] & pop$child==0))
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
      pop$season <- 0L
      lifestory[[lifecut]] <- pop
      lifecut <- lifecut+1L
      pop$age <- pop$age+1L
      ind <- which(pop$age>max.age)
      if (length(ind))
         pop$omit[ind] <- TRUE
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
      for (i in sample(tail(seq_along(mortality),-subad.ini))) { ## mortality to adult
         ind1 <- which(pop$age==i)
         n1 <- length(ind1)
         if (!n1)
            next
         n2 <- roundAmount(n1*mortality[i])
         if (!n2)
            next
         ind2 <- if ((length(ind1)>1)&&(n2>0)) sample(ind1,n2) else ind1
         pop$omit[ind2] <- TRUE
      }
      for (i in head(seq_along(mortality),subad.ini)) { ## independent
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
         pop$omit[ind2] <- TRUE
      }
      for (i in head(seq_along(mortality),subad.ini)) {  ## dependent
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
         pop$omit[ind2] <- TRUE
         pop$parent[ind2] <- NA
      }
      pop1 <- pop[pop$omit,]
      pop1$season <- 9L
      lifestory[[lifecut]] <- pop1
      lifecut <- lifecut+1L
      pop <- pop[!pop$omit,]
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
   LS$omit <- NULL
   rownames(LS) <- NULL
   list(input=input,output=LS)
}
'analyze' <- function(LS) {
   input <- LS$input
   lifestory <- LS$output
   epoch <- sort(unique(lifestory$epoch))
   season <- sort(unique(lifestory$season))
   ns <- length(season)
   max.age <- max(lifestory$age)
   subad.ini <- 3
   col.base <- c(green="#3C8D8C",blue="#428BCA",purpur="#605CA8"
                ,orange="#F39C12")[2]
   col.bg <- paste0(col.base,"40")
   col.hist <- paste0(col.base,"80")
   col.line <- paste0(col.base,"80")
   col.strip <- paste0(col.base,"60")
   p0 <- theme_grey()+
         theme(panel.background=element_rect(fill=col.bg))+
         theme(strip.background=element_rect(fill=col.strip))+#,colour="red"))
         theme(legend.margin=margin(t=-1,b=0,unit='char'))
        # theme(legend.key.size=size(1,unit="char"))
   p9 <- p8 <- p7 <- p6 <- p5 <- p4 <- p3 <- p2 <- p1 <- NULL
  # pdf("res1.pdf",width=8,height=4)
   res <- NULL
   if (TRUE) {
     # print(tail(lifestory[lifestory$season==9,],100))
     # str(lifestory[lifestory$season==9 & lifestory$epoch==max(lifestory$epoch),])
     # str(lifestory[lifestory$season==0 & lifestory$epoch==max(lifestory$epoch),])
     # str(lifestory[lifestory$season==1 & lifestory$epoch==max(lifestory$epoch),])
      age <- list('0Yr'=1,'1Yr'=2,'2Yr'=3,'Sub-Ad'=4:5,'Adult'=6:99)
      res <- NULL
      ep <- max(lifestory$age)-4
      label <- rep("",length(age))
      toSkip <- FALSE
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
         surv <- s1$id/s0$id
         surv[surv>=1] <- 1-1e-3
         res2 <- data.frame(Epoch=s0$epoch,Survival=surv#,Age=names(age)[i]
                           ,Label="")
         label[i] <- sprintf("%s\n%.2f\u00B1%.2f",names(age)[i]
                            ,mean(res2$Survival),sd(res2$Survival))
         res2$Label <- label[i]
         res <- rbind(res,res2)
      }
      if (!toSkip) {
         res$Label <- factor(res$Label,levels=label,ordered=TRUE)
         p9 <- ggplot(res,aes(Label,Survival))+
               geom_violin()+
               xlab("Age Structure")+ylab("Actual Survival")+
               scale_x_discrete(breaks=NULL)+
               facet_grid(.~Label,scales="free")+
               p0
      }
     # print(p9+p0);q()
   }
   if (TRUE) {
      pop <- lifestory[lifestory$epoch>c(0,max.age)[2] &
                       lifestory$epoch<=max(epoch)-0*4 &
                       lifestory$season==0,]
      res <- NULL
      if (nrow(pop)) {
         age <- c('0Yr'=0,'1Yr'=1,'2Yr'=2)
         for (i in seq_along(age)) {
            pop1 <- pop[pop$age==age[i] & !is.na(pop$parent),]
            res2 <- aggregate(pop1$parent,list(epoch=pop1$epoch),function(x) {
               lf <- table(table(x))
               lf <- unname(lf/sum(lf))
               while (length(lf)<3) lf <- c(lf,0)
               lf
            })
            L <- apply(res2[,-1],1,function(x) sum(x*seq_along(x)))
            lab <- sprintf("%s\n%.2f\u00B1%.2f",names(age[i]),mean(L),sd(L))
            res2 <- cbind(data.frame(age=lab,epoch=res2$epoch),res2[,-1])
            res <- rbind(res,tidyr::gather(res2,cubs,value,-epoch,-age))
         }
         p7 <- ggplot(res,aes(epoch,value,colour=cubs))+geom_line()
         p8 <- ggplot(res,aes(cubs,value))+geom_violin()+
               xlab("Litter Size")+ylab("Proportion")
      }
   }
   if (TRUE) {
      epoch <- epoch[(epoch*ns) %% ns == 0 & epoch>=max(lifestory$age)-4]
      pop <- lifestory[lifestory$epoch %in% epoch & lifestory$season==0,]
      if (nrow(pop)) {
         a1 <- aggregate(id~age+epoch,data=pop,length)
         colnames(a1)[ncol(a1)] <- "size"
         p6 <- ggplot(a1,aes(age,size))+
              # geom_violin(aes(group=cut_width(age,1)))+
               geom_boxplot(aes(group=cut_width(age,1)),outlier.alpha=0.5
                           ,colour=col.base,width=1)+
               p0
      }
   }
   if (TRUE) {
      res <- NULL
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
         }
      }
      res$era <- factor(res$era)
      res$season <- factor(res$season)
     # p5 <- ggplot(res,aes(epoch,size,colour=era))+facet_grid(season~.)+
     #       geom_line()
      p5 <- ggplot(res,aes(epoch,size))+
            geom_line(aes(colour=era,linetype=season))+
            xlab("Epoch")+ylab("Population Size")+
           # scale_y_contionuos()
            ylim(0,max(res$size))+
           # geom_line(size=2)+
            p0+
            theme(legend.position="bottom")+
            theme(legend.title=element_blank())+
            guides(colour=guide_legend(nrow=2))+
            guides(linetype=guide_legend(nrow=2))+
            NULL
   }
   if (TRUE) {
      done <- lifestory$id[lifestory$season==9]
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
         p1 <- ggplot(data.frame(v=reprod.cycle),aes(v))+
               geom_histogram(binwidth=1,fill=col.hist)+
               geom_vline(xintercept=mean(reprod.cycle),col=col.line)+
               geom_vline(xintercept=median(reprod.cycle),col=col.line
                         ,size=1.25,linetype=2)+
               scale_x_continuous(breaks=0:100,minor_breaks=NULL)+
               xlab("Interbirth, years")+ylab("Count")+
               p0
      }
      if (TRUE) {
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
            p2 <- ggplot(res,aes(dens))+
                  geom_histogram(binwidth=1,fill=col.hist)+
                  geom_vline(xintercept=mean(res$dens),col=col.line)+
                  geom_vline(xintercept=median(res$dens),col=col.line
                            ,linetype=2)+
                  scale_x_continuous(breaks=0:100,minor_breaks=NULL)+
                  xlab("Dens during lifespan")+ylab("Count")+
                  p0
            p3 <- ggplot(res,aes(cubs))+
                  geom_histogram(binwidth=1,fill=col.hist)+
                  geom_vline(xintercept=mean(res$cubs),col=col.line)+
                  geom_vline(xintercept=median(res$cubs),col=col.line
                            ,linetype=2)+
                  scale_x_continuous(breaks=0:100,minor_breaks=NULL)+
                  xlab("Cubs during lifespan")+ylab("Count")+
                  p0
            p4 <- ggplot(res,aes(adults))+
                  geom_histogram(binwidth=1,fill=col.hist)+
                  geom_vline(xintercept=mean(res$adults),col=col.line)+
                  geom_vline(xintercept=median(res$adults),col=col.line
                            ,linetype=2)+
                  scale_x_continuous(breaks=0:100,minor_breaks=NULL)+
                  xlab("Survived cubs during lifespan")+ylab("Count")+
                  p0
         }
      }
     # ursa:::.elapsedTime("C")
     # epoch <- as.numeric(names(lifestory))
   }
   list(input=input,p0=p0,p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,p6=p6,p7=p7,p8=p8,p9=p9)
}

noShiny <- .argv0.()=="main.R"
isShiny <- ("shiny" %in% loadedNamespaces())
init <- randomize()

if (noShiny) {
   ##~ mortality <- mortalityTube(max.age=max.age,mortality.cub=mortality.cub
                             ##~ ,adult=6,mortality.adult=mortality.adult,k1=k1,k2=k2)
   ##~ res <- initialState(mortality=mortality,init.den=init.den,litter=litter
                      ##~ ,max.age=max.age,sigma=sigma,removal.age=removal.age
                      ##~ ,reprod.age=reprod.age,sexratio=sexratio
                      ##~ ,reprod.cycle=reprod.cycle,removal=removal
                      ##~ ,subad=subad,equilibrium=equilibrium)
}
