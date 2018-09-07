options(stringsAsFactors=FALSE)
require(ggplot2)
source("simulate.R")
source("analyze.R")
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
   if (u<1e-3)
      u <- 1e-3
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
   pop$child[pop$child>=0] <- 0L
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
   if (FALSE)
      str(list(mortality=mortality,init.den=init.den,litter=litter,max.age=max.age
              ,sigma=sigma,removal.age=removal.age,reprod.age=reprod.age
              ,sexratio=sexratio,reprod.cycle=reprod.cycle,removal=removal
              ,subad=subad,adult=adult,equilibrium=equilibrium
              ))
   age <- seq(max.age)
   subad.ini <- subad[1]
   toBreak <- !equilibrium
   keep.den <- init.den
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
              ,indep.fraction=c(0.001,sample(seq(0.05,0.75,by=0.05),1),0.999)
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
isShiny <- ("shiny" %in% loadedNamespaces())
init <- randomize()
