'.sample' <- function(x,n) {
   if (length(x)<=1)
      return(x)
   sample(x,n)
}
'.argv0.' <- function() {
   arglist <- commandArgs(FALSE)
   if (length(ind <- grep("^--file=.+",arglist,ignore.case=FALSE))==1)
      return(basename(strsplit(arglist[ind],"=")[[1]][2]))
   if (length(ind <- grep("^-f$",arglist,ignore.case=FALSE))==1)
      return(basename(arglist[ind+1L]))
   ""
}
'fert' <- function(x,d=7,s=7,m=11.5)
{
   r1 <- x-m
   r2 <- (s+d)+(s-d)*tanh(r1)
   y <- exp(-r1*r1/(0.5*r2*r2))
   y
}
#'ageSpecificCurve'(age=age,d=d,s=s,m=m,u=u,ealry=early,desc=desc,plot=plot)
'removalCurve' <- function(age,d=12,s=6,m=3,u=1,plot=FALSE) {
   r1 <- age-m
  # d <- d*40/length(age)
   r2 <- (s+d)+(s-d)*tanh(r1)
   vuln <- exp(-r1*r1/(0.5*r2*r2))
  # ind <- which.max(vuln)
  # indL <- 1:(ind-1)
  # indR <- (ind+1):length(vuln)
  # vuln3 <- vuln[age==3]
   ##~ vuln[indR] <- vuln3+vuln[indR]*(1-vuln3)
   ##~ vuln <- vuln-vuln3
   ##~ vuln[vuln<0] <- 0
   if (u<1e-3)
      u <- 1e-3
   vuln <- vuln^u
   vuln <- vuln/max(vuln)
  # vuln <- round(vuln,3)
   if (!plot)
      return(round(vuln,3))
   col.base <- "#428BCA"
   col.line <- paste0(col.base,"FF")
   p1 <- ggplot(data.frame(age=age,vuln=vuln),aes(age,vuln))+
      geom_point()+geom_line(colour=col.line)+
      xlab("Age")+ylab("Vulnerability to removal")+
      scale_y_continuous(lim=c(0,1))+
      theme(panel.background=element_rect(fill=paste0(col.base,"40")))+
      NULL
   p1
}
'fertilityCurve' <- function(age,d=3,s=9,m=10,u=1
                              ,desc="<dummy>",plot=FALSE) {
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
  # indep.fraction <- c(C0=0.001,C1=broken.C1,C2=0.99)
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
      theme(legend.pos=c(0.35,0.85))+
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
   if (F & isShiny) {
      seed1 <- 275 ## 267
     # seed2 <- 818 ## 818
   }
   if (is.na(seed1)) {
      seed1 <- sample(100:999,1)
      seed2 <- NA
   }
   set.seed(seed1)
   removal.rate <- sample(rexp(10e3),20)
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
              ,removal.rate=sample(seq(-100.15,0.05,by=0.001),1)
             # ,removal.rate=round(0.03*sample(removal.rate/max(removal.rate),1),3)
              ,removal.age=sample(seq(0.1,1.0,by=0.01),1)
              ,k1d=10
              ,k1i=as.numeric(sample(seq(3,18),1))
              ,k2=5
              )
   res$litterF <- litterFraction(res$litter)
   res$removal.rate[res$removal.rate<0] <- 0
   if ((FALSE)&&(isShiny))
      res$removal.rate <- 0
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
'updatePrm' <- function(old,new) {
   ##~ cat("old:\n")
   ##~ str(old)
   ##~ cat("new:\n")
   ##~ str(new)
   ##~ cat("----- update -----\n")
   aname <- names(old)
   bname <- names(new)
   indep.ind <- grep("indep",aname)
   indep.fraction <- old[[indep.ind]]
   old[[indep.ind]] <- indep.fraction[2]
   for (i in seq_along(bname)) {
      j <- match(bname[i],aname)
      if (is.na(j))
         next
      b <- new[[i]]
     # a <- old[[j]]
      if (bname[i]=="litter") {
         old[["litterF"]] <- litterFraction(b)
      }
      else if (bname[i]=="litterF") {
         old[["litter"]] <- sum(seq_along(b)*b)
      }
      if ((is.numeric(b))||(length(b)>1)||(is.language(b))) {
         if (is.numeric(b))
            old[[j]] <- b
         else if (is.symbol(b)) {
            old[[j]] <- eval.parent(b)
           # q()
         }
         else if (is.language(b)) {
            print("LANGUAGE")
            old[[j]] <- eval.parent(b)
            stop("I don't know how to deal with it")
         }
         else
            stop("unkn class of 'b'")
      }
      else if (is.character(b)) {
         b <- try(eval(parse(text=paste0(a,b))))
         if (!inherits(b,"try-error"))
            old[[j]] <- b
      }
      else if (anyNA(b))
         next
      else {
         stop("required interpretation of 'b'")
      }
   }
   if ((length(i <- grep("sex",aname))==1)&&(old[[i]]>1))
      old[[i]] <- old[[i]]/100
  # if ()==1) {
  #    str(old[i])
  # }
   if (length(old[[indep.ind]])==1)
      old[[indep.ind]] <- c(indep.fraction[1],old[[indep.ind]],indep.fraction[3])
   old
}
'growthRate' <- function(lifestory,verbose=!FALSE,...) {
   if (T & length(list(...))>0) {
     # print("growthRate -> simulate")
     # lifestory <- simulate(lifestory,verbose=FALSE,...)
      lifestory <- do.call("simulate",list(lifestory,quiet=TRUE,...))
   }
   if (is.null(lifestory$output))
      return(numeric())
  # print(analyze(lifestory)$p5)
   pop <- subset(lifestory$output,season==0)
   res <- aggregate(pop$id,by=list(epoch=pop$epoch),length)$x
   if (length(res)<5)
      return(numeric())
   res <- diff(res)/head(res,-1)
   if (verbose)
      cat(sprintf("Growth rate = %+.3f\u00B1%.3f\n",mean(res),sd(res)))
   res
}
'perturb' <- function(lifestory) {
   isShiny <- ("shiny" %in% loadedNamespaces())
   if (isShiny) {
      showModal(modalDialog(title = "Perturbation in progress","Please wait"
                       ,size="s",easyClose = TRUE,footer = NULL))
      on.exit(removeModal())
   }
  # str(lifestory$input)
   prmList <- c('1'="mortality.adult",'2'="mortality.cub",'3'="max.age"
               ,'4'="litter",'5'="pregnant",'6'="indep.fraction")
   ret <- vector("list",length(prmList))
   names(ret) <- prmList
   cs <- colorScheme()
   if (isShiny)
      showNotification(closeButton=TRUE,"check reference"
                      ,id="ref",duration=99)
   g0 <- growthRate(lifestory,nochanged=TRUE)
   if (isShiny)
      removeNotification(id="ref")
   if (!length(g0))
      return(ret)
  # set.seed(NULL)
   gr0 <- mean(g0)
   agr0 <- abs(gr0)
   sgr0 <- sign(gr0)
   for (p in .sample(prmList)) {
      lab <- switch(p
                   ,'mortality.adult'="Adult mortality"
                   ,'mortality.cub'="COY mortality"
                   ,'max.age'="Maximal age"
                   ,'litter'="COY litter size"
                   ,'pregnant'="Birth success"
                   ,'indep.fraction'="Broken yearling families"
                   ,p)
      prm0 <- lifestory$input[[p]]
      if (p %in% c("indep.fraction"))
         prm0 <- prm0[2]
      message(paste0(lab,": ",prm0))
      sc <- switch(p
                  ,'mortality.adult'=-0.05
                  ,'mortality.cub'=-0.05
                  ,'max.age'=+0.05
                  ,'litter'=+0.05
                  ,'pregnant'=+0.1
                  ,'indep.fraction'=0.2
                  ,0.1)
      if (TRUE) {
        # print(sc)
         if (agr0>0.005) {
            sc <- sc*round(100*agr0)^0.75
         }
         if (p %in% c("indep.fraction","pregnant"))
            si <- c(-3,-2,-1,0,1,2,3)
         else if (gr0>(+0.002))
            si <- c(-3,-2,-1,0,1)*sign(sc)#*(-sgr0)
         else if (gr0<(-0.002))
            si <- c(-1,0,1,2,3)*sign(sc)#*(-sgr0)
         else
            si <- c(-2,-1,0,1,2)
         if (p %in% c("pregnant","indep.fraction")) {
            prm <- prm0+si*sc
           # print(prm)
            if (TRUE) {
               indP <- prm>=0 & prm<=1
               prm <- prm[indP]
               si <- si[indP]
            }
            else if (FALSE) {
               if (length(which(prm<0.05))>1)
                  prm <- 0.05+prm-min(prm)
               else if (length(which(prm>0.95))>1)
                  prm <- 0.95-max(prm)+c(prm)
            }
            else {
               indL <- which(prm<0.01)
               if (length(indL)) {
                  print(indL)
               }
            }
           # print(prm)
            s <- prm/prm0
         }
         else {
            s <- (1+abs(sc))^si
            prm <- prm0*s
         }
      }
      else {
         stop("damaged code")
         si <- c(-2,-1,0,1,2)
         s <- (1+abs(sc))^si
        # s <- 1.1^si
      }
     # print(si)
     # print(c(s=s))
      if (isShiny)
         showNotification(closeButton=TRUE,paste0(lab,"...")
                         ,id=p,duration=99)
      if (prm_validation <- TRUE) {
         if (p %in% c("pregnant","indep.fraction")) {
            prm[prm<0.01] <- 0.01
            prm[prm>0.99] <- 0.99
         }
      }
      indD <- which(!duplicated(prm))
      prm <- prm[indD]
      prmlab <- round(prm[indD],switch(p,'max.age'=0,3))
      res2 <- NULL
     # print(sprintf("%+.3f\u00B1%.3f",mean(g0),sd(g0)))
      arglist <- list(lifestory,NA)
      names(arglist) <- c("",p)
      desc <- rep("",length(indD))
      res <- data.frame(xlab=s[indD],prm=prmlab,desc="",mean=NA,sd=NA)
      si <- si[indD]
      for (i in seq(nrow(res))) {
        # print(prmlab[i])
         arglist[[p]] <- prm[i]
        # str(arglist[-1])
         g1 <- if (si[i]==0) g0 else do.call("growthRate",arglist)
         if (!length(g1))
            break
        # g1 <- growthRate(lifestory,mortality.adult=res$x[i])
        # res$mean[i] <- mean(g1)
        # res$sd[i] <- sd(g1)
        # desc <- paste0(res$x[i],"\n",sprintf("%.3f\u00B1%.3f",res$mean[i],res$sd[i]))
         desc[i] <- sprintf("%+.3f\u00B1%.3f",mean(g1),sd(g1))
         res$mean[i] <- mean(g1)
         res$sd[i] <- sd(g1)
         res2 <- rbind(res2,data.frame(prm=prmlab[i],value=g1
                                      ,desc=desc[i]))
      }
     # if (p %in% c("pregnant","indep.fraction"))
     #    print(res)
      if (isShiny)
         removeNotification(id=p)
      if (is.null(res2))
         next
      if (length(desc)==length(unique(desc)))
         res$desc <- factor(desc,levels=desc[order(prm)],ordered=TRUE)
      else {
         desc2 <- paste0(prmlab,"\n",desc)
         res$desc <- factor(desc2,levels=desc2[order(prm)],ordered=TRUE)
      }
      res$xlab <- factor(paste0(prmlab,"\n",desc))
      if (length(desc)==length(unique(desc))) {
         res2$desc <- factor(res2$desc,levels=desc[order(prm)]
                            ,ordered=TRUE)
      }
      else {
         res2$desc <- factor(paste0(res2$prm,"\n",res2$desc)
                            ,levels=paste0(prmlab,"\n",desc)
                            ,ordered=TRUE)
      }
      names(desc) <- prmlab
      if (!FALSE)
         s1 <- ggplot(res2,aes(prm,value))+
               geom_violin(fill=cs$hist,colour=cs$base)+
               geom_point(data=res,aes(prm,mean))+
               xlab(lab)+ylab("Growth Rate")+
               facet_grid(prm~.,scales="free",labeller=labeller(prm=desc))+
               scale_x_continuous(breaks=prmlab)+
               coord_flip()+
               cs$p0+
               NULL
      else if (!FALSE)
         s1 <- ggplot(res2,aes(prm,value))+
               geom_violin(fill=cs$hist,colour=cs$base)+
               geom_point(data=res,aes(prm,mean))+
               xlab(lab)+ylab("Growth Rate")+
               facet_grid(.~desc,scales="free")+
               scale_x_continuous(breaks=prmlab)+
               cs$p0+
               NULL
      else {
         res2$xlab <- factor(with(res2,paste0(prm,"\n",desc)))
         s1 <- ggplot(res2,aes(xlab,value))+
               geom_violin(fill=cs$hist,colour=cs$base)+
               xlab(lab)+ylab("Growth Rate")+
               geom_point(data=res,aes(xlab,mean))+
              # geom_line(data=res,aes(xlab,mean))+
              # facet_grid(.~desc,scales="free")+
              # scale_x_discrete(breaks=desc)+
               cs$p0+
               NULL
      }
      ret[[p]] <- s1
     # break
   }
   ret
}
'colorScheme' <- function() {
   col.base <- c(green="#3C8D8C",blue="#428BCA",purpur="#605CA8"
                ,orange="#F39C12")[2]
   col.bg <- paste0(col.base,"40")
   col.hist <- paste0(col.base,"60")
   col.line <- paste0(col.base,"80")
   col.strip <- paste0(col.base,"60")
   p0 <- theme_grey()+
         theme(panel.background=element_rect(fill=col.bg))+
         theme(strip.background=element_rect(fill=col.strip))+#,colour="red"))
         theme(legend.margin=margin(t=-1,b=0,unit='char'))+
         theme(strip.text.y=element_text(angle=0,hjust=0.5,vjust=0.5))
        # theme(legend.key.size=size(1,unit="char"))
   list(base=col.base,bg=col.bg,hist=col.hist,line=col.line,strip=col.strip
       ,p0=p0)
}
'curveInputs' <- function(indep.mortality,mortality.cub,mortality.adult,init.den
                         ,litter,broken.C1,max.age,pregnant,sexratio,seed1
                         ,seed2,fertility,removal.rate,removal.age
                         ,k1d,k1i,k2) {
   mortality <- mortalityTube(max.age=max.age,mortality.cub=mortality.cub
                             ,mortality.adult=mortality.adult
                             ,k1d=k1d,k1i=k1i,k2=k2)
   tube <- mortalityTubePlot(mortality)
   indep.mortality <- mortality$indep
   mortality <- mortality$depend
   age <- seq(max.age)
   indep.fraction <- init$indep.fraction
   indep.fraction[2] <- broken.C1
   ret <- list(mortality=mortality
              ,indep.mortality=indep.mortality
              ,mortality.cub=mortality.cub
              ,mortality.adult=mortality.adult
              ,age=age
              ,tube.lin=tube
              ,tube.log=tube+scale_y_log10()
              ,tube.fert=fertilityCurve(age=age,u=fertility,plot=TRUE)
              ,tube.removal=removalCurve(age=age,u=removal.age,plot=TRUE)
              ,init.den=init.den
              ,litter=litter
              ,indep.fraction=indep.fraction
              ,max.age=max.age
              ,pregnant=pregnant
              ,sexratio=sexratio
              ,seed1=seed1
              ,seed2=seed2
              ,fertility=fertility
              ,removal.rate=removal.rate
              ,removal.age=removal.age
              ,k1d=k1d
              ,k1i=k1i
              ,k2=k2
              )
   ret
}
