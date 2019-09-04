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
'fert' <- function(x,d=7,s=7,m=11.5) {
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
   res <- data.frame(age=age,vuln=vuln)
   cs <- colorScheme()
   if (T & "plotly" %in% loadedNamespaces()) {
      p1 <- plot_ly()
      p1 <- add_trace(p1,data=res,type="scatter",mode="lines+markers"
                     ,x=~age,y=~vuln
                     )
      p1 <- layout(p1,xaxis=cs$axis,yaxis=cs$axis,legend=cs$legend,title=cs$title)
      p1 <- layout(p1
                  ,xaxis=list(title="Age")
                  ,yaxis=list(title="Vulnerability to removal",rangemode="tozero")
                  )
      prm <- cs$config
      prm[[1]] <- p1
      p1 <- do.call("config",prm)
   }
   else {
      col.base <- "#428BCA"
      col.line <- paste0(col.base,"FF")
      p1 <- ggplot(res,aes(age,vuln))+
         geom_point()+geom_line(colour=col.line)+
         xlab("Age")+ylab("Vulnerability to removal")+
         scale_y_continuous(lim=c(0,1))+
         theme(panel.background=element_rect(fill=paste0(col.base,"40")))+
         NULL
   }
   list(plot=p1,data=res)
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
   res <- data.frame(age=age,fert=fert)
   cs <- colorScheme()
   if (T & "plotly" %in% loadedNamespaces()) {
      p1 <- plot_ly()
      p1 <- add_trace(p1,data=res,type="scatter",mode="lines+markers"
                     ,x=~age,y=~fert
                     )
      p1 <- layout(p1,xaxis=cs$axis,yaxis=cs$axis,legend=cs$legend,title=cs$title)
      p1 <- layout(p1
                  ,xaxis=list(title="Age")
                  ,yaxis=list(title="Fertility")
                  )
      prm <- cs$config
      prm[[1]] <- p1
      p1 <- do.call("config",prm)
   }
   else {
      col.base <- "#428BCA"
      col.line <- paste0(col.base,"FF")
      p1 <- ggplot(res,aes(age,fert))+
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
   }
   list(plot=p1,data=res)
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
'mortalityTube' <- function(max.age,mortality.cub,adult=c(12,8)[2],mortality.adult
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
'mortalityTubePlot' <- function(mortality,scale=c("lin","log")) {
   scale <- match.arg(scale)
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
   if (T & "plotly" %in% loadedNamespaces()) {
      cs <- colorScheme()
      daAdult <- da[da$status==st["adult"],]
      age0 <- min(daAdult$age)
      daAdult <- da[da$status==st["adult"] & da$age>=age0,]
      daIndep <- da[da$status==st["indep"] & da$age<=age0,]
      daDep <- da[da$status==st["dep"] & da$age<=age0,]
      da2 <- rbind(daAdult,daDep,daIndep)
      tube <- plot_ly()
      tube <- add_trace(tube,data=da2,type="scatter",mode="lines+markers"
                       ,x=~age,y=~mortality,split=~status)
      tube <- layout(tube,xaxis=cs$axis,yaxis=cs$axis,legend=cs$legend,title=cs$title)
      tube <- layout(tube
                    ,xaxis=list(title="Age")
                    ,yaxis=list(title=paste("Mortality"
                                  ,switch(scale,log="(log scale)","(linear scale)"))
                               ,type=switch(scale,log="log","linear")
                               )
                    ,legend=list(x=0.2,y=1,bgcolor="transparent")
                    )
      prm <- cs$config
      prm[[1]] <- tube
      tube <- do.call("config",prm)
   }
   else {
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
      if (scale=="log")
         tube <- tube+scale_y_log10()
   }
   list(plot=tube,data=da)
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
'randomize' <- function(seed1=NA,seed2=NA,seed3=NA,firstRun=FALSE,verbose=FALSE) {
   if (T & firstRun) { ## switch to 'F'
      if (is.na(seed1)) {
         seed1 <- 731 ## 267 275 703 602
        # seed2 <- 703 ## comment it
      }
     # seed2 <- 818 ## 818
   }
   if (is.na(seed1)) {
      seed1 <- sample(100:999,1)
      seed2 <- NA
   }
   print(data.frame(firstRun=firstRun,seed1=seed1,seed2=seed2,seed3=seed3))
   set.seed(seed1)
   if (T & firstRun & (fixit <- TRUE)) {
  ##~ mA  ltr pregn mCOY mAdlt  wC1  wC2 fert remR remA k1d k1i k2 rnd1 rnd2 rnd3
##~ 1 38 2.18  0.78 0.38 0.086 0.25 0.65  0.6    0 0.34   5  10  5  731  801  801
      res <- list(seed1=-1 # 731
                 ,seed2=NA
                 ,seed3=NA
                 ,max.age=38
                 ,litter=2.18
                 ,litterF=NA
                 ,sexratio=50
                 ,init.den=100
                 ,pregnant=0.78
                 ,mortality.cub=0.38
                 ,mortality.adult=0.086
                 ,indep.fraction=c(0.001,0.25,0.65)
                 ,fertility=0.6
                 ,removal.rate=round(sample(seq(-100.15,0.05,by=0.001),1),6)
                 ,removal.age=0.34
                 ,k1d=5
                 ,k1i=10
                 ,k2=5
                 )
   }
   else {
      res <- list(seed1=seed1
                 ,seed2=NA
                 ,seed3=NA
                 ,max.age=sample(26:40,1)
                 ,litter=round(sample(seq(1.2,2.2,by=0.01),1),6)
                 ,litterF=NA
                 ,sexratio=50
                 ,init.den=100
                 ,pregnant=round(sample(seq(0.5,0.9,by=0.01),1),6)
                 ,mortality.cub=round(sample(seq(0.25,0.45,by=0.01),1),6)
                 ,mortality.adult=round(sample(seq(0.08,0.12,by=0.002),1),6)
                 ,indep.fraction=round(c(0.001
                                        ,sample(seq(0.05,0.75,by=0.05),1)
                                        ,ifelse(T,0.95,sample(seq(0.25,0.95,by=0.05),1))
                                        ),6)
                 ,fertility=round(sample(seq(0.1,1.0,by=0.01),1),6)
                 ,removal.rate=round(sample(seq(-100.15,0.05,by=0.001),1),6)
                # ,removal.rate=round(0.03*sample(removal.rate/max(removal.rate),1),3)
                 ,removal.age=round(sample(seq(0.1,1.0,by=0.01),1),6)
                 ,k1d=as.numeric(sample(seq(6,14),1)) # 10
                 ,k1i=as.numeric(sample(seq(3,18),1))
                 ,k2=5
                 )
   }
   res$litterF <- litterFraction(res$litter)
   res$removal.rate[res$removal.rate<0] <- 0
  # print(res$pregnant-round(res$pregnant,6))
   if ((FALSE)&&(isShiny))
      res$removal.rate <- 0
   noseed2 <- is.na(seed2)
   noseed3 <- is.na(seed3)
   if (noseed2) {
      set.seed(NULL)
      seed2 <- sample(100:999,1)
   }
   if (noseed3) {
      if (FALSE) {
         set.seed(NULL)
         seed3 <- sample(100:999,1)
      }
      else
         seed3 <- seed2
   }
   res$seed2 <- seed2 # 702
   res$seed3 <- seed3 # 701
  # set.seed(seed2)
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
   indep.ind <- grep("indep",aname)
   if (!missing(new)) {
      bname <- names(new)
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
            if (is.numeric(b)) {
               if (is.integer(b))
                  old[[j]] <- b
               else
                  old[[j]] <- round(b,4)
              # if (length(grep("(seed|^k\\d|removal|mortality|sexratio|fertility|litter|init\\.den|max\\.age|pregnant)",bname[i]))) {
              # }
            }
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
   }
   if ((length(i <- grep("sex",aname))==1)&&(old[[i]]>1))
      old[[i]] <- old[[i]]/100
  # if ()==1) {
  #    str(old[i])
  # }
   if (length(old[[indep.ind]])==1)
      old[[indep.ind]] <- c(indep.fraction[1],old[[indep.ind]],indep.fraction[3])
   else if (length(old[[indep.ind]])==2)
      old[[indep.ind]] <- c(indep.fraction[1],old[[indep.ind]])
  # old[[indep.ind]] <- old[[indep.ind]][2]
   old
}
'growthRate' <- function(lifestory,verbose=!FALSE,...) {
  # str(list(...))
   if (T & length(list(...))>0) {
     # print("growthRate -> simulate")
     # lifestory <- simulate(lifestory,verbose=FALSE,...)
      lifestory <- do.call("simulate",list(lifestory,quiet=TRUE,...))
   }
   if (is.null(lifestory$output))
      return(numeric())
  # print(analyze(lifestory)$p5)
   pop <- subset(lifestory$output,season==0)
  # print(unique(pop$epoch))
   res2 <- aggregate(pop$id,by=list(era=pop$era,epoch=pop$epoch),length)#$x
   ind <- which(res2$era=="C")
   ind <- c(min(ind)-1L,ind)
   res2 <- res2[ind,]
  # print(res2)
   res <- res2$x
   if (length(res)<5)
      return(numeric())
   if (F & verbose) {
      names(res) <- paste0(res2$epoch,res2$era)
      print(res)
   }
   res <- diff(res)/head(res,-1)
  # str(res)
   if (verbose)
      cat(sprintf("Growth rate = %+.3f\u00B1%.3f (%d)\n",mean(res),sd(res),length(res)))
   res
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
   fs <- 12
   axis <- list(tickfont=list(size=fs),titlefont=list(size=fs),zeroline=F,showgrid=T)
   legend <- list(font=list(size=round(0.9*fs)))
   title <- list(font=list(size=fs),color='yellow')
   config <- list(NULL
                 ,displaylogo=FALSE
                 ,displayModeBar=c("hover","true")[2]
                 ,scrollZoom=TRUE
                 ,modeBarButtonsToRemove=list(NULL
                                             ,"zoom2d","pan2d","toggleSpikelines"
                                             ,"zoomIn2d","zoomOut2d"
                                             ,"autoScale2d","resetScale2d"
                                             ,"hoverClosestCartesian","hoverCompareCartesian"
                                             ,"select2d","lasso2d"
                                             )
                # ,modeBarButtonsToAdd=list("hoverCompareCartesian")
                 ,setBackground="orange"
                 ,toImageButtonOptions=list(width=760,height=540,scale=1)
                 )
   list(base=col.base,bg=col.bg,hist=col.hist,line=col.line,strip=col.strip
       ,p0=p0,axis=axis,legend=legend,title=title,font=list(size=fs),config=config)
}
'curveInputs' <- function(indep.mortality,mortality.cub,mortality.adult,init.den
                         ,litter,broken.C1,broken.C2,max.age,pregnant,sexratio,seed1
                         ,seed2,seed3,fertility,removal.rate,removal.age
                         ,k1d,k1i,k2,simulate) {
   mortality <- mortalityTube(max.age=max.age,mortality.cub=mortality.cub
                             ,mortality.adult=mortality.adult
                             ,k1d=k1d,k1i=k1i,k2=k2)
   tube.lin <- mortalityTubePlot(mortality,scale="lin")
   tube.log <- mortalityTubePlot(mortality,scale="log")
   indep.mortality <- mortality$indep
   mortality <- mortality$depend
   age <- seq(max.age)
   indep.fraction <- init$indep.fraction
   indep.fraction[2] <- broken.C1
   indep.fraction[3] <- broken.C2
   ret <- list(mortality=mortality
              ,indep.mortality=indep.mortality
              ,mortality.cub=mortality.cub
              ,mortality.adult=mortality.adult
              ,age=age
              ,tube.lin=tube.lin
              ,tube.log=tube.log
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
              ,seed3=seed3
              ,fertility=fertility
              ,removal.rate=removal.rate
              ,removal.age=removal.age
              ,k1d=k1d
              ,k1i=k1i
              ,k2=k2
              ,simulate=simulate
              )
   ret
}
'comparePrm' <- function(list1,list2) {
   res <- character()
   for (a in names(list1)) {
      v1 <- list1[[a]]
      v2 <- list2[[a]]
      if (is.null(v2))
         next
      if (!identical(as.numeric(v1),as.numeric(v2)))
         res <- c(res,a)
   }
   res
}
