'perturb' <- function(lifestory,set=c("primary","advanced","all"),pattern="") {
   isShiny <- (("shiny" %in% loadedNamespaces())&&(length(shiny::shinyOptions())>0))
   if (isShiny) {
      showModal(modalDialog(title = "Perturbation in progress","Please wait"
                       ,size="s",easyClose = TRUE,footer = NULL))
      on.exit(removeModal())
   }
  # str(lifestory$input)
  # prmDev[[1]] <- data.frame(name="mortality.adult",min=0.08,max=0.12,step=0.005)
  # prmDev[[2]] <- data.frame(name="mortality.adult",min=0.08,max=0.12,step=0.005)
   prmList <- c('1'="mortality.adult"
               ,'2'="mortality.cub"
               ,'3'="max.age"
               ,'4'="litter"
               ,'5'="broken.C1"
               ,'6'="pregnant"
               ,'7'="k1i"
               ,'8'="k1d"
               ,'9'="fertility"
               ,'10'="removal.rate"
               )
   canceled <- c("broken.C2")
   if (nchar(pattern)) {
      prmList <- grep(pattern,prmList,ignore.case=TRUE,value=TRUE)
   }
   else {
      set <- match.arg(set)
      if (set=="primary")
         prmList <- prmList[1:9]
      else if (set=="advanced")
         prmList <- prmList[-c(1:9)]
   }
   standardized <- c("broken.C1","broken.C2","pregnant"
                    ,"fertility","k1i","k1d")
   ret <- vector("list",length(prmList))
   names(ret) <- prmList
   cs <- colorScheme()
   if (isShiny)
      showNotification(closeButton=TRUE,"check reference"
                      ,id="ref",duration=99)
   cat("---- basic -----\n")
  # growthRate(lifestory) ## just for test with next line
   g0 <- growthRate(lifestory,nochanged_dummy=TRUE)
   cat("---- ===== -----\n")
   if (isShiny)
      removeNotification(id="ref")
   if (!length(g0))
      return(ret)
  # return(ret)
   gr0 <- mean(g0)
   agr0 <- abs(gr0)
   sgr0 <- sign(gr0)
   k <- 0L
   nk <- length(prmList)
   initRange <- rangePrm()
  # str(lifestory$input)
   for (p in .sample(prmList)) {
      rng <- initRange[[p]]
      if (F & !length(grep("indep.fraction",p)))
         next
      lab <- switch(p
                   ,'mortality.adult'="Adult mortality"
                   ,'mortality.cub'="COY mortality"
                   ,'max.age'="Maximal age"
                   ,'litter'="COY litter size"
                   ,'pregnant'="Birth success"
                   ,'broken.C1'="Weaning C1 probability"
                   ,'broken.C2'="Weaning C2 probability"
                   ,'fertility'="Age specific fertility"
                   ,'k1i'="Mort. slope of ind. youngs"
                   ,'k1d'="Mort. slope of dep. youngs"
                   ,'removal.rate'="Human-caused removal rate"
                   ,paste("unspecified description for",dQuote(p)))
      if (p=="broken.C1") {
         pname <- "indep.fraction"
         pind <- 2L
         prm0 <- lifestory$input[[pname]][2]
      }
      else if (p=="broken.C2") {
         pname <- "indep.fraction"
         pind <- 3L
         prm0 <- lifestory$input[[pname]][3]
      }
      else {
         pname <- p
         pind <- 1L
      }
      prm00 <- lifestory$input[[pname]]
      prm0 <- prm00[pind]
      message(paste0(lab,": ",prm0))
      sc <- rng$forcing
     # print(sc)
     # if (agr0>0.005) {
     #    sc <- sc*round(100*agr0)^0.75
     # }
      if (gr0>(+0.005))
         si <- seq(-5,3)*sign(sc)#*(-sgr0)
      else if (gr0<(-0.005))
         si <- seq(-3,5)*sign(sc)#*(-sgr0)
      else
         si <- seq(-4,4)
     # print(gr0)
     # print(si)
      prm <- with(rng,seq(from,to,len=ifelse(agr0<0.002,11,ifelse(agr0<0.005,9,7))))
      if (TRUE) {
         d <- mean(diff(prm))
         d <- round(d,-floor(log(d)/log(10)))
         prm <- prm0+d*c(seq(-15,-1),0,seq(1,15))
         prm <- prm[prm>=rng$from-1e-6 & prm<=rng$to+1e-6]
        # print(prm)
      }
      indC <- which.min(abs(prm-prm0))
      if (length(indC)>1)
         indC <- .sample(indC)
      ind <- si+indC
      ind <- sort(ind[ind>0 & ind<=length(prm)])
     # print(ind)
     # prm <- prm[ind]
     # print(prm)
      repeat({
         if (length(ind)<=5)
            break
         ind2 <- abs(range(ind)-indC)
         ind3 <- which(ind2==max(ind2))
         if (length(ind3)==2)
            ind <- head(tail(ind,-1),-1)
         else if (ind3==1)
            ind <- tail(ind,-1)
         else if (ind3==2)
            ind <- head(ind,-1)
      })
     # print(ind)
      prm[indC] <- prm0
      prm <- c(na.omit(prm[ind]))
     # print(prm)
      indC <- which(prm==prm0)
      k <- k+1L
      if (isShiny)
         showNotification(closeButton=TRUE,paste0(lab," (",k," of ",nk,") ...")
                         ,id=p,duration=99)
      prmlab <- round(prm,3)
      res2 <- NULL
     # print(sprintf("%+.3f\u00B1%.3f",mean(g0),sd(g0)))
      arglist <- list(lifestory,NA)
      names(arglist) <- c("",p)
      desc <- rep("",length(prm))
      res <- data.frame(prm=prmlab,desc="",mean=NA,sd=NA)
      for (i in seq(nrow(res))) {
         prm00[pind] <- prm[i]
         arglist[[pname]] <- prm00 ## ++
        # str(arglist[-1])
         g1 <- if (i==indC) g0 else do.call("growthRate",arglist)
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
     # if (p %in% c("pregnant","indep.fraction1"))
     #    print(res)
      if (isShiny)
         removeNotification(id=p)
      if (is.null(res2))
         next
      if (length(desc)==length(unique(desc))) {
         res$desc <- factor(desc,levels=desc[order(prm)],ordered=TRUE)
      }
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
      if ("plotly" %in% loadedNamespaces()) {
         if (T) {
            da <- res2
           # da <- da[sample(nrow(da)),]
            prm <- da$prm
            prmS <- sort(unique(prm))
            da$prm <- as.character(prm)
            prm0 <- as.character(prm0)
            ind <- which(da$prm %in% prm0)
            da$prm[ind] <- paste0("<b>",prm[ind],"</b>")
            indS <- which(prmS %in% prm0)
            prmS[indS] <- paste0("<b>",prmS[indS],"</b>")
            da$prm <- factor(da$prm,level=prmS,ordered=TRUE)
           # print(levels(da$prm))
           # print(levels(da$desc))
           # da$desc <- as.character(da$desc)
            res2 <- da
         }
         lab <- c(x="Growth Rate",y=lab)
         res2$label <- gsub("((.+)(\\n))*(.+)","\\4",res2$desc)
         s1 <- plot_ly()
         s1 <- add_trace(s1,data=res2,type="violin",split=~prm
                        ,text=~desc
                        ,name=~label
                        ,hoverinfo="name"
                        ,y=~prm,x=~value,orientation='h'
                       # ,x=~prm,y=~value,orientation='v',showlegend=FALSE
                        ,meanline=list(visible=T)
                        )
         if (FALSE)
            s1 <- layout(s1
                        ,xaxis=c(list(title=lab["x"]),cs$axis)
                        ,yaxis=c(list(title=lab["y"],type="category"),cs$axis)
                        ,legend=cs$legend
                        )
         else {
            s1 <- layout(s1,xaxis=cs$axis,yaxis=cs$axis,legend=cs$legend)
            s1 <- layout(s1
                        ,xaxis=list(title=lab["x"])
                        ,yaxis=list(title=lab["y"]
                                  # ,categoryorder="array"
                                  # ,categoryarray=prm2
                                   )
                        ,legend=list(orientation='v')
                       # ,legend=list(orientation='h',x=0.5,y=1.2)
                        )
            prm <- cs$config
            prm[[1]] <- s1
            s1 <- do.call("config",prm)
         }
      }
      else if (!FALSE)
         s1 <- ggplot(res2,aes(prm,value))+
               geom_violin(fill=cs$hist,colour=cs$base)+
               geom_point(data=res,aes(prm,mean))+
               xlab(lab)+ylab("Growth Rate")+
               facet_grid(prm~.,scales="free",labeller=labeller(prm=desc))+
               scale_x_continuous(breaks=prmlab)+
               coord_flip()+
               cs$p0+
               NULL
      else if (FALSE)
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
      ret[[p]] <- list(plot=s1,data=res2)
     # break
   }
   ret
}
