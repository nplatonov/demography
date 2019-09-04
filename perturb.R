'perturb' <- function(lifestory,set=c("primary","advanced","all"),pattern="") {
   isShiny <- (("shiny" %in% loadedNamespaces())&&(length(shiny::shinyOptions())>0))
   if (isShiny) {
      showModal(modalDialog(title = "Perturbation in progress","Please wait"
                       ,size="s",easyClose = TRUE,footer = NULL))
      on.exit(removeModal())
   }
  # str(lifestory$input)
   prmList <- c('1'="mortality.adult"
               ,'2'="mortality.cub"
               ,'3'="max.age"
               ,'4'="litter"
               ,'5'="indep.fraction1"
               ,'6'="indep.fraction2"
               ,'7'="pregnant"
               ,'8'="k1i"
               ,'9'="k1d"
               ,'10'="fertility"
               ,'11'="removal.rate"
               )
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
   standardized <- c("indep.fraction1","indep.fraction2","pregnant"
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
   str(lifestory$input)
   for (p in .sample(prmList)) {
      if (F & !length(grep("indep.fraction",p)))
         next
      lab <- switch(p
                   ,'mortality.adult'="Adult mortality"
                   ,'mortality.cub'="COY mortality"
                   ,'max.age'="Maximal age"
                   ,'litter'="COY litter size"
                   ,'pregnant'="Birth success"
                   ,'indep.fraction1'="Weaning C1 probability"
                   ,'indep.fraction2'="Weaning C2 probability"
                   ,'fertility'="Age specific fertility"
                   ,'k1i'="Mort. slope of ind. youngs"
                   ,'k1d'="Mort. slope of dep. youngs"
                   ,p)
      if (p=="indep.fraction1") {
         pname <- "indep.fraction"
         pind <- 2L
         prm0 <- lifestory$input[["indep.fraction"]][2]
      }
      else if (p=="indep.fraction2") {
         pname <- "indep.fraction"
         pind <- 3L
         prm0 <- lifestory$input[["indep.fraction"]][3]
      }
      else {
         pname <- p
         pind <- 1L
      }
      prm00 <- lifestory$input[[pname]]
      prm0 <- prm00[pind]
      message(paste0(lab,": ",prm0))
      sc <- switch(p
                  ,'mortality.adult'=-0.03
                  ,'mortality.cub'=-0.05
                  ,'max.age'=+0.05
                  ,'litter'=+0.05
                  ,'pregnant'=+0.1
                  ,'indep.fraction1'=0.2
                  ,'indep.fraction2'=0.2
                  ,'fertility'=+0.1
                  ,'k1d'=2
                  ,'k1i'=2
                  ,0.1)
      if (TRUE) {
        # print(sc)
         if (agr0>0.005) {
            sc <- sc*round(100*agr0)^0.75
         }
         if (p %in% standardized)
            si <- c(-3,-2,-1,0,1,2,3)
         else if (gr0>(+0.002))
            si <- c(-3,-2,-1,0,1)*sign(sc)#*(-sgr0)
         else if (gr0<(-0.002))
            si <- c(-1,0,1,2,3)*sign(sc)#*(-sgr0)
         else
            si <- c(-2,-1,0,1,2)
         if (p %in% standardized) {
            prm <- prm0+si*sc
           # print(prm)
            if (TRUE) {
               if (p %in% c("k1d","k1i"))
                  indP <- prm>=1 & prm<=20
               else
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
      k <- k+1L
      if (isShiny)
         showNotification(closeButton=TRUE,paste0(lab," (",k," of ",nk,") ...")
                         ,id=p,duration=99)
      if (prm_validation <- TRUE) {
         if (p %in% standardized) {
            if (p %in% c("k1d","k1i")) {
               prm[prm<1] <- 1
               prm[prm>20] <- 20
            }
            else {
               prm[prm<0.01] <- 0.01
               prm[prm>0.99] <- 0.99
            }
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
         ##~ str(p)
         ##~ str(pname)
         ##~ str(prm00)
         prm00[pind] <- prm[i]
         ##~ str(prm00)
        # print(prmlab[i])
        # arglist[[p]] <- prm[i] ## --
         arglist[[pname]] <- prm00 ## ++
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
