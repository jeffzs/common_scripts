# COPYRIGHT: 2014-present
# AUTHOR: Zhifei Sun, zhifei.sun@duke.edu
# PURPOSE: Repository for custom functions used in every project

# required packages 
list.of.packages <- c("devtools","MatchIt", "rms" ,"Hmisc","ggplot2","gridExtra","lubridate","dplyr","tidyr",
                      "stringr","gmodels","data.table","extrafont", "maps","bit64")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# devtools::install_github('rstudio/DT')
# devtools::install_github("csgillespie/addinmanager")
# devtools::install_github("rstudio/addinexamples", type = "source")
# LIBRARIES
library(MatchIt)                    # propensity matching
library(survival)                   # survival analysis
library(rms)                        # restricted cubic spline
library(Hmisc)                      # summary tables
library(ggplot2)                    # plotting
library(plyr)                       # useful other functions
library(dplyr)
library(tidyr)
library(gridExtra)
library(reshape2)
library(lubridate)
library(stringr)
library(gmodels)                    # for CrossTable()
library(data.table)                 # big data analysis
library(extrafont)                  # for new york time fonts in plots
# font functions used everytime a new font is added to the system
 #font_import()
 #loadfonts()
#library(devtools)
library(maps)                       # maps
library(bit64)
library(grid)

set.seed(100) 

# FUNCTIONS ----------------------------------------------------------
GetVarName <- function(x) { deparse(substitute(x)) }
  # Outputs variable name as a string.

CreateSummaryTable <- function(formula, data, title="Table1", save=TRUE, print.sd=T, na.include=F, exclude1=F, round=2,
                               continous.threshold = 3, prn=TRUE,long=TRUE,fisher=F, overall=T, test=T) {
  library("Hmisc")
  
  # fisher function
  u<-function(a,b){ 
    if (any(dim(a) < 2L)) {
      p <- list(P=9999, stat=1, df=1, testname="fisher", statname ="")  #won't output anything if variable does not have at least 2 rows and columns
    } else {
      j <- fisher.test(a) 
      p <- list(P=j$p.value, stat=1, df=1, testname=j$method, statname="") 
    }
    return(p) 
  } 
  
  if(fisher==F) {
    Table <-summaryM(formula=formula,  data=data, overall=overall ,test=test, na.include = na.include, continuous = continous.threshold, conTest = conTestkw, catTest = catTestchisq)
  }
  if(fisher==T) {
    Table <-summaryM(formula=formula,  data=data, overall=overall ,test=test, na.include = na.include, continuous = continous.threshold, conTest = conTestkw, catTest = u)
  }
  
  Table.print <- print(Table, what="%",  prn=prn,  prmsd=print.sd,   long=long,
                         exclude1=exclude1,   prtest='P', pctdig=1, round=round)

  if (save==TRUE) {
    write.table(Table.print, sep="," , file=paste(title,".csv",sep=""), col.names=F)
   
    print(paste("Table was saved in:",getwd()))
  } else { print("Table was not saved!")}
}

  
  
CreateOddsRatioTable <- function (gfit, save=TRUE, filename="Table_glm_OR.csv", round=3) {
  # create odds ratio table w/ Var name/OR/low95/hi95/pvalue from glm.
  # 
  # Args:
  #   gfit: Any variable.
  #   filename: Output csv file name
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory
  or.table <- NULL
  for (i in 1:length(names(gfit$coef))) {
    x <- cbind(or=exp(summary(gfit)$coef[i,1]),
               exp(confint.default(gfit)[i,1]), 
               exp(confint.default(gfit)[i,2]),
               p.value=summary(gfit)$coef[i,4])
    or.table <- rbind(or.table, x)
  }
  
  or.table <- round(or.table, digits=round)
  # or.table[or.table>100] <- 100
  or.table <- as.data.frame(cbind(names(gfit$coef), or.table))
  or.table$p.value <- as.numeric(as.character(or.table$p.value))
  or.table$p.value[or.table$p.value<0.001] <- "<0.001"
  colnames(or.table) <- c('Variables', 'Odds Ratio', 'Lower 95% CI', 'Upper 95% CI', 'p-value')
  or.table <- or.table[-1,]
  rownames(or.table) <- NULL
  if(save==TRUE) { write.table(or.table, sep=',', file=filename, row.names = FALSE) }
  #print(or.table)
  return (or.table)
}

CreateLmTable <- function (gfit, save=TRUE, filename="Table_lm_estimates.csv", decimal_point=1) {
  # create table w/ Var name/estimate/low95/hi95/pvalue from lm, using a continuous outcome var.
  # 
  # Args:
  #   gfit: Any variable.
  #   filename: Output csv file name
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory
  or.table <- NULL
  for (i in 1:length(names(gfit$coef))) {
    x <- cbind(or=round(summary(gfit)$coef[i,1], decimal_point),
               round (confint.default(gfit)[i,1], decimal_point), 
               round (confint.default(gfit)[i,2], decimal_point),
               p.value=round(summary(gfit)$coef[i,4],3))
    or.table <- rbind(or.table, x)
  }
  #or.table <- round(or.table, digits=3)
  #or.table[or.table>100] <- 100
  or.table <- as.data.frame(cbind(names(gfit$coef), or.table))
  or.table$p.value <- as.numeric(as.character(or.table$p.value))
  or.table$p.value[or.table$p.value<0.001] <- "<0.001"
  colnames(or.table) <- c('Variables', 'Estimate', 'Lower 95% CI', 'Upper 95% CI', 'p-value')
  or.table <- or.table[-1,]
  
  if(save==TRUE) { write.table(or.table, sep=',', file=filename, row.names = FALSE) }
  return (or.table)
}

CreateLogLmTable <- function (gfit, save=TRUE, filename="Table_Loglm_estimates.csv", decimal_point=1) {
  # create table w/ Var name/OR/low95/hi95/pvalue from lm, using a log-transfromed outcome var.
  # 
  # Args:
  #   gfit: Any variable.
  #   filename: Output csv file name
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory

  or.table <- NULL
  for (i in 1:length(names(gfit$coef))) {
    x <- cbind(or=exp(summary(gfit)$coef[i,1]),
               exp(confint.default(gfit)[i,1]), 
               exp(confint.default(gfit)[i,2]),
               p.value=summary(gfit)$coef[i,4])
    or.table <- rbind(or.table, x)
  }
  or.table <- round(or.table, digits=decimal_point)
  #or.table[or.table>100] <- 100
  or.table <- as.data.frame(cbind(names(gfit$coef), or.table))
  or.table$p.value <- as.numeric(as.character(or.table$p.value))
  or.table$p.value[or.table$p.value<0.001] <- "<0.001"
  colnames(or.table) <- c('VAR', 'Estimate_OrigUnit', 'LO95_L', 'HI95_L', 'PVAL')
  or.table <- or.table[-1,]
  rownames(or.table) <- NULL
  
  Intercept <- coef(gfit)["(Intercept)"]
  Estimate_OrigUnit <- round ((as.numeric(paste(or.table$Estimate_OrigUnit)) - 1) * exp(Intercept) , decimal_point)
  LO95_L <-   round ((as.numeric(paste(or.table$LO95_L)) - 1) * exp(Intercept) , decimal_point)
  HI95_L <- round((as.numeric(paste(or.table$HI95_L)) - 1) * exp(Intercept) , decimal_point)

  FinalTable <- data.frame(or.table$VAR, Estimate_OrigUnit,LO95_L,HI95_L,or.table$PVAL)
  colnames(FinalTable) <- c('Variables', 'Estimates', 'Lower 95% CI', 'Upper 95% CI', 'p-values')   # final column names
  
  #print(FinalTable)
  if(save==TRUE) { write.table(FinalTable, sep=',', file=filename, row.names = FALSE) }
  return (FinalTable)
}

CreateHazardRatioTable <- function (gfit, filename="Table_Cox_HR.csv", save = T,round=3) {
  # create hazard ratio table w/ Var name/OR/low95/hi95/pvalue from glm.
  # 
  # Args:
  #   gfit: Any variable.
  #   filename: Output csv file name
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory
  library (Hmisc)
  or.table <- NULL
  for (i in 1:length(names(gfit$coef))) {
    x <- cbind(or=exp(summary(gfit)$coef[i,1]),
               exp(confint.default(gfit)[i,1]), 
               exp(confint.default(gfit)[i,2]),
               p.value=summary(gfit)$coef[i,5])
    or.table <- rbind(or.table, x)
  }
  or.table <- round(or.table, digits=round)
  #or.table[or.table>100] <- 100
  or.table <- as.data.frame(cbind(names(gfit$coef), or.table))
  or.table$p.value <- as.numeric(as.character(or.table$p.value))
  or.table$p.value[or.table$p.value<0.001] <- "<0.001"
  colnames(or.table) <- c('Variables', 'Hazard Ratio', 'Lower 95% CI', 'Upper 95% CI', 'p-value')
  #or.table <- or.table[-1,]
  rownames(or.table) <- NULL
  write.table(or.table, sep=',', file=filename, row.names=FALSE)
  return (or.table)
}

ggkmTable <- function(sfit, risktable=TRUE,returns = T,
                      xlabs = "Time (unit)", ylabs = "Survival Probability",
                      ystratalabs = NULL, ystrataname = NULL, legend_xloc = 0.35,
                      xlim = -1, timeby = 12, 
                      main = "", 
                      pval = TRUE, pval_xloc=0.6, pval_yloc=0.1 , 
                      linetype.manual = c("solid","dashed","dotted","dotdash","twodash","longdash"), line.size = 0.4, line.color,
                      save = FALSE, file.name = "Figure.pdf", file.height = 6, file.width = 7, font.family ="Franklin Gothic Book",
                      color = FALSE) {
  # Create ggplot-based KM table w/ numbers at risk at the bottom
  # 
  # Args:
  #   sfit: survival function
  #   risktable: Boolean whether to display risk table
  #   returns: Boolean whether to return ggplot image
  #   xlab: x-axis label
  #   ylab: y-axis label
  #   ystratalabs: label for the legends to the strata
  #   ystrataname: vector of labels for different strata
  #   xlim: limits for x-axis
  #   timeby: x-axis display increments in (months)
  #   main: figure title
  #   pval: Whether to display log-rank test p-value
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory
  library("ggplot2")
  library("plyr")
  library("gridExtra")
  library("gtable")
  library("grid")
  
  surv <- NULL
  n.risk <- NULL
  if(is.null(ystratalabs)) {
    ystratalabs <- as.character(levels(summary(sfit)$strata))
  }
  m <- max(nchar(ystratalabs))
  if(is.null(ystrataname)) ystrataname <- ""
  
  if (xlim == -1){
    times <- seq(0, max(sfit$time), by = timeby)
  } else {
    times <- seq(0, xlim, by = timeby)
  }
  .df <- data.frame(time = sfit$time, n.risk = sfit$n.risk,
                    n.event = sfit$n.event, surv = sfit$surv, strata = summary(sfit, censored = T)$strata,
                    upper = sfit$upper, lower = sfit$lower)
  levels(.df$strata) <- ystratalabs
  zeros <- data.frame(time = 0, surv = 1, strata = factor(ystratalabs, levels=levels(.df$strata)),
                      upper = 1, lower = 1)
  .df <- rbind.fill(zeros, .df)
  d <- length(levels(.df$strata))
  
  if (color==FALSE)
  {
  p <- ggplot(.df, aes(time, surv, group = strata)) +
    
    # Plot Lines
    geom_step(aes(linetype = strata), size = line.size) +  # line size
    scale_linetype_manual(values=linetype.manual)+         # line type
    
    # Axis and Grid
    theme_bw() +
    
    theme(axis.title.x = element_text(vjust = 0.5,face="bold",size=14, family=font.family)) +
    theme(axis.text.x  = element_text(colour="black",size=10, family =font.family)) +
    theme(axis.title.y = element_text(face="bold",size=14, family=font.family)) +
    theme(axis.text.y  = element_text(colour="black",size=10, family = font.family)) +
    #theme(axis.line = element_line(colour = "black",size = 1)) +
    theme(axis.line.x = element_line(color="black")) +
    theme(axis.line.y = element_line(color="black")) +
    
    scale_x_continuous(xlabs, breaks = times, limits = c(0, max(times))) +  #max(sfit$time)
    scale_y_continuous(ylabs, limits = c(0, 1)) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.border = element_blank()) +
    #theme(panel.border = element_rect(colour = "black")) +
    theme(panel.background = element_blank()) +
    
    # Legend
    theme(legend.justification = c(0,0), legend.background = element_rect(fill = "transparent",colour = NA), 
          legend.position = c(0,0), legend.background = element_blank()) +
    theme(legend.title = element_text(colour="black",face="bold",size=12, family = font.family))+
    theme(legend.text = element_text(colour="black",face="bold",size=12, family = font.family)) +
    labs(linetype = ystrataname, name=ystrataname, colour=ystrataname) +   
    
    # Margin ( m = max char length of strata)
    #theme(plot.margin = unit(c(0, 1, 1, ifelse(m < 10, 1.5, 5)), "lines")) +
    theme(plot.margin = unit(c(0, 1, 1, ifelse(m < 10, 2.5, 5)), "lines")) +
    
    # Title
    ggtitle(main) 
  }
  else {  # color == TRUE
    p <- ggplot(.df, aes(time, surv, group = strata)) +
    
      # Plot Lines
      geom_step(aes(color=strata), size = line.size) +  # line size and color
      scale_linetype_manual(values=linetype.manual)+         # line type
      
      # Axis and Grid
      theme(axis.title.x = element_text(vjust = 0.5,face="bold",size=14, family=font.family)) +
      theme(axis.text.x  = element_text(colour="black",size=10, family =font.family)) +
      theme(axis.title.y = element_text(face="bold",size=14, family=font.family)) +
      theme(axis.text.y  = element_text(colour="black",size=10, family = font.family)) +
      #theme(axis.line = element_line(colour = "black",size = 1)) +
      theme(axis.line.x = element_line(color="black")) +
      theme(axis.line.y = element_line(color="black")) +
      
      scale_x_continuous(xlabs, breaks = times, limits = c(0, max(times))) +  #max(sfit$time)
      scale_y_continuous(ylabs, limits = c(0, 1)) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(panel.border = element_blank()) +
      #theme(panel.border = element_rect(colour = "black")) +
      theme(panel.background = element_blank()) +
      
      # Legend
      theme(legend.justification = c(0,0), legend.background = element_rect(fill = "transparent",colour = NA), 
            legend.position = c(0,0), legend.background = element_blank()) +
      theme(legend.title = element_text(colour="black",face="bold",size=12, family = font.family))+
      theme(legend.text = element_text(colour="black",face="bold",size=12, family = font.family)) +
      labs(linetype = ystrataname, name=ystrataname, colour=ystrataname) +   
      
      # Margin ( m = max char length of strata)
      theme(plot.margin = unit(c(0, 1, 1, ifelse(m < 10, 2.5, 5)), "lines")) +
      
      # Title
      ggtitle(main) 
  }

  if(pval) {
    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail = FALSE)
    pvaltxt <- ifelse(pval < 0.001, "p < 0.001", paste("p =", round(pval, digits=3)))
    p <- p + 
      # P-value Text
      annotate("text", 
          x = max(times), y = 0, hjust = 1,       #right justified
          label = pvaltxt, family = font.family)
  }
  
  ## Create a blank plot for place-holding
  ## .df <- data.frame()
  blank.pic <- ggplot(.df, aes(time, surv)) +
    geom_blank() +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank(), panel.grid.major = element_blank(),
          panel.border = element_blank())
  
  if(risktable) {
    ## Create table graphic to include at-risk numbers
    risk.data <- data.frame(
      strata = factor(summary(sfit, times = times, extend = TRUE)$strata),
      time = summary(sfit, times = times, extend = TRUE)$time,
      n.risk = summary(sfit, times = times, extend = TRUE)$n.risk)
    
    risk.data$strata <- factor(risk.data$strata, levels=rev(levels(risk.data$strata)))
  
    data.table <- ggplot(risk.data, aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
      geom_text(size = 3, family = font.family, hjust=0.7) +
      
      theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)), labels = rev(ystratalabs)) +
      # scale_y_discrete(#format1ter = abbreviate,  # breaks = 1:3,    # labels = ystratalabs) +
      scale_x_continuous("Numbers At Risk", limits = c(0, max(times))) +

      theme(panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            
            axis.text.x = element_blank(), 
            axis.ticks = element_blank(),
      # Numbers at risk table themes      
            axis.title.x = element_text(size = 14, vjust = 1, family = font.family), 
            axis.text.y = element_text(size = 12, face = "bold", hjust = 1,family = font.family))
    
    data.table <- data.table + theme(legend.position = "none") +  xlab(NULL) + ylab(NULL)
    data.table <- data.table + theme(plot.margin = unit(c(1, 1, 0.1, ifelse(m < 10, 2.5, 5)), "lines"))
      #theme(plot.margin = unit(c(-2, 1, 0.1, ifelse(m < 10, 2.5, 7)-0.19 * m), "lines"))
    
    grid.newpage()
    #plot_grid(p, data.table, align = 'v', rel_widths = c(7, 1))
    
    #Extract Grobs
    g1 <- ggplotGrob(p)
    g2 <- ggplotGrob(data.table)
    #Bind the tables
    g <- gtable:::rbind_gtable(g1, g2, "first")
    #Remove a row between the plots
    g <- gtable_add_rows(g, unit(-1,"cm"), pos=nrow(g1))
    # relative heights
    panels <- g$layout$t[grep("panel", g$layout$name)]
    g$heights[panels[1]] <- unit(7, 'null')
    g$heights[panels[2]] <- unit(1, 'null')
    

    # clipping
    g$layout$clip[g$layout$name == "panel"] <- "off"
    #draw
    grid.newpage()
    grid.draw(g)
    
    if (save) {
      print("Saving Figure & Summary Table")
      if (last(unlist(strsplit(file.name, "[.]"))) %in% c("pdf","PDF")) {
        dev.copy(pdf,file = file.name, width = file.width, height = file.height); dev.off()
        embed_fonts(file.name)  # embed true type fonts
      }
      else if (last(unlist(strsplit(file.name, "[.]"))) %in% c("png","PNG")) {
        dev.copy(png,file ==file.name,width=file.width,height=file.height,units="in",res=300); dev.off()
      }
      sink(file = paste(file.name,".txt",sep=""))
      print(summary(sfit, time=times))
      sink()
    }
    if (returns) {
#       a <- arrangeGrob(p, blank.pic, data.table, clip = FALSE, nrow = 3, ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null")))
      return(g)
    }
  } #end if risktable
  else {   # if no risktable
    # draw figure
    print(p)
    # save figure
    if (save == TRUE) {
      print("Saving Figure & Summary Table")
      if (last(unlist(strsplit(file.name, "[.]"))) %in% c("pdf","PDF")) {
        dev.copy(pdf,file = file.name, width = file.width, height = file.height); dev.off()
        embed_fonts(file.name)  # embed true type fonts
      }
      else if (last(unlist(strsplit(file.name, "[.]"))) %in% c("png","PNG")) {
        dev.copy(png,file.name,width=file.width,height=file.height,units="in",res=300)
        dev.off()
      }
      sink(file = paste(file.name,".txt",sep=""))
      print(summary(sfit, time=times ))
      sink()
    }
    # return figure
    if(returns) return(p)
  }
}


meandiffplot <- function (x) {
  mdiff<-data.frame(x$sum.matched)
  names(mdiff)[4]<-c("m_mean_diff")
  diffplot<-ggplot(mdiff, aes(m_mean_diff) )
  diffplot<- diffplot+ geom_histogram (fill="grey")
  diffplot<- diffplot+ geom_density (colour="red")
  diffplot 
}

meandifftable<- function (x){
  post<-data.frame(x$sum.matched[4])
  matchID <- as.vector (row.names (post) )
  names(post)[1]<-c("m_mean_diff")
  post$absolute<- abs(post[1])
  total2<-post[order (-post$absolute, na.last=NA) ,]
  meandiffover1<- subset(total2[1], total2[1]> .1 | total2[1]< -.1)
  meandiffover1
}

SaveUniqueValues <- function(x, var.list, ordered = T, show = F, filename = "Unique_Var.csv") {
  # Get and save unique values of a dataset
  # 
  # Args:
  #   DB: dataset
  #   VARIABLE: element of interest
  #   filename: output file
  #
  # Returns: n/a
  df <- unique(x[var.list])
  if (show == T ) print(df)
  write.csv(df, file=filename, row.names=FALSE)
}

termplot2 <- function (model, data = NULL, envir = environment(formula(model)), 
                       partial.resid = FALSE, rug = FALSE, terms = NULL, se = FALSE, 
                       xlabs = NULL, ylabs = NULL, main = NULL, col.term = 2, lwd.term = 1.5, 
                       col.se = "orange", lty.se = 2, lwd.se = 1, col.res = "gray", 
                       cex = 1, pch = par("pch"), col.smth = "darkred", lty.smth = 2, 
                       span.smth = 2/3, ask = dev.interactive() && nb.fig < n.tms, 
                       use.factor.levels = TRUE, smooth = NULL, ylim = "common",
                       rug.type = "rug", yscale="regular",
                       col.dens="#80808033", se.type="line", density.proportion = .1, log="",
                       ...) 
{
  # Options for new variables
  # rug.type: "rug", "density"
  # yscale: "regular", "exponential"
  # se.type: "line", "polygon"
  
  #   model  : fitted model object
  #   
  #   data  : data frame in which variables in model can be found
  #   
  #   envir	:environment in which variables in model can be found
  #   
  #   partial.resid	: logical; should partial residuals be plotted?
  #   
  #   rug	: add rugplots (jittered 1-d histograms) to the axes?
  #   
  #   terms	
  #   which terms to plot (default NULL means all terms); a vector passed to predict(.., type = "terms", terms = *).
  #   
  #   se	
  #   plot pointwise standard errors?
  #   
  #   xlabs	
  #   vector of labels for the x axes
  #   
  #   ylabs	
  #   vector of labels for the y axes
  #   
  #   main	
  #   logical, or vector of main titles; if TRUE, the model's call is taken as main title, NULL or FALSE mean no titles.
  #   
  #   col.term, lwd.term	
  #   color and line width for the ‘term curve’, see lines.
  #   
  #   col.se, lty.se, lwd.se	
  #   color, line type and line width for the ‘twice-standard-error curve’ when se = TRUE.
  #   
  #   col.res, cex, pch	
  #   color, plotting character expansion and type for partial residuals, when partial.resid = TRUE, see points.
  #   
  #   ask	
  #   logical; if TRUE, the user is asked before each plot, see par(ask=.).
  #   
  #   use.factor.levels	
  #   Should x-axis ticks use factor levels or numbers for factor terms?
  #   
  #   smooth	
  #   NULL or a function with the same arguments as panel.smooth to draw a smooth through the partial residuals for non-factor terms
  #   
  #   lty.smth, col.smth, span.smth	
  #   Passed to smooth
  #   
  #   ylim	
  #   an optional range for the y axis, or "common" when a range sufficient for all the plot will be computed, or "free" when limits are computed for each plot.
  #   
  #   plot	
  #   if set to FALSE plots are not produced: instead a list is returned containing the data that would have been plotted.
  #   
  #   transform.x	
  #   logical vector; if an element (recycled as necessary) is TRUE, partial residuals for the corresponding term are plotted against transformed values. The model response is then a straight line, allowing a ready comparison against the data or against the curve obtained from smooth-panel.smooth.
  #   
  #   ...	
  #   other graphical parameters.
  
  # Basic functions used by termplot
  se.lines <- function(x, iy, i, ff = 2) {
    tt <- ff * terms$se.fit[iy, i]
    upper_ci <- tms[iy, i] + tt
    lower_ci <- tms[iy, i] - tt
    
    if (identical(yscale, "exponential")){
      upper_ci <- exp(upper_ci)
      lower_ci <- exp(lower_ci)
    }
    lines(x, upper_ci, lty = lty.se, lwd = lwd.se, 
          col = col.se)
    lines(x, lower_ci, lty = lty.se, lwd = lwd.se, 
          col = col.se)
  }
  # The iy variable contains the ordering of the y-variable
  # the x-variable is already ordered 
  se.polygon <- function(x, iy, i, ff = 2){
    tt <- ff * terms$se.fit[iy, i]
    upper_ci <- tms[iy, i] + tt
    lower_ci <- tms[iy, i] - tt
    
    if (identical(yscale, "exponential")){
      upper_ci <- exp(upper_ci)
      lower_ci <- exp(lower_ci)
    }
    
    current_i.backw <- order(x, decreasing = TRUE)
    current_i.forw <- order(x, decreasing = FALSE)
    
    # The x-axel is always the same
    x.poly <- c(x[current_i.forw] , x[current_i.backw])
    # The y axel is based upin the current model
    y.poly <- c(upper_ci[current_i.forw], lower_ci[current_i.backw])
    polygon(x.poly , y.poly , col = col.se, border = NA)
  }
  plot.density <- function(xx){
    # calculate the coordinates of the density function
    density <- density( xx )
    # the height of the densityity curve
    max.density <- max(density$y)
    
    # transform the y-coordinates of the density
    if (density.proportion >= 1){
      warning("Can't have a density proportion of 100 % of the plot, recommended is less than 0.2")
      density.proportion <- .1
    }
    
    # Get the boundaries of the plot to
    # put the density polygon at the x-line
    yscale <- par("usr")[3:4]
    # get the "length" and range of the y-axis
    yspan <- max(yscale) - min(yscale)
    
    density_percent <- density$y/max.density
    height <- density.proportion * density_percent * yspan  + min(yscale)
    if (par("ylog")){
      # For some odd reason the default log scale is 10-based
      # when the y-scale is logarithmic
      height <- 10^(height)
    }
    
    ## plot the polygon
    polygon( density$x , height, border = F, col = col.dens)
  }
  plot.rug <- function(xx){
    n <- length(xx)
    lines(rep.int(jitter(xx), rep.int(3, n)), rep.int(ylims[1L] + 
                                                        c(0, 0.05, NA) * diff(ylims), n))
    if (partial.resid) 
      lines(rep.int(xlims[1L] + c(0, 0.05, NA) * diff(xlims), 
                    n), rep.int(pres[, i], rep.int(3, n)))
  }
  
  plot.factor <- function(i, ff, xx, 
                          xlab, ylab, main){
    
    if (!is.null(model$na.action)) 
      ff <- naresid(model$na.action, ff)
    ll <- levels(ff)
    xlims <- range(seq_along(ll)) + c(-0.5, 0.5)
    
    if (rug) {
      xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
      xlims[2L] <- xlims[2L] + 0.03 * diff(xlims)
    }
    tmp_ylims <- ylims
    if (identical(yscale, "exponential")){
      tmp_ylims <- exp(tmp_ylims)
    }
    plot(1, 0, type = "n", xlab = xlab, 
         ylab = ylab, log=log,
         xlim = xlims, ylim = tmp_ylims, 
         main = main, xaxt = "n", 
         ...)
    
    if (use.factor.levels) 
      axis(1, at = seq_along(ll), labels = ll, ...)
    else axis(1)
    for (j in seq_along(ll)) {
      ww <- which(ff == ll[j])[c(1, 1)]
      jf <- j + c(-0.4, 0.4)
      
      # Plot confidence interval
      if (se){
        if (se.type == "polygon"){
          se.polygon(jf, iy = ww, i = i)
        }else{
          se.lines(jf, iy = ww, i = i)
        }
      }
      
      yvalues <- tms[ww, i]
      if (identical(yscale, "exponential")){
        yvalues <- exp(yvalues)
      }
      lines(jf, yvalues, col = col.term, lwd = lwd.term,
            ...)
    }
  }
  
  plot.continuous <- function(i,
                              xx, 
                              xlab, ylab, main){
    
    
    if (!is.null(use.rows)) 
      xx <- xx[use.rows]
    xlims <- range(xx, na.rm = TRUE)
    if (rug && rug.type != "density") 
      xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
    oo <- order(xx)
    
    yvalues <- tms[, i]
    tmp_ylims <- ylims
    if (identical(yscale, "exponential")){
      yvalues <- exp(yvalues)
      tmp_ylims <- exp(tmp_ylims)
    }
    plot(range(xx), range(yvalues), type = "n", xlab = xlab,  log=log,
         ylab = ylab, xlim = xlims, ylim = tmp_ylims, 
         main = main[i], 
         ...)
    
    # Plot confidence interval
    if (se){
      if (se.type == "polygon"){
        se.polygon(xx[oo], iy = oo, i = i)
      }else{
        se.lines(xx[oo], iy = oo, i = i)
      }
      
    }
    
    lines(xx[oo], yvalues[oo], 
          col = col.term, lwd = lwd.term)
  }
  
  # Get the the terms that are of interest
  which.terms <- terms
  terms <- if (is.null(terms)) 
    predict(model, type = "terms", se.fit = se)
  else predict(model, type = "terms", se.fit = se, terms = terms)
  
  # Get the data used for the rug
  mf <- model.frame(model)
  if (is.null(data)) 
    data <- eval(model$call$data, envir)
  if (is.null(data)) 
    data <- mf
  
  tms <- as.matrix(if (se) 
    terms$fit
    else terms)
  
  use.rows <- if (NROW(tms) < NROW(data)) 
    match(rownames(tms), rownames(data))
  
  nmt <- colnames(tms)
  # Remove interaction terms
  if (any(grep(":", nmt) > 0)){
    for(i in grep(":", nmt)){
      warning(sprintf("The interaction term '%s' won't be displayed", nmt[i]))
      nmt <- nmt[-i]
    }
  }
  
  n.tms <- length(nmt)
  
  cn <- parse(text = nmt)
  if (!is.null(smooth)) 
    smooth <- match.fun(smooth)
  if (is.null(ylabs)) 
    ylabs <- paste("Partial for", nmt)
  if (is.null(main)) 
    main <- ""
  else if (is.logical(main)) 
    main <- if (main) 
      deparse(model$call, 500)
  else ""
  else if (!is.character(main)) 
    stop("'main' must be TRUE, FALSE, NULL or character (vector).")
  main <- rep(main, length.out = n.tms)
  pf <- envir
  carrier <- function(term) {
    if (length(term) > 1L) 
      carrier(term[[2L]])
    else eval(term, data, enclos = pf)
  }
  carrier.name <- function(term) {
    if (length(term) > 1L) 
      carrier.name(term[[2L]])
    else as.character(term)
  }
  if (is.null(xlabs)) 
    xlabs <- unlist(lapply(cn, carrier.name))
  if (partial.resid || !is.null(smooth)) {
    pres <- residuals(model, "partial")
    if (!is.null(which.terms)) 
      pres <- pres[, which.terms, drop = FALSE]
  }
  is.fac <- sapply(nmt, function(i){
    if (i %in% colnames(mf)){
      return(is.factor(mf[, i]))
    }
    cn <- grep(sprintf("(^%s$|^[a-zA-Z0-9]+[(][ ]*%s[, 0-9a-zA-Z\"\']+[)])", i, i), colnames(mf))
    if (length(cn)==0){
      stop(sprintf("Could not find '%s' in dataset", i))
    }else if (length(cn) > 1){
      cn <- cn[1]
      warning("More than one name match found")
    }
    return(is.factor(mf[, cn]))
  })
  
  nb.fig <- prod(par("mfcol"))
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  ylims <- ylim
  if (identical(ylims, "common")) {
    ylims <- if (!se) 
      range(tms, na.rm = TRUE)
    else range(tms + 1.05 * 2 * terms$se.fit, tms - 1.05 * 
                 2 * terms$se.fit, na.rm = TRUE)
    if (partial.resid) 
      ylims <- range(ylims, pres, na.rm = TRUE)
    if (rug) 
      ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
  }
  
  for (i in 1L:n.tms) {
    if (identical(ylim, "free")) {
      ylims <- range(tms[, i], na.rm = TRUE)
      if (se) 
        ylims <- range(ylims, tms[, i] + 1.05 * 2 * terms$se.fit[, 
                                                                 i], tms[, i] - 1.05 * 2 * terms$se.fit[, i], 
                       na.rm = TRUE)
      if (partial.resid) 
        ylims <- range(ylims, pres[, i], na.rm = TRUE)
      if (rug) 
        ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
    }
    
    if (is.fac[i]) {
      ff <- mf[, nmt[i]]
      xx <- as.numeric(mf[, nmt[i]])
      plot.factor(i, 
                  ff=ff,
                  xx=xx,
                  xlab=xlabs[i], ylab=ylabs[i],
                  main=main[i])
    }
    else {
      xx <- carrier(cn[[i]])
      plot.continuous(i, xx=xx, 
                      xlab=xlabs[i], 
                      ylab=ylabs[i],
                      main=main[i])
      
    }
    if (partial.resid) {
      if (!is.fac[i] && !is.null(smooth)) {
        smooth(xx, pres[, i], lty = lty.smth, cex = cex, 
               pch = pch, col = col.res, col.smooth = col.smth, 
               span = span.smth)
      }
      else points(xx, pres[, i], cex = cex, pch = pch, 
                  col = col.res)
    }
    
    if (rug){
      if (rug.type == "density") {
        plot.density(xx)
      }else {
        plot.rug(xx)
      }
      
    }
  }
  invisible(n.tms)
}

PropensityMatch <- function(DB, variables.list, formula.match, grouping.variable = "GROUP", merge.variable = "CASEID",
                            control.group = "", treat.group = "", ratio = 1 , save.stdiff = T, save.stdiff.title = "StDiff.csv",
                            formula.summary=NULL, save.summary = T, save.summary.title = "PS.csv") {
  library("MatchIt") 
  set.seed(100)
  
  # note: <<- is needed because for some reason, match.data requires a global variable
  DB.pre <<- na.omit(DB[,variables.list])

  DB.pre$treatment <<- NA
  DB.pre$treatment[eval(parse(text=paste("DB.pre$",grouping.variable,"==","\"",control.group,"\"",sep = "" )))] <<- 0
  DB.pre$treatment[eval(parse(text=paste("DB.pre$",grouping.variable,"==","\"",treat.group  ,"\"",sep = "" )))] <<- 1
  
  m.out <- matchit(formula.match, method="nearest", data=DB.pre, ratio = ratio)
  m.final <- match.data(m.out)

  if (save.stdiff == T) {
     m.s <- summary(m.out, standardize=T)$sum.match
     write.table(m.s, file=save.stdiff.title, sep = ",")
  }


  DB.postmatch <- subset(DB, eval(parse(text = merge.variable)) %in% eval(parse(text = paste("m.final$",merge.variable,sep=""))))
  
  if (save.summary == T) {
    CreateSummaryTable(formula.summary, DB.postmatch, title = save.summary.title , save = save.summary)  
  }
  return(DB.postmatch)
}

# Custom user-defined theme for ggplot2
theme_jeff_nyt <- function(base_size = 12, base_family = "Helvetica", font.regular ="Franklin Gothic Book", font.bold="Franklin Gothic Medium") {
  
  library(extrafont)    # for fonts
  
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = font.regular, face="plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin=margin(), debug = F),
    #axis.text =          element_text(size = rel(0.8), colour = "white"),
    #strip.text =         element_text(size = rel(0.8), colour = "white"),
    
    axis.line =          element_line(colour = "black"),
    axis.text.x  =       element_text(colour="black", size=10, family=font.regular),
    axis.text.y  =       element_text(colour="black", size=10, family=font.regular),
    #axis.ticks =         element_line(colour = "white", size = 0.2),
    #axis.title =         element_text(colour = "white"),
    axis.title.x =       element_text(vjust = 0.5,face="bold",size=14, family=font.bold),
    axis.title.y =       element_text(angle=90, face="bold",size=14, family=font.bold),
    #axis.ticks.length =  unit(0.3, "lines"),
    #axis.ticks.margin =  unit(0.5, "lines"),
    
    legend.background =  element_rect(fill = "transparent",colour = NA),
    #legend.margin =      unit(0.2, "cm"),
    #legend.key =         element_rect(fill = "black", colour = "white"),
    #legend.key.size =    unit(1.2, "lines"),
    #legend.key.height =  NULL,
    #legend.key.width =   NULL,
    legend.text =        element_text(colour="black",face="bold",size=12, family = font.bold),
    #legend.text.align =  NULL,
    legend.title =       element_text(colour="black",face="bold",size=12, family = font.regular),
    #legend.title.align = NULL,
    legend.position =    c(0,0),
    #legend.direction =   "vertical",
    legend.justification = c(0,0),
    #legend.box =         NULL,
    
    panel.background =   element_blank(),
    panel.border =       element_blank(),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    #panel.margin =       unit(0.25, "lines"),
    
    #strip.background =   element_rect(fill = "grey30", colour = "grey10"),
    #strip.text.x =       element_text(),
    #strip.text.y =       element_text(angle = -90),
    
    #plot.background =    element_rect(colour = "black", fill = "black"),
    plot.title =         element_text(colour="black",face="bold",size=rel(1.2), family = font.bold),
    #plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
    
    complete = TRUE
  )
}

ggForestPlot <- function(d.raw, xlab="Odds Ratio", ylab="Factors", 
                         labels=NULL, line.size, x.tick=c(0,0.5,1,1.5,2),  x.max=2.5,
                         remove=NULL, center=1,
                         save = FALSE, file.name = "Figure.pdf", file.height = 4, file.width = 8){
  
  require(ggplot2)
  require(stringr)
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  
  # error checking
  if( !is.null(labels) & (length(labels) != nrow(d.raw)))  break("Error: Length of labels must equal length of data frame!")
  if( is.null(labels)) labels <- d.raw[1]
  
  xlab <- str_c("\n",xlab,"\n")
  ylab <- str_c("\n",ylab,"\n")

  d <- data.frame( x= labels,
                   y1= d.raw[2],          
                   ylo1 = d.raw[3],       
                   yhi1 = d.raw[4], 
                   pval = d.raw[5],
                   order = rev(letters[1:nrow(d.raw)])
                   )
  colnames(d) <- c("x","y1","ylo1","yhi1", "pval","order")

  d$y <- as.numeric(paste(d$y1)) 
  d$ylo <- as.numeric(paste(d$ylo1)) 
  d$yhi <- as.numeric(paste(d$yhi1)) 
  
  if (!is.null(remove)) d<-d[-remove,]        # remove whichever row that I don't want

  # print(d)    # debugging purposes

    p <- ggplot(data=d,aes(x=y,y=order))+
         geom_point()+   # draw center point
         geom_errorbarh(aes(xmin=ylo,xmax=yhi),height=0.667)+   # draw error bar
         geom_vline(xintercept=center,linetype="dashed") +   # center line
         ylab(ylab) +
         xlab(xlab) +
         scale_x_continuous(breaks=x.tick, limits=c(0,x.max)) +         # tick markers at breaks
         scale_y_discrete(label=rev(d$x))        # labelling      

  p <- p + theme_jeff_nyt() + 
           theme(axis.ticks.y = element_blank(), axis.text.y = element_text(size = 12, face="bold", hjust = 1, vjust=0.5), legend.position = "none") +
           theme(axis.text.x=element_text(size=12)) +
           theme(axis.line.x = element_line(color="black")) +
           theme(axis.line.y = element_line(color="black"))

  if (save == TRUE) {
    print(paste("Saving Figure & Summary Table in", getwd()))
    if (last(unlist(strsplit(file.name, "[.]"))) %in% c("pdf","PDF")) {
      #dev.copy(pdf,file = file.name, width = file.width, height = file.height); dev.off()
      #embed_fonts(file.name)  # embed true type fonts
       ggsave(filename = file.name,width = file.width,height = file.height)
      }
    d <- d[,-c(6:9)]  # remove extra columns
    colnames(d) <- c("Variables","Odds Ratio","Lower 95% CI","Upper 95% CI","p-value")
    write.csv(d,file = paste0(file.name,".csv"),row.names = F)
  }
  return(p)
}

### unused ####
