####Hydrology analyses function####

library("foreach")
library("doParallel")
library("dataRetrieval")
library("tidyverse")
library("Kendall")
library("EflowStats")
library("dplyr")
library("lubridate")
library("zoo")
library("xts")
library("boot")
library("xlsx")
library("trend")
library("smwrBase")
library("broom")

testFun <- function(SitesBy_df) {
  
  sites <- unique(SitesBy_df$site_no)
  List = list() #creates an empty list we will fill with site, ken tau, and p value for future reference
  
  
  for (i in 1:length(sites)) {
    
    data.sub <- dplyr::filter(SitesBy_df, site_no == sites[i])
    
    count = rep(1,length(data.sub$meanFlow))
    data.agg = data.frame(data.sub,count)
    count.agg = aggregate(data.agg$count ~ data.agg$Date, FUN = "sum")
    
    x = xts(data.sub$meanFlow, data.sub$Date)
    
    ken.tau = MannKendall(x)
    sens.data = sens.slope(data.sub$meanFlow, conf.level = 0.95)
    List[[length(List)+1]] = data.frame(Site = data.sub$site_no, Tau = ken.tau$tau[1], Tau.p = ken.tau$sl[1], SensSlope = sens.data$estimate[1], Z = sens.data$statistic[1], Z.p = sens.data$p.value[1])#, Z.lower = sens.data$conf.int[1])#, Z.upper = sens.data$conf.int[2]) #fill list with 3 columns (site name, ken.tau, and p value)
    
#this block of code was used in early analyses to split sites into different folders based on certain relationships
    #if(ken.tau$sl[1] < 0.05){ #if sig
    #if(ken.tau$tau[1] > 0){ #and if postitive
    #save.path <- file.path("C:","Users","krodgers","Desktop","Pos and sig",paste(SitesBy_df[,6], sites[i], ".pdf", sep = "")) #make path to save a plot called "plot_SITENAME.jpg" in the folder on the desktop called "Pos and sig"
    #pdf(file = save.path)
    #layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    #plot(data.sub$Date, data.sub$meanFlow, xlab="Year", ylab = "MeanFlow (in cfs)")
    #lm(formula = data.sub$meanFlow ~ data.sub$Date, col="red")
    #lines(lowess(data.sub$meanFlow ~ data.sub$Date), col="blue")
    #acf(data.sub$meanFlow)
    #pacf(data.sub$meanFlow)
    #dev.off()
    # }
    #else{ #do this is neg and sig
    #save.path <- file.path("C:","Users","krodgers","Desktop","Neg and sig",paste(SitesBy_df[,6], sites[i], ".pdf", sep = "")) #make path to save a plot called "plot_SITENAME.jpg" in the folder on the desktop called "Pos and sig"
    #pdf(file = save.path)
    #layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    #plot(data.sub$Date, data.sub$meanFlow, xlab="Year", ylab = "MeanFlow (in cfs)")
    #lm(formula = data.sub$meanFlow ~ data.sub$Date, col="red")
    #lines(lowess(data.sub$meanFlow ~ data.sub$Date), col="blue")
    #acf(data.sub$meanFlow)
    #pacf(data.sub$meanFlow)
    #dev.off()
    #}
    #}
    #else{ #do this if not sig
    #save.path <- file.path("C:","Users","krodgers","Desktop","Not sig",paste(SitesBy_df[,6], sites[i], ".pdf", sep = "")) #make path to save a plot called "plot_SITENAME.jpg" in the folder on the desktop called "Not sig"
    #pdf(file = save.path)
    #layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    #plot(data.sub$Date, data.sub$meanFlow, xlab="Year", ylab = "MeanFlow (in cfs)")
    #lm(formula = data.sub$meanFlow ~ data.sub$Date, col="red")
    #lines(lowess(data.sub$meanFlow ~ data.sub$Date), col="blue")
    #acf(data.sub$meanFlow)
    #pacf(data.sub$meanFlow)
    #dev.off()
    #}
  }
  
  res.out <- plyr::ldply(List)
  tau.agg = aggregate(res.out$Tau ~ res.out$Site, FUN = "mean")
  p.agg = aggregate(res.out$Tau.p ~ res.out$Site, FUN = "mean")
  slope.agg = aggregate(res.out$SensSlope ~ res.out$Site, FUN = "mean")
  Z.agg = aggregate(res.out$Z ~ res.out$Site, FUN = "mean")
  p.z.agg = aggregate(res.out$Z.p ~ res.out$Site, FUN = "mean")
  #Z.lower.agg = aggregate(res.out$Z.lower ~ res.out$Site, FUN = "mean")
  #Z.upper.agg = aggregate(res.out$Z.upper ~ res.out$Site, FUN = "mean")
  res.out <- cbind(tau.agg[1], tau.agg[2], p.agg[2], slope.agg[2], Z.agg[2], p.z.agg[2])#, Z.lower.agg[2]), Z.upper.agg[2])
  colnames(res.out)[1:6] <- c("Site", "Tau", "Tau.p", "SensSlope", "Z", "Z.p")#, "Z.lower")#, "Z.upper")
  res.out.Q10_2000 <<- res.out
  
}  