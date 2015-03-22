emissions1 <- function(){
      
      library("plyr")
      library("dplyr")
      
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      ## Summarise the emissions by year
      Output <- ddply(NEI,"year",summarize,totalEmissions = sum(Emissions))
  
      ## Get the list years for the x axis
      years <- unique(Output$year)
      
      png(file = "plot1.png") 
      
      plot(Output$year,Output$totalEmissions, type = "l", xaxt="n", xlab = "Year", ylab = "Emissions", main = "Total Emissions - USA", sub = "Emissions descreasing")
      axis(1,at=years)
      abline(lm(Output$totalEmissions ~ Output$year), col = "red")
      legend("topright", legend = c("Emissions", "Trend"), lty=c(1,1), col=c("black","red"), rect(w = 5, h= 5) )
      
      dev.off()  
      
}

