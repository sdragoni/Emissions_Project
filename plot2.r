emissions2 <- function(){
      
      library("plyr")
      library("dplyr")
      
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      ## Extract just the Baltimore County emissions
      baltimore <- NEI[NEI$fips == "24510",]
      
      # Summarise the emissions by year
      Output2 <- ddply(baltimore,.(year),summarize,totalEmissions = sum(Emissions))
      
      ## Get the list years for the x axis
      years <- unique(Output2$year)
      
      png(file = "plot2.png") 
      
      plot(Output2$year,Output2$totalEmissions, xaxt="n", xlab = "Year", ylab = "Emissions", main = "Baltimore Emissions", sub = "Emissions decreasing", type = "l")
      axis(1,at=years)
      abline(lm(Output2$totalEmissions ~ Output2$year), col = "red")
      legend("topright", legend = c("Emissions", "Trend"), lty=c(1,1), col=c("black","red"), rect(w = 5, h= 5) )
      
      dev.off()
}
