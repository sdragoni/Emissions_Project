emissions1 <- function(){
      
 #     library("plyr")
      library("dplyr")
      
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      Output <- ddply(NEI,"year",summarize,totalEmissions = sum(Emissions))
      
      plot(Output$year,Output$totalEmissions)
}

