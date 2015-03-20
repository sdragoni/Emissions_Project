emissions2 <- function(){
      
      #     library("plyr")
      library("dplyr")
      
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      baltimore <- NEI[NEI$fips == "24510",]
      
      Output2 <- ddply(baltimore,.(year),summarize,totalEmissions = sum(Emissions))
 #     Output <- ddply(NEI,"year",summarize,totalEmissions = sum(Emissions))
      
      plot(Output2$year,Output2$totalEmissions)
}
