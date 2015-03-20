emissions3 <- function(){
      
      # library("plyr")
      library("ggplot2")
      library("dplyr")
      
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      baltimore <- NEI[NEI$fips == "24510",]
      
      Output3 <- ddply(baltimore,.(year, type),summarize,totalEmissions = sum(Emissions))
   
      qplot(x= year, y=totalEmissions, data = Output3, facets=~type)
      
  
}
