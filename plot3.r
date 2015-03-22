emissions3 <- function(){
      
      library("plyr")
      library("ggplot2")
      library("dplyr")
      
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      
      ## Extract just the Baltimore County emissions
      baltimore <- NEI[NEI$fips == "24510",]
      
      # Summarise the emissions by year and type (Source)
      Output3 <- ddply(baltimore,.(year, type),summarize,totalEmissions = sum(Emissions))
   
      png(file = "plot3.png") 
      
      out <- qplot(x= year, y=totalEmissions, data = Output3, facets=~type) + geom_smooth(method=lm) +ggtitle("Baltimore County by Source\nNon-Road, On-Road, NonPoint are decreasing, Slight increase in Point") 
      
      print(out)
      dev.off()
}
