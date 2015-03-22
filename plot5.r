emissions5 <- function(){
      
      library("plyr")
      library("ggplot2")
      library("dplyr")
      
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      ## Set the SCC$SCC to character to make compatible with NEI$SCC 
      ## Used for merge
      SCC$SCC <- as.character(SCC$SCC)
      
      ## Generate a logical vector of the SCC containing vehicle
      EI.sec <- grepl("vehicle",SCC$EI.Sector,ignore.case=TRUE)
      
      ## Create a dataframe of just the vehicle codes
      vehicles <- SCC[EI.sec,]
      
      ## Baltimore County emissions 
      baltimore <- NEI[NEI$fips == "24510",]
      
      ## Baltimore county vehicles. 
      baltimore.codes <- merge(x = baltimore, y = vehicles, all.y = TRUE)
      
      png(file = "plot5.png") 
      
      ## Summarize to get just the year and total emissions for each year
      Output5 <- ddply(baltimore.codes,.(year),summarize,totalEmissions = sum(Emissions))
      
      out <- qplot(x= year, y=totalEmissions, data = Output5) + ggtitle("Baltimore Motor Vehicles Emmissions\nEmissions are decreasing") + geom_smooth(method=lm)  
      
      print(out)
      
      dev.off()
}
