emissions5 <- function(){
      
      library("plyr")
      library("ggplot2")
      library("dplyr")
      
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      SCC$SCC <- as.character(SCC$SCC)
      
      EI.sec <- grepl("vehicle",SCC$EI.Sector,ignore.case=TRUE)
      
      vehicles <- SCC[EI.sec,]
      
      baltimore <- NEI[NEI$fips == "24510",]
      
      baltimore.codes <- merge(x = baltimore, y = vehicles, all.y = TRUE)
      
      Output5 <- ddply(baltimore.codes,.(year),summarize,totalEmissions = sum(Emissions))
      
      qplot(x= year, y=totalEmissions, data = Output5)
      
      
}
