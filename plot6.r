emissions6 <- function(){
      
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
      
      los.angeles <- NEI[NEI$fips == "06037",]
      
      LA.codes <- merge(x = los.angeles, y = vehicles, all.y = TRUE)
      
      Output.balt <- ddply(baltimore.codes,.(year),summarize,Baltimore.Emissions = sum(Emissions))

      Output.LA <- ddply(LA.codes,.(year),summarize,LA.Emissions = sum(Emissions))

      LA.Emissions <- Output.LA$LA.Emissions
      Output6 <- cbind(Output.balt, LA.Emissions)
      
      plot(Output6$year, Output6$Baltimore.Emissions, type = "l", col = "blue",  xlab = "Year", ylab = "Emissions")
      lines(Output6$year, Output6$LA.Emissions, col = "red")
      legend("topright", legend = c("Baltimore", "Los Angeles"), lty=c(1,1), col=c("black","red"))
      
      print(Output6)
      
}
