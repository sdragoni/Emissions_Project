emissions6 <- function(){
      
      library("plyr")
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
      
      ## LA County emissions 
      los.angeles <- NEI[NEI$fips == "06037",]
      
      ## Baltimore county vehicle emissions
      LA.codes <- merge(x = los.angeles, y = vehicles, all.y = TRUE)
      
      ## Summarize to get just the year and total emissions for each city
      Output.balt <- ddply(baltimore.codes,.(year),summarize,Baltimore.Emissions = sum(Emissions))
      Output.LA <- ddply(LA.codes,.(year),summarize,LA.Emissions = sum(Emissions))


      ## combine the LA emissions and the Baltimore County emissions into Output6 data frame 
      LA.Emissions <- Output.LA$LA.Emissions
      Output6 <- cbind(Output.balt, LA.Emissions)
  
      
      ## Get the relative differences
      Output6$LA <- 100+(((Output6$LA.Emissions-Output6$LA.Emissions[1])/Output6$LA.Emissions[1])*100)
      Output6$Baltimore <- 100+(((Output6$Baltimore.Emissions-Output6$Baltimore.Emissions[1])/Output6$Baltimore.Emissions[1])*100)
  
      ## Get the list years for the x axis
      years = unique(Output6$year)
      
      ## Graph to show totals - Did not use
#      plot(Output6$year, Output6$LA.Emissions, type = "l", xaxt="n", xlab = "Year", ylab = "Emissions", ylim=c(0,5000), col = "blue")
#      lines(Output6$year, Output6$Baltimore.Emissions, col = "red")
#      axis(1,at=years)
#      legend("topright", legend = c("Los Angeles", "Baltimore"), lty=c(1,1), col=c("blue","red"),  rect(w = 8, h= 5) )

      ## As the cities are such different sizes (as can be seen by their emissions) I decided that is is 
      ## better to just look at the relative changes in the two counties.
      ## This is discussed in the forum here  https://class.coursera.org/exdata-012/forum/thread?thread_id=172

      png(file = "plot6.png") 
      
      plot(Output6$year, Output6$LA, type = "l", xaxt="n", xlab = "Year", ylim=c(0,120),  ylab = "Emissions", col = "blue", main = "Relative changes Baltimore and LA Vehicle Emissions\n Both cities values started at 100", sub = "Baltimore Emissions decrease most")
      lines(Output6$year, Output6$Baltimore, col = "red")
      axis(1,at=years)
      legend("topright", legend = c("Los Angeles", "Baltimore"), lty=c(1,1), col=c("blue","red"), rect(w = 8, h= 5) )
  
      dev.off()
      
}
