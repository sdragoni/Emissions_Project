emissions4 <- function(){
      
      library("dplyr")
      library("plyr")
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      SCC$SCC <- as.character(SCC$SCC)
      
      
      
      ## Find the coal or comb. 
      ## Use AND to find the cells with both coal and comb
      ## Use OR to find which rows have any coal comb and place this in coal.comb
      shrt.nm <- grepl("coal",SCC$Short.Name,ignore.case=TRUE) & grepl("comb",SCC$Short.Name,ignore.case=TRUE)
      EI.sec <- grepl("coal",SCC$EI.Sector,ignore.case=TRUE) & grepl("comb",SCC$EI.Sector,ignore.case=TRUE)
      SSC.Lev1 <- grepl("coal",SCC$SCC.Level.One,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.One,ignore.case=TRUE)
      SSC.Lev2 <- grepl("coal",SCC$SCC.Level.Two,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.Two,ignore.case=TRUE)
      SSC.Lev3 <- grepl("coal",SCC$SCC.Level.Three,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.Three,ignore.case=TRUE)
      SSC.Lev4 <- grepl("coal",SCC$SCC.Level.Four,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.Four,ignore.case=TRUE)
      coal.comb <- shrt.nm | EI.sec | SSC.Lev1 | SSC.Lev2 | SSC.Lev3 | SSC.Lev4
      
      # Create a data frame with just coal combustion codes
      coal.codes <- SCC[coal.comb,]
      
      # merge the NEI and the coal.codes togeher to give only those emissions generated by coal combustion
      NEI.SCC <- merge(x = NEI, y = coal.codes, all.y = TRUE)
      
      ## Summarize the emissions by year
      Output4 <- ddply(NEI.SCC,"year",summarize,totalEmissions = sum(Emissions))
      
      ## Get the list years for the x axis
      years <- unique(Output4$year)
      
      png(file = "plot4.png") 
      
      plot(Output4$year,Output4$totalEmissions, xaxt="n", xlab = "Year", ylab = "Emissions", main = "United States, Emissions from coal combustion-related sources\n", sub = "Emissions are decreasing", type = "l")
      axis(1,at=years)
      abline(lm(Output4$totalEmissions ~ Output4$year), col = "red") 
      legend("topright", legend = c("Emissions", "Trend"), lty=c(1,1), col=c("black","red"), rect(w = 5, h= 5) )
 
      dev.off()
}