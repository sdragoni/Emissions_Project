emissions4 <- function(){
      
      library("dplyr")
      library("plyr")
      ## This first line will likely take a few seconds. Be patient!
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      SCC$SCC <- as.character(SCC$SCC)
      
      
      
      
      shrt.nm <- grepl("coal",SCC$Short.Name,ignore.case=TRUE) & grepl("comb",SCC$Short.Name,ignore.case=TRUE)
      EI.sec <- grepl("coal",SCC$EI.Sector,ignore.case=TRUE) & grepl("comb",SCC$EI.Sector,ignore.case=TRUE)
      
      SSC.Lev1 <- grepl("coal",SCC$SCC.Level.One,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.One,ignore.case=TRUE)
      
      SSC.Lev2 <- grepl("coal",SCC$SCC.Level.Two,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.Two,ignore.case=TRUE)
      
      SSC.Lev3 <- grepl("coal",SCC$SCC.Level.Three,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.Three,ignore.case=TRUE)
      
      SSC.Lev4 <- grepl("coal",SCC$SCC.Level.Four,ignore.case=TRUE) & grepl("comb",SCC$SCC.Level.Four,ignore.case=TRUE)
      
      coal.comb <- shrt.nm | EI.sec | SSC.Lev1 | SSC.Lev2 | SSC.Lev3 | SSC.Lev4
      
      coal.codes <- SCC[coal.comb,]
      
      NEI.SCC <- merge(x = NEI, y = coal.codes, all.y = TRUE)
      rm(NEI)
      rm(SCC)
      
      
      Output4 <- ddply(NEI.SCC,"year",summarize,totalEmissions = sum(Emissions))
      
      #      Output4 <- ddply(baltimore,.(year, type),summarize,totalEmissions = sum(Emissions))
      
      plot(Output4$year,Output4$totalEmissions)
      
}