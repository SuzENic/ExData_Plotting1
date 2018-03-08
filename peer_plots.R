plot1 <- function() {
	   
	   NEI <- readRDS("exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
	   SCC <- readRDS("exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")
	   total_by_year <-tapply(NEI$Emissions, NEI$year, sum)
	   years <- c(1999,2002, 2005, 2008)
            png(file="plot1.png") ### trend of total pollutants by years
	    plot(years, total_by_year, main = "Total Emissions by Year", ylab = "Total pm2.5 in tons")
            dev.off()
}


plot2 <- function() {

	   NEI <- readRDS("exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
	   SCC <- readRDS("exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")
	    balt <- subset(NEI, fips == "24510")
	   years <- c(1999,2002, 2005, 2008)
	    total_Balt_by_year <- tapply(balt$Emissions, balt$year, sum)
            png(file="plot2.png") ### trend of total pollutants by years in Baltimore
	    plot(years, total_Balt_by_year, main = "Total Emissions in Baltimore by Year", ylab = "Total pm2.5 in tons")
            dev.off()
}


plot3 <- function() {

	   NEI <- readRDS("exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
	   SCC <- readRDS("exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")
	    balt <- subset(NEI, fips == "24510")
	    png(file="plot3.png") 
	    g <- ggplot(data = balt, aes(balt$year, balt$Emissions, group = balt$type, color = balt$type))+geom_smooth(method = "lm", se = FALSE)+scale_y_continuous(trans='log10')
	    g+geom_point()
	    dev.off()
}


plot4 <- function() {

	   NEI <- readRDS("exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
	   SCC <- readRDS("exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")
	    coal <- SCC$SCC[grepl("Coal", SCC$EI.Sector)]
	    coal_list <- NEI[NEI$SCC %in% coal,]
	    png(file="plot4.png")
	    g <- ggplot(data = coal_list, aes(year, Emissions, group = year))+scale_y_continuous(trans='log10')+geom_boxplot()
	    g
	    dev.off()
}


plot5 <- function() {

	   NEI <- readRDS("exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
	   SCC <- readRDS("exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")
	    mv <- SCC$SCC[grepl("On-Road", SCC$EI.Sector)]
	    mv_list <- NEI[NEI$SCC %in% mv,]
	    balt_mv <- subset(mv_list, fips == "24510")
	    png(file="plot5.png")
	    g <- ggplot(data = balt_mv, aes(year, Emissions))+geom_smooth(method = "lm", se = FALSE)+scale_y_continuous(trans='log10')
	    g+geom_point()+ggtitle("Baltimore Vehicles")
	    dev.off()
}


plot6 <- function() {

	   NEI <- readRDS("exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
	   SCC <- readRDS("exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")
	    mv <- SCC$SCC[grepl("On-Road", SCC$EI.Sector)]
	    mv_list <- NEI[NEI$SCC %in% mv,]
	    balt_mv <- subset(mv_list, fips == "24510")
	    LA_mv <- subset(mv_list, fips == "06037")
	    both_mv <- rbind(LA_mv, balt_mv)
	    both_mv$fips[both_mv$fips=="06037"] <-"LA"
	    both_mv$fips[both_mv$fips=="24510"] <-"Balt"
	    png(file="plot6.png")
	    g <- ggplot(data = both_mv, aes(year, Emissions, group = fips, color = fips))+geom_smooth(method = "lm", se = FALSE)+scale_y_continuous(trans='log10')
	    g+geom_point()+ggtitle("Vehicles in LA and Baltimore")
	    dev.off()
}

