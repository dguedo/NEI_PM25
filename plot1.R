library(plyr)
library(ggplot2)
library(data.table)

## PLOT 1
## -------------------------------------------------------------------------------------------------
#
## change the location to the raw data file on your system. set working
## directory to that of the source script
NEI <- readRDS("../data/summarySCC_PM25.rds")
SCC <- readRDS("../data/Source_Classification_Code.rds")

# sum the years
total <- with(NEI, aggregate(NEI[,'Emissions'], by = list(year), sum, na.rm=TRUE))

# rename for access
names(total) <- c('Year', 'TotalEmissions')

png(filename = "plot1.png", width = 480, height = 480, units = "px")
plot(total, type = "b", ylab = "Emissions", xlab = "Year",xaxt="n", main = "Total Annual Emissions")
axis(1, at=as.integer(total$Year), las=1)

dev.off()

## PLOT 2
## -------------------------------------------------------------------------------------------------
#
## change the location to the raw data file on your system. set working
## directory to that of the source script
NEI <- readRDS("../data/summarySCC_PM25.rds")
SCC <- readRDS("../data/Source_Classification_Code.rds")

baltimore <- NEI[which(NEI$fips == "24510"), ]

# sum the years
total <- with(baltimore, aggregate(baltimore[,'Emissions'], by = list(year), sum, na.rm=TRUE))

# rename for access
names(total) <- c('Year', 'TotalEmissions')

png(filename = "plot2.png", width = 480, height = 480, units = "px")
plot(total, type = "b", ylab = "Emissions", xlab = "Year",xaxt="n", main = "Total Baltimore  Emissions")
axis(1, at=as.integer(total$Year), las=1)

dev.off()

## PLOT 3
## -------------------------------------------------------------------------------------------------
#
## change the location to the raw data file on your system. set working
## directory to that of the source script
NEI <- readRDS("../data/summarySCC_PM25.rds")
SCC <- readRDS("../data/Source_Classification_Code.rds")

baltimore <- NEI[which(NEI$fips == "24510"), ]

# sum the years
total <- with(baltimore, aggregate(baltimore[,'Emissions'], by = list(year), sum, na.rm=TRUE))

# rename for access
names(total) <- c('Year', 'TotalEmissions')

total.type <- ddply(baltimore, .(type, year), summarize, Emissions = sum(Emissions))
total.type$Pollutant <- total.type$type

png(filename = "plot3.png", width = 480, height = 480, units = "px")

qplot(year, Emissions, data = total.type, group = Pollutant, 
      color = Pollutant, geom = c("point", "line"), ylab = expression("Emissions"[2.5]), 
      xlab = "Year", main = "Total Emissions")

dev.off()


## PLOT 4
## -------------------------------------------------------------------------------------------------
#
## change the location to the raw data file on your system. set working
## directory to that of the source script
NEI <- readRDS("../data/summarySCC_PM25.rds")
SCC <- readRDS("../data/Source_Classification_Code.rds")
SCCDataTable <- data.table(SCC)
NEIDataTable <- data.table(NEI)

#
coal = SCCDataTable[grep("Coal", SCC.Level.Three), SCC]
coal_emissions = NEIDataTable[SCC %in% coal, sum(Emissions), by = "year"]
colnames(coal_emissions) <- c("year", "Emissions")

# plot
png(filename = "plot4.png", width = 480, height = 480, units = "px")
plot(coal_emissions, type = "b", ylab = "Emissions", xlab = "Year",xaxt="n", main = "Total Coal Emissions")
axis(1, at=as.integer(total$Year), las=1)
dev.off()


## PLOT 5
## -------------------------------------------------------------------------------------------------
#
## change the location to the raw data file on your system. set working
## directory to that of the source script
NEI <- readRDS("../data/summarySCC_PM25.rds")
SCC <- readRDS("../data/Source_Classification_Code.rds")
SCCDataTable <- data.table(SCC)
NEIDataTable <- data.table(NEI)

#grep("On-Road", unique(SCCDataTable$EI.Sector))
motor = SCCDataTable[grep("On-Road", EI.Sector), SCC]
motor_emissions = NEIDataTable[SCC %in% motor, sum(Emissions), by = c("year", "fips")][fips == "24510"]
colnames(motor_emissions) <- c("year", "fips", "Emissions")

# plot
png(filename = "plot5.png", width = 480, height = 480, units = "px")
qplot(year, Emissions, data=motor_emissions, geom="line") +
ggtitle(expression("Motor Vehicle Emissions by Year for Baltimore")) + xlab("Year") + ylab(expression("Total Emissions"))
dev.off()


## PLOT 6
## -------------------------------------------------------------------------------------------------
#
## change the location to the raw data file on your system. set working
## directory to that of the source script
NEI <- readRDS("../data/summarySCC_PM25.rds")
SCC <- readRDS("../data/Source_Classification_Code.rds")
SCCDataTable <- data.table(SCC)
NEIDataTable <- data.table(NEI)

#grep("On-Road", unique(SCCDataTable$EI.Sector))
motor = SCCDataTable[grep("On-Road", EI.Sector), SCC]
motor_emissions = NEIDataTable[SCC %in% motor, sum(Emissions), by = c("year", "fips")][fips %in% c("24510","06037")]
colnames(motor_emissions) <- c("year", "fips", "Emissions")
# add names
motor_emissions <- transform(motor_emissions, city = ifelse(fips == "24510", "Baltimore", "Los Angeles"))

# normalize and create a year zero baseline
norm_ds <- transform(motor_emissions, change = ifelse(fips == "24510", Emissions / base_year[fips=="24510"]$Emissions , Emissions/ base_year[fips!="24510"]$Emissions))

# plot
png(filename = "plot6.png", width = 480, height = 480, units = "px")
qplot(year, change, data=norm_ds, group=city, geom="line", color = city, 
      ylab = "Normalized Emissions", xlab = "Year", main = "Motor Vehicle Emissions Normalized to 1999")
dev.off()