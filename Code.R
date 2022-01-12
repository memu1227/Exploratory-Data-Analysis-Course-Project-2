#load libraries
library(tidyverse)

#read in files
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

#Have total emissions from PM2.5 decreased in the United States 
#from 1999 to 2008? 

#group by year
total_PM25_emissions <- summarise(group_by(NEI,year), Emissions = sum(Emissions))

#create png
png("plot1.png")

#plot
pm25<-barplot(height = total_PM25_emissions$Emissions/1000,ylim = c(0,8000),
              xlab = "Years", ylab = expression('PM'[2.5]*' Emission in Kilotons'),
              main = expression('PM'[2.5]*' Emissions for Different Years'),
              col = c("red","yellow","blue", "purple"))
#annotations
text(x= pm25, y = round(total_PM25_emissions$Emissions/1000,2),
     labels = round(total_PM25_emissions$Emissions/1000,2), pos = 3)
#x labels
axis(1, at = pm25, labels = total_PM25_emissions$year, tick = FALSE)

dev.off()

##Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
##from 1999 to 2008?

#create png for second plot
png("plot2.png")

#filter by location
balti <- summarise(group_by(filter(NEI,fips == "24510"),year),
                   Emissions = sum(Emissions))
baltipm<-barplot(balti$Emissions,ylim = c(0,4000),xlab = "Years",
                 ylab = ylab = expression('PM'[2.5]*' Emission in Tons'), 
                 main = "Total Emissions in Baltimore for Different Years",
                 col = c("red","yellow","blue", "purple"))

#annotations
text(x= baltipm, y = round(balti$Emissions,2), labels = round(balti$Emissions,2),
     pos = 3)
#x labels
axis(1, at = baltipm, labels = balti$year, tick = FALSE)

dev.off()

##Of the four types of sources indicated by the type variable, 
##which of these four sources have seen decreases/increases in emissions 
##from 1999–2008 for Baltimore City? 

#create png
png("plot3.png", height=600,width = 1000)

balti_type <- summarise(group_by(filter(NEI,fips == "24510"),year,type),
                        Emissions = sum(Emissions))

#plot
ggplot(data = balti_type, aes(x = factor(year),y = Emissions, fill = type,
          label = round(Emissions,2))) + 
    geom_bar(stat = "identity") + 
    facet_grid(.~type) +
    xlab("Year") + 
    ylab("Total Emission in Tons") +
    ggtitle("Emissions in Baltimore by Type for Different Years") +
    theme(plot.title = element_text(hjust = 0.5))+
    geom_label(fontface = "bold")
    
dev.off()

##Across the United States, how have emissions from 
##coal combustion-related sources changed from 1999–2008?

#create png
png("plot4.png")

coal <- SCC[grep("[Cc][Oo][Aa][Ll]", SCC$EI.Sector),"SCC"]
cc <- summarise(group_by(filter(NEI,SCC %in% coal),year),Emissions = sum(Emissions))

#plot
ggplot(data = cc,aes(x= year, y = round(Emissions/1000,2),
                     label = round(Emissions/1000,2),fill = year)) +
    geom_bar(stat= "identity") +
    ylab(expression('PM'[2.5]*' Emission in Kilotons')) +
    xlab("Years") +
    ggtitle(expression('Coal Combustion Emissions for Different Years')) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_label(col = "white", fontface = "bold")

dev.off()


##How have emissions from motor vehicle sources changed from 1999–2008 
##in Baltimore City?

#create png
png("plot5.png")

vehicle <- SCC[grep("[Vv]ehicle",SCC$EI.Sector),"SCC"]

baltmv<-summarise(group_by(filter(NEI,SCC %in% vehicle & fips == "24510"),year), 
                  Emissions = sum(Emissions))

#plot
ggplot(data = baltmv,aes(x= year, y = round(Emissions,2),
                     label = round(Emissions,2),fill = year)) +
    geom_bar(stat= "identity") +
    ylab(expression('PM'[2.5]*' Emission in Tons')) +
    xlab("Years") +
    ggtitle(expression('Emissions from Motor Vehicles in Baltimore for Different Years')) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_label(col = "white", fontface = "bold")

dev.off()

##Compare emissions from motor vehicle sources in Baltimore City with emissions 
##from motor vehicle sources in Los Angeles County, California

#create png
png("plot6.png")

cities<-data.frame(fips = c("06037", "24510"), city = c("Los Angeles", "Baltimore"))
baltila<-summarise(group_by(filter(NEI,SCC %in% vehicle & fips %in% cities$fips),
                   year,fips), Emissions = sum(Emissions))

data <-merge(baltila,cities)

#plot
ggplot(data = data,aes(x= year, y = round(Emissions,2),
                         label = round(Emissions,2),fill = year)) +
    geom_bar(stat= "identity") +
    facet_grid(.~city) +
    ylab(expression('PM'[2.5]*' Emission in Tons')) +
    xlab("Years") +
    ggtitle(expression('Emissions from Motor Vehicles for Baltimore and Los Angeles')) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_label(col = "white", fontface = "bold")

dev.off()


 
