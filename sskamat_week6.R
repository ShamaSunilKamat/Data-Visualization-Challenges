# Load the data
crime <- read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.csv',
         sep=",", header=TRUE)
View(crime)
#1. Scatterplot Matrix, Figure 6-9

plot(crime[,2:9])
pairs(crime[,2:9], panel=panel.smooth,
      main = "Scatter Matrix of Crime Rates")


#2. Bubble chart, Figure 6-15

crime2 <-
  read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv",
           header=TRUE, sep="\t")
symbols(crime2$murder, crime2$burglary, circles=crime2$population)

radius <- sqrt( crime2$population/ pi )
symbols(crime2$murder, crime2$burglary, circles=radius)

symbols(crime2$murder, crime2$burglary, circles=radius, inches=0.35,
        fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate",
        main = "Murders vs Burlgaries in USA")

#symbols(crime2$murder, crime2$burglary,
#       squares=sqrt(crime2$population), inches=0.5)

text(crime2$murder, crime2$burglary, crime2$state, cex=0.5)

#3. Histogram, Figure 6-24

birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
stem(birth$X2008)
hist(birth$X2008)
hist(birth$X2008, breaks=5)
hist(birth$X2008, breaks=16, col = "#9400D3", xlab = "live births per 1,000 population",
     ylab = "Count", main = "Global Distribution of Birth Rates")

#4. Density Plot. Figure 6-32

birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)

plot(d2008)

plot(d2008, type="n")
polygon(d2008, col="#821122", border="#cccccc",
        xlab = "live births per 1,000 population",main = "Global Distribution of Birth Rates")

library(lattice)
histogram(birth$X2008, breaks=10)
lines(d2008)


#multidimensional plots
file <- file.choose()
volcano<- read.csv(file=file, sep= ',' , header = TRUE, stringsAsFactors = FALSE)
View(volcano)

volcano<-na.omit(volcano)
#6-38
#latitude,longitude,volcanotype
par(mfrow=c(3,1))

v1<- volcano[volcano$Primary.Volcano.Type=="Caldera",]
hist(v1$Elevation..m., col = "red")

v2<- volcano[volcano$Primary.Volcano.Type=="Stratovolcano",]
hist(v2$Elevation..m., col = "red")

v3<- volcano[volcano$Primary.Volcano.Type=="Shield",]
hist(v3$Elevation..m., col = "red")





#6-40
filteredvolcano<-volcano[volcano$Country=="United States"|volcano$Country=="Canada"| volcano$Country=="Japan",]
View(filteredvolcano)

xyz<-table(filteredvolcano$Primary.Volcano.Type,filteredvolcano$Country)
xyz1<-data.frame(xyz)
View(xyz1)

toplot<-xyz1[xyz1$Var1=="Caldera"|xyz1$Var1=="Complex"|xyz1$Var1=="Pyroclastic cone(s)"| xyz1$Var1=="Shield"| xyz1$Var1=="Stratovolcano"| xyz1$Var1=="Submarine",]

View(toplot)
colnames(toplot)<-c("Volcano Type","Country","No of occurences")
par(mfrow=c(2,3))

#firstquad
one<-toplot[toplot$`Volcano Type`=="Caldera",]
barplot(one$`No of occurences`, main = "Caledra Volcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("Red","Blue","pink"))

#secondquad
two<-toplot[toplot$`Volcano Type`=="Complex",]
barplot(two$`No of occurences`, main = "Complex Volcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("Red","Blue","pink"))

#third
three<-toplot[toplot$`Volcano Type`=="Pyroclastic cone(s)",]
barplot(three$`No of occurences`, main = "Pyroclastic cone Volcano",
        xlab = "Country", ylab = "No of occurence", 
        legend.text = c("Canada", "Japan","US"),
        col = c("Red","Blue","pink"))

#fourth
four<-toplot[toplot$`Volcano Type`=="Shield",]
barplot(four$`No of occurences`, main = "Shield Volcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("Red","Blue","pink"))


#fifth
five<-toplot[toplot$`Volcano Type`=="Stratovolcano",]
barplot(five$`No of occurences`, main = "Stratovolcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("Red","Blue","pink"))

#sixth
six<-toplot[toplot$`Volcano Type`=="Submarine",]
barplot(six$`No of occurences`, main = "Submarine Volcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("Red","Blue","pink"))

#like first
plot(volcano[,9:11])

