#author: shama kamat
#purpose: Homework 5

#volcano eruption
#https://data.world/makeovermonday/2018w28-volcano-eruptions
#install.packages("gdata")
file <- file.choose()
df<- read.csv(file=file, sep= ',' , header = TRUE, stringsAsFactors = FALSE)
View(df)

df1<-na.omit(df)
str(df)
#first single dimension plot

plot(df$Elevation..m., ylab="Elevation", type="p",main="Distribution of Volcanic Eruptions", col = "red")
barplot(df$Elevation..m., ylab="Elevation", main="Distribution of Volcanic Eruptions")

#second and third single dimension plots
par(mfrow = c(1,2))
boxplot(df$Latitude, main= "Latitude Distribution", col = "lightgreen")
boxplot(df$Longitude, main="Longitude Distribution", col = "purple")

#fourth single dimension plot
volcanofreq<-data.frame(table(df$Region))
colnames(volcanofreq)<-c("Region","Frequency")
volcanofreq
sortde<-volcanofreq[order(-volcanofreq$Frequency),]
label= (sortde$Region[1:6])
#label2=sortde$Frequency[1:6]
pie(sortde$Frequency[1:6], labels = (label),main = "Top 6 Volcanic-prone Regions")

# multi-dimensional plot
volctype<-df1[df1$Country=="Tonga" | df1$Country=="Canada",c('Country','Primary.Volcano.Type')]
forgraph<-table(volctype$Country,volctype$Primary.Volcano.Type)
#colnames(forgraph)=c("Volcano Type","Country","Freq")
str(forgraph)
#stacked bar chart
barplot(forgraph,col = c("orange","red"), cex.names = 0.5, xlab = "Volcano Types", ylab = "Number of Occurences "
                        ,ylim=c(0,20),main = "Ocuurences of Volcano types", legend.text = c("Canada","Tonga"), args.legend = list(x="topleft", bty='n'))
#besides bar chart
barplot(forgraph,col = c("orange","red"), beside = TRUE, cex.names = 0.5,xlab = "Volcano Types", ylab = "Number of Occurences "
        ,ylim=c(0,14),main = "Ocuurences of Volcano types", legend.text = c("Canada","Tonga"), args.legend = list(x="topleft", bty='n'))



