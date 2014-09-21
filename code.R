setwd(getwd())
if(!file.exists("repdata-data-StormData.csv.bz2")){
    fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    filename <- "repdata-data-StormData.csv.bz2"
    invisible(download.file(fileurl, filename, method="curl"))
}

if(!file.exists("repdata-data-StormData.csv")){
    invisible(library(R.utils))
    invisible(bunzip2("repdata-data-StormData.csv.bz2"))
} 

data = read.csv("repdata-data-StormData.csv")
head(data,n=3)
dim(data)

data_subset = data[,c(8,23,24,25,26,27,28)]
names(data_subset) = c("event.type","fatalities","injuries","property.damage","property.damage.exp","crop.damage","crop.damage.exp")
data_subset = data_subset[data_subset$fatalities > 0 | data_subset$injuries > 0 |
                              data_subset$property.damage > 0 | data_subset$crop.damage > 0, ]
head(data_subset,n=3)


damage_factors <- function(damage,damage.exp) {
    
    damage.exp  <-  toupper(damage.exp)
    
    if (damage.exp == "") {
        damage * 1
    } else if (damage.exp == "1") {
        damage * 10
    } else if (damage.exp == "H") {
        damage * 100
    } else if (damage.exp == "K") {
        damage * 1000
    } else if (damage.exp == "M") {
        damage * 1000000
    } else if (damage.exp == "B") {
        damage * 1000000000
    } else 0
}
data_subset$act.prop.damage <- mapply(damage_factors, data_subset$property.damage, data_subset$property.damage.exp)
data_subset$act.crop.damage <- mapply(damage_factors, data_subset$crop.damage, data_subset$crop.damage.exp)


fatality = aggregate(fatalities~event.type,data=data_subset, FUN=sum)
fatality = fatality[order(fatality$fatalities, decreasing = TRUE), ][1:10,c("fatalities","event.type")]
print(fatality)


library(ggplot2)
g1 = ggplot(data=fatality,aes(event.type,fatalities))
g1 + geom_bar(aes(fill=event.type), stat = "identity") +
    ylab("Total number of fatalities") +
    xlab("Event Type") +
    ggtitle("Fatalities by severe weather event") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


injury = aggregate(injuries~event.type,data=data_subset, FUN=sum)
injury = injury[order(injury$injuries, decreasing = TRUE), ][1:10,c("injuries","event.type")]
print(injury)


library(ggplot2)
g2 = ggplot(data=injury,aes(event.type, injuries))
g2 + geom_bar(aes(fill=event.type), stat = "identity") +
    ylab("Total number of injuries") +
    xlab("Event Type") +
    ggtitle("Injuries by severe weather event") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

economic = aggregate(combined.damage~event.type,data=data_subset,FUN="sum")
economic = economic[order(economic$combined.damage,decreasing=TRUE),][1:10,c("combined.damage","event.type")]
print(economic)

g3 = ggplot(data=economic, aes(event.type,combined.damage))
g3 + geom_bar(stat="identity",aes(fill=event.type)) +
    ggtitle("Total economic damage by severe weather events") +
    ylab("Economic damage") + xlab("Severe weather events") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))




