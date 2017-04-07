library(XML)
library(RCurl)
htmlcontent<-getURL("file:///C:/Data%20Science%20with%20R/Assignments/Graded%20Assignments/Topic%206.2%20-%20%20Data%20Visualization%20in%20R/The%20World's%20Most%20Valuable%20Brands%20List%20-%20Forbes.html",
                    ssl.verifypeer=FALSE)

htmltable<-readHTMLTable(htmlcontent)
datatable<-as.data.frame(htmltable[18])
View(datatable)
#loading the dplyr package
library(dplyr)
#to see the number of NA values
colSums(is.na(datatable))
#to omit nas
datatable<-na.omit(datatable)
View(datatable)
#omitting the the_list. column
datatable%>%select(-the_list.)->datatable
View(datatable)
#omitting the # from the rank
datatable$the_list.Rank<-gsub("#"," ",datatable$the_list.Rank)
View(datatable)
#omitting the $ from the_list.Brand.Value
datatable$the_list.Brand.Value<-gsub("[:$:]"," ",datatable$the_list.Brand.Value)
datatable$the_list.Brand.Value<-gsub("B"," ",datatable$the_list.Brand.Value)
View(datatable)
datatable$the_list.Brand.Revenue<-gsub("[:$:]"," ",datatable$the_list.Brand.Revenue)
datatable$the_list.Brand.Revenue<-gsub("B"," ",datatable$the_list.Brand.Revenue)
View(datatable)
#manipulating the_list.Company.Advertising
datatable$the_list.Company.Advertising<-gsub("[:$:]"," ",datatable$the_list.Company.Advertising)
datatable$the_list.Company.Advertising<-gsub("B"," ",datatable$the_list.Company.Advertising)
datatable$the_list.Company.Advertising[grep("M",datatable$the_list.Company.Advertising)]<-as.numeric(gsub("M"," ",datatable$the_list.Company.Advertising[grep("M",datatable$the_list.Company.Advertising)]))/1000
View(datatable)

#-------PLoTTING PART

#filtering the data for TECHNOLOGY
library(dplyr)
datatable%>%filter(datatable$the_list.Industry=="Luxury")->luxury
View(luxury)
library(ggplot2)
#converting to numeric
luxury$the_list.Brand.Revenue<-as.numeric(luxury$the_list.Brand.Revenue)
luxury$the_list.Company.Advertising<-as.numeric(luxury$the_list.Company.Advertising)
luxury$the_list.Brand.Value<-as.numeric(luxury$the_list.Brand.Value)
#aesthetic
lux<-ggplot(luxury,aes(x=luxury$the_list.Company.Advertising,y=luxury$the_list.Brand.Revenue,label=the_list.Brand))
#labelling
a<-lux+xlab("Comapany Advertising in Billions of $") + ylab("Brand Revenue in Billions of $")
#geometry
b<-a+geom_point(aes(size=the_list.Brand.Value,color=the_list.Brand))+guides(color=F)
b
b<-b+scale_size_continuous(name="Brand Value $(Billions)",breaks=c(10,28.1),labels=c(10,28.1))
#putting text on map
b+scale_x_continuous(breaks=seq(0,5,0.1))+geom_text(vjust=0,nudge_y =-0.2,aes(color=the_list.Brand,size=the_list.Brand.Value))+theme_bw()+theme(legend.key = element_rect(fill = "lightblue",color = "black"),plot.title = element_text(size = 40) )+ggtitle("Luxury")
