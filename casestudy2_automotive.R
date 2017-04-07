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
#manipulating the advertising column
datatable$the_list.Company.Advertising<-gsub("[:$:]"," ",datatable$the_list.Company.Advertising)
datatable$the_list.Company.Advertising<-gsub("B"," ",datatable$the_list.Company.Advertising)
datatable$the_list.Company.Advertising[grep("M",datatable$the_list.Company.Advertising)]<-as.numeric(gsub("M"," ",datatable$the_list.Company.Advertising[grep("M",datatable$the_list.Company.Advertising)]))/1000
View(datatable)

#-------PLoTTING PART

#filtering the data for TECHNOLOGY
library(dplyr)
datatable%>%filter(datatable$the_list.Industry=="Automotive")->automotive
View(automotive)
library(ggplot2)
#converting to numeric
automotive$the_list.Brand.Revenue<-as.numeric(automotive$the_list.Brand.Revenue)
automotive$the_list.Company.Advertising<-as.numeric(automotive$the_list.Company.Advertising)
automotive$the_list.Brand.Value<-as.numeric(automotive$the_list.Brand.Value)
#aesthetic
aut<-ggplot(automotive,aes(x=automotive$the_list.Company.Advertising,y=automotive$the_list.Brand.Revenue,label=the_list.Brand))
#labelling
w<-aut+xlab("Comapany Advertising in Billions of $") + ylab("Brand Revenue in Billions of $")
#geometry
z<-w+geom_point(aes(size=the_list.Brand.Value,color=the_list.Brand))+guides(color=F)
z
z<-z+scale_size_continuous(name="Brand Value $(Billions)",breaks=c(6.2,20,37.8),labels=c(6.2,20,37.8))
#putting text on map
z+scale_x_continuous(breaks=seq(0.8,6,0.1))+scale_y_continuous(breaks=seq(40,150,10))+geom_text(vjust=0.5,nudge_y =-1.9,aes(color=the_list.Brand,size=the_list.Brand.Value))+theme_bw()+theme(legend.key = element_rect(fill = "lightblue",color = "black"),plot.title = element_text(size = 40) )+ggtitle("Automotive")
