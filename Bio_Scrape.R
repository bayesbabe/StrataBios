#Script to scrape conference bios from Strata website and save in csv doc to local machine 
#The following fields are scraped: bio, name, job title, session, session number (if speaker participates in multiple sessions), topic, topic number (if speaker participates in multiple sessions/topics)

##Libraries needed
library(XML)
library(gdata)
library(Snowball)
library(RWeka)
library(rJava)
library(RWekajars)
library(tm)
library(wordcloud)
library(RColorBrewer)

url<-"http://strataconf.com/strata2014/public/schedule/speakers" #Strata 2014 - change year for different years
doc<-htmlParse(url)
links<-xpathSApply(doc, "//*/span[@class='en_speaker_name']//a/@href") #links to individual bios

bio.strata<-NULL
name.strata<-NULL
title.strata<-NULL
topic.strata<-NULL
session.strata<-NULL
session.num.strata<-NULL
topic.num.strata<-NULL
for(i in 1:length(links)){
	url=paste("http://strataconf.com", as.character(links[i]), sep="")
	doc<-htmlParse(url)
	bio<-xpathSApply(doc, "//*/div[@class='en_speaker_bio note']", xmlValue)
	bio<-gsub("â\u0080\u0099","'",bio) #change apostrophes to apostrophes
	bio<-gsub("â\u0080\u009c","'",bio) #change double quotes to single quotes
	bio<-gsub("\r"," ",bio) #remove return symbols
	bio<-gsub("\n", "", bio) #remove \n
	bio<-gsub("\t", "", bio) #remove \t
	bio.strata<-c(bio.strata,bio)
	name.title<-xpathSApply(doc, "//*/h1[@class='fn']", xmlValue)
	name<-strsplit(name.title, "\r")[[1]][1]
	name.strata<-c(name.strata,name)
	title<-xpathSApply(doc, "//*/span[@class='info']", xmlValue)
	title.strata<-c(title.strata, title)
	session<-xpathSApply(doc, "//*/div[@class='en_session_title summary']", xmlValue)
	session.num=length(session)
	session.num.strata<-c(session.num.strata,session.num)	
	if(length(session)>1){session=paste(session, collapse=" / ")} 
	else{session=session}
	if(length(session)==0){session="NA"} 
	else{
		session<-gsub("\r", "", session)
		session<-gsub("\n", "", session)
		session<-gsub("\t", "", session)
		}
	session<-gsub("\r", "", session)
	session<-gsub("\n", "", session)
	session=trim(session)
	session.strata<-c(session.strata,session)
	topic<-xpathSApply(doc, "//*/span[@class='en_session_topics category']", xmlValue)
	topic.num=length(topic)
	if(length(topic)==0){topic="NA"} else{
		topic<-gsub("\r", "", topic)
		topic<-gsub("\n", "", topic)
		topic<-gsub("\t", "", topic)
	}
	topic=trim(topic)
	topic.num.strata<-c(topic.num.strata,topic.num)
	if(length(topic)>1){topic=paste(topic, collapse=" / ")} else{topic=topic}
	topic.strata<-c(topic.strata,topic)
	#Might need to replace a period with a space since a period is at the end of the paragraph and removing \n removes the space
}
strataspeakers=cbind(name.strata,title.strata,topic.num.strata, topic.strata, session.num.strata, session.strata, bio.strata)
colnames(strataspeakers)=c("Name", "Title", "Number_Topics", "Topics", "Number_Sessions", "Sessions", "Bio")
strataspeakers=as.data.frame(strataspeakers)
strataspeakers$Number_Sessions=as.numeric(strataspeakers$Number_Sessions)-1 #for some reason R added 1 to session number
strataspeakers$Number_Topics=as.numeric(strataspeakers$Number_Topics)-1 #for some reason R added 1 to topic number when it put it in a dataframe
year=rep(2014,length(name.strata)) #add column for year
conference=rep("strata", length(name.strata)) #add column for conference
strataspeakers=cbind(strataspeakers,year,conference)
colnames(strataspeakers)=c("Name", "Title", "Number_Topics", "Topics", "Number_Sessions", "Sessions", "Bio", "Year", "Conference")

write.csv(strataspeakers, "/Users/Rahel/Desktop/stratabios.csv") #save to local machine
