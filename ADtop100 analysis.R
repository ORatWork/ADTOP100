## script to analyse the AD top 100 hospital scores

######
# the highs and the lows
# *1) the top 5 2010, were are they in 2014? 
# *2) the botom 5 of 2010, where are they in 2014? 
# *3) largest improvement (2010-2014)
# *4) biggest decline (2010-2014)
# *5) the list using the average score over 5 year, to most continous
# *6) the list over the average position, over 10 years, and 5 years, look at differences
# geography
# *7) quality in parts of the netherlands (west vs east, north vs south)
#
# correlation over time
# * 9) Sterrenhemel
# *10) verschil tussen de beste en de slechtste over de jaren heen
# *11) verdeling van de ziekenhuisen (density) per score klasse (0-10, 10-20, 20-30, 30-40, etc)
# *12) correlatie tussen op eenvolgende jaren
###############

## empty mem and set wd

rm(list=ls())
setwd("D:/P R O J E C T S/Data Visualisations/Ziekenhuis Top100")

## required lib's

require(ggplot2)
require(reshape)
require(grid)
require(plyr)
require(gridExtra)
require(RgoogleMaps)
require(RColorBrewer)

## get the data

df<-read.csv("Ziekenhuis-AD-TOP100 20141024.csv", header=TRUE, sep=";", dec=",",  na.strings="#N/A")

# conversion steps

t<-is.na(df$Match)
df[t,]$Positie<--1

str(df)
summary(df)
dim(df)

# take out the hospitals that are eihter academic one or special purpose, score  = -1

Academic<-df[df$Positie<1,]
GPHosp<-df[df$Positie>0 & df$Jaar>=2010,] # only have scores form the benchmark 2010 onward
GPHospAll<-df[df$Positie>0,] #for analysis using ranks

##### some descriptives

#boxplot per year of the score

g<- ggplot(GPHosp, aes(x=factor(Jaar), y=Score, fill="TRUE")) + geom_boxplot(fill="steelblue")
g<- g + guides(fill=FALSE)+ xlab("Years") +  ylab("Score") 
g<- g + stat_summary(fun.y="mean", geom="point", shape=23, size=4, fill="white")
g<- g + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
g<- g + ggtitle("Hospital scores over the years")
print(g)

###### check the outliers, note boxplot gives one more outlier

outliers = boxplot(GPHosp$Score, plot=FALSE)$out
GPHosp[GPHosp$Score %in% outliers,]

# plot all the rankings

g<- ggplot(data=GPHosp, aes(x=Jaar, y=Score, group = Ziekenhuis, colour = Ziekenhuis)) +
    geom_line(size=1) +
    geom_point( size=4, shape=21, fill="white")
g<- g + theme(legend.position="none") + xlab("Years") +  ylab("Score")
g<- g + theme(plot.margin=unit(c(1,6,0.5,0,5),"cm"))
g<- g + geom_text(data = GPHosp[GPHosp$Jaar == 2014, ], aes(label = Ziekenhuis), hjust = -0.05, vjust = 0.5, size=4)

# turn of clipping of the labels
gt <- ggplot_gtable(ggplot_build(g))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

png("plot.png", width=2000, height=5000, res=240) # large plot of all the scores
grid.draw(gt)
dev.off()

# plot past performance of the top 5 and bottom 5 hospitals over the past years

Best2014<-GPHosp[GPHosp$Jaar==2014,]

top5<-head(Best2014[with(Best2014, order(-Score)),],5)
bottom5<-tail(Best2014[with(Best2014, order(-Score)),],5)

top5series<-GPHosp[(GPHosp$Ziekenhuis %in% top5$Ziekenhuis) & (GPHosp$Plaatsnaam %in% top5$Plaatsnaam),]
bottom5series<-GPHosp[(GPHosp$Ziekenhuis %in% bottom5$Ziekenhuis) & (GPHosp$Plaatsnaam %in% bottom5$Plaatsnaam),]
#plotseries<-top5series

plotseries<-rbind(top5series, bottom5series)

g<- ggplot(data=plotseries, aes(x=Jaar, y=Score, group = Ziekenhuis, colour = Ziekenhuis)) +
    geom_line(size=1) +
    geom_point( size=4, shape=21, fill="white")
g<- g + theme(legend.position="none") + xlab("Years") +  ylab("Score")
g<- g + theme(plot.margin=unit(c(1,4,0.5,0,5),"cm"))
g<- g + geom_text(data = plotseries[plotseries$Jaar == 2014, ], aes(label = Ziekenhuis), hjust = -0.05, vjust = 0.5, size=3)

# turn of clipping of the labels
gt <- ggplot_gtable(ggplot_build(g))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

# determine avarage score over 5 years, only take into account hospitals that have a score in all 5 years

AvgScore<-aggregate(Score ~ Ziekenhuis + Plaatsnaam, data = GPHosp, mean)
AvgScore[with(AvgScore, order(-Score)),]

head(AvgScore[with(AvgScore, order(-Score)),])
tail(AvgScore[with(AvgScore, order(-Score)),])

#leave out the hospitals with less than 5 observations

CntObs<-aggregate(Jaar ~ Ziekenhuis + Plaatsnaam, data = GPHosp, length)
HospSubSet<-subset(CntObs, Jaar==5)

GPHospCorrected<-GPHosp[(GPHosp$Ziekenhuis %in% HospSubSet$Ziekenhuis) & (GPHosp$Plaatsnaam %in% HospSubSet$Plaatsnaam),]

dim(GPHospCorrected)
dim(GPHosp)

AvgScoreCorr<-aggregate(Score ~ Ziekenhuis + Plaatsnaam, data = GPHospCorrected, mean)
AvgScoreCorr[with(AvgScoreCorr, order(-Score)),]
top5<-head(AvgScoreCorr[with(AvgScoreCorr, order(-Score)),],5)
bottom5<-tail(AvgScoreCorr[with(AvgScoreCorr, order(-Score)),],5)

# make a plot of these 10 hospitals

top5series<-GPHosp[(GPHosp$Ziekenhuis %in% top5$Ziekenhuis) & (GPHosp$Plaatsnaam %in% top5$Plaatsnaam),]
bottom5series<-GPHosp[(GPHosp$Ziekenhuis %in% bottom5$Ziekenhuis) & (GPHosp$Plaatsnaam %in% bottom5$Plaatsnaam),]
plotseries<-rbind(top5series, bottom5series)

#plotseries<-top5series

g<- ggplot(data=plotseries, aes(x=Jaar, y=Score, group = Ziekenhuis, colour = Ziekenhuis)) +
    geom_line(size=1) +
    geom_point( size=4, shape=21, fill="white")
g<- g + theme(legend.position="none") + xlab("Years") +  ylab("Score")
g<- g + theme(plot.margin=unit(c(1,4,0.5,0,5),"cm"))
g<- g + geom_text(data = plotseries[plotseries$Jaar == 2014, ], aes(label = Ziekenhuis), hjust = -0.05, vjust = 0.5, size=3)

# turn of clipping of the labels
gt1 <- ggplot_gtable(ggplot_build(g))
gt1$layout$clip[gt$layout$name == "panel"] <- "off"
#grid.draw(gt1)

# plot the graph using the 2014 ranking and the average ranking side by side

grid.arrange(gt, gt1, ncol=2)

##### match the average score ranking with the 2014 ranking, what are the differences in rank
##### which is the largest in position

# create a DF to store the results
# Ziekenhuis, Plaatsnaam, AvgScore, 2014Score

DiffDF<-AvgScoreCorr
DiffDF$TypeScore<- "AvgScore"  #indicate what type of score
Best2014$TypeScore<- "2014Score" 
Best2014$Jaar<-NULL
Best2014$Match<-NULL
Best2014$Latitude<-NULL
Best2014$Longitude<-NULL
Best2014$Region<-NULL
Best2014$Positie<-NULL

DiffDF<-rbind(DiffDF, Best2014)

# Make a subset of the top en bottom 5 of 2014
# make a subset of the avg top and bottom 5 
# create two plots for comparrison

#order set according to AvgScore
tmp<-DiffDF[DiffDF$TypeScore=="AvgScore",]

top<-head(tmp[with(tmp, order(-Score)),],7)
tmpTop<-DiffDF[(DiffDF$Ziekenhuis %in% top$Ziekenhuis) & (DiffDF$Plaatsnaam %in% top$Plaatsnaam),]

bottom<-tail(tmp[with(tmp, order(-Score)),],5)
tmpBottom<-DiffDF[(DiffDF$Ziekenhuis %in% bottom$Ziekenhuis) & (DiffDF$Plaatsnaam %in% bottom$Plaatsnaam),]

#create first plot

g<- ggplot(data=tmpTop, aes(x=TypeScore, y=Score, group = Ziekenhuis, colour = Ziekenhuis)) +
    geom_line(size=1) +
    geom_point( size=4, shape=21, fill="white")
g<- g + theme(legend.position="none") + xlab("Years") +  ylab("Score") 
g<- g + theme(plot.margin=unit(c(1,4,0.5,0,5),"cm"))
g<- g + geom_text(data = tmpTop[tmpTop$TypeScore=="2014Score" , ], aes(label = Ziekenhuis), hjust = 1, vjust = 0.5, size=3)
g<- g + geom_text(data = tmpTop[tmpTop$TypeScore=="AvgScore" , ], aes(label = Ziekenhuis), hjust = -0.1, vjust = 0.5, size=3)
print(g)

## 1. & 2 top 5/Bottom of 2010 (score) where are they in 2014?

# subset the DF for 2010

Best2010<-GPHosp[GPHosp$Jaar==2010,]

# identify best and worst of 2010

top5_2010<-head(Best2010[with(Best2010, order(-Score)),],5)
bot5_2010<-tail(Best2010[with(Best2010, order(-Score)),],5)

#get the complete series for the selected subset 

t5series<-GPHosp[(GPHosp$Ziekenhuis %in% top5_2010$Ziekenhuis) & (GPHosp$Plaatsnaam %in% top5_2010$Plaatsnaam),]
b5series<-GPHosp[(GPHosp$Ziekenhuis %in% bot5_2010$Ziekenhuis) & (GPHosp$Plaatsnaam %in% bot5_2010$Plaatsnaam),]

# find min and max score to set the right range for the Y axis

min_y_ax<-floor(min(t5series$Score, b5series$Score)/10)*10
max_y_ax<-ceiling(max(t5series$Score, b5series$Score)/10)*10

g<- ggplot(data=t5series, aes(x=Jaar, y=Score, group = Ziekenhuis, colour = Ziekenhuis)) +
    geom_line(size=1) +
    geom_point( size=4, shape=21, fill="white") + ylim(min_y_ax, max_y_ax)
g<- g + theme(legend.position="none") + xlab("Years") +  ylab("Score") 
g<- g + theme(plot.margin=unit(c(1,4,0.5,0,5),"cm"))
g<- g + geom_text(data = t5series[t5series$Jaar == 2014, ], aes(label = Positie), hjust = -1, vjust = 0.5, size=3)

# turn of clipping of the labels
gt <- ggplot_gtable(ggplot_build(g))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
#grid.draw(gt)

g<- ggplot(data=b5series, aes(x=Jaar, y=Score, group = Ziekenhuis, colour = Ziekenhuis)) +
    geom_line(size=1) +
    geom_point( size=4, shape=21, fill="white") + ylim(min_y_ax, max_y_ax)
g<- g + theme(legend.position="none") + xlab("Years") +  ylab("Score") 
g<- g + theme(plot.margin=unit(c(1,4,0.5,0,5),"cm"))
g<- g + geom_text(data = b5series[b5series$Jaar == 2014, ], aes(label = Positie), hjust = -1, vjust = 0.5, size=3)

gt1 <- ggplot_gtable(ggplot_build(g))
gt1$layout$clip[gt1$layout$name == "panel"] <- "off"

grid.arrange(gt, gt1, ncol=2)

## 3 & 4 Largest improvement, biggest decline over 2010-2014 period

# make sure we have data for 2010 && 2014

CntObs<-aggregate(Jaar ~ Ziekenhuis + Plaatsnaam, data = GPHosp, length)
HospSubSet<-subset(CntObs, Jaar==5)

GPHospCorrected<-GPHosp[(GPHosp$Ziekenhuis %in% HospSubSet$Ziekenhuis) & (GPHosp$Plaatsnaam %in% HospSubSet$Plaatsnaam),]

#create subsets

set2010<- GPHospCorrected[GPHospCorrected$Jaar==2010,]
set2014<- GPHospCorrected[GPHospCorrected$Jaar==2014,]

# merge them by ziekenhuis and plaatsnaam

GPHospDev<-merge(set2010, set2014, c("Ziekenhuis", "Plaatsnaam"))

# calculate the difference

GPHospDev$DiffScore<-GPHospDev$Score.y-GPHospDev$Score.x  #2014-2010 + better, - worse
GPHospDev$DiffPosition<-GPHospDev$Positie.x-GPHospDev$Positie.y  #2010-2014 + better, - worse

# analyse the difference

GPHospDev[with(GPHospDev, order(-DiffScore)),]
GPHospDev[with(GPHospDev, order(-DiffPosition)),]

# list winners and losers

head(GPHospDev[with(GPHospDev, order(-DiffScore)),],5)
tail(GPHospDev[with(GPHospDev, order(-DiffScore)),],5)
head(GPHospDev[with(GPHospDev, order(-DiffPosition)),],5)
tail(GPHospDev[with(GPHospDev, order(-DiffPosition)),],5)

#unscaled

plot(GPHospDev$DiffScore, GPHospDev$DiffPosition)
cor(GPHospDev$DiffScore, GPHospDev$DiffPosition)

#scaled
plot(scale(GPHospDev$DiffScore), scale(GPHospDev$DiffPosition))
cor(scale(GPHospDev$DiffScore), scale(GPHospDev$DiffPosition))

## 6. order of the list with the best avarge ranking

## do it for the complte series

## find all the hospital with 10 observations, discard the ones with less than 10
CntObs<-aggregate(Jaar ~ Ziekenhuis + Plaatsnaam, data = GPHospAll, length)
HospSubSet<-subset(CntObs, Jaar==10)

GPHospAll<-GPHospAll[(GPHospAll$Ziekenhuis %in% HospSubSet$Ziekenhuis) & (GPHospAll$Plaatsnaam %in% HospSubSet$Plaatsnaam),]

AvgRank10<-aggregate(Positie~ Ziekenhuis + Plaatsnaam, data = GPHospAll, mean)
AvgRank10[with(AvgRank10, order(Positie)),]

head(AvgRank10[with(AvgRank10, order(Positie)),])
tail(AvgRank10[with(AvgRank10, order(Positie)),])

## now take a look at the last 5 years, for which we have the scores

CntObs<-aggregate(Jaar ~ Ziekenhuis + Plaatsnaam, data = GPHosp, length)
HospSubSet<-subset(CntObs, Jaar==5)

GPHosp<-GPHosp[(GPHosp$Ziekenhuis %in% HospSubSet$Ziekenhuis) & (GPHosp$Plaatsnaam %in% HospSubSet$Plaatsnaam),]

AvgRank5<-aggregate(Positie~ Ziekenhuis + Plaatsnaam, data = GPHosp, mean)
AvgRank5[with(AvgRank5, order(Positie)),]

head(AvgRank5[with(AvgRank5, order(Positie)),])
tail(AvgRank5[with(AvgRank5, order(Positie)),])

## 7. Geographic analysis of the hospital quality

# plot both 2010 and 2014. 

set2010<- GPHospCorrected[GPHospCorrected$Jaar==2010,]
set2014<- GPHospCorrected[GPHospCorrected$Jaar==2014,]

average<-mean(set2010$Score)
set2010$Color<-ifelse(set2010$Score<average, "#d73027", "#4575b4")

average<-mean(set2014$Score)
set2014$Color<-ifelse(set2014$Score<average, "#d73027", "#4575b4")

table(set2010$Region, set2010$Color)
table(set2014$Region, set2014$Color)

center = c(mean(set2010$Latitude), mean(set2010$Longitude))
zoom <- min(MaxZoom(range(set2010$Latitude), range(set2010$Longitude)))

MyMap <- GetMap(center=center, zoom=zoom,destfile = "MyTile1.png", maptype= "terrain", GRAYSCALE =T)

par(mfrow=c(1,2))

PlotOnStaticMap(MyMap, lat = set2010$Latitude, lon = set2010$Longitude, cex=2,pch=20,col=set2010$Color,add=F)

PlotOnStaticMap(MyMap, lat = set2014$Latitude, lon =set2014$Longitude,cex=2,pch=20,col=set2014$Color, add=F)

par(mfrow=c(1,1))


## 10. verschil tussen slechtste en beste over de jaren heen
# not very isnsightfull, looks like the top and low ends of the box plot. 

# get the best and worst score 
highest<-aggregate(Score ~ Jaar, data = GPHosp, max)
lowest<-aggregate(Score ~ Jaar, data = GPHosp, min)

names(highest)[names(highest)=="Score"] <- "Max"
names(lowest)[names(lowest)=="Score"] <- "Min"
highest-lowest

#make a plot of it

g<-ggplot()
g<- g + geom_line(data=highest, aes(x=Jaar, y=Max, colour="steelblue"))
g<- g + geom_line(data=lowest, aes(x=Jaar, y=Min, colour="red"))
g<- g + geom_line(size=2) + geom_point( size=4, shape=21, fill="white")
g<- g + theme(legend.position="none") + xlab("Years") +  ylab("Score") 
g<- g + theme(plot.margin=unit(c(1,4,0.5,0,5),"cm"))
print(g)

## 11. categorize the hospiltals

GPHosp$Bracket<- cut(GPHosp$Score, breaks = c(35,40,45,50,55,60,65,70,75,80,85,90,95,100), include.lowest = TRUE)
g<- ggplot(GPHosp,aes(x=Bracket)) + geom_bar(fill="steelblue")+facet_grid(~Jaar)+ coord_flip()
g<- g + xlab("Score Bracket") + ylab("") 
print(g)

aggregate(Score ~ Jaar, data = GPHosp, mean)
aggregate(Score ~ Jaar, data = GPHosp, sd)

## 12. correlation of scores/position over the years, "Sterrenhemel"

CntObs<-aggregate(Jaar ~ Ziekenhuis + Plaatsnaam, data = GPHosp, length)
HospSubSet<-subset(CntObs, Jaar==5)

GPHospCorrected<-GPHosp[(GPHosp$Ziekenhuis %in% HospSubSet$Ziekenhuis) & (GPHosp$Plaatsnaam %in% HospSubSet$Plaatsnaam),]

min_score<-floor(min(GPHospCorrected$Score)/10)*10
max_score<-ceiling(max(GPHospCorrected$Score)/10)*10
max_score<-95

ScoreCast<-cast(GPHospCorrected, Ziekenhuis + Plaatsnaam ~ Jaar, value="Score")
cor(ScoreCast[3:7], ScoreCast[3:7]) #pearsons correlation
#cor(ScoreCast[3:7], ScoreCast[3:7], method=c("spearman")) #spearman correlation
#or(ScoreCast[3:7], ScoreCast[3:7], method=c("kendall")) #kendall correlation

plotdf<-data.frame(matrix(unlist(ScoreCast), dim(ScoreCast)[1], dim(ScoreCast)[2]))

PostCast<-cast(GPHospCorrected, Ziekenhuis + Plaatsnaam ~ Jaar, value="Positie")
cor(PostCast[3:7], PostCast[3:7]) #pearsons correlation
#cor(PostCast[3:7], PostCast[3:7], method=c("spearman")) #spearman correlation
#cor(PostCast[3:7], PostCast[3:7], method=c("kendall")) #kendall correlation

## plots of the year to year scores of the hospitals

p1<-ggplot(plotdf, aes(x=X3, y=X4))+ 
	geom_abline(intercept = 0, slope = 1, linetype=2, size=1.2, color="lightgrey")+
	geom_point(size=3, shape=21, fill="steelblue") +  
	xlab("2010") +  ylab("2011")+ 
	xlim(min_score, max_score)+ ylim(min_score, max_score) +geom_smooth(method=lm, se=FALSE) 
p2<-ggplot(plotdf, aes(x=X4, y=X5))+
	geom_abline(intercept = 0, slope = 1, linetype=2, size=1.2, color="lightgrey")+
	geom_point(size=3, shape=21, fill="steelblue") +  
	xlab("2011") +  ylab("2012")+ 
	xlim(min_score, max_score)+ ylim(min_score, max_score)  +geom_smooth(method=lm, se=FALSE) 
p3<-ggplot(plotdf, aes(x=X5, y=X6))+
	geom_abline(intercept = 0, slope = 1, linetype=2, size=1.2, color="lightgrey")+
	geom_point(size=3, shape=21, fill="steelblue") +  
	xlab("2012") +  ylab("2013")+ 
	xlim(min_score, max_score)+ ylim(min_score, max_score) +geom_smooth(method=lm, se=FALSE) 
p4<-ggplot(plotdf, aes(x=X6, y=X7))+
	geom_abline(intercept = 0, slope = 1, linetype=2, size=1.2, color="lightgrey")+
	geom_point(size=3, shape=21, fill="steelblue") +  
	xlab("2013") +  ylab("2014")+ 
	xlim(min_score, max_score)+ ylim(min_score, max_score) +geom_smooth(method=lm, se=FALSE) 


grid.arrange(p1, p2, p3,p4, ncol=2, nrow=2)

## plots of the year to year position of the hospitals

plotdf<-data.frame(matrix(unlist(PostCast), dim(PostCast)[1], dim(PostCast)[2]))
min_score<-1
max_score<-100
p1<-ggplot(plotdf, aes(x=X3, y=X4))+ 
	geom_abline(intercept = 0, slope = 1, linetype=2, size=1.2, color="lightgrey")+
	geom_point(size=3, shape=21, fill="steelblue") +  
	xlab("2010") +  ylab("2011")+ 
	xlim(min_score, max_score)+ ylim(min_score, max_score)
p2<-ggplot(plotdf, aes(x=X4, y=X5))+
	geom_abline(intercept = 0, slope = 1, linetype=2, size=1.2, color="lightgrey")+
	geom_point(size=3, shape=21, fill="steelblue") +  
	xlab("2011") +  ylab("2012")+ 
	xlim(min_score, max_score)+ ylim(min_score, max_score)
p3<-ggplot(plotdf, aes(x=X5, y=X6))+
	geom_abline(intercept = 0, slope = 1, linetype=2, size=1.2, color="lightgrey")+
	geom_point(size=3, shape=21, fill="steelblue") +  
	xlab("2012") +  ylab("2013")+ 
	xlim(min_score, max_score)+ ylim(min_score, max_score)
p4<-ggplot(plotdf, aes(x=X6, y=X7))+
	geom_abline(intercept = 0, slope = 1, linetype=2, size=1.2, color="lightgrey")+
	geom_point(size=3, shape=21, fill="steelblue") +  
	xlab("2013") +  ylab("2014")+ 
	xlim(min_score, max_score)+ ylim(min_score, max_score)

grid.arrange(p1, p2, p3,p4, ncol=2, nrow=2)





