########################################## reload dataframe from working directory:
setwd("C:/Users/fatem/OneDrive/Documents/doc/free-data")
df <- read.csv('US_accident_r.csv',header = TRUE)
print(table(df$year))
print(nrow(df))
print(ncol(df))
print(summary(df))
print(colnames(df))

library(pastecs)
?stat.desc
colnames(df) #selecting numeric columns
df_2 <- subset(df , select = c('Severity','Distance.mi.','Temperature.F.','Wind_Chill.F.',
                               'Humidity...','Pressure.in.'))
stat.desc(df_2)

####################################### remove outliers:
library(grDevices)
library(e1071)

colnames(df)
#find the percentage of outliers in every column:
df_out <- data.frame(round(sapply(df, function(x) length(x[which(x %in% boxplot.stats(x)$out)])/nrow(df)),3))
print(df_out)

#replace outliers of columns with mean:
df$Distance.mi.[which(df$Distance.mi. %in% boxplot.stats(df$Distance.mi.)$out)]<- NA #replace with NA
df_na <- data.frame(round(sapply(df, function(x) (sum(is.na(x))/nrow(df))*100),1)) #find NA
df$Distance.mi. <- impute(as.matrix(df$Distance.mi.),what = 'mean') #replace with mean



df$Wind_Speed.mph.[which(df$Wind_Speed.mph. %in% boxplot.stats(df$Wind_Speed.mph.)$out)]<- NA #replace with NA

df_na <- data.frame(round(sapply(df, function(x) (sum(is.na(x))/nrow(df))*100),1)) #find NA
df$Wind_Speed.mph. <- impute(as.matrix(df$Wind_Speed.mph.),what = 'mean') #replace with mean



df$Precipitation.in.[which(df$Precipitation.in. %in% boxplot.stats(df$Precipitation.in.)$out)]<- NA #replace with NA
df_na <- data.frame(round(sapply(df, function(x) (sum(is.na(x))/nrow(df))*100),1)) #find NA
df$Precipitation.in. <- impute(as.matrix(df$Precipitation.in.),what = 'mean') #replace with mean

df$Visibility.mi.[which(df$Visibility.mi. %in% boxplot.stats(df$Visibility.mi.)$out)]<- NA #replace with NA
df_na <- data.frame(round(sapply(df, function(x) (sum(is.na(x))/nrow(df))*100),1)) #find NA
df$Visibility.mi. <- impute(as.matrix(df$Visibility.mi.),what = 'mean') #replace with mean

df$Pressure.in.[which(df$Pressure.in. %in% boxplot.stats(df$Pressure.in.)$out)]<- NA #replace with NA
df_na <- data.frame(round(sapply(df, function(x) (sum(is.na(x))/nrow(df))*100),1)) #find NA
df$Pressure.in. <- impute(as.matrix(df$Pressure.in.),what = 'mean') #replace with mean


df_out <- data.frame(round(sapply(df, function(x) length(x[which(x %in% boxplot.stats(x)$out)])/nrow(df)),3))
print(df_out)



######################################### find na for each column/compute the percentage of missing data:
print(nrow(df))
df_na <- data.frame(round(sapply(df, function(x) (sum(is.na(x))/nrow(df))*100),1))
print(df_na)

####################################### fillna with mean
library(e1071)
#fill missing values:
df$Precipitation.in.<- impute(as.matrix(df$Precipitation.in.),what = 'mean')
df$Pressure.in.<- impute(as.matrix(df$Pressure.in.),what = 'mean')
df$Humidity...<- impute(as.matrix(df$Humidity...),what = 'mean')
df$Temperature.F.<- impute(as.matrix(df$Temperature.F.),what = 'mean')
df$Visibility.mi.<- impute(as.matrix(df$Visibility.mi.),what = 'mean')
df$Wind_Chill.F.<- impute(as.matrix(df$Wind_Chill.F.),what = 'mean')


####################################### Overall average for numeric columns

df_numeric <- subset(df,select = c(Precipitation.in.,Pressure.in.,Humidity...,Temperature.F.,Visibility.mi.,Wind_Chill.F.))
colMeans(df_numeric[sapply(df_numeric, is.numeric)]) 


######################################### Normalize data
colnames(df)
df_Numeric<- subset(df,select = c('Humidity...','Pressure.in.','Temperature.F.',
                                  'Visibility.mi.','Wind_Chill.F.'))
print(head(scale(df_Numeric)))
print(head(df$Humidity...))

############################################ pie plot:
library(dplyr)
library(ggplot2)

df_svrity <- data.frame(table(df$Severity))
df_svrity <- df_svrity  %>% arrange(desc(Var1))   %>%
  mutate(prop = Freq / sum(df_svrity$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
df_svrity

#OK
ggplot(df_svrity, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color = 'white') +
  coord_polar("y", start=0)+theme_void() + theme(legend.position = 'none')+
  geom_text(aes( y = ypos , label = Var1),color = 'white',size = 6)+
  scale_fill_brewer(palette="Set1")
###################################map plot: showing the the frequency of accidents in every state by color
library(usmap)
library(ggplot2)
data = table(df$State)

data  <- data.frame(data,stringsAsFactors = FALSE)
colnames(data) <- c('state','freq')
#head(data)
#ncol(data)
#data
plot_usmap(data = data , values = 'freq',regions = 'states',color = 'indianred2',label_color = 'green')+
  scale_fill_continuous(low = "white", high = "indianred2", name = "frequency", label = scales::comma)+
  theme(legend.position = "right")+
  labs(title = "US Accidents between 2018 and 2019",
       subtitle = "Color Shows the Number of Accidents",
       size = "Magnitude")


###################################################Donut chart
#OK
df_svrity <- df_svrity  %>% arrange(desc(Var1))   %>%
  mutate(prop = as.integer(Freq / sum(df_svrity$Freq) *100) )%>%
  mutate(ypos = as.integer(cumsum(prop)- 0.5*prop ))

df_svrity


mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
p <- ggplot(df_svrity, aes(x = 2, y = prop, fill = Var1)) +
  geom_bar(stat = "identity", color = "white") +
  ggtitle("Severity of Accidents, Percentage(%)")+
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = ypos, label = prop), color = "black")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
p


###############################histogram:OK
library(ggplot2)
library(devtools)


p1 <- ggplot(df, aes(x=Distance.mi., color=Severity)) +
  geom_histogram(colour="red", binwidth=50,fill = '#FF6666')

p2 <- ggplot(df, aes(x=Severity, color=year)) +
  geom_histogram(fill="red", alpha=0.5, position="identity")


p5 <- ggplot(df, aes(x=Humidity...)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

p6 <- ggplot(df, aes(x=Pressure.in.)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 



p8 <- ggplot(df, aes(x=Temperature.F.)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


p9 <- ggplot(df, aes(x=Wind_Chill.F.)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 



p11 <- ggplot(df, aes(x=Start_Lng)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


p12 <- ggplot(df, aes(x=Start_Lat)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


## multiplot function: reference:
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
###
multiplot(p1, p2,p5, p6, p8,p9, p11, p12, cols=3)

#############################################plotting categorical variables
par(mfrow=c(3,6))    # set the plotting area into a 1*2 array

stop <- table(df$Stop)/length(df$Stop)
barplot(stop,main="Stop",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)


Bump <- table(df$Bump)/length(df$Bump)
Bump
barplot(Bump,
        main = "Bump",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Amenity <- table(df$Amenity)/length(df$Amenity)
Amenity
barplot(Amenity,
        main = "Amenity",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Astronomical_Twilight <- table(df$Astronomical_Twilight)/length(df$Astronomical_Twilight)
Astronomical_Twilight
barplot(Astronomical_Twilight,
        main = "Astronomical_Twilight",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Junction <- table(df$Junction)/length(df$Junction)
Junction
barplot(Junction,
        main = "Junction",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Traffic_Calming <- table(df$Traffic_Calming)/length(df$Traffic_Calming)
Traffic_Calming
barplot(Traffic_Calming,
        main = "Traffic_Calming",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Turning_Loop <- table(df$Turning_Loop)/length(df$Turning_Loop)
Turning_Loop
barplot(Turning_Loop,
        main = "Turning_Loop",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Sunrise_Sunset <- table(df$Sunrise_Sunset)/length(df$Sunrise_Sunset)
Sunrise_Sunset
barplot(Sunrise_Sunset,
        main = "Sunrise_Sunset",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

No_Exit <- table(df$No_Exit)/length(df$No_Exit)
No_Exit
barplot(No_Exit,
        main = "No_Exit",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Civil_Twilight <- table(df$Civil_Twilight)/length(df$Civil_Twilight)
Civil_Twilight
barplot(Civil_Twilight,
        main = "Civil_Twilight",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Station <- table(df$Station)/length(df$Station)
Station
barplot(Station,
        main = "Station",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)


Roundabout <- table(df$Roundabout)/length(df$Roundabout)
Roundabout
barplot(Roundabout,
        main = "Roundabout",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Railway <- table(df$Railway)/length(df$Railway)
Railway
barplot(Railway,
        main = "Railway",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Nautical_Twilight <- table(df$Nautical_Twilight)/length(df$Nautical_Twilight)
Nautical_Twilight
barplot(Nautical_Twilight,
        main = "Nautical_Twilight",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Crossing <- table(df$Crossing)/length(df$Crossing)
Crossing
barplot(Crossing,
        main = "Crossing",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Traffic_Signal <- table(df$Traffic_Signal)/length(df$Traffic_Signal)
Traffic_Signal
barplot(Traffic_Signal,
        main = "Traffic_Signal",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)

Source <- table(df$Source)/length(df$Source)
Source
barplot(Source,
        main = "Source",
        border="dark blue",
        col="#FF6666",
        las=2,
        density=10)



######################################## time series with frequency and severity:
#create new data frame from time,
#severity and frequency of accidents:OK
class(df$Weather_Timestamp)
df$Weather_Timestamp <- as.Date(df$Weather_Timestamp)

count0 <- aggregate(df$Severity, list(df$Weather_Timestamp),FUN = mean )
count0
count1 <- aggregate(df$ID, list(df$Weather_Timestamp),FUN = length )
count1$x
new_data <- cbind(count0,count1$x)
head(new_data)
#adding month and year columns to new data frame:
new_data['month'] <- format(new_data$Group.1,'%m')
new_data['year'] <- format(new_data$Group.1,'%Y')
head(new_data$year)

library(ggplot2)
p <- ggplot(new_data, aes(Group.1, `count1$x`, group = 1,color =x )) +
  geom_point() +
  xlab("") + ylab("Daily Views")
#p + scale_x_date(date_breaks = '3 months')
p+ stat_smooth(color = "#FC4E07", fill = "#FC4E07",method = "loess")
p + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


#########################################forecasting in time series OK
library(forecast)

new_data['year_month'] <- format(as.Date(new_data$Group.1), "%Y-%m")
count_mnth <- aggregate(new_data$`count1$x`,list(new_data$year_month),FUN=sum)
count_mnth

mtc <- as.matrix(count_mnth$x)
mtc
dim(mtc) <- c(3,8)
mtc1 <- colSums(mtc)
mtc1
ts <- ts(mtc1, frequency = 4, start = 2018) 
ts
#plot(meanf(ts,h = 8))
#plot(naive(ts,h = 8))
plot(snaive(ts,h = 8))#OK
plot(forecast(HoltWinters(ts), h = 8)) #OK

######################################chi-square and contingency table for categorical columns
c1 <- table(df$Severity,df$Traffic_Calming)
c1[,2]
c2 <- table(df$Severity,df$Bump)
c2[,2]
c3 <- table(df$Severity,df$Turning_Loop)
c3
c4 <- table(df$Severity,df$Sunrise_Sunset)
c4
c5 <- table(df$Severity,df$Amenity)
c5
c6 <- table(df$Severity,df$Civil_Twilight)
c6
c7 <- table(df$Severity,df$Nautical_Twilight)

c7
c8 <- table(df$Severity,df$Astronomical_Twilight)
c8
c9 <- table(df$Severity,df$Station)
c9
c10 <- table(df$Severity,df$Stop)
c10
c11 <- table(df$Severity,df$Junction)
c11
c12 <- table(df$Severity,df$No_Exit)
c12
c13 <- table(df$Severity,df$Railway)
c13
c14 <- table(df$Severity,df$Roundabout)
c14
table <- cbind(c1[,2],c2[,2],c3,c5[,2],c9[,2],c10[,2],c11[,2],c12[,2],c13[,2],c14[,2])
colnames(table) = c("Traffic_Calming","Bump","Turning_Loop","Amenity","Station",
                    "Stop","Junction","No_Exit","Railway","Roundabout")
table
library(corrplot)
chisq <- chisq.test(table)
chisq
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

#Contibution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

########################################## decision tree algorithm
df_tree <- subset(df, select = c('Severity',"Traffic_Calming","Bump","Turning_Loop","Amenity","Station",
                                 "Stop","Junction","No_Exit","Railway","Roundabout"))
#df_tree


library(party)
#png(file = "decision_tree.png")
output.tree <- ctree(Severity ~., data = df_tree)

# Plot the tree.
plot(output.tree)
#dev.off()
