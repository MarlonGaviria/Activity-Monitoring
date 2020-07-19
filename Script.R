library(dplyr)

act <- read.csv('activity.csv', sep = ',')

total_steps <- na.omit(act %>% group_by(as.Date(date)) %>% summarise(total_steps=sum(steps)))
###
png('plot1.png',width = 480,height = 480)
hist(total_steps$total_steps, main = 'Total steps each day',
     xlab = 'Steps',col = rainbow(n = 3,v = 0.5,alpha = 0.5))
grid()
dev.off()
###


avrg_steps <- na.omit(act %>% group_by(date=as.Date(date)) %>% summarise(avrg_steps=mean(steps)))
###
png('plot2.png',width = 480,height = 480)
plot(avrg_steps$date,avrg_steps$avrg_steps, type = 'l',lwd = 4, xlab = 'Day', ylab = 'Steps', 
     col = 'cyan',)
points(avrg_steps$date,avrg_steps$avrg_steps, type = 'l', col = 'darkblue')
grid()
dev.off()
###

act <- na.omit(transform(act,interval=as.character(interval)))
avgstps <- act %>% group_by(interval=as.character(interval)) %>% summarise(avrg_steps=mean(steps))
avgstps[which.max(avgstps$avrg_steps),1]


act_miss <- read.csv('activity.csv', sep = ',')

act_miss$steps[is.na(act_miss$steps)] <- mean(act_miss$steps,na.rm = T)


###
png('plot3.png',width = 480,height = 480)
total_steps <- na.omit(act_miss %>% group_by(as.Date(date)) %>% summarise(total_steps=sum(steps)))
hist(total_steps$total_steps, main = 'Total steps each day',
     xlab = 'Steps',col = rainbow(n = 3,v = 0.5,alpha = 0.5))
grid()
dev.off()
###


library(lubridate)
library(dplyr)
weekend <- which( wday(as.Date(act_miss$date))==1 | wday(as.Date(act_miss$date))==7)

act_miss_weekend <- act_miss[weekend,]
act_miss_week <- act_miss[-weekend,]

act_week <- act_miss_week %>% group_by(interval = as.character(interval)) %>% summarise(avr=mean(steps))
act_weekend <- act_miss_weekend %>% group_by(interval = as.character(interval)) %>% summarise(avr=mean(steps))

act_week <- act_week[order(as.numeric(act_week$interval)),]
act_weekend <- act_weekend[order(as.numeric(act_weekend$interval)),]

###
png('plot4.png',width = 480,height = 480)
par(mfrow = c(2,1))
# plot1
plot(act_week$interval,act_week$avr, type = 'l', xlab = 'Intervals', ylab = 'Average of steps',
     main='Week', col = 'cyan',lwd=4,fg='blue')
points(act_week$interval,act_week$avr, type = 'l', col = 'blue')
points(1250,75,pch=16,cex=100, col = rgb(51/255,212/255,255/255,0.1))
grid()
# plot2
plot(act_weekend$interval,act_weekend$avr, type = 'l', xlab = 'Intervals', ylab = 'Average of steps',
     main='Weekend', col = 'cyan',lwd=4,fg='blue')
points(act_weekend$interval,act_weekend$avr, type = 'l', col = 'blue')
points(1250,75,pch=16,cex=100, col = rgb(51/255,212/255,255/255,0.1))
grid()
dev.off()
###