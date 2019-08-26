library(ggplot2)
library(plyr) 
library(lubridate)
'%!in%' <- function(x,y)!('%in%'(x,y))
violations <- read.csv('franklin.csv')
meta <- read.csv('franklin-meta.csv')
meta = meta[!duplicated(meta$disposition.of.case.term),]
violations <- merge(violations, meta, by.x='case.disposition', by.y='disposition.of.case.term', all.x = TRUE)
violations$action.description[grepl('SPEED',violations$action.description)] <- 'SPEED'
violations$citation.date <- as.Date(violations$citation.date, format= '%m/%d/%Y')
violations$hour <- hour(strptime(violations$citation.time, "%l:%M %p"))
violations$weekday <- weekdays(violations$citation.date)
date_count <- count(violations, 'weekday')
hour_count <- count(violations, 'hour')
sex_count <- count(violations, 'sex')
sex_count$pct <- (sex_count$freq/sum(sex_count$freq))*100
color_count <- count(violations, 'color')
color_count$pct <- (color_count$freq/sum(color_count$freq))*100
sex_count
ggplot(sex_count, aes(x="", y=freq, fill=sex))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)

ggplot(color_count, aes(x="", y=freq, fill=color))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)


ggplot(date_count, aes(x=weekday, y=freq, fill=freq)) + geom_bar(stat='identity')+
  scale_x_discrete(limits=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
(date_count$freq)
ggplot(hour_count, aes(x=hour, y=freq)) + geom_line() + geom_point()

actions <- as.data.frame(table(violations$action.description))
disp <- as.data.frame(table(violations$term.meaning))

topDisp <- disp[order(-disp$Freq),][1:10, ]
library(htmlTable)
topActions <- actions[order(-actions$Freq),][1:10, ]
names(topActions) <- c('Violation', 'Occurances')
htmlTable(topActions)

ggplot(topActions, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)