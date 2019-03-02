crime <- read.csv("D:/PGA/python pro/Project/Crime/Crimes_-_2018.csv")
head(crime)
nrow(crime)
str(crime)
summary(crime)
View(crime)
unique(crime['Location.Description'])
crime$Location.Description[crime$Location.Description == ""] <- NA


install.packages("VIM", dependencies = TRUE)
library(VIM)
?kNN()

crime1 <- kNN(crime, variable = "Location.Description", k=5)
summary(crime1)

ncol(crime)
ncol(crime1)

crime2 <- crime1[1:22]
ncol(crime2)
summary(crime2)

write.csv(crime2,'D:/PGA/python pro/Project/Crime/crime.csv')
