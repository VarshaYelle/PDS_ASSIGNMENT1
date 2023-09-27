raw_data <- read.csv("C:/Users/Dell/OneDrive/Documents/PDS/Frailty/RawData/first_question.csv")
clean_data<-na.omit(raw_data[raw_data != "N", ])
write.csv(clean_data,"C:/Users/Dell/OneDrive/Documents/PDS/Frailty/Data Cleaning/clean_data.csv")

#Loading  clean Data

clean_data


typeof(clean_data$Height..Inches.)

Summ<-summary(clean_data)
Summ
capture.output(Summ,file="C:/Users/Dell/OneDrive/Documents/PDS/Frailty/results.txt")

##ploting between Age and frailty
barplot(clean_data$Age, clean_data$Grip.Strength,xlab="Age",ylab="Frailty",main="Age vs Frailty", col="green")

