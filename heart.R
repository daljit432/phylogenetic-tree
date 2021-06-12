data1 <- read.csv(file.choose(), header = TRUE)
data1

#line graph
x <- data1[1:15, "chol"]
plot(x,type = "b",col = "red", xlab = "row no.", ylab = "chol", main = "heart details of male and female and cholestrol")

#pie chart
x<- data1[1:21, "oldpeak"]
Lables<- data1[1:21, "oldpeak"]
pie(x,Lables,main = "oldpeak in heart disease",col = rainbow(length(x)))

#barplot
D <- data1[1:20, "age"]
N <- data1[1:20, "cp"]
barplot(D,names.arg = N,xlab = "age",ylab = "cp",main = "details of heart disease   patients ages and cp", col = "blue")

#boxplot
input_data <- data1[,c('age','cp')]
boxplot(age ~ cp, data = data1, xlab = "no of age", ylab = "cp", main = "details of heart attack patient", col = "purple","green")

#scatter plot
ss <- data1[1:303,c("trtbps","thalachh")]
ss
plot(x=data1$trtbps, y=data1$thalachh,xlab = "trtbps",ylab = "thalachh", main = "trtbps vs thalachh", col = "red")

# creating pdf
pdf("heart,plots.pdf")
plot(x,type = "o",col = "red", xlab = "Gender", ylab = "chol", main = "heart details of male and female and cholestrol")
pie(x,Lables,main = "oldpeak in heart disease",col = rainbow(length(x)))
barplot(D,names.arg = N,xlab = "age",ylab = "cp",main = "details of heart disease   patients ages and cp", col = "blue")
boxplot(age ~ cp, data = data1, xlab = "no of age", ylab = "cp", main = "details of heart attack patient", col = "purple","green")
plot(x=data1$trtbps, y=data1$thalachh,xlab = "trtbps",ylab = "thalachh", main = "trtbps vs thalachh", col = "red")
dev.off()
