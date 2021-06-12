cancer <- read.csv(choose.files(), header = TRUE)
cancer
head(cancer)
str(cancer)
summary(cancer)
dim(cancer)
glimpse(cancer)
cancer <- cancer[-33]
summary(cancer)
# no of women affected in begnin and malignant stage
cancer %>% count(diagnosis)

# percentage of women affected in begnin and malignant stage
cancer %>% count(diagnosis)%>%group_by(diagnosis)%>%
     summarise(perc_dx = round((n / 569)*100, 2))
#data visualization
# frequency of cancer diagnosis
diagnosis.table <- table(cancer$diagnosis)
colors <- terrain.colors(2)

# create a pie chart
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielables <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")
pie(diagnosis.prop.table,lables = pielables, clockwise = TRUE,col = colors,
    border = "gainsboro",radius = 0.8,cex = 0.8, main = "frequency of cancer diagnosis")
legend(1, .4, legend = diagnosis.prop.df[,1],cex = 0.7, fill = colors)

#corrrelation plot
#calculate collineraity
c <- cor(cancer[,3:31])
corrplot(c, order = "hclust", tl.cex = 0.7)

#comparing the radius, area concavity of begnin and malignant stage
ggplot(cancer, aes(x=diagnosis, y=radius_mean,fill="pink"))+geom_boxplot(fill="yellow")+ggtitle( "radius_mean ofbegnin")
ggplot(cancer, aes(x=diagnosis, y=area_mean,fill="pink"))=geom_boxplot()+ggtitle("area of begnin vs malignant")
same for concavity,
#barplot
ggplot(cancer, aes(x=diagnosis, fill = texture_mean, fill = "yellow"))+geom_bar()+ggtitle("women affected in begnin and malignant")

#women affected at higher level based on meam from the analysis of boxplot
sel_data<- cancer[cancer$radius_mean]>10&cancer$radius_mean<15&cancer$compactness_mean>0.1


#density plot based on texture and mean
ggplot(cancer, aes(x=texture_mean,fill=as.factor(diagnosis)))+geom_density()+ggtitle("texture mean")

#barplot for area_se 15
ggplot(cancer, aes(x=area_se>15, fill = diagnosis))+ geom_bar(position = "fill")+ggtitle("area for begnin vs malignant tumour")

#ditribution of data via histogram
ggplot(cancer, aes(x=concavity_mean,fill=diagnosis))+geom_histogram(binwidth = 5)+ggtitle("concavity mean")
ggplot(cancer, aes(x= texture_se)) + facet_wrap(~ diagnosis)+ggtitle("texture se for beginin vs malignant")
ggplot(cancer, aes(x = perimeter_mean))+geom_histogram(binwidth = 15)+ facet_wrap(~ diagnosis)+ggtitle("perimeter mean for begnin and malignant")

# split the data into trainng and testing sets(logistic regression)
cancer$diagnosis <- factor(cancer$diagnosis,levels = c("B","M"),labels = c(0,1))
split=sample.split(cancer$diagnosis,SplitRatio = 0.65)
cancer<-cancer[-33]
training_set <- subset(cancer,split==T)
training_set

test_set <- subset(cancer,split==F)
test_set

#normalization process
training_set[,3:32]<-scale(training_set[,3:32])
training_set

test_set[,3:32]<-scale(test_set[,3:32])
test_set

# create a model
reg<-lm(training_set ~ test_set)
reg<-lm(formula = diagnosis ~ ., family=quasibinomial(),cancer=training_set)
summary(reg)

# preddict the model
prob_prep <- predict(object = reg,type="response",newdata=test_set[-2])

cancer_model = lm(texture_mean ~ radius_mean + width, data = cancer)
