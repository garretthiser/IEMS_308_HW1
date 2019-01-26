set.seed(420)
library(cluster) 

data = read.table("Medicare_Provider_Util_Payment_PUF_CY2016.txt", fill=TRUE, header=TRUE, sep="\t", nrows = 100000);
data = data[-1,];


vars = c("NPPES_ENTITY_CODE", "NPPES_PROVIDER_STATE", "NPPES_PROVIDER_GENDER", 
	"MEDICARE_PARTICIPATION_INDICATOR", "PLACE_OF_SERVICE", 
	"AVERAGE_MEDICARE_STANDARD_AMT")

datproc = data[vars]

datproc <- data.frame(datproc[sample(nrow(datproc), 10000), ])

#data2 = data.frame(datproc$NPPES_ENTITY_CODE, datproc$NPPES_PROVIDER_STATE, 
#	datproc$MEDICARE_PARTICIPATION_INDICATOR, datproc$PLACE_OF_SERVICE, 
#	datproc$AVERAGE_MEDICARE_PAYMENT_AMT, datproc$AVERAGE_MEDICARE_ALLOWED_AMT,
#	datproc$AVERAGE_SUBMITTED_CHRG_AMT, datproc$AVERAGE_MEDICARE_STANDARD_AMT)

datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "XX",]
datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "AE",]
datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "AP",]
datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "AS",]
datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "GU",]
datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "MP",]
datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "PR",]
datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "VI",]
datproc <- datproc[datproc$NPPES_PROVIDER_STATE != "ZZ",]

drops <- c("NPPES_PROVIDER_STATE")
datproc = datproc[ , !(names(datproc) %in% drops)]

datproc <- na.omit(datproc)

datproc$AVERAGE_MEDICARE_STANDARD_AMT = as.numeric(datproc$AVERAGE_MEDICARE_STANDARD_AMT)

datproc$ENTITY_I <- ifelse(datproc$NPPES_ENTITY_CODE == "I", (datproc$ENTITY_I = 1), (datproc$ENTITY_I = 0))
datproc$ENTITY_O <- ifelse(datproc$NPPES_ENTITY_CODE == "O", (datproc$ENTITY_O = 1), (datproc$ENTITY_O = 0))

datproc$GENDER_M <- ifelse(datproc$NPPES_PROVIDER_GENDER == "M", (datproc$GENDER_M = 1), (datproc$GENDER_M = 0))
datproc$GENDER_F <- ifelse(datproc$NPPES_PROVIDER_GENDER == "F", (datproc$GENDER_F = 1), (datproc$GENDER_F = 0))

datproc$PLACE_OF_SERVICE_OFF <- ifelse(datproc$PLACE_OF_SERVICE == "O", (datproc$PLACE_OF_SERVICE_OFF = 1), (datproc$PLACE_OF_SERVICE_OFF = 0))
datproc$PLACE_OF_SERVICE_FAC <- ifelse(datproc$PLACE_OF_SERVICE == "F", (datproc$PLACE_OF_SERVICE_FAC = 1), (datproc$PLACE_OF_SERVICE_FAC = 0))

datproc$MEDICARE_PAR_YES <- ifelse(datproc$MEDICARE_PARTICIPATION == "Y", (datproc$MEDICARE_PAR_YES = 1), (datproc$MEDICARE_PAR_YES = 0))
datproc$MEDICARE_PAR_NO <- ifelse(datproc$MEDICARE_PARTICIPATION != "Y", (datproc$MEDICARE_PAR_NO = 1), (datproc$MEDICARE_PAR_NO = 0))


drops <- c("NPPES_PROVIDER_GENDER", "NPPES_ENTITY_CODE","PLACE_OF_SERVICE", "MEDICARE_PARTICIPATION_INDICATOR")
datproc = datproc[ , !(names(datproc) %in% drops)]

datproc <- na.omit(datproc) 
mydata <- scale(datproc)

#wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
#for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
#   centers=i)$withinss)
#plot(1:15, wss, type="b", xlab="Number of Clusters",
#  ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 6) # 6 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

datproc$fit.cluster <- mydata$fit.cluster 

clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
   labels=2, lines=0)


cluster1 <- datproc[datproc$fit.cluster == "1",]
cluster2 <- datproc[datproc$fit.cluster == "2",]
cluster3 <- datproc[datproc$fit.cluster == "3",]
cluster4 <- datproc[datproc$fit.cluster == "4",]
cluster5 <- datproc[datproc$fit.cluster == "5",]
cluster6 <- datproc[datproc$fit.cluster == "6",]

#whole subset
mean(datproc$AVERAGE_MEDICARE_STANDARD_AMT)
sd(datproc$AVERAGE_MEDICARE_STANDARD_AMT)
sum(datproc$GENDER_M)/nrow(datproc)
sum(datproc$PLACE_OF_SERVICE_OFF)/nrow(datproc)
sum(datproc$MEDICARE_PAR_YES)/nrow(datproc)
sum(datproc$ENTITY_I)/nrow(datproc)

#cluster 1
mean(cluster1$AVERAGE_MEDICARE_STANDARD_AMT)
sd(cluster1$AVERAGE_MEDICARE_STANDARD_AMT)
sum(cluster1$GENDER_M)/nrow(cluster1)
sum(cluster1$PLACE_OF_SERVICE_OFF)/nrow(cluster1)
sum(cluster1$MEDICARE_PAR_YES)/nrow(cluster1)
sum(cluster1$ENTITY_I)/nrow(cluster1)

#cluster 2
mean(cluster2$AVERAGE_MEDICARE_STANDARD_AMT)
sd(cluster2$AVERAGE_MEDICARE_STANDARD_AMT)
sum(cluster2$GENDER_M)/nrow(cluster2)
sum(cluster2$PLACE_OF_SERVICE_OFF)/nrow(cluster2)
sum(cluster2$MEDICARE_PAR_YES)/nrow(cluster2)
sum(cluster2$ENTITY_I)/nrow(cluster2)

#cluster 3
mean(cluster3$AVERAGE_MEDICARE_STANDARD_AMT)
sd(cluster3$AVERAGE_MEDICARE_STANDARD_AMT)
sum(cluster3$GENDER_M)/nrow(cluster3)
sum(cluster3$PLACE_OF_SERVICE_OFF)/nrow(cluster3)
sum(cluster3$MEDICARE_PAR_YES)/nrow(cluster3)
sum(cluster3$ENTITY_I)/nrow(cluster3)

#cluster 4
mean(cluster4$AVERAGE_MEDICARE_STANDARD_AMT)
sd(cluster4$AVERAGE_MEDICARE_STANDARD_AMT)
sum(cluster4$GENDER_M)/nrow(cluster4)
sum(cluster4$PLACE_OF_SERVICE_OFF)/nrow(cluster4)
sum(cluster4$MEDICARE_PAR_YES)/nrow(cluster4)
sum(cluster4$ENTITY_I)/nrow(cluster4)

#cluster 5
mean(cluster5$AVERAGE_MEDICARE_STANDARD_AMT)
sd(cluster5$AVERAGE_MEDICARE_STANDARD_AMT)
sum(cluster5$GENDER_M)/nrow(cluster5)
sum(cluster5$PLACE_OF_SERVICE_OFF)/nrow(cluster5)
sum(cluster5$MEDICARE_PAR_YES)/nrow(cluster5)
sum(cluster5$ENTITY_I)/nrow(cluster5)

#cluster 6
mean(cluster6$AVERAGE_MEDICARE_STANDARD_AMT)
sd(cluster6$AVERAGE_MEDICARE_STANDARD_AMT)
sum(cluster6$GENDER_M)/nrow(cluster6)
sum(cluster6$PLACE_OF_SERVICE_OFF)/nrow(cluster6)
sum(cluster6$MEDICARE_PAR_YES)/nrow(cluster6)
sum(cluster6$ENTITY_I)/nrow(cluster6)

nrow(cluster1)
nrow(cluster2)
nrow(cluster3)
nrow(cluster4)
nrow(cluster5)
nrow(cluster6)

head(datproc)

