## --------------------Final Exam- Data Analytics------------------------##
##-----------------------ENGM 182 Winter 2023----------------------------## 
##--------------Submitted By- Supritha Shankar Rao-------------------------##

#Setting working directory
setwd("C:/Users/supri/OneDrive/Desktop/Data Analytics Class")

#Loading Libraries
library(NbClust)
library(flexclust)
library(cluster)
library(factoextra)
library(mgcv)
library(gamair)
library(bootstrap)
library(dplyr)
library(FactoMineR)
library(psych)
library(forecast)
library(seasonal)
library(zoo)

#Loading data
State <- read.csv("C:/Users/supri/Downloads/State.csv")
View(State)
Quarter <- read.csv("C:/Users/supri/Downloads/Quarter.csv")
View(Quarter)
#adding region to states
region_data <- data.frame(
  state= c("Alabama", "Alaska","Arizona","Arkansas" ,"California", "Colorado","Connecticut","Delaware",            
           "District of Columbia", "Florida", "Georgia","Hawaii","Idaho", "Illinois","Indiana", "Iowa" ,               
           "Kansas","Kentucky", "Louisiana","Maine" ,
           "Maryland","Massachusetts","Michigan", "Minnesota",           
           "Mississippi", "Missouri", "Montana" ,  "Nebraska",            
           "Nevada","New Hampshire","New Jersey", "New Mexico",          
           "New York",  "North Carolina", "North Dakota", "Ohio",                
           "Oklahoma", "Oregon", "Pennsylvania","Rhode Island",        
           "South Carolina","South Dakota", "Tennessee","Texas",               
           "Utah",  "Vermont" ,"Virginia" , "Washington",          
           "West Virginia", "Wisconsin","Wyoming"),
  region = c("South", "West", "West", "South", "West", 
             "West", "Northeast", "South", "South", "South", 
             "South", "West", "West", "Midwest", "Midwest",
             "Midwest", "South", "South", "South", "Northeast", 
             "South", "Northeast", "Midwest", "Midwest", "South", 
             "Midwest", "West", "Midwest","West", "Northeast",
             "Northeast", "West", "Northeast", "South", "Midwest", 
             "Midwest", "South","West","Northeast","Northeast","South", "Midwest", 
             "South","South", "West", "Northeast", "South","West", 
             "South", "Midwest","West")
  
)
states <- merge(State, region_data, by = "state")
head(states)
states$state
#new data set to be used: states


##1
##K-Means clustering
#Extracting  the  numeric  variables
State_Num <- select_if(states, is.numeric)
#Scaling data
State_Num <- scale(State_Num, center=TRUE)
#data cleaning
State_Num <- State_Num[-2, ]
state <- states[-2, ]

#K-means
set.seed(1234)
nc_kmeans = NbClust(State_Num, method = "kmeans") #number of clusters
barplot(table(nc_kmeans$Best.n[1,]),col = "lavender",main="K-Means Cluster Plot", xlab = "Number of Clusters", ylab = "Number of Criteria")
fviz_nbclust(State_Num,kmeans,method="silhouette",barfill = "#4c00b0",
             barcolor = "#4c00b0",
             linecolor = "#4c00b0",) #number of clusters
fit_kmeans = kmeans(State_Num, 2, nstart = 25)
km.clusters<-fit_kmeans$cluster
#rownames(State_Num)<-paste(state$region, state$state, sep = "_")
fviz_cluster(list(data=State_Num, cluster = km.clusters),palette = c(  "#1DB954","#4c00b0"),ggtheme = theme_classic())


#Bivariate Cluster Plot
fit_pam = pam(State_Num, k=2, stand=TRUE)
clusplot(fit_pam, main = "Bivariate Cluster Plot")


# Extract the cluster assignments for each data point
cluster_assignments <- fit_kmeans$cluster

# Add the cluster assignments to the original dataset
State_clusters <- state %>%
  mutate(cluster = cluster_assignments)

# Calculate the mean values for each variable by cluster
# not the one with scaled data
cluster_means <- State_clusters %>%
  group_by(cluster) %>%
  summarize_all(mean)

# View the cluster means
cluster_mean= (cluster_means[, -2])
cluster_mean[,-2]


#grouping-deciding the states into north, south, midwest and northeast states

# Group data by region
grouped_data <- group_by(states, region)
grouped_data
# Create separate data frames region wise
West_data <- filter(grouped_data, region == "West")
head(West_data)
South_data <- filter(grouped_data, region == "South")
head(South_data)
Northeast_data <- filter(grouped_data, region == "Northeast")
head(Northeast_data)
Midwest_data <- filter(grouped_data, region == "Midwest")
head(Midwest_data)

# calculate the mean of the 'x' column
mean_west <- mean(West_data$gdp)
mean_South_data <- mean(South_data$gdp)
mean_Northeast_data <- mean(Northeast_data$gdp)
mean_Midwest_data <- mean(Midwest_data$gdp)

w=paste0(round(mean_west*100,2), "%")
sp=paste0(round(mean_South_data*100,2), "%")
ne=paste0(round(mean_Northeast_data*100,2), "%")
mw=paste0(round(mean_Midwest_data*100,2), "%")

region_dataframe <- data.frame(RegionNames = c("West", "South", "Northeast", "Midwest"),
                               gdp_Regionwise = c(w, sp, ne, mw))


a=ggplot(region_dataframe, aes(x = RegionNames, y = gdp_Regionwise)) + 
  geom_bar(stat = "identity", fill = "lavender") + 
  geom_text(aes(label = gdp_Regionwise), vjust = -0.5) +
  labs(x = "Regions of USA", y = "GDP")  + ggtitle("Mean percentage increase in GDP across US Regions")

a+theme_classic()

#--------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------

#2
#create time series object
QuarterOBJ=ts(Quarter, start=c(2020,1), frequency = 4) #annually
QuarterOBJ

# Plot the time series
autoplot(QuarterOBJ[,-1])+theme_classic() +
  labs(title = "Time Series Forecast Plot")
#plot
plot(QuarterOBJ, type="o", pch=19,col="#4c00b0",main="Changes in economic indicators in the U.S. from 2020 to 2022")
#evidence of trends or seasonality in the time series
#Decomposing the  US economic indicators into  seasonal, trend,  and  irregular components.
#income
Quarter_Income=ts(Quarter$income, start=c(2020,1),frequency = 4) #annually
fit_income <- stl(Quarter_Income, s.window="periodic")
plot(fit_income,col="#4c00b0",main="INCOME decomposed into seasonal, trend, and irregular components")
#employment
Quarter_employment=ts(Quarter$employment, start=c(2020,1),frequency = 4) #annually
fit_employment <- stl(Quarter_employment, s.window="periodic")
plot(fit_employment,col="#4c00b0",main="EMPLOYMENT decomposed into seasonal, trend, and irregular components")
#expenditures
Quarter_expenditures=ts(Quarter$expenditures, start=c(2020,1),frequency = 4) #annually
fit_expenditures <- stl(Quarter_expenditures, s.window="periodic")
plot(fit_expenditures,col="#4c00b0",main="EXPENDITURES decomposed into seasonal, trend, and irregular components")
#production
Quarter_production=ts(Quarter$production, start=c(2020,1),frequency = 4) #annually
fit_production <- stl(Quarter_production, s.window="periodic")
plot(fit_production,col="#4c00b0",main="PRODUCTION decomposed into seasonal, trend, and irregular components")
#manufacturing
Quarter_manufacturing=ts(Quarter$manufacturing, start=c(2020,1),frequency = 4) #annually
fit_manufacturing <- stl(Quarter_manufacturing, s.window="periodic")
plot(fit_manufacturing,col="#4c00b0",main="MANUFACTURING decomposed into seasonal, trend, and irregular components")


#--------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------

#3
###predictions
#Predictions income
Pred_Income<-ets(Quarter_Income, model="AAA")
accuracy(Pred_Income)
# Forecast in 2023, 12 months
pred<-forecast(Pred_Income,h=4)
pred
plot(pred,col="#4c00b0",ylab="Quarter_Income",xlab="Year",main="Forecast of Income in 2023")


#Predictions employment
Pred_employment<-ets(Quarter_employment, model="AAA")
accuracy(Pred_employment)
# Forecast in 2023, 12 months
pred_employment<-forecast(Pred_employment,h=4)
pred_employment
plot(pred_employment,col="#4c00b0",ylab="Quarter_Employment",xlab="Year",main="Forecast of employment in 2023")


#Predictions expenditures
Pred_expenditures<-ets(Quarter_expenditures, model="AAA")
accuracy(Pred_expenditures)
# Forecast in 2023, 12 months
pred_expenditures<-forecast(Pred_expenditures,h=4)
pred_expenditures
plot(pred_expenditures,col="#4c00b0",ylab="Quarter_Expenditures",xlab="Year",main="Forecast of expenditures in 2023")



#Predictions production
Pred_production<-ets(Quarter_production, model="AAA")
accuracy(Pred_production)
# Forecast in 2023, 12 months
pred_production<-forecast(Pred_production,h=4)
pred_production
plot(pred_production,col="#4c00b0",ylab="Quarter_Production",xlab="Year",main="Forecast of production in 2023")


#Predictions manufacturing
Pred_manufacturing<-ets(Quarter_manufacturing, model="AAA")
accuracy(Pred_manufacturing)
# Forecast in 2023, 12 months
pred_manufacturing<-forecast(Pred_manufacturing,h=4)
pred_manufacturing
plot(pred_manufacturing,col="#4c00b0",pch=20,lty = "solid",
     cex = 1,ylab="Quarter_Manufacturing",xlab="Year",main="Forecast of manufacturing in 2023")


#--------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------

