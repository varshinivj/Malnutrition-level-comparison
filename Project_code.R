
#importing the dataset which contains all the major states of India
#data source in stata file, so had to import package haven to import the dataset
library(haven)
high_hdi_final <- read_dta("~/Desktop/Files/high hdi final.dta")

#checking the variable titles
names(high_hdi_final)

#renaming the variables


names(high_hdi_final)[names(high_hdi_final) == 'v024'] <- 'state'
names(high_hdi_final)[names(high_hdi_final) == 'v106'] <- 'high_edulvl'
names(high_hdi_final)[names(high_hdi_final) == 'v130'] <- 'religion'
names(high_hdi_final)[names(high_hdi_final) == 'v212'] <- 'age_resp_1'
names(high_hdi_final)[names(high_hdi_final) == 'v457'] <- 'anemia_lvl'
names(high_hdi_final)[names(high_hdi_final) == 'v714'] <- 'resp_wrk'
names(high_hdi_final)[names(high_hdi_final) == 'b4'] <- 'sex_child'
names(high_hdi_final)[names(high_hdi_final) == 'm14'] <- 'antnetal_visit'
names(high_hdi_final)[names(high_hdi_final) == 'm15'] <- 'place_del'
names(high_hdi_final)[names(high_hdi_final) == 'm18'] <- 'size_child'
names(high_hdi_final)[names(high_hdi_final) == 's45'] <- 'caste2'
names(high_hdi_final)[names(high_hdi_final) == 'v025'] <- 'type_resi'
high_hdi_final$state <- as.factor(high_hdi_final$state)
names(high_hdi_final)

#dropping repititive columns
high_hdi_final$s46 <- NULL
high_hdi_final$caste <- NULL
high_hdi_final$caste2 <- NULL
high_hdi_final$type_resi <- NULL
high_hdi_final$residence <- NULL
high_hdi_final$relegion <- NULL
high_hdi_final$swht <- NULL
high_hdi_final$place_del <- NULL

#coverting character columns into factors
high_hdi_final$high_edulvl <- as.factor(high_hdi_final$high_edulvl)
high_hdi_final$resp_wrk <- as.factor(high_hdi_final$resp_wrk)
high_hdi_final$sex_child <- as.factor(high_hdi_final$sex_child)
high_hdi_final$wealth <- as.factor(high_hdi_final$wealth)
high_hdi_final$sanitation <- as.factor(high_hdi_final$sanitation)
high_hdi_final$dri_water <- as.factor(high_hdi_final$dri_water)
high_hdi_final$place_delivery <- as.factor(high_hdi_final$place_delivery) # 1-home, 2-public, 3-private

#subsetting the data into required two sets of northern and southern states 
north_data <- high_hdi_final[high_hdi_final$state %in% c(3, 6), ]
south_data <- high_hdi_final[high_hdi_final$state %in% c(29, 32), ]

#taking the northern state dataset first

#removing the NA  values if any


#counting the number of NAs
sum(is.na(north_data))

#removing the NAs
north_data <- na.omit(north_data)


#splitting into train and test dataset
index <- sample(2,nrow(north_data),replace=TRUE,prob = c(0.7,0.3))

#making the datasets
train_north_data <- north_data[index==1,]
test_north_data <- north_data[index==2,]

# Create the model

#for hfa (excluding wfh and wfa)
fit <- glm(hfa ~ . -wfh-wfa-state , data = train_north_data, family = "binomial")
summary(fit)

# Find the predicted value for the testing dataset 

test_pred <- predict.glm(fit,test_north_data, type = 'response')
test_pred <- data.frame(test_north_data)
summary(test_pred)

#for wfa (excluding wfh and hfa)
fit <- glm(wfa ~ . -wfh-hfa-state , data = train_north_data, family = "binomial")
summary(fit)


# Find the predicted value for the testing dataset 

test_pred <- predict.glm(fit,test_north_data,type='response')
test_pred <- data.frame(test_north_data)
summary(test_pred)

#for wfa (excluding wfa & hfa)
fit <- glm(wfh ~ . -wfa-hfa-state , data = train_north_data, family = "binomial")
summary(fit)


# Find the predicted value for the testing dataset 

test_pred <- predict.glm(fit,test_north_data,type='response')
test_pred <- data.frame(test_north_data)
summary(test_pred)

#taking the southern state dataset 

#removing the NA  values if any


#counting the number of NAs
sum(is.na(south_data))

#removing the NAs
south_data <- na.omit(south_data)


#splitting into train and test dataset
index <- sample(2,nrow(south_data),replace=TRUE,prob = c(0.7,0.3))

#making the datasets
train_south_data <- south_data[index==1,]
test_south_data <- south_data[index==2,]

# Create the model

#for hfa
fit <- glm(hfa ~ . -wfh-wfa-state , data = train_south_data, family = "binomial")
summary(fit)

# Find the predicted value for the testing dataset 

test_pred <- predict.glm(fit,test_south_data,type='response')
test_pred <- data.frame(test_south_data)
summary(test_pred)

#for wfa
fit <- glm(wfa ~ . -wfh-hfa-state , data = train_south_data, family = "binomial")
summary(fit)


# Find the predicted value for the testing dataset 
test_pred <- predict.glm(fit,test_south_data,type='response')
test_pred <- data.frame(test_south_data)
summary(test_pred)

#for wfa
fit <- glm(wfh ~ . -wfa-hfa-state , data = train_south_data, family = "binomial")
summary(fit)


# Find the predicted value for the testing dataset 

test_pred <- predict.glm(fit,test_south_data,type='response')
test_pred <- data.frame(test_south_data)
summary(test_pred)


