# GAGANDEEP SINGH KHANUJA 

#### We have been asked to conduct Statistical Analysis. ####
#### A couple of pre-requisites have been to be first met before performing Statistical Analysis. 

#######################----------------------############################

#### a.) In the first part, I have combined all the 9 csv files. I wasn't able to extract the 4th CSV file because it contained Special characters in it's reviews.


reviews_google_submission_1 <- read.csv("reviews_google_submission_1.csv")
reviews_google_submission_2 <- read.csv("reviews_google_submission_2.csv")
reviews_google_submission_3 <- read.csv("reviews_google_submission_3.csv")
reviews_google_submission_5 <- read.csv("reviews_google_submission_5.csv")
reviews_google_submission_6 <- read.csv("reviews_google_submission_6.csv")
reviews_google_submission_7 <- read.csv("reviews_google_submission_7.csv")
reviews_google_submission_8 <- read.csv("reviews_google_submission_8.csv")
reviews_google_submission_9 <- read.csv("reviews_google_submission_9.csv")
reviews_google_submission_10 <- read.csv("reviews_google_submission_10.csv")


# All the data has been pushed to one variable and all duplicates have been removed
data <- rbind(reviews_google_submission_1,reviews_google_submission_2,reviews_google_submission_3,
                     reviews_google_submission_5,reviews_google_submission_6,reviews_google_submission_7,
                     reviews_google_submission_8,reviews_google_submission_9,reviews_google_submission_10
                     )
data <- data[!(duplicated(data)),]

#######################----------------------############################

#### b.) The second part involves us to convert string to numeric  

data$review_rating <- as.character(data$review_rating)
data$review_rating <- substr(data$review_rating, 0, 3)
data$review_rating <- as.numeric(data$review_rating)

#######################----------------------############################


#### c.	The Entity names, Entity Types, and Entity Salience columns also have a string.  There is a comma separating each value in these strings.  For example, letâs say that the Entity names returned for a specific review are (hits,item,miss).  The corresponding types values would be (7,7,4). The salience scores might be (0.4,0.2,0.1).  You can think about using the split function to deal with these strings.

library(stringr)
data$Entity_Types <- as.character(data$Entity_Types)
data$Count_Unknown <- str_count(data$Entity_Types, "0")
data$Count_person <- str_count(data$Entity_Types, "1")
data$Count_location <- str_count(data$Entity_Types, "2")
data$Count_organization <- str_count(data$Entity_Types, "3")
data$Count_event <- str_count(data$Entity_Types, "4")
data$Count_WorkofArt <- str_count(data$Entity_Types, "5")
data$Count_ConsumerGood <- str_count(data$Entity_Types, "6")
data$Count_Other <- str_count(data$Entity_Types, "7")

#######################----------------------############################

#### Part 2 : Statistical Analysis.

####1.) Assessing the Hypothesis. 
vis=data[data$vis == 1, "review_rating"]
non_vis=data[data$vis == 0, "review_rating"]
t.test(non_vis,vis)

#######################----------------------############################

####2.) Assessing the hypothesis. 
nlp_non_vis=data[data$vis == 0, "Sentiment_Score"]
nlp_vis=data[data$vis == 1, "Sentiment_Score"]
t.test(nlp_non_vis,nlp_vis)

#######################----------------------############################

####3.) Finding the 6 combinations of zipcodes that have been most reviewed in the sample provided. 

library(plyr)
zip=count(data, "address_zipcode")
zip=zip[order(zip$freq, decreasing = TRUE), ]

#looking at the top 4 zips
head(zip,4)

#Keeping only top 4 and storing them in corresponding variables
zip=head(zip,4)
zip1=zip$address_zipcode[1]
zip2=zip$address_zipcode[2]
zip3=zip$address_zipcode[3]
zip4=zip$address_zipcode[4]

#3a.) Calculating the t value, p-value of Zip 1 and Zip 2
# Performing tests for review ratings
rating_zip1=data[data$address_zipcode==zip1, "review_rating"]
rating_zip2=data[data$address_zipcode==zip2, "review_rating"]
t.test(rating_zip1,rating_zip2)
# Performing tests for sentiment score
nlp_zip1=data[data$address_zipcode==zip1, "Sentiment_Score"]
nlp_zip2=data[data$address_zipcode==zip2, "Sentiment_Score"]
t.test(nlp_zip1,nlp_zip2)

#3b.) Calculating the t value, p-value of Zip 1 and Zip 3
# Performing tests for review ratings
rating_zip3=data[data$address_zipcode==zip3, "review_rating"]
t.test(rating_zip1,rating_zip3)
# Performing tests for sentiment score
nlp_zip3=data[data$address_zipcode==zip3, "Sentiment_Score"]
t.test(nlp_zip1,nlp_zip3)

#3c.) Calculating the t value, p-value of Zip 1 and Zip 4
# Performing tests for review ratings
rating_zip4=data[data$address_zipcode==zip4, "review_rating"]
t.test(rating_zip1,rating_zip4)
# Performing tests for sentiment score
nlp_zip4=data[data$address_zipcode==zip4, "Sentiment_Score"]
t.test(nlp_zip1,nlp_zip4)

#3d.) Calculating the t value, p-value of Zip 2 and Zip 3
# Performing tests for review ratings
t.test(rating_zip2,rating_zip3)
# Performing tests for sentiment ratings
t.test(nlp_zip2,nlp_zip3)

#3e.) Calculating the t value, p-value of Zip 2 and Zip 4
# Performing tests for review ratings
t.test(rating_zip2,rating_zip4)
# Performing tests for sentiment score
t.test(nlp_zip2,nlp_zip4)

#3f.) Calculating the t value, p-value of Zip 3 and Zip 4
# Performing tests for review ratings
t.test(rating_zip3,rating_zip4)
# Performing tests for sentiment score
t.test(nlp_zip3,nlp_zip4)


#######################----------------------############################

###4.) Finding the mismatch between the review scores and the sentiment of review.
# Finding the 75th percentile of the sentiments given.
s_s_75=quantile(data$Sentiment_Score, 0.75) 
# Creating a variable mismatch. 
data$mismatch =0
#Updating the mismatch variable based on the conditions mentioned. 
data$mismatch[data$Sentiment_Score>s_s_75 & data$review_rating<=2] =1
data$vis = as.factor(data$vis)
#Making a logit model with mismatch as dependent variable and vis as independent variable
mylogit <- glm(mismatch ~ Sentiment_Magnitudes*vis , data = data, family = "binomial") 
summary(mylogit)


#######################----------------------############################


#### 5.) To find out whether there is a statistical difference between the count of entities in each category between viss and non-viss. 
t.test(data$Count_location ~ as.factor(data$vis),alternative = "two.sided", mu = 0)
review_entity_counts <- data[,17:24]
test_entity <- function(x) {
  t <- t.test(x ~ as.factor(data$vis),alternative = "two.sided", mu = 0)
  c(t$estimate[2], t$estimate[1],"p_value" = t$p.value)
}
knitr::kable(t(sapply(review_entity_counts, test_entity)),caption = "T test results for difference in entity counts for visitors and non visitors")

#######################----------------------############################


#### 6.) To create model to assess the relationship between the count of each entity for each review and sentiment of the review. 
model <- lm(Sentiment_Score ~ Count_Other * Sentiment_Magnitudes + 
               Count_ConsumerGood * Sentiment_Magnitudes +
               Count_WorkofArt * Sentiment_Magnitudes +
               Count_event * Sentiment_Magnitudes +
               Count_organization * Sentiment_Magnitudes +
               Count_location * Sentiment_Magnitudes +
               Count_person * Sentiment_Magnitudes, data = data)
summary(model)

#######################----------------------############################

