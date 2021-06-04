library(ggplot2)
library(lazyeval)
library(mosaic)
library(statisticalModeling)
library(dplyr)
library(tidyverse)
library(readxl)
library(reshape2)
library(treemap)


#importing the data set
data <- read.csv("C:\\Users\\alekhya\\Desktop\\R programs\\MoviesOnStreamingPlatforms_updated.csv")
colnames(data)

#with help of summary  would help us spot any anomalies like negative values. 
#It would also indicate the fields with missing values and their counts.
summary(data)

#In run time column we can see 592 Na's

#Deletion of unnecessary columns

#Few of the column like X we won't be needing for analysis because these contain index values.
#Let's get rid of the these column.

data_clean <- data %>% select(-X)

#Checking final dimensions of cleaned data set
dim(data_clean)

# No. of movies present in all ott platforms(Netflix, Prime, Hulu, Disney)

movie_data <- data_clean  %>% select(Title,Netflix,Hulu,Prime.Video,Disney.) %>% 
  summarise(sum_netflix = sum(Netflix,na.rn=TRUE),
            sum_Hulu = sum(Hulu,na.rn=TRUE),
            sum_Prime.Video = sum(Prime.Video,na.rn=TRUE),
            sum_Disney. = sum(Disney.,na.rn=TRUE))
movie_data
molted=melt(movie_data)

ggplot(molted, aes(x="", y=value, fill=variable)) + 
  geom_bar(width = 3, stat = "identity") + 
  coord_polar("y", start=0) +
  geom_text(aes(label=value),position=position_stack(vjust = 0.5))


#Find movies with long runtime in overall.

movie_runtime <-data_clean %>% 
  select(Title,Runtime) %>% 
  arrange(desc(Runtime))

f2 <- head(movie_runtime,10) 
ggplot(data=f2,aes(x=Runtime,y=Title,col=Title))+geom_jitter()
ggplot(data=f2,aes(y=Title,fill=Runtime))+geom_bar()


#Total Number of movies based on genre and language overall


# Total movies based on genre:
ott <- distinct(data_clean,Title,Genres,Language, .keep_all= TRUE)
g <- str_split(ott$Genres, ",")
ott_genres <- data.frame(ID = rep(ott$ID, sapply(g, length)), genres = unlist(g))
ott_genres $ genres <- as.character(gsub(",","",ott_genres$genres))
df_by_genres_full <- ott_genres %>% group_by(genres) %>% summarise(count = n()) %>% arrange(desc(count)) %>% filter(genres != "")
ggplot(data = df_by_genres_full,aes(x=count,fill=genres)) +geom_histogram()

# Total number of movies based on languages:-

# We can either print some particular languages: 
Total_movies <- data_clean %>% select(Title,Genres,Language)
tmovie_subset <- Total_movies[Total_movies$Language %in% 
                                c("English", "Hindi","Spanish" ,"French"	,"Others",
                                  "German","Japanese","Arabic"	, "Mandarin","Italian",
                                  "Turkish", "Cantonese","Russian","Tamil	",
                                  "Punjabi"," Portuguese","Indonesian"," Malayalam",
                                  "Filipino","Korean"),]
ggplot(data = tmovieset,aes(y=Language)) +geom_bar()


# or we can print all:
l<- str_split(ott$Language,",")
ott_language <- data.frame(ID = rep(ott$ID, sapply(l, length)), language = unlist(l))
ott_language$language <- as.character(gsub(",","",ott_language$language))
df_by_language_full <- ott_language %>% group_by(language) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% filter(language != "")
ggplot(data = df_by_language_full,aes(y=count,fill=language) )+geom_histogram()
f2 <-head(df_by_language_full,20)
ggplot(data = f2,aes(y=count,fill=language) )+geom_histogram()



