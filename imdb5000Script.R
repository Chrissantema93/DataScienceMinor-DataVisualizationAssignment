#Research questions
#What genres are making the most money? 
#What genres get the best ratings? 
#How does this change over the years?

#apologies for what you're about to see...

library(shiny)
library(tidyverse)
# install.packages("scales") uncomment if necessary
library(scales)

movies <- read.csv("movies.csv")


#genres
genresList <- c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime",
                "Documentary", "Drama", "Family", "Fantasy", "Film Noir", "History",
                "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                "Short Film", "Sport", "Superhero", "Thriller", "War", "Western")

# we only need the gross and genres, imdb_score(rating) and title year
grossgenres <- select(movies, gross, genres, title_year, imdb_score)

#we only want the values not na's
grossgenres <- filter(grossgenres, !is.na(gross), !is.na(genres), !is.na(title_year), !is.na(imdb_score))



#separating the genres in 
test <- separate(grossgenres, genres, into=c("genre1", "genre2", "genre3", "genre4", "genre5", 
                                             "genre6", "genre7", "genre8"
), sep="\\|")

#combine with a comma for readability
test2 <- unite(test, "genres", genre1:genre8, na.rm=T, remove=T, sep=", ")


#add new columns from imdb genres list
test2[genresList] <- NA

#fill columns with corresponding genres
test2$Action[grepl("Action", test2$genres, fixed=TRUE)]=1
test2$Adventure[grepl("Adventure", test2$genres, fixed=TRUE)]=1
test2$Animation[grepl("Animation", test2$genres, fixed=TRUE)]=1
test2$Biography[grepl("Biography", test2$genres, fixed=TRUE)]=1
test2$Comedy[grepl("Comedy", test2$genres, fixed=TRUE)]=1
test2$Crime[grepl("Crime", test2$genres, fixed=TRUE)]=1
test2$Documentary[grepl("Documentary", test2$genres, fixed=TRUE)]=1
test2$Drama[grepl("Drama", test2$genres, fixed=TRUE)]=1
test2$Family[grepl("Family", test2$genres, fixed=TRUE)]=1
test2$Fantasy[grepl("Fantasy", test2$genres, fixed=TRUE)]=1
test2$`Film Noir`[grepl("Film Noir", test2$genres, fixed=TRUE)]=1
test2$History[grepl("History", test2$genres, fixed=TRUE)]=1
test2$Horror[grepl("Horror", test2$genres, fixed=TRUE)]=1
test2$Music[grepl("Music", test2$genres, fixed=TRUE)]=1
test2$Musical[grepl("Musical", test2$genres, fixed=TRUE)]=1
test2$Mystery[grepl("Mystery", test2$genres, fixed=TRUE)]=1
test2$Romance[grepl("Romance", test2$genres, fixed=TRUE)]=1
test2$`Sci-Fi`[grepl("Sci-Fi", test2$genres, fixed=TRUE)]=1
test2$`Short Film`[grepl("Short Film", test2$genres, fixed=TRUE)]=1
test2$Sport[grepl("Sport", test2$genres, fixed=TRUE)]=1
test2$Superhero[grepl("Superhero", test2$genres, fixed=TRUE)]=1
test2$Thriller[grepl("Thriller", test2$genres, fixed=TRUE)]=1
test2$War[grepl("War", test2$genres, fixed=TRUE)]=1
test2$Western[grepl("Western", test2$genres, fixed=TRUE)]=1

# short film film noir and superhero dont have any values thus can be removed
test2$`Film Noir` <-  NULL
test2$Superhero <- NULL
test2$`Short Film` <- NULL

#updated genresList
genresListActual <- c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime",
                      "Documentary", "Drama", "Family", "Fantasy", "History",
                      "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                      "Sport", "Thriller", "War", "Western")


#amount of genres per row
test2$GenreCount <- rowSums(test2[, genresListActual], na.rm=T)  

#remove genreslist for cor matrix
test3 <- test2
test3$genres <- NULL
#set all na to 0
test3[is.na(test3)] <- 0


#linear model genres to gross
lmGross <- lm(gross ~. -title_year -imdb_score -GenreCount , data=test3)



#take GrossCoefficients to plot the data for genres against gross
coefGross <- lmGross$coefficients
coefGrossmatrix <- as.matrix(coefGross)

#the intercept gross
(coefGrossintercept <- unname(coefGrossmatrix[1,]))
coefGrossmatrix <- coefGrossmatrix[-1,]

#convert to df to use in ggplot
coefGrossdf <- as.data.frame(coefGrossmatrix)
coefGrossdf <- rownames_to_column(coefGrossdf, "VALUE")

#rename to proper names
coefGrossdf <- coefGrossdf %>% rename(genre = VALUE )
coefGrossdf <- coefGrossdf %>% rename(gross = coefGrossmatrix )

#add percentage and calculated gross
coefGrossdf <- mutate(coefGrossdf, percentage = round((gross/coefGrossintercept) * 100, 2))
coefGrossdf <- mutate(coefGrossdf, calc_gross = (gross + coefGrossintercept)/10^6 )


#linear model for gross over the decades because doing it per year would result in way too many models and busy plots
lmGross1980 <- lm(gross ~. -title_year -imdb_score -GenreCount , data=filter(test3, between(test3$title_year, 1970, 1980)))
lmGross1990 <- lm(gross ~. -title_year -imdb_score -GenreCount , data=filter(test3, between(test3$title_year, 1980, 1990)))
lmGross2000 <- lm(gross ~. -title_year -imdb_score -GenreCount , data=filter(test3, between(test3$title_year, 1990, 2000)))
lmGross2010 <- lm(gross ~. -title_year -imdb_score -GenreCount , data=filter(test3, between(test3$title_year, 2000, 2010)))
lmGross2020 <- lm(gross ~. -title_year -imdb_score -GenreCount , data=filter(test3, between(test3$title_year, 2010, 2020)))

#coefficents for gross decades, there should be a better way to do this, lmao
coefGross1980 <- lmGross1980$coefficients
coefGross1990 <- lmGross1990$coefficients
coefGross2000 <- lmGross2000$coefficients
coefGross2010 <- lmGross2010$coefficients
coefGross2020 <- lmGross2020$coefficients

#matrixes for gross decades
coefGross1980matrix <- as.matrix(coefGross1980)
coefGross1990matrix <- as.matrix(coefGross1990)
coefGross2000matrix <- as.matrix(coefGross2000)
coefGross2010matrix <- as.matrix(coefGross2010)
coefGross2020matrix <- as.matrix(coefGross2020)

#the intercept gross
(coefGross1980intercept <- unname(coefGross1980matrix[1,]))
(coefGross1990intercept <- unname(coefGross1990matrix[1,]))
(coefGross2000intercept <- unname(coefGross2000matrix[1,]))
(coefGross2010intercept <- unname(coefGross2010matrix[1,]))
(coefGross2020intercept <- unname(coefGross2020matrix[1,]))

#remove unneeded headers
coefGross1980matrix <- coefGross1980matrix[-1,]
coefGross1990matrix <- coefGross1990matrix[-1,]
coefGross2000matrix <- coefGross2000matrix[-1,]
coefGross2010matrix <- coefGross2010matrix[-1,]
coefGross2020matrix <- coefGross2020matrix[-1,]

#convert to df to use in ggplot
coefGross1980df <- as.data.frame(coefGross1980matrix)
coefGross1990df <- as.data.frame(coefGross1990matrix)
coefGross2000df <- as.data.frame(coefGross2000matrix)
coefGross2010df <- as.data.frame(coefGross2010matrix)
coefGross2020df <- as.data.frame(coefGross2020matrix)

#make a better df out of it
coefGross1980df <- rownames_to_column(coefGross1980df, "genre")
coefGross1990df <- rownames_to_column(coefGross1990df, "genre")
coefGross2000df <- rownames_to_column(coefGross2000df, "genre")
coefGross2010df <- rownames_to_column(coefGross2010df, "genre")
coefGross2020df <- rownames_to_column(coefGross2020df, "genre")


#renaming
coefGross1980df <- coefGross1980df %>% rename(gross = coefGross1980matrix )
coefGross1990df <- coefGross1990df %>% rename(gross = coefGross1990matrix )
coefGross2000df <- coefGross2000df %>% rename(gross = coefGross2000matrix )
coefGross2010df <- coefGross2010df %>% rename(gross = coefGross2010matrix )
coefGross2020df <- coefGross2020df %>% rename(gross = coefGross2020matrix )


#add estimated gross
coefGross1980df <- mutate(coefGross1980df, calc_gross = (gross + coefGross1980intercept)/10^6 )
coefGross1990df <- mutate(coefGross1990df, calc_gross = (gross + coefGross1990intercept)/10^6 )
coefGross2000df <- mutate(coefGross2000df, calc_gross = (gross + coefGross2000intercept)/10^6 )
coefGross2010df <- mutate(coefGross2010df, calc_gross = (gross + coefGross2010intercept)/10^6 )
coefGross2020df <- mutate(coefGross2020df, calc_gross = (gross + coefGross2020intercept)/10^6 )

#add the decade
coefGross1980df <- mutate(coefGross1980df, decade = 1980)
coefGross1990df <- mutate(coefGross1990df, decade = 1990)
coefGross2000df <- mutate(coefGross2000df, decade = 2000)
coefGross2010df <- mutate(coefGross2010df, decade = 2010)
coefGross2020df <- mutate(coefGross2020df, decade = 2020)

#remove the unnecessary gross column
coefGross1980df$gross <- NULL 
coefGross1990df$gross <- NULL 
coefGross2000df$gross <- NULL 
coefGross2010df$gross <- NULL 
coefGross2020df$gross <- NULL 

#put them all into 1 df
coefGrossDecadesdf <- bind_rows(
  coefGross1980df, coefGross1990df, coefGross2000df, coefGross2010df, coefGross2020df)


#linear model genres to imdb score
lmScore <- lm(imdb_score ~. -gross -title_year -GenreCount, data=test3)

#take coefficients2 to plot the data for genres against imdb rating
coefScore <- lmScore$coefficients
coefScorematrix <- as.matrix(coefScore)

#the intercept imdb_score
(coefScoreintercept <- unname(coefScorematrix[1,]))
coefScorematrix <- coefScorematrix[-1,]

#convert to df for plots
coefScoredf <- as.data.frame(coefScorematrix)
coefScoredf <- rownames_to_column(coefScoredf, "VALUE")

coefScoredf <- coefScoredf %>% rename(genre = VALUE )
coefScoredf <- coefScoredf %>% rename(imdb_score = coefScorematrix )

#add calculated score
coefScoredf <- mutate(coefScoredf, calc_score = round(imdb_score + coefScoreintercept, 2))


#for the decades
lmScore1980 <- lm(imdb_score ~. -title_year -gross -GenreCount , data=filter(test3, between(test3$title_year, 1970, 1980)))
lmScore1990 <- lm(imdb_score ~. -title_year -gross -GenreCount , data=filter(test3, between(test3$title_year, 1980, 1990)))
lmScore2000 <- lm(imdb_score ~. -title_year -gross -GenreCount , data=filter(test3, between(test3$title_year, 1990, 2000)))
lmScore2010 <- lm(imdb_score ~. -title_year -gross -GenreCount , data=filter(test3, between(test3$title_year, 2000, 2010)))
lmScore2020 <- lm(imdb_score ~. -title_year -gross -GenreCount , data=filter(test3, between(test3$title_year, 2010, 2020)))

#coefficents for score decades
coefScore1980 <- lmScore1980$coefficients
coefScore1990 <- lmScore1990$coefficients
coefScore2000 <- lmScore2000$coefficients
coefScore2010 <- lmScore2010$coefficients
coefScore2020 <- lmScore2020$coefficients

#matrixes for score decades
coefScore1980matrix <- as.matrix(coefScore1980)
coefScore1990matrix <- as.matrix(coefScore1990)
coefScore2000matrix <- as.matrix(coefScore2000)
coefScore2010matrix <- as.matrix(coefScore2010)
coefScore2020matrix <- as.matrix(coefScore2020)

#the intercept score
(coefScore1980intercept <- unname(coefScore1980matrix[1,]))
(coefScore1990intercept <- unname(coefScore1990matrix[1,]))
(coefScore2000intercept <- unname(coefScore2000matrix[1,]))
(coefScore2010intercept <- unname(coefScore2010matrix[1,]))
(coefScore2020intercept <- unname(coefScore2020matrix[1,]))

#remove unneeded headers
coefScore1980matrix <- coefScore1980matrix[-1,]
coefScore1990matrix <- coefScore1990matrix[-1,]
coefScore2000matrix <- coefScore2000matrix[-1,]
coefScore2010matrix <- coefScore2010matrix[-1,]
coefScore2020matrix <- coefScore2020matrix[-1,]

#convert to df to use in ggplot
coefScore1980df <- as.data.frame(coefScore1980matrix)
coefScore1990df <- as.data.frame(coefScore1990matrix)
coefScore2000df <- as.data.frame(coefScore2000matrix)
coefScore2010df <- as.data.frame(coefScore2010matrix)
coefScore2020df <- as.data.frame(coefScore2020matrix)

#make a better df out of it
coefScore1980df <- rownames_to_column(coefScore1980df, "genre")
coefScore1990df <- rownames_to_column(coefScore1990df, "genre")
coefScore2000df <- rownames_to_column(coefScore2000df, "genre")
coefScore2010df <- rownames_to_column(coefScore2010df, "genre")
coefScore2020df <- rownames_to_column(coefScore2020df, "genre")


#renaming
coefScore1980df <- coefScore1980df %>% rename(Score = coefScore1980matrix )
coefScore1990df <- coefScore1990df %>% rename(Score = coefScore1990matrix )
coefScore2000df <- coefScore2000df %>% rename(Score = coefScore2000matrix )
coefScore2010df <- coefScore2010df %>% rename(Score = coefScore2010matrix )
coefScore2020df <- coefScore2020df %>% rename(Score = coefScore2020matrix )


#add estimated Score
coefScore1980df <- mutate(coefScore1980df, calc_Score = round((Score + coefScore1980intercept),2) )
coefScore1990df <- mutate(coefScore1990df, calc_Score = round((Score + coefScore1990intercept),2) )
coefScore2000df <- mutate(coefScore2000df, calc_Score = round((Score + coefScore2000intercept),2) )
coefScore2010df <- mutate(coefScore2010df, calc_Score = round((Score + coefScore2010intercept),2) )
coefScore2020df <- mutate(coefScore2020df, calc_Score = round((Score + coefScore2020intercept),2) )

#add the decade
coefScore1980df <- mutate(coefScore1980df, decade = 1980)
coefScore1990df <- mutate(coefScore1990df, decade = 1990)
coefScore2000df <- mutate(coefScore2000df, decade = 2000)
coefScore2010df <- mutate(coefScore2010df, decade = 2010)
coefScore2020df <- mutate(coefScore2020df, decade = 2020)

#remove the unnecessary Score column
coefScore1980df$Score <- NULL 
coefScore1990df$Score <- NULL 
coefScore2000df$Score <- NULL 
coefScore2010df$Score <- NULL 
coefScore2020df$Score <- NULL 

#put them all into 1 df
coefScoreDecadesdf <- bind_rows(
  coefScore1980df, coefScore1990df, coefScore2000df, coefScore2010df, coefScore2020df)
