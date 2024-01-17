library(tidyverse)
fav_color1 = c("Purple","Red","Gold","Maroon","Pink","Green","Pink","Blue","Purple")
sleep_time1 = c(6,9,7,7,7,6,8,6,7)
marvel_movies1 = c("No","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No")
hours_study1= c(2,1,2,2,3,1,1,2,0)

math2442 <- tibble(fav_color = fav_color1,
                       sleep_time = sleep_time1,
                       marvel_movies = marvel_movies1,
                       hours_study = hours_study1)

#Tibble give you size of table and type of variable you have. (in comparison to a data frame)

#Pipe is %>%

f <- function(x){x^2}
g <- function(x){log(x)}

exp(4) %>% g() %>% f()

#Test 2 Vector Correction
digits <- 1:9
prob_dig <- log10(1+1/digits) #This is the Vector based programming

Test2Q4 <- function(noOfSim){
  outcome <- sample(1:9,noOfSim,replace = T, prob = c(.3,.1,.1,.3,.2,0,0,0,0))
  df <- data.frame(Prob1 = sum(outcome == "1")/noOfSim,
                   Prob2 = sum(outcome == "2")/noOfSim,
                   Prob3 = sum(outcome == "3")/noOfSim,
                   Prob4 = sum(outcome == "4")/noOfSim,
                   Prob5 = sum(outcome == "5")/noOfSim,
                   Prob6 = sum(outcome == "6")/noOfSim,
                   Prob7 = sum(outcome == "7")/noOfSim,
                   Prob8 = sum(outcome == "8")/noOfSim,
                   Prob9 = sum(outcome == "9")/noOfSim
  )
  return(df)
}
#Evaluation
print(round(Test2Q4(10),3))
print(round(Test2Q4(10000),3))


#Find the mean of the square root of the vector below.
array <- c(14,16,19,20,36,81)

array %>% sqrt() %>% mean()

#Arrange by hours of study in descending order

math2442 %>% arrange(desc(hours_study))

#Group and the tibble using fav_color
math2442 %>%
  group_by(fav_color)

#Group summarize and the tibble using fav_color
#counting the number of obs(students) for each category of fav_color
math2442 %>%
  group_by(fav_color) %>%
  summarise(n = n())

#Frequenct of students in each category if marvel_movies
math2442 %>%
  group_by(marvel_movies) %>%
  summarise(n = n())

#or use count
math2442 %>%
  count(marvel_movies)

#Check if students who watch marvel movies sleep less or more than those who dont.

math2442 %>%
    group_by(marvel_movies) %>%
    summarise(frequency = n(),
              average = mean(sleep_time))

#Center of variable = {mean,median}
#spread of variable = {sd}
#shape of the variable = {histogram, boxplot}
# using the $ turns it into an array instead of a tibble.
sleep <- math2442 $sleep_time

#center of variable sleep time
mean(sleep)
