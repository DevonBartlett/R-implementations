library(tidyverse)
#a tibble tells the size and tells whether the data is 
# characters or "double" which is numbers
fav_color1 <- c("purple", "red", "gold", "maroon", 
                "pink", "green", "pink", "blue", "purple")
sleep_time1 <- c(6, 9, 7, 7, 7, 6, 8, 6, 7)
marvel_movies1 <- c("no", "yes", "yes", "yes", "yes", 
                    "yes", "yes", "yes", "no")
hours_study1 <- c(2, 1, 2, 2, 3, 1, 1, 2, 0)

math2441 <- tibble(fav_color = fav_color1,
                   sleep_time = sleep_time1,
                   marvel_movies = marvel_movies1,
                   hours_study = hours_study1)

#Piping
#   f(x) = x^2    g(x) = ln(x)
#       f(g(e^4)) = (ln(e^4))^2 = 16
#     e^4   % > %   g()   % > %   f()   =   16

#Pipelines with 2 arguments
f <- function(w,v){w^2+v^2}
g <- function(x,y){log(x)+log(y)}

exp(2) %>% g(exp(5)) %>% f(2)

#Pipelines with 2 arguments
h <- function(w,v){sqrt(w+v)}
k <- function(z,r){z^2+log(r)}

#Problem 1 k(h(2,2),1)
2 %>% h(2) %>% k(1)

#Problem 2 h(k(2,1/2),3)

2 %>% k(1/2) %>% h(3)

#Using default value of function
f <- function(w,v=2){w^2+v^2}
g <- function(x,y){log(x)+log(y)}

#f(g(exp(2), exp(5),2)
exp(2) %>% g(exp(5)) %>% f()

sleep <- math2441$sleep_time
#center of variable sleeptime
mean(sleep)

median(sleep)

sd(sleep)
#standard deviation measures how much the obs are spread about the center.

#Shape of variable
library(ggplot2)
ggplot(math2441, aes(x=sleep_time1)) +
  geom_histogram(bins = 3)

ggplot(math2441, aes(x=1, y=sleep_time1)) +
  geom_boxplot() +
  coord_flip()

ggplot(math2441, aes(x=marvel_movies1)) +
  geom_bar(fill = "purple")

#---------------------------------------------------------

#Both variables are continuous
ggplot(mpg, aes(x=cty, y=hwy, color = fl)) +
  geom_point(position = "jitter")

ggplot(mpg, aes(x=cty, y=hwy)) +
  geom_point(position = "jitter", alpha = 0.7)

#Both variables are categorical

ggplot(mpg, aes(x=fl, y=class)) +
  geom_count()

mpg %>%
  count(fl,class)

#---------------------------------------------------------

#One variable is categorical, the other is continuous.
#cty vs fl
ggplot(mpg, aes(x=fl ,y=cty)) +
  geom_boxplot() +
  coord_flip()

mpg %>%
  count(fl)

