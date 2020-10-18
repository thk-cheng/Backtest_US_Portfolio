#Demonstration for the pipe operator %>%

library(magrittr)

head(mtcars)
class(mtcars)

car_data <- 
  mtcars %>%
  subset(hp > 100) %>%
  aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
  transform(kpl = mpg %>% multiply_by(0.4251)) %>%
  print

?aggregate
?transform
?multiply_by
car_data