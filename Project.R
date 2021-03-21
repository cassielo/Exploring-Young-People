library(ggplot2)
response <- read.csv(file.choose(), na.strings='')
response <- na.omit(response)
list<- c("Opera", "Romantic", "Shopping", "Spiders", "Life.struggles", "Age", 
         "Gender", "Left...right.handed", "Only.child","Village...town",
         "House...block.of.flats")
summary(response[list])

#analyses + viz------------------------------------------

#1
m1 <- lm(Age~Opera,response)
summary(m1)

temp <- aggregate(Opera ~ Age, response, mean)
ggplot(temp, aes(x =Age, y = Opera)) + geom_point() +
  geom_smooth(method = "lm")

# There was a significant (p < 0.05) relationship between age and  the enjoyment 
# of opera. The relationship between  age and the enjoyment of opera is positive. 
# As age goes on, the level of how they enjoy opera goes up. So older people tend 
# to enjoy opera more than younger people.

#2
m2 <- aov(Spiders~Gender, data = response)
summary(m2)

ggplot(response, aes(x = Gender, y = Spiders)) +
  geom_boxplot(aes(fill = Gender))

# There was a statistically significant relationship between gender and the fears
# of spider(p<0.05). Female had a significantly higher fears of spider compared to 
# male, which really surprised me.


#3
life1 <- response[response$Left...right.handed == "left handed","Life.struggles"]
life2 <- response[response$Left...right.handed == "right handed","Life.struggles"]
t.test(life1,life2,var.equal = T)

ggplot(response, aes(x = Left...right.handed, y = Life.struggles)) +
  geom_boxplot(aes(fill = Left...right.handed)) + xlab("Left or right handed") + ylab("Life struggle")+
  labs(fill = "Left or right handed")

# The result shows that there was no significant difference of life struggles between 
# two groups of people(p>0.05). In contrast, the distribution of life struggle level 
# was really similar in tow groups.


#4
m4 <- glm(Gender ~ Shopping + Romantic, data = response,
          family = "binomial")
summary(m4)

predict <- predict(m4, type = 'response')
table(response$Gender, predict > 0.5)

ggplot(response, aes(Shopping, Romantic)) +
  geom_jitter(aes(color = Gender))

#         FALSE TRUE
# FALSE    335   67
# TRUE     123   149
# Overall, this model performed relatively well. Using the 
# interest in shopping and romantic movies, the model was able 
# to predict 484 out of 674 genders in the dataset correctly. 
# The accuracy is about 70%, and it seems to have higher accuracy 
# in female than male.


#5
t <- xtabs(~Only.child+Village...town, data = response)
chisq.test(t)

ggplot(response, aes(Village...town, fill = House...block.of.flats)) +
  geom_bar() + facet_grid(~Only.child) + xlab("City or village") + 
  labs(fill = "House or Block of flats") + ggtitle("Only child vs City or Village")

# Being an only child is not significantly related to spending most 
# of childhood time in city or village. However, the bar chart tells
# that people spending most of childhood time in city tend to live 
# most of childhood in a block of flats more than house or bungalow, 
# and people spending most of childhood time in village tend to live 
# most of childhood in house or bungalow more than a block of flats, 
# which makes sense.


