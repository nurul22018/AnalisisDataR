#1. import file stroke

library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)

stroke <- fread("http://www.statsci.org/data/oz/stroke.txt")


#mengubah data dari wide menjadi long
stroke_long = stroke%>% select(c(1:6,39:46))%>%
  pivot_longer(cols=Bart1:Bart8,
               names_to = "Time",
               names_prefix = "Bart",
               values_to = "Ability")


#2. membuat grafik per individu
stroke_graph <- stroke_long %>%
  mutate(label = if_else(Time == max(Time), as.character(Subject), NA_character_)) %>%
  ggplot(aes(x = Time, y = Ability, group = Subject, colour = Subject)) + geom_line()
stroke_graph + labs(title = "Individual Barthel Index Score", x="Week", y="Barthel Index Score")


#3. membuat grafik nilai rata-rata
#rerata:
stroke_new <- stroke_long
stroke_new <- stroke_long %>%
  group_by(Group, Time) %>%
  mutate(BSImean = mean(Ability)) %>%
  as.data.frame()


#grafik:
stroke_group <- stroke_new %>%
  mutate(label = if_else(Time == max(Time), as.character(Group), NA_character_)) %>%
  ggplot(aes(x = Time, y = BSImean, group = Group, colour = Group)) + geom_line()
stroke_group + labs(title = "Barthel Index Score by Group", x="Week", y="Barthel Index Score")


#4. membuat matrix scatterplot dari nilai BSI antar waktu/pekan
stroke_wide = stroke_long %>%
  pivot_wider(names_from = "Time",
              names_prefix = "Week",
              values_from = "Ability")

pairs(stroke_wide[, 7:14], pch=19)


#5. Menghitung dan membuat tabel silang koefisien korelasi BSI antar pekan
cor(stroke_wide$Week1, stroke_wide$Week2)
cor(stroke_wide$Week1, stroke_wide$Week3)
cor(stroke_wide$Week1, stroke_wide$Week4)
cor(stroke_wide$Week1, stroke_wide$Week5)
cor(stroke_wide$Week1, stroke_wide$Week6)
cor(stroke_wide$Week1, stroke_wide$Week7)
cor(stroke_wide$Week1, stroke_wide$Week8)

cor(stroke_wide$Week2, stroke_wide$Week3)
cor(stroke_wide$Week2, stroke_wide$Week4)
cor(stroke_wide$Week2, stroke_wide$Week5)
cor(stroke_wide$Week2, stroke_wide$Week6)
cor(stroke_wide$Week2, stroke_wide$Week7)
cor(stroke_wide$Week2, stroke_wide$Week8)

cor(stroke_wide$Week3, stroke_wide$Week4)
cor(stroke_wide$Week3, stroke_wide$Week5)
cor(stroke_wide$Week3, stroke_wide$Week6)
cor(stroke_wide$Week3, stroke_wide$Week7)
cor(stroke_wide$Week3, stroke_wide$Week8)

cor(stroke_wide$Week4, stroke_wide$Week5)
cor(stroke_wide$Week4, stroke_wide$Week6)
cor(stroke_wide$Week4, stroke_wide$Week7)
cor(stroke_wide$Week4, stroke_wide$Week8)

cor(stroke_wide$Week5, stroke_wide$Week6)
cor(stroke_wide$Week5, stroke_wide$Week7)
cor(stroke_wide$Week5, stroke_wide$Week8)

cor(stroke_wide$Week6, stroke_wide$Week7)
cor(stroke_wide$Week6, stroke_wide$Week8)

cor(stroke_wide$Week7, stroke_wide$Week8)


#7. Menghitung intercept dan slope beserta SD nya masing-masing dari hubungan BSI dengan pekan, dipresentasikan dalam tabel
summary(fit1 <- (lmList(Ability ~ Time | Subject, data = stroke_new)))






