# Library statements
library(tidyverse)
library(knitr)
library(kableExtra)
library(readxl)
library(ggplot2)
library(forcats)
library(viridis)

# Data Preparation

setwd("C:/Users/Quyen Le/Downloads")
dat = read_excel("Interaction Data.xlsx")

dat = dat %>%
  mutate(Teacher_ID = paste(`School Code`, Teacher, sep = '-'))

interactions = dat %>%
  group_by(Teacher_ID) %>%
  summarise(Interactions = n())

interactions = dat %>%
  group_by(Teacher_ID) %>%
  summarise(Interactions = n()) %>%
  left_join(dat[c(2, 4, 8)]) %>%
  distinct()

interactions = interactions[c(1, 3, 4, 2)]

school_interactions = interactions %>%
  group_by(School) %>%
  summarize(count = n())

grade_interactions = dat %>%
  group_by(Grade) %>%
  summarize(Interactions = n()) %>%
  drop_na() %>%
  arrange(desc(Interactions))

# Descriptive analytics

## Interactions with Schoolyard Gardens by Teacher (22-23)

# Create a kable table
top_10 = interactions %>%
  arrange(desc(Interactions)) %>%
  head(10)

kable(top_10, align = "lccc", col.names = c("", colnames(top_10)[-1]),
      caption = "Interactions with Schoolyard Gardens by Teacher (22-23)", 
      format = 'latex') %>%
  kable_styling(latex_options = "HOLD_position") %>%
  column_spec(1, width = "4cm", border_right = TRUE, border_left = TRUE) %>%
  column_spec(2, width = "3cm") %>%
  column_spec(3, width = "3cm") %>%
  column_spec(4, width = "3cm", border_right = TRUE) %>%
  row_spec(0,bold=TRUE)

## Interactions with Schoolyard Gardens by School (22-23)
ggplot(school_interactions, aes(School, count, fill = School)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("Burnley Moran" = "white",
                               "Clark" = magma(1),
                               "Greenbrier" = "white",
                               "Jackson" = viridis(n = 2),
                               "Johnson" = "white",
                               "Venable" = "white")) +
  labs(y = "Interaction Count") +
  guides(fill="none")

## Interactions with Schoolyard Gardens by Grade Level (22-23)
custom_colors <- c("Kindergarten" = "black",
                   "1st" = "gray10",
                   "2nd" = "gray20",
                   "3rd" = "gray40",
                   "4th" = "gray30",
                   "4th (SPED)" = "white",
                   "ECSE" = "gray80",
                   "Kindergarten 4" = "white",
                   "LEAP" = "white",
                   "LEAP Kindergarten 4" = "gray80",
                   "Pre-K" = "gray50",
                   "Pre-K 3" = "gray70",
                   "Pre-K 4" = "gray60")

ggplot(grade_interactions, aes(fct_reorder(Grade, Interactions), Interactions, fill = Grade)) +
  geom_col(color = "black") +
  guides(fill="none") + 
  coord_flip() +
  scale_fill_manual(values = custom_colors) +  # Manually assign colors
  labs(x = "Grade Level",
       y = "Interaction Count") 
