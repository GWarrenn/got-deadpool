## Author: August Warren
## Description: Analysis of DC Stop and Frisk Data
## Date: 11/25/2018
## Status: Published
## Specs: R version 3.3.2 (2016-10-31)

library(tidyverse)
library(googlesheets)
library(curl)
library(reshape2)
library(ggbeeswarm)
library(scales)

gs_ls()

got_doc <- gs_title("GoT Battle of Winterfell Deadpool (Responses)")

gs_ws_ls(got_doc)

deadpool_data <- gs_read(ss = got_doc)

columns <- colnames(deadpool_data)

columns <- sub(x=columns,pattern = "What will become of the following characters\\? \\[(.*)\\]",
               replacement = "\\1")

colnames(deadpool_data) <- columns

characters <- c("Email Address","Jon Snow","Sansa Stark","Arya Stark","Dany Targaryen","Tyrion Lannister","Jamie Lannister",
                "Bran Stark","Ser Brienne of Tarth","Podrick Payne","Tormund Giantsbane","Davos Seaworth","Greyworm",
                "Missandei","Sam Tarly","Gilly","Sandor Clegane -- The Hound" ,"Jorah Mormont","Lianna Mormont","Gendry",
                "Lord Varys","Theon Greyjoy","Drogon","Rhaegal","Viserion","Eddison Tollett","Beric Dondarrion","Cersei Lannister",
                "The girl who reminded us all of Shireen :(","Night King")

predictions <- deadpool_data %>%
  select(characters) %>%
  rename(email = `Email Address`)

predictions_long <- melt(predictions,id.vars = "email")

predictions_long$value <- gsub(x=predictions_long$value,pattern = "(Live|Die).*",replacement = "\\1")

live_die_pct <- predictions_long %>%
  group_by(email,value) %>%
  summarise(n=n()) %>%
  mutate(percent = n/29)

live_die_pct_w <- dcast(data = live_die_pct,formula = email ~ value,value.var = c("percent"))

rip <- c("Jorah Mormont","Lianna Mormont","Theon Greyjoy","Viserion","Eddison Tollett","Night King","Beric Dondarrion")

predictions_long <- predictions_long %>%
  mutate(actual = ifelse(variable %in% rip,"Die","Live"),
         outcome = ifelse(actual == value,"Correct","Incorrect"))

outcomes <- predictions_long %>%
  group_by(email,outcome) %>%
  summarise(n=n()) %>%
  mutate(percent=n/29)

outcomes_w <- dcast(data = outcomes,formula = email ~ outcome,value.var = c("percent"))

character_outcomes <- predictions_long %>%
  group_by(variable,outcome) %>%
  summarise(n=n()) %>%
  mutate(percent=n/34)

character_outcomes_w <- dcast(data = character_outcomes,formula = variable ~ outcome,value.var = c("percent"))

character_outcomes_w <- character_outcomes_w %>%
  mutate(actual = ifelse(variable %in% rip,"Die","Live"))

character_outcomes_plot <- ggplot(character_outcomes_w,aes(x=reorder(variable,-Correct),y=Correct,fill=actual)) +
  geom_bar(stat="identity",color="black") +
  geom_text(aes(reorder(variable,-Correct),y=Correct,label=percent(round(Correct,2))),hjust=-.25) +
  theme(axis.title.y = element_blank(),legend.position = "bottom") +
  scale_fill_manual(values = c("#ff9197","#90f98e")) +
  scale_y_continuous(labels = percent,expand = c(0,0),limits = c(0,1.05)) +
  labs(y="% Fate Correctly Predicted",fill="Actual Fate",
       title="GoT Deadpool: % Correctly Predicted",
       caption="Data from 35 Game of Thrones Battle of Winterfell Deadpool Predictions | Graph by August Warren") +
  coord_flip() 

ggsave(plot = character_outcomes_plot, "C:\\users\\augus\\desktop\\character_outcomes.png", w = 10.67, h = 8,type = "cairo-png")

################################################################
##
## comparison of predicted % dead vs. % correct  
##
################################################################

actual_dead <- round(7/29,digits = 2)

outcomes_w_live_die <- merge(outcomes_w,live_die_pct_w,by="email")

model <- lm(data = outcomes_w_live_die,formula = Correct ~ Die)

outcomes_w_live_die_plot <- ggplot(outcomes_w_live_die,aes(x=Die,y=Correct)) +
  ##geom_point(size=4,alpha=.3) +
  geom_beeswarm(size=4,cex=1,groupOnX=FALSE,alpha=.75,color="#DC143C") +
  geom_smooth(method = "lm",size=2) +
  geom_vline(xintercept = actual_dead) +
  scale_x_continuous(labels=percent,limits = c(0,1)) +
  scale_y_continuous(labels=percent,limits = c(0,1)) +
  geom_text(aes(.85,0,label = paste("R-Squared: ",as.character(round(summary(model)$r.squared,3))), hjust = 0)) +
  geom_text(aes(actual_dead,0,label = paste("Actual % Dead: ",as.character(percent(actual_dead))), hjust = 0)) +
  labs(y="Percent of Fates Correctly Predicted",
       x="Percent of Characters Predicted to Die")
       ##title="GoT Deadpool: % of Characters Predicted to Die vs. % Correctly Predicted",
       ##caption="Data from Game of Thrones Battle of Winterfell Deadpool Predictions | Graph by August Warren")

################################################################
##
## comparison of predicted % dead vs. actual  
##
################################################################

dead_vs_actual_hist_plot <- ggplot(outcomes_w_live_die,aes(x=Die)) +
  geom_histogram(binwidth = .1,color="black",fill="#DC143C") +
  geom_vline(xintercept = actual_dead) +
  geom_vline(xintercept = mean(outcomes_w_live_die$Die)) +
  geom_text(aes(mean(outcomes_w_live_die$Die),11,label = paste("Avg. Predicted \n% Dead: ",as.character(percent(round(mean(outcomes_w_live_die$Die),2)))), hjust = 0)) +
  geom_text(aes(actual_dead,11,label = paste("Actual \n% Dead: ",as.character(percent(actual_dead))), hjust = 1)) +
  scale_x_continuous(labels=percent) +
  labs(x="Percent of Character's Predicted to Die",
       y="Frequency")
       ##title="GoT Deadpool: % of Characters Predicted to Die vs. % Correctly Predicted",
       ##caption="Data from Game of Thrones Battle of Winterfell Deadpool Predictions | Graph by August Warren")

fates_plots <- grid.arrange(outcomes_w_live_die_plot, dead_vs_actual_hist_plot, nrow = 1,
                            top="GoT Deadpool: % of Characters Predicted to Die vs. % Correctly Predicted",  
                            bottom = textGrob(
                              "Data from 34 Game of Thrones Battle of Winterfell Deadpool Predictions | Graph by August Warren",
                              gp = gpar(fontface = 3, fontsize = 9),
                              hjust = 1,
                              x = 1
                            )
                          )

ggsave(plot = fates_plots, "C:\\users\\augus\\desktop\\fates_plots.png", w = 12, h = 8,type = "cairo-png")

################################################################
##
## % correct by demographics
##
################################################################

demos <- deadpool_data %>%
  select(`Email Address`,`To which gender do you most closely identify?`,`Have you read any of the books (Song of Ice and Fire)?`) %>%
  rename(gender = `To which gender do you most closely identify?`,
         books = `Have you read any of the books (Song of Ice and Fire)?`,
         email = `Email Address`)

demos$gender <- ifelse(is.na(demos$gender),"Female",demos$gender)
demos$books <- ifelse(is.na(demos$books),"Yes",demos$books)

demos_predictions <- merge(predictions_long,demos,by="email")

gender <- demos_predictions %>%
  group_by(gender,outcome) %>%
  summarise(n=n()) %>%
  mutate(percent=n/sum(n)) %>%
  filter(gender!= "Two of us: one male one female") %>% ##goddamnit mika and chris 
  rename(subgroup = gender)

gender$demo <- "Gender"

books <- demos_predictions %>%
  group_by(books,outcome) %>%
  summarise(n=n()) %>%
  mutate(percent=n/sum(n)) %>%
  rename(subgroup = books)

books$demo <- "Read the Books"

combined_demos <- rbind(gender,books)

combined_demos$subgroup <- factor(combined_demos$subgroup,levels = c("Yes","No","Male","Female"))

demo_plot <- ggplot(combined_demos,aes(x=subgroup,y=percent,fill=outcome)) +
  geom_bar(position=position_dodge(),stat="identity",color="black") +
  geom_text(aes(label = percent(round(percent,2))),position = position_dodge(width = 1),vjust=-.5) +
  facet_wrap(~demo,scales = "free") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("#90f98e","#ff9197")) +
  theme(axis.title.x = element_blank(),legend.position = "bottom") +
  labs(y="Percent of Fates Correctly Predicted",
       title="GoT Deadpool: % of Fates Predicted by Demographics",
       fill="Prediction",
       caption="Data from 34 Game of Thrones Battle of Winterfell Deadpool Predictions | Graph by August Warren")

ggsave(plot = demo_plot, "C:\\users\\augus\\desktop\\demo_plot.png", w = 10.67, h = 8,type = "cairo-png")

