library(tidyverse)
library(ggalluvial)

mayor_prefs <- extract_mayor_preference_data("pdfs/Mayoral_Final_Results_2021.pdf")

mayor_prefs_graph <- mayor_prefs %>%
  filter(first_vote != "Shaun BAILEY" & first_vote != "Sadiq Aman KHAN") %>%
  mutate(
    second_vote_group = fct_lump_n(second_vote, n = 2, w = votes),
  )

ggplot(mayor_prefs_graph,
       aes(y = votes, axis1 = first_vote, axis2 = second_vote_group)) +
  geom_alluvium(aes(fill = first_vote)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))


vote_bins <- mayor_prefs %>%
  filter(first_vote == "Count BINFACE") %>%
  mutate(second_vote = fct_reorder(second_vote, -votes),
         percent = votes / sum(votes),
         label = paste(second_vote, 
                       scales::comma(votes, accuracy = 1), 
                       scales::percent(percent, accuracy = 1),
                       sep = "; "),
         direction = votes > 20000,
         label_y = if_else(direction, 500, votes + 500))

ggplot(vote_bins, aes(y = votes, x = second_vote)) +
  geom_col(aes(fill = second_vote), show.legend = FALSE) +
  geom_text(aes(label = label, y = label_y), angle = 90, size = 3, hjust = 0) +
  theme_void() +
  labs(title = "Count BINFACE second preferences")
