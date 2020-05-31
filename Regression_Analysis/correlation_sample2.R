Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)



Teams %>% filter(yearID %in% 1961:2001 )%>% mutate(win_rate = win/G, error_rate = E/G)%>% summarize(r = cor(AB_per_game, R_per_game)) %>% pull(r)