library(dplyr)
library(gapminder)
library(gganimate)

datos2 <- gapminder %>%
  group_by(year) %>%
  arrange(year, desc(gdpPercap)) %>%
  slice(1:15) %>%
  ungroup()

animacion <- datos2 %>%
  ggplot(aes(x = reorder(country, gdpPercap), y = gdpPercap, fill = country)) +
  geom_col() +
  geom_text(aes(label = round(gdpPercap, 2)), position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 0, label = country), hjust = 1.1) +
  geom_text(aes(x = 15, y = max(gdpPercap), label = as.factor(year)), vjust = 0.2, alpha = 0.5, col = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(1, 4, 1, 3, "cm")
  ) +
  transition_states(year, transition_length = 2) +
  enter_fade() +
  exit_fade() +
  ease_aes('quadratic-in-out') 

animate(animacion, width = 700, height = 432, fps = 5, duration = 30, rewind = FALSE)

# ... (Código anterior)

# Grabar animación como un archivo GIF
anim_save("img/animacion.gif", animate(animacion, width = 700, height = 432, fps = 5, duration = 30, rewind = FALSE))
