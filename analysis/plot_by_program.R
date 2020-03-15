setwd("~/GitHub/keyboard_layouts/idobo_xd75/workman")

library(tidyverse)
library(jsonlite)

layer_map <- 
  tibble(
    layout_key = fromJSON("workman_layout.json")$layers[1,],
    x = rep(1:15, 5),
    y = rep(5:1, each = 15)
  ) %>% 
  mutate(
    layout_key = ifelse(x == 7 & y == 1, "KC_LCTL", layout_key)
  ) %>% 
  filter(!(x == 9 & y == 4))


key_map <- 
  read_csv("key_crosswalk.csv") %>% 
  separate_rows(key, sep = " ")

log_file <- "~/GitHub/keyboard_layouts/analysis/key_log.txt"

raw_data <- 
  read_fwf(
    log_file, 
    col_positions = fwf_cols(
      time = c(1, 23),
      key_full = c(24, 43),
      window_full = c(44, 300)
    )
  ) 

key_log <-
  raw_data %>% 
  filter(key_full != lag(key_full)) %>% 
  mutate(
    window = str_extract(window_full, "Chrome|Slack|Spyder|RStudio|Excel"),
    key = 
      tolower(key_full) %>% 
      str_replace("^.(.).$", "\\1") %>% 
      str_replace("key.shift", "key.shift_right") %>% 
      str_replace("<77>", "m") %>% # happens with ctrl + shift
      str_replace("x01", "a") %>% 
      str_replace("x03", "c") %>%
      str_replace("x13", "s") %>%
      str_replace("x16", "v") %>% 
      str_replace("x19", "y") %>% 
      str_replace("x1a", "z")  
  ) %>% 
  left_join(key_map) %>% 
  left_join(layer_map)


count(key_log, window_full, sort = T)
count(key_log, window, sort = T)


window_totals <-
  key_log %>% 
  add_count(window, name = "window_n") %>% 
  count(window, window_n, layout_key, color, x, y) %>% 
  mutate(pct = (n/window_n * 100)) 

  
window_totals %>% 
  filter(window_n > 1000) %>% 
  drop_na() %>% 
  ggplot(aes(factor(x), factor(y), alpha = pct)) +
  geom_point(aes(color = color, size = pct)) +
  facet_wrap(~window, ncol = 1) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    panel.background = element_rect(color = "grey90", fill = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    aspect.ratio = 0.25
  )
