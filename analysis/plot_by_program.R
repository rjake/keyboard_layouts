library(tidyverse)
library(lubridate)
library(jsonlite)

layer_map <-
  tibble(
    layout_key_raw = fromJSON(
      "../layouts/idobo_xd75/workman/workman_layout.json"
    )$layers[1,],
    x = rep(1:15, 5),
    y = rep(5:1, each = 15)
  ) %>%
  mutate(
    layout_key = str_remove_all(layout_key_raw, "(L|C_S_T).*\\(|.*,|\\)")
  )


a <-
  tibble(dec = 1:255) %>%
  mutate(
    char = rawToChar(as.raw(coderange), multiple = TRUE),
    hex = as.raw(dec)
  )

# CTRL + ALT + C = <67> (Decimal)
# CTRL + ALT + U = <85> (Decimal)

key_map <-
  read_csv("key_crosswalk.csv") %>%
  separate_rows(key, sep = " ")

log_file <- "key_log.txt"

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
  mutate(window_full = gsub('[^ -~]', '', window_full)) %>%
  mutate(
    date_time =
      ymd_hms(str_sub(time, 1, 19)),
    seconds = date_time + milliseconds(as.integer(str_sub(time, 21, 23))),
    diff = as.numeric(seconds - lag(seconds, default = first(seconds))),
    same_value = key_full == lag(key_full, default = "")
  ) %>%
  filter(!(same_value & diff < 0.04)) %>%
  mutate(
    window =
      tolower(window_full) %>%
      str_replace("- (message|meeting|appointment)", "outlook") %>%
      str_replace("acpr", "epic") %>%
      str_replace("ppt", "powerpoint") %>%
      str_replace("\\\\remote", "epic") %>%
      # str_replace("", "") %>%
      # str_replace("", "")
      str_extract(
          "chrome|slack|spyder|rstudio|excel|github|aginity|outlook|powerpoint|epic|word$"
    ),
    key =
      tolower(key_full) %>%
      str_replace_all("'|- ", "") %>%
      #str_replace("key.shift", "key.shift_right") %>%
      str_replace("<77>", "m") %>% # happens with ctrl + shift
      str_replace("x01", "a") %>%
      str_replace("x03", "c") %>%
      str_replace("x13", "s") %>%
      str_replace("x16", "v") %>%
      str_replace("x19", "y") %>%
      str_replace("x1a", "z") %>%
      str_replace("x0c", "l") %>%
      str_replace("x12", "r")
  ) %>%
  left_join(key_map) %>%
  left_join(layer_map)


count(key_log, window, sort = T)

key_log %>%
  filter(is.na(window)) %>%
  count(window_full, sort = TRUE) %>%
  print(n = Inf)


window_totals <-
  key_log %>%
  drop_na() %>%
  add_count(window, name = "window_n") %>%
  filter(window_n > 1000) %>%
  count(window, window_n, layout_key, color, x, y) %>%
  mutate(pct = (n/window_n * 100)) %>%
  group_by(layout_key) %>%
  mutate(mean = mean(pct)) %>%
  ungroup() %>%
  mutate(diff = pct - mean,
         #diff = ifelse(diff > 5, 5, diff)
         )


key_log %>%
  drop_na() %>%
  filter(as.Date(date_time) == today()) %>%
  ggplot(aes(factor(x), factor(y))) +
  geom_count(aes(color = color)) +
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


window_totals %>%
  ggplot(aes(factor(x), factor(y), alpha = pct)) +
  geom_point(aes(color = color, size = pct)) +
  facet_wrap(~window) +
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

window_totals %>%
  filter(diff > 0) %>%
  ggplot(aes(factor(x), factor(y))) +
  geom_tile(aes(fill = color, alpha = pct)) +
  facet_wrap(~window, ncol = 2) +
  scale_fill_identity() +
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


