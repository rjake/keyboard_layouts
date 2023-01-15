setwd(dirname(.rs.api.getSourceEditorContext()$path))
file_location <- "../layouts/idobo_xd75/condensed_layout"
arrow_letters <- c("N", "E", "O", "U")

library(tidyverse)
library(jsonlite)
library(glue)

raw_json <- file.path(file_location, "layout.json") |> read_json()
crosswalk <- read_csv("key_crosswalk.csv")

keymap <-
  tibble(layout_key = raw_json$layers) |>
  mutate(
    layer = glue("Layer {i-1}", i = row_number())
  ) |>
  unnest_longer(layout_key) |>
  group_by(layer) |>
  mutate(
    key_id = row_number() - 1,
    x = key_id %% 15,
    y = rev(cumsum(x == 0))
  ) |>
  ungroup() |>
  left_join(crosswalk) |>
  print(n = 20)


colors <-
  c(
    "white" = "grey90",
    "empty" = "grey30",
    "grey" = "grey60",
    "red" = "red"
  )

keymap |>
  mutate(
    size = case_when(
      nchar(label) == 1 ~ 3,
      TRUE ~ 1
    ),
    label = str_replace(label, " ", "\n"),
    color =
      ifelse(label %in% arrow_letters, "grey", color) |>
      recode(!!!colors)
  ) |>
  ggplot(aes(x, y)) +
  facet_wrap(~layer, ncol = 2) +
  geom_tile(aes(fill = color), color = "grey30") +
  geom_text(
    #data = ~filter(.x, key_type != "empty"),
    aes(label = label, size = size), fontface = "bold"#, size = 1.5
  ) +
  geom_point(
    data = ~filter(.x, layout_key == "KC_TRNS"),
    pch = 25, color = "grey70", fill = "grey80", size = 1
  ) +
  scale_size(range = c(1.5, 2)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    panel.background = element_rect(color = "grey90", fill = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    aspect.ratio = 0.3
  )

file.path(file_location, "layers.png") |>
  ggsave(width = 8, height = 5)
