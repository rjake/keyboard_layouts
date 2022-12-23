setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(jsonlite)
library(glue)

raw_json <- read_json("workman_layout.json")
crosswalk <- read_csv("../../../analysis/key_crosswalk.csv")

keymap <-
  tibble(layout_key = raw_json$layers) |>
  mutate(
    layer = glue("layer_{i-1}", i = row_number())
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

#keymap |> distinct(layout_key, color) |> filter(is.na(color))

keymap |>
  ggplot(aes(x, y)) +
  facet_wrap(~layer, ncol = 2) +
  geom_tile(aes(fill = color), color = "white") +
  geom_text(aes(label = label), size = 1) +
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


# not using ----
#
#
#
key_names <-
  set_names(
    x = crosswalk$label,
    nm = glue("\\b{crosswalk$layout_key}\\b")
  )

keymap[1] |>
  unlist() |>
  matrix(byrow = TRUE, nrow = 5)




recode_keys <- function(x) {
  # x <- "LT(2,KC_A)"
  x |>
    str_replace_all("[\\(\\)]", " ") |>
    str_replace_all("^LT.(\\d),(\\w+).*", "\\2\n(layer \\1)") |>
    #                    -----  -------
  str_replace_all(key_names)
}
