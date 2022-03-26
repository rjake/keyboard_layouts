library(RSQLite)
library(tidyverse)
library(jsonlite)

# https://github.com/gabfv/logitech-g-hub-settings-extractor/blob/main/ghub-settings.py

# copy file
db_path <-
  file.path(
    Sys.getenv("LOCALAPPDATA"),
    "LGHUB",
    "settings.db"
  )

file.copy(
  from = db_path, 
  to = "~/github/keyboard_layouts/layouts/mouse/logitech/g502/settings.db",
  overwrite = TRUE
)

db <-
  dbConnect(
    dbDriver("SQLite"),
    dbname = db_path
  )


dbListTables(db)

res <- dbReadTable(db, "data")

contents <- res$file[[1]] |> rawToChar()

settings <-
  fromJSON(contents, flatten = TRUE)

settings_df <- 
  settings$profiles |> 
  map_dfr(flatten) |> 
  as_tibble()

# commands <-
#   settings$applications$applications$commands |> 
#   flatten_dfr() |> 
#   rename(command = name) |> 
#   print()

commands <-
  data.table::fread(
    "cardId, command
    0f82f693-5b78-4cf5-867e-010601000000, copy-qwerty 
    0f82f693-5b78-4cf5-867e-011901000000, paste-qwerty
    0f82f693-5b78-4cf5-867e-014b00000000, page-up
    0f82f693-5b78-4cf5-867e-014e00000000, page-down
    0f82f693-5b78-4cf5-867e-020100000000, left-click
    0f82f693-5b78-4cf5-867e-020200000000, right-click
    0f82f693-5b78-4cf5-867e-020300000000, middle-click
    0f82f693-5b78-4cf5-867e-040800000000, double-click
    0f82f693-5b78-4cf5-867e-040b00000000, scroll-left
    0f82f693-5b78-4cf5-867e-040c00000000, scroll-right
    0f82f693-5b78-4cf5-867e-090100000000, profile-cycle
    0f82f693-5b78-4cf5-867e-090500000000, g-shift
    0f82f693-5b78-4cf5-867e-090700000000, NA
    078150c0-432c-48d0-891e-95f9d2f5325a, copy-workman
    1c703851-de84-48ed-94a2-26b5dd8d0a5b, arrow-right
    362bc208-51cd-4458-b507-35e361d94543, volume-up-c
    5d0f589a-889e-4d4d-9e74-bfc10f714f48, volume-down-c
    b311afbb-3051-47a2-9380-1328943a0378, profile-workman
    b813a90c-baa3-4618-80f3-5f3cca46b6da, paste-workman
    ccb69c61-5795-47b1-aba5-338f0a0a1153, profile-qwerty
    e51eb733-91b0-4e54-bcea-864bb4a4540c, print-screen
    "
  ) |> 
  as_tibble() |> 
  print()


locations <-
  data.table::fread(
    "id, x, y
    1, 5, 5
    2, 7, 5
    3, 6, 4.5
    4, 2, 1
    5, 2, 2
    6, 1, 3
    7, 3.5, 3.75
    8, 3.5, 5
    9, 6, 2.5
    10, 6.5, 3.75
    11, 5.5, 3.75
    "
) |> 
  as_tibble() |> 
  mutate(button = paste0("g", id)) |> 
  print()



locations |> 
  ggplot(aes(x, y)) +
  geom_text(aes(label = id)) +
  coord_fixed()

assignments |> 
  group_by(cardId, button, shifted) |> 
  summarise(profiles = paste(profile, collapse = ", ")) |> 
  ungroup() |> 
  left_join(commands) |>
  filter(button == "g8") |> 
  spread(shifted, command) |> 
  print()


assignments <-
  settings_df |> 
  select(profile = name, assignments) |> 
  unnest(assignments) |> 
  arrange(slotId) |> 
  filter(str_detect(slotId, "g502hero_g")) |>
  separate(
    col = slotId, 
    into = c("device", "button", "unknown", "shifted"),
    fill = "right",
    remove = FALSE
  ) |> 
  mutate(
    profile = ifelse(profile == "PROFILE_NAME_DEFAULT", "main", profile),
    shifted = replace_na(shifted, "default")
  ) |>
  print()

anti_join(
  distinct(assignments, cardId, profile, button),
  commands
)

# View(assignments)  

final_mapping <-
  assignments |> 
  left_join(commands) |>
  left_join(locations) |> 
  mutate(
    color = case_when(
      str_detect(command, "click") ~ "click",
      str_detect(command, "scroll|arrow") ~ "move",
      x < 3 ~ "side",
      x < 5 | str_detect(command, "print") ~ "top-left",
      is.na(command) ~ NA_character_
    )
  ) |> 
  print()


final_mapping |> 
  ggplot(aes(x, y)) +
  geom_vline(
    xintercept = c(3, 3, 4),
    color = "grey90", linetype = "dashed"
  ) +
  ggrepel::geom_text_repel(
    aes(label = command, color = color), 
    size = 3,
    nudge_y = -0.25
  ) +
  geom_label(
    aes(label = button, fill = color),
    size = 3, color = "grey40", alpha = 0.3,
    label.padding = unit(0.15, "lines")
  ) +
  facet_grid(shifted~profile) +
  scale_x_continuous(expand = expansion(0.1)) +
  scale_y_continuous(expand = expansion(0.1)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect("white", "grey50")
  )

ggsave(
  "~/github/keyboard_layouts/layouts/mouse/logitech/g502/map.png",
  width = 7, height = 4.50, dpi = "retina"
)


# from 
# # https://www.amazon.com/Logitech-Lightspeed-Wireless-Lightsync-Powerplay/product-reviews/B083C41BS4/ref=cm_cr_arp_d_viewopt_kywd?reviewerType=all_reviews&pageNumber=1&filterByKeyword=cardid
# It's in plain-text XML-style JSON format, and you can carefully edit it if you 
# like (probably best done with GHub closed, and subsequently re-launched). You can, 
# for example, create exotic keystroke assignments. Search the file for "cardId" values 
# that look like GUIDs and end in a "01XX00000000" pattern. These XX are hexadecimal 
# "USB Scan Codes" (google Scan Codes Demystified by John Savard) that you can also 
# find in the "Universal Serial Bus (USB) HID Usage Tables" document (hut1_12v2.pdf) 
# section "Keyboard/Keypad Page (0x07)" starting on page 53. Useful if F13~F24 and 
# the like aren't enough for you. Codes you could try: 85, 88, 8A, 8B, 90, 91, 93.
# 
# https://www.usb.org/sites/default/files/documents/hut1_12v2.pdf
# https://docs.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes
# https://blog.puzey.net/logitech-ghub-bind-all-the-things/
