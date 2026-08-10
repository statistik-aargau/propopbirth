# Hex-Sticker logo for the propopbirth package

library(hexSticker)
library(ggplot2)
library(sysfonts)
library(showtext)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("inter")

## Automatically use showtext to render text for future devices
showtext_auto()

# Create data
# Past data
past_data <- data.frame(
  x = seq(4, 10),
  y = c(0.28, 0.32, 0.31, 0.33, 0.35, 0.34, 0.37),
  phase = "past"
)

# Trend period
trend_data <- data.frame(
  x = seq(10, 13, length.out = 2),
  y = c(0.37, 0.42),
  phase = "trend"
)

# Temporal period
temporal_x <- seq(13, 18, length.out = 15)
temporal_data <- data.frame(
  x = temporal_x,
  y = 0.4 + 0.05 * (1 - exp(-(temporal_x - 12) / 2.2)),
  phase = "temporal"
)

# Constant period
constant_data <- data.frame(
  x = seq(18, 21, length.out = 2),
  y = rep(tail(temporal_data$y, 1), 2),
  phase = "constant"
)

# Points
breakpoints <- data.frame(
  x = c(10, 13, 18, 21),
  y = c(
    tail(past_data$y, 1),
    tail(trend_data$y, 1),
    tail(temporal_data$y, 1),
    tail(constant_data$y, 1)
  )
)

# Plot
p <- ggplot() +
  geom_line(
    data = tail(past_data), aes(x, y),
    color = "#4A6CF0", linewidth = 1.1, lineend = "round"
  ) +
  geom_line(
    data = trend_data, aes(x, y),
    color = "#F2A65A", linewidth = 1.3, lineend = "round"
  ) +
  geom_line(
    data = temporal_data, aes(x, y),
    color = "#E8555A", linewidth = 1.3, lineend = "round"
  ) +
  geom_line(
    data = constant_data, aes(x, y),
    color = "#6E1F2A", linewidth = 1.3, lineend = "round"
  ) +
  geom_point(
    data = breakpoints, aes(x, y),
    shape = 21, size = 3, stroke = 1.1,
    fill = "#0A2F35", color = "white"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

p

# Create hex sticker and export to man/figures/
sticker(
  subplot   = p,
  # Package name
  package   = "propopbirth",
  p_size    = 20,
  p_family  = "inter",
  p_color   = "white",
  p_y       = 1.45,
  # Subplot position
  s_x       = 1,
  s_y       = 0.85,
  s_width   = 1.5,
  s_height  = 0.75,
  # Background and borders
  h_fill    = "#004774",
  h_color   = "#c0c0c0",
  h_size    = 1.4,
  # Export
  filename  = "man/figures/logo.png",
  dpi       = 300
)

# usethis::use_logo("man/figures/logo.png")
