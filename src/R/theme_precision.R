# =============================================================================
# PRECISION LAB — ggplot2 theme
# Store in: src/R/theme_precision.R
# Source at top of any analysis script: source(here("src", "R", "theme_precision.R"))
# =============================================================================

library(ggplot2)
library(showtext)

# --- Load Precision Lab fonts ---
font_add_google("Raleway",        "raleway")
font_add_google("Source Serif 4", "source_serif")
font_add_google("JetBrains Mono", "jetbrains")
showtext_auto()

# --- Colour palette ---
precision_colours <- c(
  teal_dark  = "#1a6b7c",
  teal_mid   = "#2a9d8f",
  teal_light = "#4fc3b0",
  teal_pale  = "#e8f5f3",
  slate      = "#2c3e50",
  muted      = "#6b8a8f",
  border     = "#d0e8e4",
  white      = "#ffffff"
)

# --- Qualitative palette (for discrete colour/fill scales) ---
precision_pal_qualitative <- c(
  "#1a6b7c",  # teal dark
  "#e76f51",  # coral
  "#e9c46a",  # gold
  "#2a9d8f",  # teal mid
  "#264653",  # dark slate
  "#a8dadc",  # pale cyan
  "#f4a261"   # warm orange
)

# --- Scale helpers ---
scale_colour_precision <- function(...) {
  scale_colour_manual(values = precision_pal_qualitative, ...)
}

scale_fill_precision <- function(...) {
  scale_fill_manual(values = precision_pal_qualitative, ...)
}

# For sequential continuous data — teal gradient
scale_colour_precision_seq <- function(direction = 1, ...) {
  scale_colour_gradientn(
    colours = c("#e8f5f3", "#4fc3b0", "#2a9d8f", "#1a6b7c")[::seq_len(4)[::seq_len(4) * direction]],
    ...
  )
}

# --- Main theme ---
theme_precision <- function(
    base_size   = 12,
    base_family = "raleway",
    grid        = "major"   # "major", "both", "none"
) {

  half_line <- base_size / 2

  t <- theme(
    # --- Base text ---
    text             = element_text(family = base_family, size = base_size,
                                    color = "#1a2a2e"),

    # --- Plot background ---
    plot.background  = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),

    # --- Panel border ---
    panel.border     = element_rect(color = "#d0e8e4", fill = NA, linewidth = .6),

    # --- Grid lines ---
    panel.grid.major = if (grid %in% c("major", "both")) {
      element_line(color = "#e8f5f3", linewidth = .5)
    } else {
      element_blank()
    },
    panel.grid.minor = if (grid == "both") {
      element_line(color = "#e8f5f3", linewidth = .3, linetype = "dashed")
    } else {
      element_blank()
    },

    # --- Axes ---
    axis.title       = element_text(family = "raleway", size = base_size * 1.1,
                                    color = "#1a6b7c", face = "bold"),
    axis.title.x     = element_text(margin = margin(t = half_line)),
    axis.title.y     = element_text(angle = 90, margin = margin(r = half_line)),
    axis.text        = element_text(size = base_size * 0.9, color = "#6b8a8f"),
    axis.ticks       = element_line(color = "#d0e8e4", linewidth = .4),
    axis.line        = element_blank(),

    # --- Legend ---
    legend.background  = element_rect(fill = "transparent", color = NA),
    legend.key         = element_rect(fill = "transparent", color = NA),
    legend.title       = element_text(family = "raleway", face = "bold",
                                      size = base_size * .9, color = "#1a6b7c"),
    legend.text        = element_text(family = "raleway", size = base_size * .85,
                                      color = "#1a2a2e"),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.key.size    = unit(.9, "lines"),

    # --- Facet strips ---
    strip.background   = element_blank(),
    strip.text         = element_text(family = "raleway", face = "bold",
                                      size = base_size, color = "#1a6b7c",
                                      hjust = 0),
    strip.placement    = "outside",

    # --- Titles ---
    plot.title         = element_text(family = "raleway", face = "bold",
                                      size = base_size * 1.6, color = "#1a6b7c",
                                      margin = margin(b = half_line * 1.2)),
    plot.subtitle      = element_text(family = "raleway", size = base_size * 1.1,
                                      color = "#6b8a8f",
                                      margin = margin(b = half_line)),
    plot.caption       = element_text(family = "raleway", size = base_size * .8,
                                      color = "#6b8a8f", hjust = 0,
                                      margin = margin(t = half_line)),
    plot.title.position  = "plot",
    plot.caption.position = "plot",
    plot.margin          = margin(rep(base_size, 4)),

    complete = TRUE
  )

  t
}

# --- Set as default ---
theme_set(theme_precision())

# =============================================================================
# USAGE NOTES
# =============================================================================
#
# Source this file at the top of any analysis script:
#   source(here("src", "R", "theme_precision.R"))
#
# Apply to a plot:
#   ggplot(...) + theme_precision()
#
# Discrete colour scale:
#   scale_colour_precision()
#   scale_fill_precision()
#
# Override grid lines:
#   theme_precision(grid = "none")    # no grid
#   theme_precision(grid = "both")    # major + minor
#   theme_precision(grid = "major")   # major only (default)
#
# Override base size for presentations:
#   theme_precision(base_size = 16)
#
# The theme is set globally via theme_set() when this file is sourced,
# so all subsequent ggplot calls use it automatically.
