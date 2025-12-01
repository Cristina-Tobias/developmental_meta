library(forestplot)
library(dplyr)
library(patchwork)
library(ggplot2)
library(ggplotify)
library(grid)


#data <- data.frame(
#    labeltext = c("S1", "S2", "S3", "S4",  "S5",   "S6", "S7",  "S8", "S9", "S10",  "S11", "S12", "S13", "S14"),
#    mean = c(    0.18,  0.11, 0.18, 0.08,   0.05,  0.21, 0.21, 0.21,  0.25, -0.05,  -0.15, -0.25, 0.58, 0.42),
#    lower = c(   0.17,  0.01, 0.15, -0.01, -0.19,  0.19, 0.03, -0.01, 0.23, -.15,   -0.25, -0.26, 0.26, 0.32),
#    upper = c(   0.19,  0.21, 0.21, 0.15,   0.24,  0.23, 0.39, 0.39,  0.27, 0.05,   -0.05, -0.24, 0.90, 0.52),
#    group = c("difference", "difference", "difference", "delay", "delay", "difference", "difference", "delay", "difference", "delay", "difference", "difference", "huge_difference","huge_difference"),
#    group2 = c("delay", "difference", "difference", "delay", "?", "difference", "difference", "?", "difference", "delay", "difference", "difference", "huge_difference","huge_difference")
#) #old eq bounds of 0.2

data <- data.frame(
  labeltext = c("S1", "S2", "S3", "S4",  "S5",   "S6", "S7",  "S8", "S9", "S10",  "S11", "S12", "S13", "S14"),
  mean = c(    0.45,  0.275, 0.45, 0.20,  0.125,  0.525, 0.525, 0.525, 0.625, -0.125, -0.375, -0.625, 1.45, 1.05),
  lower = c(   0.425, 0.025, 0.375, -0.025, -0.475, 0.475, 0.075, -0.025, 0.575, -0.375, -0.625, -0.65, 0.65, 0.80),
  upper = c(   0.475, 0.525, 0.525, 0.375,  0.60, 0.575, 0.975, 0.975, 0.675, 0.125, -0.125, -0.60, 2.25, 1.30),
  group = c("difference", "difference", "difference", "delay", "delay", "difference", "difference", "delay", "difference", "delay", "difference", "difference", "difference","difference"),
  group2 = c("delay", "difference", "difference", "delay", "?", "difference", "difference", "?", "difference", "delay", "difference", "difference", "difference","difference")
)


styles <- fpShapesGp(grid = list(
#  gpar(lty = 2, col = "lavender", lwd =2),
#  gpar(lty = 2, col = "purple", lwd =2),
#  gpar(lty = 2, col = "violet", lwd =2),
#  gpar(lty = 2, col = "pink", lwd =2),
#  gpar(lty = 2, col = "pink2", lwd =2),
  gpar(lty = 2, col = "powderblue", lwd =2),
  gpar(lty = 2, col = "lightblue", lwd =2)
  #,
#  gpar(lty = 2, col = "pink2", lwd =2),
#  gpar(lty = 2, col = "pink", lwd =2),
#  gpar(lty = 2, col = "violet", lwd =2),
#  gpar(lty = 2, col = "purple", lwd =2),
#  gpar(lty = 2, col = "lavender", lwd =2)
  ))

text_style <- fpTxtGp(
  label = gpar(fontsize = 15, fontface = "bold"),       # y-axis labels (e.g., S1, S2)
  ticks = gpar(fontsize = 14),                          # x-axis tick labels
  xlab = gpar(fontsize = 14, fontface = "bold"),        # x-axis label
  legend = gpar(fontsize = 16),                         # legend text
  summary = gpar(fontsize = 13)
)


p1 <- data |>
  group_by(group) |>
  forestplot(xlab = "Effect Size",
             clip = c(-1.5, 1.5),  # Ensures x-axis range
             xticks = seq(-1, 1, by = 0.2),
             grid = c(#-.8,-.73,-.5,-.48,-.34, 
               -.5, .5
                      #,
                      #.34, .48, .5, .73, .8
                      ),
             shapes_gp = styles,
             txt_gp = text_style) |>
  # fp_add_lines("steelblue") |>
  fp_add_header("NHST") |>
  fp_set_style(box = c("blue", "darkred") |> lapply(function(x) gpar(fill = x, col = "#555555")),
               default = gpar(vertices = TRUE)) 





p2 <- data |>
  group_by(group2) |>
  forestplot(xlab = "Effect Size",
             clip = c(-1.5, 1.5),  # Ensures x-axis range
             xticks = seq(-1, 1, by = 0.2),
             grid = c(#-.8,-.73,-.5,-.48,-.34, 
               -.5, .5
               #,
               #.34, .48, .5, .73, .8
             ),
             shapes_gp = styles,
             txt_gp = text_style) |>
  # fp_add_lines("steelblue") |>
  fp_add_header("Equivalence test") |>
  fp_set_style(box = c("darkred","blue", "yellow") |> lapply(function(x) gpar(fill = x, col = "#555555")),
               default = gpar(vertices = TRUE)) 
p1 <- grid2grob(print(p1))
p2 <- grid2grob(print(p2))
p_both <- wrap_elements(p1) / wrap_elements(p2)
p_both

ggsave("eq_sig.png", p_both, 
       width = 400, 
       height = 400, 
       units = "mm",
       dpi = 300)
