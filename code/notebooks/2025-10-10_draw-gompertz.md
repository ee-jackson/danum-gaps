# Draw Gompertz
eleanorjackson
2025-10-17

``` r
library("tidyverse")
library("ggtext")
```

``` r
A <- 50 # Asymptotic maximum size
T_i <- -1 # Time at inflection point
k_G <- 0.3 # Growth rate parameter
```

``` r
gompertz <- function(t, A, k_G, T_i) {
  A * exp(-exp(- k_G * (t - T_i)))
}
```

``` r
# Calculate slope and intercept at inflection point
slope_max <- A * k_G / exp(1)
size_at_Ti <- A / exp(1)
intercept <- size_at_Ti - slope_max * T_i
```

``` r
# Create data frame with curve and tangent line
df <- tibble(
  t = seq(-5, 20, by = 0.1)
) %>%
  mutate(
    size = gompertz(t, A, k_G, T_i),
    tangent = slope_max * t + intercept
  )

# Create inflection point data
inflection_point <- tibble(
  t = T_i,
  size = size_at_Ti
)

# Plot
toy_fig <- 
  ggplot(df, aes(x = t)) +
  geom_line(
    aes(y = size, color = "Gompertz curve"),
    colour = "forestgreen",
    .width = 0,
    linewidth = 1,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = tangent, 
        color = "Absolute maximum<br>growth rate, _A k<sub>G</sub> / e_"),
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_point(data = inflection_point,
             aes(y = size, color = "Inflection point"),
             size = 3) +
  geom_vline(
    xintercept = T_i,
    linetype = "dotted",
    color = "gray50",
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = A,
    linetype = "dotted",
    color = "gray50",
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Gompertz curve" = "forestgreen",
      "Absolute maximum<br>growth rate, _A k<sub>G</sub> / e_" = "gray40",
      "Inflection point" = "blue"
    ),
    breaks = c(
      "Gompertz curve",
      "Absolute maximum<br>growth rate, _A k<sub>G</sub> / e_",
      "Inflection point"
    )
  ) +
  labs(x = "Time (years)", y = "Basal diameter (mm)", color = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.2),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_markdown(),
    axis.text.y = element_markdown(),
    legend.text = element_markdown()
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, A, 60),
    labels = c(0, 20, 40, 
               "<span style = 'color:gray40;'>_A_</span>", 
               60),
    limits = c(0, 60)
  ) +
  scale_x_continuous(
    breaks = c(-5, T_i, 0, 5, 10, 15, 20),
    labels = c(-5, 
               "<span style = 'color:gray40;'>_T<sub>i</sub>_</span>", 
               0, 5, 10, 15, 20)
  )

toy_fig
```

![](figures/2025-10-10_draw-gompertz/unnamed-chunk-5-1.png)

``` r
jpeg(
  here::here("output", "figures", "gompertz.jpeg"),
  width = 80,
  height = 60,
  res = 600,
  pointsize = 6,
  units = "mm",
  type = "cairo"
)
toy_fig
dev.off()
```

    quartz_off_screen 
                    2 

``` r
# Print parameters
cat("Parameters:\n")
```

    Parameters:

``` r
cat("A =", A, "\n")
```

    A = 50 

``` r
cat("k_G =", k_G, "\n")
```

    k_G = 0.3 

``` r
cat("T_i =", T_i, "\n\n")
```

    T_i = -1 

``` r
cat("At inflection point:\n")
```

    At inflection point:

``` r
cat("Maximum slope =", round(slope_max, 3), "\n")
```

    Maximum slope = 5.518 

``` r
cat("Size at T_i =", round(size_at_Ti, 3), "\n")
```

    Size at T_i = 18.394 

``` r
cat("Tangent intercept =", round(intercept, 3), "\n")
```

    Tangent intercept = 23.912 
