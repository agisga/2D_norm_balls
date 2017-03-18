library(dplyr)
library(ggplot2)
library(magick)

# generate an equidistant grid in the X-Y-plane
u <- seq(-2.5, 2.5, by = 0.01)
m  <- length(u)
xy <- matrix(c(rep(u, each = m), rep(u, m)), ncol = 2)
colnames(xy) <- c("x", "y")
xy <- as_data_frame(xy)

# evaluate the penalty function on the grid, draw points with value <=1 only, and save a figure for each value of `p`
a <- 0.6
p <- seq(1, 10, by = 1)
for(i in 1:length(p)) {
  xy %>%
    mutate( Value = (abs(x) + abs(y)) - a * (abs(x)^p[i] + abs(y)^p[i])^(1/p[i]) ) %>%
    filter(abs(Value) <= 1) %>%
    ggplot(aes(x, y, color = Value)) + geom_point(pch = ".") +
    annotate("text", x = -1.5, y = 1.5, parse = TRUE, size = 6,
             color = "blue", label = paste0("p == ", p[i])) +
    xlab(expression(beta[1])) + ylab(expression(beta[2])) +
    xlim(-2.5, 2.5) + ylim(-2.5, 2.5) + scale_color_continuous(limits = c(0, 1)) +
    ggtitle(expression((group("|", beta[1], "|") + group("|", beta[2], "|")) - 0.6 * sqrt(group("|", beta[1], "|")^p + group("|", beta[2], "|")^p, 1/p) <= 1)) +
    theme_minimal()
  ggsave(paste0("../img/l1-lp_img/", sprintf("%03.0f", i), ".jpeg"),
         width = 10, height = 8, units = "cm")
}

# make a GIF
images <- paste0("../img/l1-lp_img/", list.files(path = "../img/l1-lp_img/", pattern = "jpeg"))
frames <- c()
for (i in length(images):1) {
  x <- images[i] %>% image_read() %>% image_scale("500")
  frames <- c(x, frames)
}
animation <- image_animate(frames, fps = 2)
image_write(animation, "../gif/l1-lp_balls.gif")
