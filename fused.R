library(dplyr)
library(ggplot2)
library(magick)

# generate an equidistant grid in the X-Y-plane
u <- seq(-2, 2, by = 0.01)
m  <- length(u)
xy <- matrix(c(rep(u, each = m), rep(u, m)), ncol = 2)
colnames(xy) <- c("x", "y")
xy <- as_data_frame(xy)

# evaluate the fused penalty on the grid, draw points with value <=1 only, and save a figure for each value of `a`
a <- c(0, seq(0.3, 0.8, by = 0.1), 1)
for(i in 1:length(a)) {
  xy %>%
    mutate(Value = a[i] * (abs(x) + abs(y)) + (1 - a[i]) * abs(x - y)) %>%
    filter(abs(Value) <= 1) %>%
    ggplot(aes(x, y, color = Value)) + geom_point(pch = ".") +
    annotate("text", x = -1.5, y = 1.5, parse = TRUE, size = 6,
             color = "blue", label = paste0("alpha == ", a[i])) +
    xlab(expression(beta[1])) + ylab(expression(beta[2])) +
    ggtitle(expression(alpha*(group("|", beta[1], "|")+group("|", beta[2], "|"))+(1-alpha)*group("|", beta[1] - beta[2], "|")<=1)) +
    xlim(-2, 2) + ylim(-2, 2) +
    theme_minimal()
  ggsave(paste0("./fused_img/", sprintf("%03.0f", i), ".jpeg"),
         width = 10, height = 8, units = "cm")
}

# make a GIF
images <- paste0("./fused_img/", list.files(path = "./fused_img/", pattern = "jpeg"))
frames <- c()
for (i in length(images):1) {
  x <- images[i] %>% image_read() %>% image_scale("500")
  frames <- c(x, frames)
}
animation <- image_animate(frames, fps = 2)
image_write(animation, "fused_penalty_balls.gif")
