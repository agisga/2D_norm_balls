library(dplyr)
library(ggplot2)
library(magick)

# generate an equidistant grid in the X-Y-plane
u <- seq(-1, 1, by = 0.01)
m  <- length(u)
xy <- matrix(c(rep(u, each = m), rep(u, m)), ncol = 2)
colnames(xy) <- c("x", "y")
xy <- as_data_frame(xy)

a <- c(1, 5, 10, 20, 35, 50)
for(i in 1:length(a)) {
  xy %>%
    mutate(Value = tanh(a[i] * x^2) + tanh(a[i] * y^2)) %>%
    ggplot(aes(x, y, z = Value)) + geom_contour(color = "darkgrey") +
    annotate("text", x = -0.8, y = 0.8, parse = TRUE, size = 6,
             color = "blue", label = paste0("a == ", a[i])) +
    xlab(expression(beta[1])) + ylab(expression(beta[2])) +
    ggtitle(expression(tanh(a*beta[1]^2) + tanh(a*beta[2]^2))) +
    xlim(-1, 1) + ylim(-1, 1) +
    theme_minimal()
  ggsave(paste0("../img/tanh_img/", sprintf("%03.0f", i), ".jpeg"),
         width = 8, height = 8, units = "cm")
}

# make a GIF
images <- paste0("../img/tanh_img/", list.files(path = "../img/tanh_img/", pattern = "jpeg"))
frames <- c()
for (i in length(images):1) {
  x <- images[i] %>% image_read() %>% image_scale("400")
  frames <- c(x, frames)
}
animation <- image_animate(frames, fps = 2)
image_write(animation, "../gif/hyperbolic_tangent_penalty.gif")
