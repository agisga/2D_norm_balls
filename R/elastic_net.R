library(dplyr)
library(ggplot2)
library(magick)

x1 <- seq(-1, 1, by = 0.01)
a <- seq(0, 0.9, by = 0.1)
for(i in 1:length(a)) {
  x2 <- sapply(x1, function(x) {
                 x <- abs(x)
                 (-a[i] + sqrt(a[i]^2 - 4 * (1 - a[i]) * ((1 - a[i]) * x^2 + a[i] * x - 1))) / (2 - 2 * a[i])
})
  data_frame(x_1 = c(x1, rev(x1), x1[1]), x_2 = c(x2, -rev(x2), x2[1])) %>%
    ggplot(aes(x_1, x_2)) + geom_path() +
    annotate("text", x = 0, y = 0, parse = TRUE, size = 6,
             color = "blue", label = paste0("alpha == ", a[i])) +
    xlab(expression(beta[1])) + ylab(expression(beta[2])) +
    ggtitle(expression(alpha*(group("|", beta[1], "|")+group("|", beta[2], "|"))+(1-alpha)*(beta[1]^2+beta[2]^2)==1)) +
    theme_minimal()
  ggsave(paste0("../img/elastic_net_img/", sprintf("%03.0f", i), ".jpeg"),
         width = 9, height = 9, units = "cm")
}

# make a GIF
images <- paste0("../img/elastic_net_img/", list.files(path = "../img/elastic_net_img/", pattern = "jpeg"))
frames <- c()
for (i in length(images):1) {
  x <- images[i] %>% image_read() %>% image_scale("400")
  frames <- c(x, frames)
}
animation <- image_animate(frames, fps = 2)
image_write(animation, "../gif/elastic_net_balls.gif")
