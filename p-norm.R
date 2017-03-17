library(dplyr)
library(ggplot2)
library(magick)

x1 <- seq(-1, 1, by = 0.01)
p <- c(seq(0.1, 1, by = 0.1), seq(1.5, 4, by = 0.5), Inf)
for(i in 1:length(p)) {
  x2 <- sapply(x1, function(x) abs((1-abs(x)^p[i])^(1/p[i])) )
  data_frame(x_1 = c(x1, rev(x1), x1[1]), x_2 = c(x2, -rev(x2), x2[1])) %>%
    ggplot(aes(x_1, x_2)) + geom_path() +
    annotate("text", x = 0, y = 0, parse = TRUE, size = 6,
             color = "blue", label = paste0("p == ", p[i])) +
    xlab(expression(beta[1])) + ylab(expression(beta[2])) +
    ggtitle(expression(group("|", beta[1], "|")^p+group("|", beta[2], "|")^p==1)) +
    theme_minimal()
  ggsave(paste0("./p-norm_img/", sprintf("%03.0f", i), ".jpeg"),
         width = 8, height = 8, units = "cm")
}

# make a GIF
images <- paste0("./p-norm_img/", list.files(path = "./p-norm_img/", pattern = "jpeg"))
frames <- c()
for (i in length(images):1) {
  x <- images[i] %>% image_read() %>% image_scale("400")
  frames <- c(x, frames)
}
animation <- image_animate(frames, fps = 2)
image_write(animation, "p-norm_balls.gif")
