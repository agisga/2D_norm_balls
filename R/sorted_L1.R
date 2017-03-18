library(dplyr)
library(ggplot2)
library(magick)

lambda1 <- 1
lambda2 <- c(seq(0, 1, by = 0.2), seq(1, 2, by = 0.2))
x1 <- seq(-1, 1, by = 0.01)

get_x2_of_sorted_L1 <- function(x, lambda1, lambda2) {
  if (abs(x) <= 1 / (lambda1 + lambda2)) {
    y <- (1 - lambda2 * abs(x)) / lambda1
  } else {
    y <- (1 - lambda1 * abs(x)) / lambda2
  }
  return(y)
}

for(i in 1:length(lambda2)) {
  x2 <- sapply(x1, function(x) get_x2_of_sorted_L1(x, lambda1, lambda2[i]))
  data_frame(x_1 = c(x1, rev(x1), x1[1]), x_2 = c(x2, -rev(x2), x2[1])) %>%
    ggplot(aes(x_1, x_2)) + geom_path() +
    annotate("text", x = 0, y = 0, parse = TRUE, size = 6, color = "blue",
             label = paste0("list(lambda[1] == ", lambda1,
                            ", lambda[2] == ", lambda2[i], ")")) +
    xlab(expression(beta[1])) + ylab(expression(beta[2])) +
    ggtitle(expression(list(lambda[1]*group("|", beta, "|")[(1)]+lambda[2]*group("|", beta, "|")[(2)]==1, group("|", beta, "|")[(1)] >= group("|", beta, "|")[(2)]))) +
    theme_minimal()
  ggsave(paste0("../img/sorted_L1_img/", sprintf("%03.0f", i), ".jpeg"),
         width = 8.5, height = 8.5, units = "cm")
}

# make a GIF
images <- paste0("../img/sorted_L1_img/", list.files(path = "../img/sorted_L1_img/", pattern = "jpeg"))
frames <- c()
for (i in length(images):1) {
  x <- images[i] %>% image_read() %>% image_scale("400")
  frames <- c(x, frames)
}
animation <- image_animate(frames, fps = 2)
image_write(animation, "../gif/sorted_L1_balls.gif")
