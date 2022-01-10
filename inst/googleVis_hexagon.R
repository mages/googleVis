library(hexSticker)
library(ggplot2)
N <- 50
set.seed(121)
DF <- data.frame(x=runif(N)*0.9, y=runif(N)*0.9, 
                  z = sample(1:4, N, replace = TRUE),
                  w = sample(1:5, N, replace = TRUE))
p <- ggplot(DF, aes(x=x, y=y, color = factor(z))) + 
  geom_point(pch = c("G", "o", "o", "g", "l", "e")[DF$w], cex=10)  
p <- p + labs(y="", x="") + 
  xlim(0,1) + ylim(0,1) + 
  scale_color_manual(values = c("#3cba54", "#db3236", "#f4c20d", "#4885ed")) +
  theme_void() + theme_transparent()  + 
  theme(legend.position = 'none')
p
sticker(p, package="googleVis", p_size=20, s_x=1, s_y=.75, s_width=1, s_height=0.91,
        h_fill="white", h_color="#4885ed", p_color = "#db3236",
        filename="man/figures/logo.png")
