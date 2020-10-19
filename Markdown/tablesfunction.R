## ---- tf1 --------
library(knitr)
tf1 <- data.frame(c("Ressource","[]Regenwasser"),
                  c("","[] Schmutzwasser"),
                  c("","[] Baustoffe"),
                  c("","[] Energie"),
                  c("","[] Fläche")
                  )
tf1 %>% kable(caption = "Ressource")

