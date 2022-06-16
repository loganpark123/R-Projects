library(tidyverse)
library(rvest)
library(robotstxt)
library(KeyboardSimulator)
paths_allowed("https://www.lttstore.com/collections/all")

continue <- TRUE
while(continue){
page <- read_html("https://www.lttstore.com/collections/all")
Sys.sleep(.01)
lowest <- page %>% html_nodes(".money") %>%  html_text()
index <- 1
for (x in lowest){
  x <- as.numeric(str_sub(x,2,4))
  lowest <- replace(lowest, index, x)
  index <- index +1
}
min <- min(as.numeric(lowest))
print(min)
if(min != 9.9) continue <- FALSE
mouse.click()
mouse.move(886,549)
}