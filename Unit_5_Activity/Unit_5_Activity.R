install.packages("tidyverse")
library(tidyverse)

name <- c("Logan","Scott","Parker","Keven","Ken")
username <- c("lsparker","scottie33","prezPark","swaggie","thebigken")
state <- c("MS","OK","HI","TX","FL")
credits <- c("55","13","30","40","21")
resident_GPA <- c("4.0","3.7","2.6","3.5","3.9")
overall_GPA <- c("4.0","3.9","3.0","2.5","3.9")

students <- tibble(name,username,state,credits,resident_GPA,overall_GPA)
