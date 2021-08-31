

library(tidyverse)
library(haven)
library(here)
library(ggeasy)
library(janitor)
library(viridis)
#library(washb)
library(survey)
library(geepack)
library(lme4)
library(caret)
library(janitor)
library(forcats)
library(metafor)
library(R.utils)
library(tlverse)
library(sl3)
try(library(washb))

#remotes::install_github("Larsvanderlaan/tmle3@firstChanges") #Lar's development version with survey weights
library(tmle3)

library(mice) # Data imputation
library(miceadds)
library(purrr) # Flexible functional programming

#remotes::install_github("epix-project/mics")




theme_ki <- function() {
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

#hbgdki pallets
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
tableau11 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

theme_set(theme_ki())




scaleFUN <- function(x) sprintf("%.2f", x)
scaleRR <- function(x) sprintf("%.1f", x)


