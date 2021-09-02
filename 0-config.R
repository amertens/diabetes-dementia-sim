
library(data.table)
library(heaven)
library(tidyverse)
#library(ltmle)
library(knitr)
library(here)
library(kableExtra)
library(ltmle)



# load modified version; mcSuperLearner, deterministic censoring, etc. 
#{
  library(Matrix)
  library(matrixStats)
  library(speedglm)
#   temp <- list.files(here("/ltmle-master/R"), full.names = T)
#   for (i in temp) source(i)
# }
library(hal9001)
library(pryr)


# library(haven)
# library(here)
# library(ggeasy)
# library(janitor)
# library(viridis)
# #library(washb)
# library(survey)
# library(geepack)
# library(lme4)
# library(caret)
# library(janitor)
# library(forcats)
# library(metafor)
# library(R.utils)
# library(tlverse)
# library(sl3)
#try(library(washb))

#remotes::install_github("Larsvanderlaan/tmle3@firstChanges") #Lar's development version with survey weights
# library(tmle3)
# 
# library(mice) # Data imputation
# library(miceadds)
# library(purrr) # Flexible functional programming

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


source(here::here("functions.R"))

glp1 <- c("A10BJ01", "A10BJ02", "A10BJ03", "A10BJ04", "A10BJ05", "A10BJ06", "A10BJ07")  
dpp4 <- c("A10BH01", "A10BH02", "A10BH03", "A10BH04", "A10BH05", "A10BH06", "A10BH07", "A10BH08", "A10BH51", "A10BH52")
SGLT2i <- c("A10BK01", "A10BK02", "A10BK03", "A10BK04", "A10BK05", "A10BK06", "A10BK07")
hypertension <- unlist(hypertensionATC)
dementia <- c("N06DA01", "N06DA02", "N06DA03", "N06DA04", "N06DA05", "N06DA52", "N06DA53")