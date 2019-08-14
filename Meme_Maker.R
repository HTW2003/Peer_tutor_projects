# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("sctyner/memer")

library(tidyverse)
library(magick)

library(memer)
meme_list()

meme_get("DistractedBf") %>%
  meme_text_distbf("Coding", "Me", "Summer Work")

#meme_get("CondescendingWonka") %>%
  #meme_text_top("You thought the code would work?") 
  #meme_text_bottom("You must be new here")
