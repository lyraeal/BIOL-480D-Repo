
library(tidyverse)
# library(taxize)
library(ggplot2)


# dat = readxl::read_xlsx("files/ButterflyNIRData.xlsx")
dat = read.csv("files/butterfly-dat-fam.csv")


dat %>% 
  lm(
    NirDorsE ~ temp.mean + precip.mean + SolarRadi + WaterVapor + Isothermality + Size,
    data = .
  ) %>% 
  summary()

genusdat = dat %>% 
  mutate(
    genus = 
      str_match(
        gsub("_", " ", 
             dat$Species), 
        "([A-Z,a-z]*) ")[,2]
  )

# https://stackoverflow.com/questions/64835375/using-taxize-package-to-get-a-dataframe-of-family-names-from-species-list
# use_entrez()
# usethis::edit_r_environ()
# ENTREZ_KEY = 'ff255e4536fbec274fa92f593dc5ab58d608'
# fam = tax_name(genusdat$Species, get = 'family', db = 'ncbi')

# genusdat$fam = fam[[3]]

# i searched this up
# genusdat$fam[which(is.na(genusdat$fam == T))] = "Nymphalidae"

# write.csv(genusdat, file = "files/butterfly-dat-genus.csv")

# did not work
# xlsx::write.xlsx(genusdat, 
#                  file = "files/ButterflyNIRData.xlsx",
#                  sheetName = "withfam", 
#                  append = T)




genusdat %>%
  pivot_longer(colnames(genusdat)[13:24],
               names_to = "measurement") %>%
  # only 1 measurement of
  filter(fam != "Riodinidae") %>%
  ggplot(aes(x = fam,
             y = value)) +
  geom_boxplot() +
  # geom_jitter() +
  facet_wrap( ~ measurement,
              scales = "free_y") +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ))


genusdat %>%
  lm(
    NirDorsE ~ temp.mean + precip.mean + SolarRadi + WaterVapor + Isothermality + Size + fam,
    data = .
  ) %>% 
  # car::Anova()
  summary()


genusdat %>%
  lm(
    NirDorsE ~ genusdat$temp.mean + genusdat$precip.mean + genusdat$SolarRadi + genusdat$WaterVapor + genusdat$Isothermality + genusdat$Size + genusdat$fam,
    data = .
  ) %>% 
  summary()

# lapply() with the cols of interest.
# i'm removing row 89 bc that's the only species in its family
lapply(genusdat[-89, 13:24], function(x)
  summary(
    lm(
      x ~ genusdat$temp.mean[-89] +
        genusdat$precip.mean[-89] +
        genusdat$SolarRadi[-89] +
        genusdat$WaterVapor[-89] +
        genusdat$Isothermality[-89] +
        genusdat$Size[-89] +
        genusdat$fam[-89]
    )
  ))

genusdat %>% 
  lme4::glmer(
    NirDorsE ~ temp.mean + precip.mean + SolarRadi + WaterVapor + Isothermality + Size + fam,
    data = .
  )

genusdat %>% 
  pivot_longer(
    cols = 13:24, 
    names_to = "refl"
  ) %>% view()
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~ refl) + theme_light()




genusdat %>% 
  pivot_longer(13:24, 
               # this pattern has as many capturing groups as you need for your cols, so in this case, i am separating ir / uv and body location, so i need two capturing groups
               # capture group one captures either Vis or Nir, and the second capture group captures lowercase letters sandwiched between 
               names_pattern = "([A-Z]i[a-z])([A-Z][a-z]*[A-Z])",
               names_to = c(".value", "loc"))






# this is from a sep section ----------------------------------------------

longdat[[2]] %>% median()

longdat[[2]] %>% length()

longdat %>% nrow

for (i in 1:nrow(longdat)){
  
}





















