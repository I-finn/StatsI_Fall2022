#####################
# regression models #
#####################

# load packages
library(tidyverse)
library(broom)
library(stargazer)

# load in data
dat <- readRDS("data/train.rds")

# model
mod1 <- lm(AdjSalePrice ~ SqFtTotLiving + Bathrooms, data = dat)
stargazer(mod1, type = "text")

mod2 <- lm(AdjSalePrice ~ SqFtTotLiving + Bathrooms + NbrLivingUnits, data = dat)
stargazer(mod2, type = "text")

mod3 <- lm(AdjSalePrice ~ SqFtTotLiving + NbrLivingUnits + Bathrooms * Bedrooms, data = dat)
stargazer(mod3, type = "text")

# Bathrooms and bedrooms
size <- lm(AdjSalePrice ~ SqFtTotLiving, data = dat)
bathrooms <- lm(AdjSalePrice ~ SqFtTotLiving + Bathrooms, data = dat)
bedrooms <- lm(AdjSalePrice ~ SqFtTotLiving + Bedrooms, data = dat)
stargazer(size, type = "text")
stargazer(bathrooms, type = "text")
stargazer(bedrooms, type = "text")

size_dat <- augment(size, interval = "confidence")
bath_dat <- augment(bathrooms, interval = "confidence")
bed_dat <- augment(bedrooms, interval = "confidence")

ggplot(dat, aes(SqFtTotLiving, AdjSalePrice)) +
  geom_point(alpha = 0.5) +
  geom_line(data = size_dat, aes(y = .fitted)) +
  geom_ribbon(data = size_dat, 
              aes(ymin=.lower, ymax=.upper), alpha=0.2)

ggplot(dat, aes(SqFtTotLiving, AdjSalePrice, group = as.factor(Bathrooms))) +
  geom_point(alpha = 0.5, aes(colour = as.factor(Bathrooms))) +
  geom_line(data = bath_dat, aes(y = .fitted, colour = as.factor(Bathrooms))) +
  geom_ribbon(data = bath_dat, 
              aes(ymin=.lower, ymax=.upper), alpha=0.2)

ggplot(dat, aes(SqFtTotLiving, AdjSalePrice, group = as.factor(Bedrooms))) +
  geom_point(alpha = 0.5, aes(colour = as.factor(Bedrooms))) +
  geom_line(data = bed_dat, aes(y = .fitted, colour = as.factor(Bedrooms))) +
  geom_ribbon(data = bed_dat, 
              aes(ymin=.lower, ymax=.upper), alpha=0.2)
