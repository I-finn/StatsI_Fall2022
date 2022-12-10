
#todo graph model
Prestige %>% filter(!is.na(type)) %>% 
  ggplot(aes( x= income +  professional ,  y = prestige)) +
  geom_point(size=2, aes(colour = as.factor(professional))) +
  geom_smooth(method = "lm", colour = "red", formula = y ~ x, 
              se = FALSE, linetype = 'dashed')

Prestige2 %>% ggplot(aes( x= income, y = prestige)) +
  geom_point(size=2, aes(colour = as.factor(professional))) +
  geom_smooth(method = "lm", colour = "red", formula = y ~ x, 
              se = FALSE, linetype = 'dashed')

ggsave("Graphics/professional.png")


prestige_add <- augment(pres_inc_prof)
head(prestige_add)



plot(y = Prestige$prestige, x=Prestige$income)
identify(y = Prestige$prestige, x=Prestige$income)


#--------------------------------------------
dat_add <- augment(mod_vote_spend_pres)
summary(dat_add)

dat %>% ggplot( aes(x = difflog + presvote , y = voteshare)) +
  geom_point(alpha = 0.5)+
  geom_line(data = dat_add, aes(y = .fitted)) + # we change our data to the fitted values of the additive model
  geom_smooth(method = "lm", colour = "red", formula = y ~ x, linetype = 'dashed') 


