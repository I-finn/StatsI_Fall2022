Races<-read.table("http://stat4ds.rwth-aachen.de/data/ScotsRaces.dat", header=TRUE)
head(Races)
matrix(cbind(mean(Races$timeW), sd(Races$timeW), mean(Races$climb), 
             sd(Races$climb), mean(Races$distance), sd(Races$distance)), nrow=2)
pairs(~timeW + distance + climb, data = Races)
fit.d <- lm(timeW~distance , data = Races)
summary(fit.d)


cor(Races[,c("timeW","distance","climb")])# correlation matrix
#symmetricaround "main diagonal"correlation=1.0

# example - florida crime

Florida<-read.table("http://stat4ds.rwth-aachen.de/data/Florida.dat", header=TRUE)
head(Florida,2)

#County CrimeIncomeHSUrban# HS =percentagewithatleasthighschooleducation
#1 ALACHUA10422.182.773.2
#2 BAKER2025.864.121.5
#> cor(Florida$Crime,Florida$HS)
#[1] 0.4669119
summary(lm(Crime~HS,data=Florida))

#Estimate Std.Error
#(Intercept) -50.856924.4507
#HS 1.4860 0.3491#marginaleffectofHS education >0

cor(Florida$HS,Florida$Urban)#urbanization strongly positivelycorrelated
cor(Florida$Crime,Florida$Urban)
