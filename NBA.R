install.packages("plm")
install.packages("ivreg")
install.packages("stargazer")
install.packages("readr")
library(plm)
library(sandwich)
library(lmtest)
library(AER)
library(stargazer)
library(readr)


lmusg <- lm(data = nba,
            nba$usg ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
            + nba$name + nba$season,
            weights = nba$min)

coeftest(lmusg, vcov. = vcovHC, type = "HC1")

lmdws <- lm(data = nba,
            nba$dws ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
            + nba$name + nba$season,
            weights = nba$min)

coeftest(lmdws, vcov. = vcovHC, type = "HC1")

lmows <- lm(data = nba,
            nba$ows ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
            + nba$name + nba$season,
            weights = nba$min)

coeftest(lmows, vcov. = vcovHC, type = "HC1")

lmws <- lm(data = nba,
            nba$ws ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
           + nba$name + nba$season,
            weights = nba$min)

coeftest(lmws, vcov. = vcovHC, type = "HC1")

lmdm <- lm(data = nba,
            nba$dist_miles ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
           + nba$name + nba$season,
            weights = nba$min)

coeftest(lmdm, vcov. = vcovHC, type = "HC1")

lmas <- lm(data = nba,
            nba$avg_speed ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
           + nba$name + nba$season,
            weights = nba$min)

coeftest(lmas, vcov. = vcovHC, type = "HC1")

lmaso <- lm(data = nba,
           nba$avg_speed_off ~ nba$contract_year + nba$salary_current
           + nba$min + nba$age + nba$pos + nba$team + nba$g
           + nba$name + nba$season,
           weights = nba$min)

coeftest(lmaso, vcov. = vcovHC, type = "HC1")

lmasd <- lm(data = nba,
            nba$avg_speed_def ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
            + nba$name + nba$season,
            weights = nba$min)

coeftest(lmasd, vcov. = vcovHC, type = "HC1")

lmaspt <- lm(data = nba,
            nba$Avg.Sec.Per.Touch ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
            + nba$name + nba$season,
            weights = nba$min)

coeftest(lmaspt, vcov. = vcovHC, type = "HC1")

lmadpt <- lm(data = nba,
            nba$Avg.Drib.Per.Touch ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
            + nba$name + nba$season,
            weights = nba$min)

coeftest(lmadpt, vcov. = vcovHC, type = "HC1")

lmbo <- lm(data = nba,
            nba$Box.Outs ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
           + nba$name + nba$season,
            weights = nba$min)

coeftest(lmbo, vcov. = vcovHC, type = "HC1")

lmobo <- lm(data = nba,
           nba$OFF.Box.Outs ~ nba$contract_year + nba$salary_current
           + nba$min + nba$age + nba$pos + nba$team + nba$g
           + nba$name + nba$season,
           weights = nba$min)

coeftest(lmobo, vcov. = vcovHC, type = "HC1")

lmdbo <- lm(data = nba,
           nba$DEF.Box.Outs ~ nba$contract_year + nba$salary_current
           + nba$min + nba$age + nba$pos + nba$team + nba$g
           + nba$name + nba$season,
           weights = nba$min)

coeftest(lmdbo, vcov. = vcovHC, type = "HC1")

lmws48 <- lm(data = nba,
            nba$ws_48 ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team + nba$g
            + nba$name + nba$season,
            weights = nba$min)

coeftest(lmws48, vcov. = vcovHC, type = "HC1")

write.csv(nba, "ivnba.csv")

nba$contract_2018 <- ifelse(
  nba$contract_year==1 & nba$season==2018, 1, 0)

ivlmusg <- ivreg(nba$usg ~ nba$contract_year + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team
            + nba$name + nba$season | nba$contract_2018 + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team
            + nba$name + nba$season,
            weights = nba$min)

coeftest(ivlmusg, vcov. = vcovHC, type = "HC1")

tsls1usg <- lm(nba$contract_year ~ nba$salary_current
             + nba$min + nba$age + nba$pos + nba$team
             + nba$name + nba$season + nba$contract_2018)

nba$usghat <- predict(tsls1usg, nba)

tsls2usg <- lm(nba$usg ~ nba$usghat + nba$salary_current
            + nba$min + nba$age + nba$pos + nba$team
            + nba$name + nba$season + nba$contract_2018)

stargazer(tsls2usg)


ivlmdws <- ivreg(nba$dws ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmdws, vcov. = vcovHC, type = "HC1")

ivlmows <- ivreg(nba$ows ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmows, vcov. = vcovHC, type = "HC1")

ivlmws <- ivreg(nba$ws ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmws, vcov. = vcovHC, type = "HC1")

ivlmdm <- ivreg(nba$dist_miles ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmdm, vcov. = vcovHC, type = "HC1")

ivlmas <- ivreg(nba$avg_speed ~ nba$contract_year + nba$salary_current
                + nba$min + nba$age + nba$pos + nba$team
                + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                + nba$min + nba$age + nba$pos + nba$team
                + nba$name + nba$season,
                weights = nba$min)

coeftest(ivlmas, vcov. = vcovHC, type = "HC1")

ivlmaso <- ivreg(nba$avg_speed_off ~ nba$contract_year + nba$salary_current
                + nba$min + nba$age + nba$pos + nba$team
                + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                + nba$min + nba$age + nba$pos + nba$team
                + nba$name + nba$season,
                weights = nba$min)

coeftest(ivlmaso, vcov. = vcovHC, type = "HC1")

ivlmasd <- ivreg(nba$avg_speed_def ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmasd, vcov. = vcovHC, type = "HC1")


ivlmaspt <- ivreg(nba$Avg.Sec.Per.Touch ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmaspt, vcov. = vcovHC, type = "HC1")

ivlmasdpt <- ivreg(nba$Avg.Drib.Per.Touch ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmasdpt, vcov. = vcovHC, type = "HC1")

ivlmbo <- ivreg(nba$Box.Outs ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmbo, vcov. = vcovHC, type = "HC1")

ivlmobo <- ivreg(nba$OFF.Box.Outs ~ nba$contract_year + nba$salary_current
                + nba$min + nba$age + nba$pos + nba$team
                + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                + nba$min + nba$age + nba$pos + nba$team
                + nba$name + nba$season,
                weights = nba$min)

coeftest(ivlmobo, vcov. = vcovHC, type = "HC1")

ivlmdbo <- ivreg(nba$DEF.Box.Outs ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmdbo, vcov. = vcovHC, type = "HC1")

testiv <- lm(nba$contract_year ~ nba$contract_2018)

linearHypothesis(testiv, "nba$contract_2018 = 0", 
                 vcov = vcovHC, type = "HC1")

ivstrength <- linearHypothesis(testiv, "nba$contract_2018 = 0", 
                 vcov = vcovHC, type = "HC1")

summary(ivstrength)

stargazer(ivstrength)

summary(ivlmbo)

stargazer(ivlmbo, type = "latex",
          title="IV Using Box Outs as the Dependent Variables")

ivlmws48 <- ivreg(nba$ws_48 ~ nba$contract_year + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season | nba$contract_2018 + nba$salary_current
                 + nba$min + nba$age + nba$pos + nba$team
                 + nba$name + nba$season,
                 weights = nba$min)

coeftest(ivlmws48, vcov. = vcovHC, type = "HC1")
