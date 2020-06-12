reg.dws <- lm(formula = dws ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba, weights = nba$min)
nba$predicted_regdws <- predict(reg.dws, nba)

reg.ows <- lm(formula = ows ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba, weights = nba$min)
nba$predicted_regows <- predict(reg.ows, nba)

reg.ws <- lm(formula = ws ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
             + salary_current, data = nba, weights = nba$min)
nba$predicted_regws <- predict(reg.ws, nba)

plot(nba$predicted_regdws, nba$dws, col='blue', pch = 16, 
     main = "Actual Defensive Win Shares Against Predicted Defensive Win Shares Using WLS",
     ylab = "Defensive Win Shares", 
     xlab = "Predicted Defensive Win Shares")

plot(nba$predicted_regows, nba$ows, col='green', pch = 16, 
     main = "Actual OFfensive Win Shares Against Predicted Offensive Win Shares Using WLS",
     ylab = "Offensive Win Shares", 
     xlab = "Predicted Offensive Win Shares")

plot(nba$predicted_regws, nba$ws, col='red', pch = 16, 
     main = "Actual Win Shares Against Predicted Win Shares Using WLS",
     ylab = "Win Shares", 
     xlab = "Predicted Win Shares")