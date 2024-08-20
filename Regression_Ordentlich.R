p_load(pscl, boot, car, lmtest)

df4 <- df4 %>%
  mutate(`CDU/CSU` = CDU + CSU)

df4 <- df4[!(df4$half_year %in% c("2024-H1", "2024-H2")), ]
output <- output[!(output$half_year %in% c("2024-H1", "2024-H2")), ]

table(df4$ladepunkte)
df4 <- df4[df4$ladepunkte <= 200, ]
table(df4$ladepunkte == 0)
plot(df4$ladepunkte)
output$Monat <- df4$Monat
df4$Monat <- output$Monat
# negativ-binomial
p_load(stargazer)
m1 <- glm.nb(ladepunkte ~ Monat + pkw + bipEW + Bundesland + `CDU/CSU` + FDP + AfD + GRÜNE + SPD + `Freie Wähler` + LINKE, data = df4)
stargazer(m1, type= "text")

# Effektstärken evaluieren
p_load(ggeffects)
predGRÜNE <- ggpredict(m1, terms="GRÜNE")
predSPD <- ggpredict(m1, terms="SPD")
plot(predGRÜNE)
plot(predSPD)

# Testen
p_load(zoo, lmtest)
mnull <- glm.nb(ladepunkte ~ Monat + pkw + bipEW, data = df4)
lrtest(mnull, m1)

mnull0 <- glm.nb(ladepunkte ~ 1, data = df4)
lrtest(mnull, m1)
anova(mnull, m1)
# Degrees of freedom for the test (difference in the number of parameters between m1 and mnull0)
df <- 7  # Based on your output

# Significance level (commonly set to 0.05)
alpha <- 0.01

# Calculate the critical value using the qchisq function
critical_value <- qchisq(1 - alpha, df)



# Deutlich besser angepasst als ein Poisson-Modell
mpoisson <- glm(ladepunkte ~ Monat + pkw + bipEW + Bundesland + `CDU/CSU` + FDP + AfD + GRÜNE + SPD + `Freie Wähler` + LINKE, family = "poisson", data = df4)
lrtest(mpoisson, m1)

stargazer(m1, mnull, mnull0, mpoisson, type="html", out="./graphics/m1.html")

# Linearität
plot(m1, 1)

# Multikollinearität
p_load(car)
stargazer(vif(m1), type="html", out="./graphics/vif.html")

stargazer(mod, type = "html", out="C://Your//Path//Name.html")

# Varianzhomogenität
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(m1, 2)
p_load(olsrr)
ols_plot_resid_fit(m1)

# Residuen
# Extract deviance residuals
residuals_deviance <- residuals(m1, type = "deviance")

# Extract Pearson residuals
residuals_pearson <- residuals(m1, type = "pearson")

# Extract raw residuals
residuals_raw <- residuals(m1, type = "response")

# Plot residuals against fitted values
fitted_values <- fitted(m1)

# Deviance residuals vs. fitted values
plot(fitted_values, residuals_deviance, 
     xlab = "Vorhersagewerte", 
     ylab = "Devianzresiduen",
     main = "Standardisierte Residuen | Vorhersagewerte")
abline(h = 0, col = "red")

# Pearson residuals vs. fitted values
plot(fitted_values, residuals_pearson, 
     xlab = "Fitted values", 
     ylab = "Pearson Residuals",
     main = "Pearson Residuals vs Fitted Values")
abline(h = 0, col = "red")

# Raw residuals vs. fitted values
plot(fitted_values, residuals_raw, 
     xlab = "Vorhersagewerte", 
     ylab = "Residuen",
     main = "Überprüfung der Varianzhomogenität")
abline(h = 1, col = "red")


