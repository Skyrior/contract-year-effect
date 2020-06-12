## Charles Shi, Jonathan Liu, Terry Culpepper, and Sean Choi
## Contract Year Effect in the NBA
##
## -------------------------------------------------------------------------
##
## Data
##
## The raw file can be downloaded at 
## https://github.com/Skyrior/contract-year-effect/blob/master/player_stats.csv
## The cleaned+merged file can be downloaded at
## https://github.com/Skyrior/contract-year-effect/blob/master/nba.csv
##
## -------------------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(janitor)
library(stringr)
library(hdm) ## for double lasso
library(ggplot2)
library(stargazer)
library(fastDummies)
library(lmtest)
library(geepack)
library(broom)

## -------------------------------------------------------------------------
##
## Suppress LM summary
##
## https://stackoverflow.com/questions/35388010/hide-some-coefficients-in-regression-summary-while-still-returning-call-r-squar
##
## -------------------------------------------------------------------------

my.summary.lm = function (x, digits = max(3L, getOption("digits") - 3L), 
                          symbolic.cor = x$symbolic.cor, 
                          signif.stars = getOption("show.signif.stars"), 
                          my.rows, ...)                     # NOTE NEW my.rows ARGUMENT
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
  cat(if (!is.null(x$weights) && diff(range(x$weights))) 
    "Weighted ", "Residuals:\n", sep = "")
  if (rdf > 5L) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    rq <- if (length(dim(resid)) == 2L) 
      structure(apply(t(resid), 1L, quantile), dimnames = list(nam, 
                                                               dimnames(resid)[[2L]]))
    else {
      zz <- zapsmall(quantile(resid), digits + 1L)
      structure(zz, names = nam)
    }
    print(rq, digits = digits, ...)
  }
  else if (rdf > 0L) {
    print(resid, digits = digits, ...)
  }
  else {
    cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
    cat("\n")
  }
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    if (nsingular <- df[3L] - df[1L]) 
      cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
          sep = "")
    else cat("\nCoefficients:\n")
    coefs <- x$coefficients[my.rows,]                      # SUBSET my.rows
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, 
                                                              colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
                 na.print = "NA", ...)
  }
  cat("\nResidual standard error:", format(signif(x$sigma, 
                                                  digits)), "on", rdf, "degrees of freedom")
  cat("\n")
  if (nzchar(mess <- naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
                                           digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
                                                                                       digits = digits), "on", x$fstatistic[2L], "and", 
        x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                                                          x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                                                       digits = digits))
    cat("\n")
  }
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl <- format(round(correl, 2), nsmall = 2, 
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}

## -------------------------------------------------------------------------
##
## Data Import
##
## -------------------------------------------------------------------------

nba <- read.csv("nba.csv")
nba <- clean_names(nba)

nbaiv <- read.csv("ivnba.csv")
nbaiv <- clean_names(nbaiv)
## -------------------------------------------------------------------------
##
## Test for heteroskedasticity
##
## -------------------------------------------------------------------------

hetero2 <- lm(formula = avg_drib_per_touch ~ contract_year + as.factor(name) + pos + as.factor(season) +salary_current, data = nba)

bptest(hetero2)



## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Usage Rate
##
## -------------------------------------------------------------------------

reg.usage <- lm(formula = usg ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba, weights = nba$min)
summary.usage <- my.summary.lm(summary(reg.usage), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.usage))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Defensive Win Shares
##
## -------------------------------------------------------------------------

reg.dws <- lm(formula = dws ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.dws <- my.summary.lm(summary(reg.dws), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.dws))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Offensive Win Shares
##
## -------------------------------------------------------------------------

reg.ows <- lm(formula = ows ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba, weights = nba$min)
summary.ows <- my.summary.lm(summary(reg.ows), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.ows))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Total Win Shares
##
## -------------------------------------------------------------------------

reg.ws <- lm(formula = ws ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba, weights = nba$min)
summary.ws <- my.summary.lm(summary(reg.ws), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.ws))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Distance (feet)
##
## -------------------------------------------------------------------------

reg.distfeet <- lm(formula = dist_feet ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
             + salary_current, data = nba, weights = nba$min)
summary.distfeet <- my.summary.lm(summary(reg.distfeet), 
                            my.rows=grep("contract_year|min|season|salary_current",
                                         names(coef(reg.distfeet))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Distance (feet): Defensive
##
## -------------------------------------------------------------------------

reg.distdef <- lm(formula = dist_miles_def ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
                   + salary_current, data = nba, weights = nba$min)
summary.distdef <- my.summary.lm(summary(reg.distdef), 
                                  my.rows=grep("contract_year|min|season|salary_current",
                                               names(coef(reg.distdef))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Distance (feet): Offensive
##
## -------------------------------------------------------------------------

reg.distoff <- lm(formula = dist_miles_off ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
                  + salary_current, data = nba, weights = nba$min)
summary.distoff <- my.summary.lm(summary(reg.distoff), 
                                 my.rows=grep("contract_year|min|season|salary_current",
                                              names(coef(reg.distoff))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Speed
##
## -------------------------------------------------------------------------

reg.avgs <- lm(formula = avg_speed ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.avgs <- my.summary.lm(summary(reg.avgs), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.avgs))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Speed: Defense
##
## -------------------------------------------------------------------------

reg.sdef <- lm(formula = avg_speed_def ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.sdef <- my.summary.lm(summary(reg.sdef), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.sdef))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Speed: Offense
##
## -------------------------------------------------------------------------

reg.soff <- lm(formula = avg_speed_off ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.soff <- my.summary.lm(summary(reg.soff), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.soff))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Win Shares per 48 Mins
##
## -------------------------------------------------------------------------

reg.ws48 <- lm(formula = ws_48 ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.ws48 <- my.summary.lm(summary(reg.ws48), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.ws48))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Box Outs
##
## -------------------------------------------------------------------------

reg.box <- lm(formula = box_outs ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.box <- my.summary.lm(summary(reg.box), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.box))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Off Box Outs
##
## -------------------------------------------------------------------------

reg.obox <- lm(formula = off_box_outs ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba, weights = nba$min)
summary.obox <- my.summary.lm(summary(reg.obox), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.obox))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Def Box Outs
##
## -------------------------------------------------------------------------

reg.dbox <- lm(formula = def_box_outs ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.dbox <- my.summary.lm(summary(reg.dbox), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.dbox))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Seconds per Touch
##
## -------------------------------------------------------------------------

reg.avgt <- lm(formula = avg_sec_per_touch ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.avgt <- my.summary.lm(summary(reg.avgt), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.avgt))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Dribbles per Touch
##
## -------------------------------------------------------------------------

reg.avgd <- lm(formula = avg_drib_per_touch ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba, weights = nba$min)
summary.avgd <- my.summary.lm(summary(reg.avgd), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.avgd))))

## -------------------------------------------------------------------------
##
## Export Weighted Fixed Effect OLS as tables (w/ Stargazer)
##
## -------------------------------------------------------------------------

cat(stargazer(reg.dbox, reg.obox, reg.box, dep.var.labels = c("Defensive Box Outs",
                                                              "Offensive Box Outs",
                                                              "Box Outs"),
              omit = c("name", "season", "pos"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"),
              add.lines = list(c("Player Fixed Effects", "Yes", "Yes", "Yes"), 
                               c("Year Fixed Effects", "Yes", "Yes", "Yes"),
                               c("Position Fixed Effects", "Yes", "Yes", "Yes")),
              report = "vcsp*",
              ci = TRUE,
              title = "Using Box Outs as the Dependent Variables"),
    label = 'regboxout',
    sep = '\n', file = "tables/boxout.txt")

cat(stargazer(reg.ws, reg.ows, reg.dws, reg.ws48, dep.var.labels = c("Win Shares",
                                                              "Offensive Win Shares",
                                                              "Defensive Win Shares",
                                                              "Win Shares per 48 Minutes"),
              omit = c("name", "season", "pos"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"),
              add.lines = list(c("Player Fixed Effects", "Yes", "Yes", "Yes", "Yes"), 
                               c("Year Fixed Effects", "Yes", "Yes", "Yes", "Yes"),
                               c("Position Fixed Effects", "Yes", "Yes", "Yes", "Yes")),
              report = "vcsp*",
              ci = TRUE,
              title = "Using Win Shares as the Dependent Variables"),
    label = 'regwinshares',
    sep = '\n', file = "tables/winshares.txt")

cat(stargazer(reg.avgs, reg.soff, reg.sdef, dep.var.labels = c("Average Speed", 
                                                               "Offensive Speed", "Defensive Speed"),
              omit = c("name", "season", "pos"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"),
              add.lines = list(c("Player Fixed Effects", "Yes", "Yes", "Yes"), 
                               c("Year Fixed Effects", "Yes", "Yes", "Yes"),
                               c("Position Fixed Effects", "Yes", "Yes", "Yes")),
              report = "vcsp*",
              ci = TRUE,
              title = "Using Speed Metrics as the Dependent Variable"),
    label = 'regspeed',
    sep = '\n', file = "tables/speed.txt")

cat(stargazer(reg.distdef, reg.distoff, reg.distfeet, dep.var.labels = c("Distance: Defensive", "Distance: Offensive", "Distance"),
              omit = c("name", "season", "pos"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"),
              add.lines = list(c("Player Fixed Effects", "Yes", "Yes", "Yes"), 
                               c("Year Fixed Effects", "Yes", "Yes", "Yes"),
                               c("Position Fixed Effects", "Yes", "Yes", "Yes")),
              report = "vcsp*",
              ci = TRUE,
              title = "Using Distance as the Dependent Variable"),
    label = 'regdist',
    sep = '\n', file = "tables/dist.txt")

cat(stargazer(reg.avgd, reg.avgt, reg.usage, dep.var.labels = c("Average Seconds per Dribble", "Average Seconds per Touch", "Usage Rate"),
              omit = c("name", "season", "pos"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"),
              add.lines = list(c("Player Fixed Effects", "Yes", "Yes", "Yes"), 
                               c("Year Fixed Effects", "Yes", "Yes", "Yes"),
                               c("Position Fixed Effects", "Yes", "Yes", "Yes")),
              report = "vcsp*",
              ci = TRUE,
              title = "Using Dribbles, Touches, and Usage Rate as the Dependent Variable"),
    label = 'regmisc',
    sep = '\n', file = "tables/misc.txt")

## -------------------------------------------------------------------------
##
## Get nba.LASSO
##
## -------------------------------------------------------------------------

nba.LASSO <- nba %>%
  filter(team != "TOT") %>%
  group_by(team, season) %>%
  mutate_all(funs(weighted.mean(., min))) %>%
  summarize_all(mean) %>%
  select_if(~sum(!is.na(.)) > 0)

nba.LASSO.iv <- nbaiv %>%
  filter(team != "TOT") %>%
  group_by(team, season) %>%
  mutate_all(funs(weighted.mean(., min))) %>%
  summarize_all(mean) %>%
  select_if(~sum(!is.na(.)) > 0)

## -------------------------------------------------------------------------
##
## Generalized estimating equation
##
## -------------------------------------------------------------------------

## we require the data to be sorted by the cluster.
nba <- nba %>%
  arrange(team)

gee.ws <- geeglm(formula = ws ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
                 + salary_current,
                 family = gaussian,
                 data = nba,
                 weights = nba$min,
                 id = team)
summary.geews <- my.summary.lm(summary(gee.ws), 
                               my.rows=grep("contract_year|min|season|salary_current|Intercept",
                                            names(coef(gee.ws))))
broom::confint_tidy(gee.ws, parm = "contract_year")
broom::confint_tidy(gee.ws, parm = "min")
broom::confint_tidy(gee.ws, parm = "salary_current")
broom::confint_tidy(gee.ws, parm = "(Intercept)")

gee.ows <- geeglm(formula = ows ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
                 + salary_current,
                 family = gaussian,
                 data = nba,
                 weights = nba$min,
                 id = team)
summary.geeows <- my.summary.lm(summary(gee.ows), 
                               my.rows=grep("contract_year|min|season|salary_current|Intercept",
                                            names(coef(gee.ows))))
broom::confint_tidy(gee.ows, parm = "contract_year")
broom::confint_tidy(gee.ows, parm = "min")
broom::confint_tidy(gee.ows, parm = "salary_current")
broom::confint_tidy(gee.ows, parm = "(Intercept)")

gee.dws <- geeglm(formula = dws ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
                  + salary_current,
                  family = gaussian,
                  data = nba,
                  weights = nba$min,
                  id = team)
summary.geedws <- my.summary.lm(summary(gee.dws), 
                                my.rows=grep("contract_year|min|season|salary_current|Intercept",
                                             names(coef(gee.dws))))
broom::confint_tidy(gee.dws, parm = "contract_year")
broom::confint_tidy(gee.dws, parm = "min")
broom::confint_tidy(gee.dws, parm = "salary_current")
broom::confint_tidy(gee.dws, parm = "(Intercept)")


gee.usg <- geeglm(formula = usg ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
                  + salary_current,
                  family = gaussian,
                  data = nba,
                  weights = nba$min,
                  id = team)
broom::confint_tidy(gee.ws, parm = "contract_year")


## -------------------------------------------------------------------------
##
## Double LASSO: WS
##
## -------------------------------------------------------------------------

## We require X to be the matrix of controls. That would be:
## Height
## Weight
## Age
## PER - player efficiency rating
## min - minutes played
## TS - True shooting percentage
## x3p_ar - 3-point attempt rate
## f_tr - Free throw attempt rate
## orb, drb, trb - rebound percentage
## ast, stl, blk - assist, steal, block
## tov - turnover
## usg - usage percentage
## obpm, dbpm, bpm - box plus/minus
## vorp - value over replacement player
## dist_miles, dist_miles_off, dist_miles_def
## avg_speed, avg_speed_off, avg_speed_def
## salary_current,
## box_outs, off_box_outs, def_box_outs
## team_reb_on_box_outs,
## player_reb_on_box_outs,
## touches
## front_ct_touches,
## time_of_poss,
## avg_sec_per_touch,
## avg_drib_per_touch,
## pts_per_touch,
## elbow_touches,
## post_ups,
## paint_touches,
## pts_per_elbow_touch,
## pts_per_post_touch,
## pts_per_paint_touch

X.controls <- c("height", "weight", "age",
       "min", "f_tr", "orb", "drb", "trb",
       "ast", "stl", "blk", "tov", "dist_miles",
       "dist_miles_off", "dist_miles_def", "salary_current",
       "box_outs", "off_box_outs", "def_box_outs",
       "team_reb_on_box_outs", "player_reb_on_box_outs",
       "touches", "front_ct_touches", "time_of_poss",
       "avg_sec_per_touch", "avg_drib_per_touch",
       "pts_per_touch", "elbow_touches",
       "post_ups", "paint_touches", "pts_per_elbow_touch",
       "pts_per_post_touch", "pts_per_paint_touch")

X <- nba.LASSO %>%
  ungroup %>%
  select(all_of(X.controls))

X <- as.matrix(X)

## Y is the matrix of outcome.

Y.outcome <- c("ws")

Y <- nba.LASSO %>%
  ungroup %>%
  select(all_of(Y.outcome))

Y <- as.matrix(Y)

## Finally D is the matrix of inference variable.

D.inference <- c("contract_year")

D <- nba.LASSO %>%
  ungroup %>%
  select(all_of(D.inference))

D <- as.matrix(D)

lasso.ws <- rlassoEffect(x = X, y = Y, d = D, method = "double selection",
                         I3 = NULL,
                         post = TRUE)

print(lasso.ws)
summary(lasso.ws)
confint(lasso.ws)
plot(lasso.ws)
print(lasso.ws$selection.index)

## ----------------

Y.outcome <- c("ows")

Y <- nba.LASSO %>%
  ungroup %>%
  select(all_of(Y.outcome))

Y <- as.matrix(Y)

lasso.ows <- rlassoEffect(x = X, y = Y, d = D, method = "double selection",
                         I3 = NULL,
                         post = TRUE)

print(lasso.ows)
summary(lasso.ows)
confint(lasso.ows)
plot(lasso.ows)
print(lasso.ows$selection.index)

## ----------------

Y.outcome <- c("dws")

Y <- nba.LASSO %>%
  ungroup %>%
  select(all_of(Y.outcome))

Y <- as.matrix(Y)

lasso.dws <- rlassoEffect(x = X, y = Y, d = D, method = "double selection",
                          I3 = NULL,
                          post = TRUE)

print(lasso.dws)
summary(lasso.dws)
confint(lasso.dws)
plot(lasso.dws)
print(lasso.dws$selection.index)


## -------------------------------------------------------------------------
##
## Double LASSO: IV
##
## -------------------------------------------------------------------------

lasso.iv.ws <- rlassoIV(formula = ws ~ height + weight + age +
                        min + f_tr + orb + drb + trb +
                        ast + stl + blk +  tov + dist_miles+dist_miles_off+dist_miles_def+salary_current+
                        box_outs+off_box_outs+def_box_outs+team_reb_on_box_outs+player_reb_on_box_outs+
                        touches+front_ct_touches+time_of_poss+avg_sec_per_touch+avg_drib_per_touch+
                        pts_per_touch+elbow_touches+post_ups+paint_touches+pts_per_elbow_touch+
                        pts_per_post_touch+pts_per_paint_touch + contract_year | height + weight + age +
                          min + f_tr + orb + drb + trb +
                          ast + stl + blk +  tov + dist_miles+dist_miles_off+dist_miles_def+salary_current+
                          box_outs+off_box_outs+def_box_outs+team_reb_on_box_outs+player_reb_on_box_outs+
                          touches+front_ct_touches+time_of_poss+avg_sec_per_touch+avg_drib_per_touch+
                          pts_per_touch+elbow_touches+post_ups+paint_touches+pts_per_elbow_touch+
                          pts_per_post_touch+pts_per_paint_touch + contract_2018,
                        data = nba.LASSO.iv, select.Z = FALSE, select.X = TRUE)

print(lasso.iv.ws)
summary(lasso.iv.ws)
confint(lasso.iv.ws)

