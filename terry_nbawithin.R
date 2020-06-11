install.packages("plm")
library(plm)



lm(data = nba, 
    nba$usg ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x,
    weights = nba$min)

plmusg <- plm(data = nba, model = "within", 
    nba$usg ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmdws <- plm(data = nba, model = "within", 
    nba$dws ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmows <- plm(data = nba, model = "within", 
    nba$ows ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmws <- plm(data = nba, model = "within", 
    nba$ws ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmdm <- plm(data = nba, model = "within", 
    nba$dist_miles ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmas <- plm(data = nba, model = "within", 
    nba$avg_speed ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmaspt <- plm(data = nba, model = "within", 
    nba$avg_sec_per_touch ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmadpt <- plm(data = nba, model = "within", 
    nba$avg_drib_per_touch ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmbo <- plm(data = nba, model = "within", 
    nba$box_outs ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmobo <- plm(data = nba, model = "within", 
    nba$off_box_outs ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)

plmdbo <- plm(data = nba, model = "within", 
    nba$def_box_outs ~ nba$contract_year + nba$salary_current + nba$min
    + nba$name + nba$season + nba$age.x + nba$pos 
    + nba$team.x + nba$g,
    index = "season",
    effect = "twoways",
    weights = nba$min)







