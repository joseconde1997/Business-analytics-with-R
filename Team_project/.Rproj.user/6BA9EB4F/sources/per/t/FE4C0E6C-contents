library(readxl)
library(data.table)
library(tidyverse)
library(dplyr)
library(tidyr)
library(formattable)
library(knitr)

jaya <- read_excel("jayalaxmi.xlsx")
jaya_cor <- select(jaya,income_per_acre,loan_amount , crop_insured ,
                     training_on_sericulture , own_vermi_compost , 
                     bio_fertilizers, mechanization , mulberry_diseases , 
                     affected_by_pest , rearing_cost , temp_mgmt , chawki_bivol)
mcor <- round(cor(jaya_cor),2)
view(mcor)
upper.tri(mcor)
upper <- mcor
upper[upper.tri(mcor)] <- ""
upper <- as.data.frame(upper)
view(upper)
customGreen0 = "#DeF7E9"
customGreen = "green"
customRed = "pink"
formattable(upper, list(
            `loan_amount`= color_tile(customRed,customGreen),
            `loan_repaid`= color_tile(customRed,customGreen),
            `crop_insured`= color_tile(customRed,customGreen),
            `years_of_exp_in_sericulture`= color_tile(customRed,customGreen),
            `training_on_sericulture`= color_tile(customRed,customGreen),
            `krishi_pond`= color_tile(customRed,customGreen),
            `borewell_recharge`= color_tile(customRed,customGreen),
            `rain_harvesting`= color_tile(customRed,customGreen),
            `own_compost_manure`= color_tile(customRed,customGreen),
            `own_vermi_compost`= color_tile(customRed,customGreen),
            `trenching_mulching`= color_tile(customRed,customGreen),
            `bio_fertilizers`= color_tile(customRed,customGreen),
            `mechanization`= color_tile(customRed,customGreen),
            `mulberry_diseases`= color_tile(customRed,customGreen),
            `affected_by_pest`= color_tile(customRed,customGreen),
            `rearing_cost`= color_tile(customRed,customGreen),
            `instrument_mgmt_cost`= color_tile(customRed,customGreen),
            `temp_mgmt`= color_tile(customRed,customGreen),
            `humidity_mgmt`= color_tile(customRed,customGreen),
            `airvent_temp_mgmt`= color_tile(customRed,customGreen),
            `rotary_mounting`= color_tile(customRed,customGreen),
            `seri_total_subsidy`= color_tile(customRed,customGreen),
            `income_per_acre`= color_tile(customRed,customGreen),
            `chawki_bivol`= color_tile(customRed,customGreen)))
