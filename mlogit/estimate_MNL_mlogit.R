install.packages("mlogit")

library(mlogit)
library(dfidx)
library(readr)
library(dplyr)

# データの読み込み
df <- readr::read_csv("../data/data.csv", locale = locale(encoding = "Shift_JIS"))

# 着目する交通手段に限定（鉄道・バス・乗用車等）
df <- df[(df$mode_name == "幹線バス" | df$mode_name == "鉄道" | df$mode_name == "乗用車等"),]

# 不要な列を削除（文字列・使用しないLOS変数）
df <- df[, !(colnames(df) %in% c('O_name', 'D_name', 'purpose_name', 'sex_name', 'ship_time', 'ship_cost', 'air_time', 'air_cost'))]

# 欠損値を含む行を削除
df <- na.omit(df)

# トリップ数に応じてデータを展開（各行を'num'回繰り返す）
df <- df[rep(seq_len(nrow(df)), df$num), , drop = FALSE]

# 末尾に選択肢識別子がくるように列名を変更
df <- 
  df |> 
  rename(
    time_car = car_time, 
    time_bus = bus_time, 
    time_rail = rail_time, 
    cost_car = car_cost, 
    cost_bus = bus_cost, 
    cost_rail = rail_cost
  )

df$stime_car <- df$time_car / 60
df$stime_bus <- df$time_bus / 60
df$stime_rail <- df$time_rail / 60
df$scost_car <- df$cost_car / 10000
df$scost_bus <- df$cost_bus / 10000
df$scost_rail <- df$cost_rail / 10000

code2alt <- c(`5`="car", `4`="bus", `2`="rail")
df$choice_alt <- unname(code2alt[as.character(df$mode_code)])
str(df)

df_mlogit <- mlogit.data(
  df, 
  shape = "wide", 
  choice ="choice_alt", 
  varying = c(9:20), 
  sep="_"
)

str(df_mlogit)

mode_MNL <- mlogit(
  formula  = choice_alt ~ stime + scost | 1,  # | 1 で ASCs を入れる
  data     = df_mlogit,
  reflevel = "bus"
)
summary(mode_MNL)

ll <- summary(mode_MNL)$logLik

num_samples <- nrow(df)
ll0 <- num_samples * log(1 / 3)
as.numeric( 1 - (ll / ll0) )

cf <- coef(mode_MNL)
asc_car  <- unname(cf["(Intercept):car"])
asc_rail <- unname(cf["(Intercept):rail"])
b_time   <- unname(cf["stime"])
b_cost   <- unname(cf["scost"])

df$pred_V_car  <- asc_car  + b_time * df$stime_car  + b_cost * df$scost_car
df$pred_V_bus  <-            b_time * df$stime_bus  + b_cost * df$scost_bus 
df$pred_V_rail <- asc_rail + b_time * df$stime_rail + b_cost * df$scost_rail

deno <- exp(df$pred_V_car) + exp(df$pred_V_bus) + exp(df$pred_V_rail)

df$pred_P_car <- exp(df$pred_V_car) / deno
df$pred_P_bus <- exp(df$pred_V_bus) / deno
df$pred_P_rail <- exp(df$pred_V_rail) / deno

pred_P <- cbind(
  bus  = df$pred_P_bus,
  car  = df$pred_P_car,
  rail = df$pred_P_rail
)

df$pred_alt <- colnames(pred_P)[max.col(pred_P, ties.method = "first")]

stopifnot(
  sum(df$pred_alt == df$choice_alt, na.rm = TRUE) +
    sum(df$pred_alt != df$choice_alt, na.rm = TRUE) ==
    num_samples
)

accuracy <- sum(df$pred_alt == df$choice_alt, na.rm = TRUE) / num_samples * 100
accuracy
