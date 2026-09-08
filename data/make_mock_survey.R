# 授業用の架空調査データ。実在する回答者・尺度・人口構成は再現しない。
# コード: MIT、生成する模擬データ: CC0。
# 公開リポジトリのルートで Rscript data/make_mock_survey.R を実行する。
# 第1引数で出力先を変更できる。source() では保存せず、関数だけを読み込む。
# CSVの入出力を教材と統一するため、保存時にreadrを使用する。

make_mock_survey <- function(seed = 20260908L) {
  # 乱数の種類も固定する。生成順序:
  # 年齢・地域 → 教育 → 就業 → 年収 → 潜在変数・項目 → 補助属性。
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  decade_counts <- c(167L, 167L, 167L, 167L, 166L, 166L)
  region_a_counts <- c(84L, 83L, 84L, 83L, 83L, 83L)
  decade <- rep(2:7, times = decade_counts)
  region <- unlist(lapply(seq_along(decade_counts), function(j) {
    c(rep("A", region_a_counts[j]),
      rep("B", decade_counts[j] - region_a_counts[j]))
  }), use.names = FALSE)
  n <- length(decade)
  age <- decade * 10L + sample.int(10L, n, replace = TRUE) - 1L
  order <- sample.int(n)
  age <- age[order]
  region <- region[order]

  education_values <- c(9L, 12L, 14L, 16L, 18L)
  education_weights <- c(0.12, 0.42, 0.15, 0.26, 0.05)
  education_years <- vapply(age, function(current_age) {
    eligible <- education_values + 6L <= current_age
    sample(education_values[eligible], 1L, prob = education_weights[eligible])
  }, integer(1))

  a <- (age - 50) / 10
  e <- (education_years - 12) / 2
  # 真値（生の年齢・教育年数）: 切片3.05、年齢-0.075、教育0.125。
  employment_probability <- stats::plogis(0.8 - 0.75 * a + 0.25 * e)
  employed <- stats::rbinom(n, 1L, employment_probability)
  employment_status <- rep("not_employed", n)
  # 非正規雇用を基準にする。
  # 正規の真値: 切片-0.45、年齢-0.015、教育0.150。
  # 自営の真値: 切片-1.55、年齢0.025、教育0.025。
  logits <- cbind(nonregular = 0, regular = 0.6 - 0.15 * a + 0.30 * e,
                  self_employed = 0.25 * a + 0.05 * e)
  probabilities <- exp(logits) / rowSums(exp(logits))
  for (i in which(employed == 1L)) {
    employment_status[i] <- sample(colnames(probabilities), 1L,
                                   prob = probabilities[i, ])
  }

  d <- education_years - 12
  c_age <- age - 50
  # 算術平均の真値。年齢と教育を省いた授業用回帰とは別のモデル。
  mu_by_status <- cbind(
    regular = 500 + 25 * d - 0.25 * c_age^2,
    nonregular = 210 + 12 * d - 0.09 * c_age^2,
    self_employed = 420 + 20 * d - 0.16 * c_age^2,
    not_employed = 160
  )
  mu <- mu_by_status[cbind(seq_len(n), match(employment_status,
                                            colnames(mu_by_status)))]
  sigma <- c(regular = 0.30, nonregular = 0.45,
             self_employed = 0.60, not_employed = 0.55)[employment_status]
  income_error <- stats::rnorm(n, sd = sigma)
  income_unrounded <- mu * exp(income_error - sigma^2 / 2)
  not_employed <- which(employed == 0L)
  income_unrounded[not_employed] <- income_unrounded[not_employed] *
    (1L - stats::rbinom(length(not_employed), 1L, 0.30))
  income <- as.integer(round(income_unrounded))

  # 固定した理論上の年齢の基準を使う。標本ごとのscale()は使わない。
  z <- (age - 49.5) / sqrt((60^2 - 1) / 12)
  u_support <- stats::rnorm(n, sd = sqrt(0.91))
  u_stress <- stats::rnorm(n, sd = sqrt(0.84))
  u_satisfaction <- stats::rnorm(n, sd = sqrt(0.60))
  support <- 0.3 * z + u_support
  stress <- -0.4 * support + u_stress
  b <- ifelse(region == "A", -0.2, -0.5)
  life_satisfaction <- 0.3 * support + 0.3 * z + b * stress + u_satisfaction
  # 地域差はbだけ。残差分散と測定規則は共通。潜在分散はそろえない。
  latent <- data.frame(support, stress, life_satisfaction)
  loadings <- c(0.80, 0.75, 0.70, 0.85, 0.65)
  continuous <- items <- vector("list", 15L)
  item_names <- unlist(lapply(names(latent), function(scale) {
    paste0(scale, "_", 1:5)
  }), use.names = FALSE)
  k <- 0L
  for (scale in names(latent)) {
    for (j in seq_along(loadings)) {
      k <- k + 1L
      # 切片0、誤差分散1-lambda^2。交差負荷・残差相関は設けない。
      response <- loadings[j] * latent[[scale]] +
        stats::rnorm(n, sd = sqrt(1 - loadings[j]^2))
      continuous[[k]] <- response
      positive <- as.integer(cut(response, c(-Inf, -1.2, -0.4, 0.4, 1.2, Inf),
                                 labels = FALSE))
      # CSVには第5項目の元回答を保存する。得点化は分析時に行う。
      items[[k]] <- if (j == 5L) 6L - positive else positive
    }
  }
  names(items) <- names(continuous) <- item_names

  gender <- sample(c("female", "male", "other"), n, replace = TRUE,
                   prob = c(0.49, 0.49, 0.02))
  marital_weights <- rbind(
    c(0.75, 0.24, 0.01, 0.00), c(0.35, 0.60, 0.05, 0.00),
    c(0.20, 0.70, 0.09, 0.01), c(0.12, 0.70, 0.13, 0.05),
    c(0.08, 0.66, 0.14, 0.12), c(0.05, 0.55, 0.12, 0.28)
  )
  marital_status <- vapply(age, function(current_age) {
    sample(c("never_married", "married", "divorced", "widowed"), 1L,
           prob = marital_weights[current_age %/% 10L - 1L, ])
  }, character(1))
  household_size <- vapply(marital_status, function(status) {
    if (status == "married") {
      sample(2:6, 1L, prob = c(0.40, 0.25, 0.20, 0.10, 0.05))
    } else {
      sample(1:6, 1L, prob = c(0.55, 0.20, 0.12, 0.08, 0.04, 0.01))
    }
  }, integer(1), USE.NAMES = FALSE)
  hours_settings <- rbind(regular = c(40, 5, 30, 60),
                          nonregular = c(20, 7, 5, 35),
                          self_employed = c(40, 12, 10, 70))
  weekly_work_hours <- integer(n)
  for (i in which(employed == 1L)) {
    setting <- hours_settings[employment_status[i], ]
    repeat {
      hours <- round(stats::rnorm(1L, setting[1], setting[2]))
      if (hours >= setting[3] && hours <= setting[4]) break
    }
    weekly_work_hours[i] <- as.integer(hours)
  }

  survey <- data.frame(id = seq_len(n), region, age, education_years, income,
                       gender, employment_status, marital_status,
                       household_size, weekly_work_hours, items,
                       row.names = NULL, check.names = FALSE)
  # 補助属性の制約は授業上の簡略化であり、現実の不可能条件ではない。
  # 真値・誤差・離散化前の応答は作成側の検証だけに使い、CSVに保存しない。
  list(survey = survey, latent = latent, continuous = as.data.frame(continuous),
       age_z = z, income_mu = mu, income_sigma = unname(sigma),
       income_unrounded = income_unrounded,
       errors = data.frame(u_support, u_stress, u_satisfaction, income_error))
}

if (sys.nframe() == 0L) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("CSVの保存にはreadrが必要です。install.packages('readr')で導入してください。")
  }
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L) stop("引数はCSVの出力先1つだけを指定してください。")
  output <- if (length(args) == 1L) args[1] else "data/mock_survey.csv"
  generated <- make_mock_survey()
  stopifnot(nrow(generated$survey) == 1000L, ncol(generated$survey) == 25L,
            !anyNA(generated$survey))
  readr::write_csv(generated$survey, output, eol = "\n")
  message("模擬調査データを保存しました: ", output)
}
