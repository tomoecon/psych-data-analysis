# マグカップに対する支払意思額の架空実験。実際の研究結果の再現ではない。
# コード: MIT、生成する模擬データ: CC0。
# Rscript data/make_anchoring.R [出力先] でCSVを保存する。source()では保存しない。
# 教材とCSVの入出力を統一するため、readrを使う。
make_anchoring <- function(seed = 20260922L) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  # 丸め前の条件付き平均の真値: 950 + 0.30 * 提示額 - 7 * 制限時間。
  # 誤差は独立な平均0、標準偏差200円の正規乱数。回答は10円刻みに丸める。
  # 時間の負の係数は教材用の設定であり、実際の心理的効果の主張ではない。
  # 交互作用は設けない。負値の切り上げ、外れ値除去、シードの選別はしない。
  make_amount <- function(anchor_yen, time_limit_sec) {
    amount <- round((950 + 0.30 * anchor_yen - 7 * time_limit_sec +
                       stats::rnorm(length(anchor_yen), sd = 200)) / 10) * 10
    if (any(amount < 0)) stop("負の回答額があります。生成設定を検討してください。")
    amount
  }
  n <- 300L
  anchor_yen <- sample(seq(500L, 3000L, by = 10L), n, replace = TRUE)
  time_limit_sec <- sample(10L:60L, n, replace = TRUE)
  regression <- data.frame(
    id = seq_len(n), anchor_yen = anchor_yen, time_limit_sec = time_limit_sec,
    willingness_yen = make_amount(anchor_yen, time_limit_sec)
  )
  # 比較例は別の参加者。各群100人の均等割付を無作為に並べ、時間は35秒に固定。
  high_anchor <- sample(rep(0L:1L, each = 100L))
  two_anchor <- ifelse(high_anchor == 0L, 1000L, 2500L)
  two_groups <- data.frame(
    id = seq_along(high_anchor), anchor_yen = two_anchor,
    time_limit_sec = 35L, high_anchor = high_anchor,
    willingness_yen = make_amount(two_anchor, 35L)
  )
  anchor_condition <- sample(rep(c("A_low", "B_middle", "C_high"), each = 100L))
  three_anchor <- unname(c(A_low = 1000L, B_middle = 1750L, C_high = 2500L)[anchor_condition])
  three_groups <- data.frame(
    id = seq_along(anchor_condition), anchor_yen = three_anchor,
    time_limit_sec = 35L, anchor_condition = anchor_condition,
    willingness_yen = make_amount(three_anchor, 35L)
  )
  list(anchoring = regression, anchoring_two_groups = two_groups,
       anchoring_three_groups = three_groups)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args)) args[[1L]] else "data"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  datasets <- make_anchoring()
  for (name in names(datasets)) {
    readr::write_csv(datasets[[name]], file.path(output_dir, paste0(name, ".csv")))
  }
}
