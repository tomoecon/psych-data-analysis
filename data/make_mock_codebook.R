# 教材ページを正本として、配布用のMarkdownコードブックを生成する。
# base Rのみを使い、Quartoのpre-renderからプロジェクト直下で実行する。
source_path <- "data/mock-survey.qmd"
output_path <- "data/mock_survey_codebook.md"
lines <- readLines(source_path, encoding = "UTF-8", warn = FALSE)

section_between <- function(start, end) {
  start_line <- which(lines == start)
  end_line <- which(lines == end)
  if (length(start_line) != 1L || length(end_line) != 1L || end_line <= start_line) {
    stop("コードブックの抽出範囲を確認してください: ", start, " / ", end)
  }
  lines[seq.int(start_line + 1L, end_line - 1L)]
}

# 概要は目的の最初の段落を共有する。ページ固有の操作案内は含めない。
purpose <- section_between("## 目的", "## ファイルとRの準備")
first_paragraph <- purpose[which(nzchar(purpose))[1L]]
variables <- c(
  "## データの内容",
  section_between("## データの内容", "## 得点の計算方法")
)
scoring <- section_between("## 得点の計算方法", "## AIへの指示例")
# ページ内の作業案内は、データ説明書には含めない。
scoring <- scoring[!startsWith(scoring, "ここでは、計算の考え方を確認します。")]
body <- c(variables, "## 得点の計算方法", scoring)

# Quarto固有の見出し属性・折りたたみを、通常のMarkdownに直す。
body <- sub('^::: \\{.*title="([^"]+)".*\\}$', "#### \\1", body)
body <- body[body != ":::"]
body <- sub(" \\{#variables\\}$", "", body)
body <- sub(" \\{#response-options\\}$", "", body)
body <- gsub("[回答の選択肢](#response-options)", "回答の選択肢", body, fixed = TRUE)
output <- c(
  "# 模擬調査データのコードブック", "",
  "対象ファイル: `mock_survey.csv`", "",
  "## データの概要", "", first_paragraph, "",
  "1,000人分のデータで、1行が1人、1列が1変数です。最初の行には列名が入っています。", "",
  body
)
# 内容が同じ場合は書き換えず、プレビューの不要な再生成を避ける。
if (!file.exists(output_path) ||
    !identical(readLines(output_path, encoding = "UTF-8", warn = FALSE), output)) {
  writeLines(enc2utf8(output), output_path, useBytes = TRUE)
}
