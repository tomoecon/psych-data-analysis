# psych-data-analysis

心理学のためのデータ解析（R と生成 AI）の授業サイトの原稿です。立命館大学「心理学データ解析法Ⅱ」「心理学統計法（展開）」で使用しています。

サイト: https://tomoecon.github.io/psych-data-analysis/ （準備中）

## 構成

サイドバーの節と同じ形でディレクトリを分けています。

```
index.qmd            はじめに（トップ）
courses/             科目別の日程（学部・大学院）
start/               はじめる: R と Positron のセットアップ、生成 AI と統計プログラミング、データの前処理
regression/          回帰分析: 単回帰モデル、対数変換、多項式回帰
glm/                 一般化線形モデル: ロジスティック回帰、多項ロジスティック回帰
factor-analysis/     因子分析: 因子分析の基礎、探索的因子分析
sem/                 構造方程式モデリング: SEM の基礎、確認的因子分析、多母集団同時分析
advanced/            発展: AI アシスタントとエージェント
data/                模擬調査データ（CSV、生成スクリプト、説明ページ）
_templates/          in R ページの雛形
_theme/              ハイライト配色、Lua フィルタ、head に入れるスクリプト
theme.scss           配色・書体
_quarto.yml          サイト設定
_quarto-preview.yml  下書き（draft: true）も表示する手元確認用プロファイル
```

手元で確認するには `quarto preview --profile preview` を実行します。書き上がっていないページは `draft: true` にしてあり、通常の描画と公開には含まれません。

## ライセンス

- 本文と図: [CC BY-NC-ND 4.0](LICENSE)
- コードと設定ファイル（`AGENTS.md`、`_quarto.yml` を含む）: [MIT](LICENSE-CODE)
- 模擬データ: CC0

## 貢献について

Pull Request は受け付けていません。誤りの指摘は Issue でお願いします。

## AI の利用

教材は Claude Code などの AI を使って作成し、著者が検証しています。エージェント向けの指示は [AGENTS.md](AGENTS.md) にあります。

## 著者

森 知晴（立命館大学総合心理学部）
