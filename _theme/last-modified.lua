-- ページ末尾に「最終更新: 日付」を出す。
-- 先頭に日付を置くとブログ記事に見えるため、title block の日付は CSS で隠し、ここで末尾に足す。
-- YAML に last-modified-note: false と書いたページには出さない。
function Pandoc(doc)
  if doc.meta["last-modified-note"] == false then
    return doc
  end
  local dm = doc.meta["date-modified"]
  if dm == nil then
    return doc
  end
  local text = pandoc.utils.stringify(dm)
  local para = pandoc.Para({ pandoc.Str("最終更新: " .. text) })
  local div = pandoc.Div({ para }, pandoc.Attr("", { "last-modified" }))
  table.insert(doc.blocks, div)
  return doc
end
