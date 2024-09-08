-- Borrowed from Kieran Healy (github.com/kjhealy/)
-- Reformat all heading text
function Header(el)
  if el.level == 1 then
      table.insert(el.classes, "inv-title")
      el.attributes["data-background-color"] = "#106470"
      return el
  end
end