-- Copied from Kieran Healy (https://github.com/kjhealy/)
-- Reformat all heading text
function Header(el)
  if el.level == 1 then
      table.insert(el.classes, "inv-title")
      el.attributes["data-background-color"] = "#b0514a"
      return el
  end
end
