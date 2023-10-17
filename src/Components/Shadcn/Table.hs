{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Table where

import           Lucid (Html, Term (termWith), class_, div_)

tableWrapper_ :: Html () -> Html ()
tableWrapper_ = div_ [class_ "relative w-full overflow-auto "]

table_ :: Term arg res => arg -> res
table_ = termWith "table" [class_ "w-full whitespace-nowrap "]

tableHeader_ :: Term arg res => arg -> res
tableHeader_ = termWith "thead" [class_ "[&_tr]:border-b "]

tableBody_ :: Term arg res => arg -> res
tableBody_ = termWith "tbody" [class_ "[&_tr:last-child]:border-0 "]

tableFooter_ :: Term arg res => arg -> res
tableFooter_ = termWith "tfoot" [class_ "bg-primary font-medium text-primary-foreground "]

tableRow_ :: Term arg res => arg -> res
tableRow_ = termWith "tr" [class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted "]

tableHead_ :: Term arg res => arg -> res
tableHead_ = termWith "th" [class_ "h-10 px-2 text-left align-middle font-medium text-muted-foreground [&:has([role=checkbox])]:pr-0 [&>[role=checkbox]]:translate-y-[2px] "]

tableCell_ :: Term arg res => arg -> res
tableCell_ = termWith "td" [class_ "p-2 align-middle [&:has([role=checkbox])]:pr-0 [&>[role=checkbox]]:translate-y-[2px] "]

tableCaption_ :: Term arg res => arg -> res
tableCaption_ = termWith "caption" [class_ "mt-4 text-sm text-muted-foreground "]
