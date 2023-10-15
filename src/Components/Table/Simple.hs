{-# LANGUAGE OverloadedStrings #-}

module Components.Table.Simple where

import           Data.Text (Text)
import           Lucid     (Html, ToHtml (toHtml), class_, div_, table_, tbody_,
                            th_, thead_, tr_)

data TableHeader = TableHeader {
  title :: Text,
  cls   :: Text
}

simpleTable :: [TableHeader] -> Html () -> Html ()
simpleTable hs children = div_ [class_ "rounded-md border"] $
        div_ [class_ "relative w-full overflow-auto"] $ do
          table_ [class_ "w-full caption-bottom text-sm"] $ do
            thead_
              [class_ "[&amp;_tr]:border-b"]
              ( tr_ [class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted"] $
                  foldl1 (<>) $
                    fmap (\h -> th_
                      [ class_ $ "h-10 px-2 text-left align-middle font-medium text-muted-foreground [&:has([role=checkbox])]:pr-0 [&>[role=checkbox]]:translate-y-[2px]" <> cls h ]
                      (toHtml $ title h)
                    ) hs
              )
            tbody_ [class_ "[&_tr:last-child]:border-0"] children
