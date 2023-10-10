{-# LANGUAGE OverloadedStrings #-}

module Components.Table.Simple where

import           Data.Text (Text)
import           Lucid     (Html, ToHtml (toHtml), class_, table_, tbody_, th_,
                            thead_, tr_)

data TableHeader = TableHeader {
  title :: Text,
  cls   :: Text
}

simpleTable :: [TableHeader] -> Html () -> Html ()
simpleTable hs children = table_ [class_ "w-full caption-bottom text-sm"] $
          thead_
            [class_ "[&amp;_tr]:border-b"]
            ( tr_ [class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted"] $
                foldl1 (<>) $
                  fmap (\h -> th_
                    [ class_ $ "h-12 px-4 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0 " <> cls h ]
                    (toHtml $ title h)
                  ) hs
            )
            <> tbody_ [class_ "[&amp;_tr:last-child]:border-0"] children
