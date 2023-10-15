{-# LANGUAGE OverloadedStrings #-}

module Components.Table.Simple where

import           Components.Shadcn.Table (tableBody_, tableHead_, tableHeader_,
                                          tableRow_, tableWrapper_, table_)
import           Data.Text               (Text)
import           Lucid                   (Html, ToHtml (toHtml), class_, div_)

data TableHeader = TableHeader {
  title :: Text,
  cls   :: Text
}

simpleTable :: [TableHeader] -> Html () -> Html ()
simpleTable hs children = div_ [class_ "rounded-md border"] $
        tableWrapper_ $ do
          table_ $ do
            tableHeader_
              ( tableRow_ $
                  foldl1 (<>) $
                    fmap (\h -> tableHead_ [ class_ $ cls h ] (toHtml $ title h)) hs
              )
            tableBody_ children
