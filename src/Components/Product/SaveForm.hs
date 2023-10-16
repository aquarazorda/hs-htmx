{-# LANGUAGE OverloadedStrings #-}

module Components.Product.SaveForm where

import           Components.Shadcn.Button   (cnBtn)
import           Components.Shadcn.Input    (cnInput)
import           Components.Shadcn.Label    (cnLabel)
import           Components.Shadcn.Textarea (cnTextarea)
import           Components.Shadcn.Toggle   (ToggleSize (DefaultSize),
                                             ToggleVariant (Outline), cnToggle)
import           Lucid                      (Html, blockquote_, class_, div_,
                                             footer_, for_, img_, p_, rows_,
                                             src_, type_, value_)

productSaveForm :: Html ()
productSaveForm = div_ [class_ "container h-full overflow-hidden rounded-[0.5rem] border bg-background shadow relative hidden flex-col items-center justify-center md:grid lg:max-w-none lg:grid-cols-2 lg:px-0"] $ do
        div_ [class_ "relative hidden h-full flex-col bg-muted p-10 text-white dark:border-r lg:flex"] $ do
          div_ [class_ "absolute inset-0 bg-zinc-900"] ""
          div_ [class_ "relative z-20 flex items-center text-lg font-medium"] "Sinéad O'Connor – I Do Not Want What I Haven't Got"
          img_ [class_ "w-64 z-20 m-auto", src_ "https://i.discogs.com/xMJKhqd9Uw8Zb-6GiM21DdTi38PKlvW78DdND3-xqJs/rs:fit/g:sm/q:90/h:600/w:600/czM6Ly9kaXNjb2dz/LWRhdGFiYXNlLWlt/YWdlcy9SLTg2NzE1/MS0xNDg3MTc1MTk3/LTc5MjUuanBlZw.jpeg"]
          div_ [class_ "relative z-20 mt-auto"] $ do
            blockquote_ [class_ "space-y-2"] $ do
              p_ [class_ "text-lg"] "Modify release to your liking, add price and stock in order to save."
              footer_ [class_ "text-sm"] "You can always edit release later."
        div_ [class_ "flex flex-col h-full p-4"] $ do
          div_ [class_ "flex w-full flex-col justify-center space-y-6"] $ do
            div_ [class_ "flex flex-col space-y-2"] $ do
              cnLabel [for_ "release-title"] "Title"
              cnInput [value_ "Sinéad O'Connor – I Do Not Want What I Haven't Got"]
            div_ [class_ "flex gap-2"] $ do
              div_ [class_ "flex flex-col flex-1 space-y-2"] $ do
                cnLabel [for_ "release-title"] "Label"
                cnInput [value_ "Ensign / 210 547"]
              div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                cnLabel [for_ "release-title"] "Year"
                cnInput [value_ "1990", type_ "number"]
            div_ [class_ "flex gap-2"] $ do
              div_ [class_ "flex flex-col flex-1 space-y-2"] $ do
                cnLabel [for_ "release-title"] "Category"
                div_ [class_ "flex gap-1 flex-wrap"] $ do
                  cnToggle Outline DefaultSize "Rock"
                  cnToggle Outline DefaultSize "Pop"
              div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                cnLabel [for_ "release-title"] "Price"
                cnInput [value_ "19.99", type_ "number"]
            div_ [class_ "flex flex-col space-y-2"] $ do
              cnLabel [for_ "release-title"] "Image URL"
              cnInput [value_ "https://i.discogs.com/xMJKhqd9Uw8Zb-6GiM21DdTi38PKlvW78DdND3-xqJs/rs:fit/g:sm/q:90/h:600/w:600/czM6Ly9kaXNjb2dz/LWRhdGFiYXNlLWlt/YWdlcy9SLTg2NzE1/MS0xNDg3MTc1MTk3/LTc5MjUuanBlZw.jpeg"]
            div_ [class_ "flex flex-col space-y-2"] $ do
              cnLabel [for_ "release-title"] "Description"
              cnTextarea [value_ "Here goes some html text", class_ "h-full", rows_ "10"]
          div_ [class_ "flex w-full mt-auto justify-end"] $ do
            cnBtn "Save"

