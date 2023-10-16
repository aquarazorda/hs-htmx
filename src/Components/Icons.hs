{-# LANGUAGE OverloadedStrings #-}

module Components.Icons where

import           Data.Text  (Text)
import           Lucid      (Attribute, Html, Term (term), height_, width_,
                             xmlns_)
import           Lucid.Base (makeAttribute)

viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

fill_ :: Text -> Attribute
fill_ = makeAttribute "fill"

d_ :: Text -> Attribute
d_ = makeAttribute "d"

stroke_ :: Text -> Attribute
stroke_ = makeAttribute "stroke"

strokeLinejoin_ :: Text -> Attribute
strokeLinejoin_ = makeAttribute "strokeLinejoin"

strokeWidth_ :: Text -> Attribute
strokeWidth_ = makeAttribute "strokeWidth"

strokeLinecap_ :: Text -> Attribute
strokeLinecap_ = makeAttribute "strokeLinecap"

x1 :: Text -> Attribute
x1 = makeAttribute "x1"

x2 :: Text -> Attribute
x2 = makeAttribute "x2"

y1 :: Text -> Attribute
y1 = makeAttribute "y1"

y2 :: Text -> Attribute
y2 = makeAttribute "y2"

path_ :: [Attribute] -> Html ()
path_ attrs = term "path" attrs ""

g_ :: Term arg result => arg -> result
g_ = term "g"

line_ :: Term arg result => arg -> result
line_ = term "line"

currentColor :: Attribute
currentColor = fill_ "currentColor"

type Icon = [Attribute] -> Html ()

svg :: [Attribute] -> Html () -> Icon
svg svgArgs path attrs = term "svg" ([xmlns_ "http://www.w3.org/2000/svg", width_ "1em", height_ "1em"] <> svgArgs <> attrs) path

vortexIcon :: Icon
vortexIcon = svg [viewBox_ "0 0 256 256"] $
  path_
    [currentColor, d_ "M248 144a8 8 0 0 1-16 0a96.11 96.11 0 0 0-96-96a88.1 88.1 0 0 0-88 88a80.09 80.09 0 0 0 80 80a72.08 72.08 0 0 0 72-72a64.07 64.07 0 0 0-64-64a56.06 56.06 0 0 0-56 56a48.05 48.05 0 0 0 48 48a40 40 0 0 0 40-40a32 32 0 0 0-32-32a24 24 0 0 0-24 24a16 16 0 0 0 16 16a8 8 0 0 0 8-8a8 8 0 0 1 0-16a16 16 0 0 1 16 16a24 24 0 0 1-24 24a32 32 0 0 1-32-32a40 40 0 0 1 40-40a48.05 48.05 0 0 1 48 48a56.06 56.06 0 0 1-56 56a64.07 64.07 0 0 1-64-64a72.08 72.08 0 0 1 72-72a80.09 80.09 0 0 1 80 80a88.1 88.1 0 0 1-88 88a96.11 96.11 0 0 1-96-96A104.11 104.11 0 0 1 136 32a112.12 112.12 0 0 1 112 112Z"]

categoryIcon :: Icon
categoryIcon = svg [viewBox_ "0 0 32 32"] $
  path_ [currentColor, d_ "M29 30H19a1 1 0 0 1-1-1V19a1 1 0 0 1 1-1h10a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1zm-9-2h8v-8h-8v8zM8 30c-3.308 0-6-2.692-6-6s2.692-6 6-6s6 2.692 6 6s-2.692 6-6 6zm0-10c-2.206 0-4 1.794-4 4s1.794 4 4 4s4-1.794 4-4s-1.794-4-4-4zm14-6H10a1 1 0 0 1-.857-1.515l6.002-10.003C15.338 2.16 15.669 2 16 2s.662.16.855.482l6.002 10.003A1 1 0 0 1 22 14zm-10.234-2h8.468L16 4.944L11.766 12z"]

productIcon :: Icon
productIcon = svg [viewBox_ "0 0 256 256"] $
  path_ [currentColor, d_ "M128 24a104 104 0 1 0 104 104A104.11 104.11 0 0 0 128 24Zm0 192a88 88 0 1 1 88-88a88.1 88.1 0 0 1-88 88Zm0-144a56.06 56.06 0 0 0-56 56a8 8 0 0 1-16 0a72.08 72.08 0 0 1 72-72a8 8 0 0 1 0 16Zm72 56a72.08 72.08 0 0 1-72 72a8 8 0 0 1 0-16a56.06 56.06 0 0 0 56-56a8 8 0 0 1 16 0Zm-40 0a32 32 0 1 0-32 32a32 32 0 0 0 32-32Zm-48 0a16 16 0 1 1 16 16a16 16 0 0 1-16-16Z"]

foldersIcon :: Icon
foldersIcon = svg [viewBox_ "0 0 24 24"] $
  g_ [fill_ "none", stroke_ "currentColor", strokeLinecap_ "round", strokeLinejoin_ "round", strokeWidth_ "2"] $ do
    path_ [d_ "M8 17h12a2 2 0 0 0 2-2V9a2 2 0 0 0-2-2h-3.93a2 2 0 0 1-1.66-.9l-.82-1.2a2 2 0 0 0-1.66-.9H8a2 2 0 0 0-2 2v9c0 1.1.9 2 2 2Z"]
    path_ [d_ "M2 8v11c0 1.1.9 2 2 2h14"]

dialogX :: Icon
dialogX = svg [viewBox_ "0 0 24 24", fill_ "none", stroke_ "currentColor", strokeWidth_ "2", strokeLinecap_ "round"] $ do
  line_ [x1 "18", x2 "6", y1 "6", y2 "18"] ""
  line_ [x1 "6", x2 "18", y1 "6", y2 "18"] ""

doubleArrowLeft :: Icon
doubleArrowLeft = svg [viewBox_ "0 0 24 24"] $ do
  path_ [currentColor, d_ "M17.59 18L19 16.59L14.42 12L19 7.41L17.59 6l-6 6z"]
  path_ [currentColor, d_ "m11 18l1.41-1.41L7.83 12l4.58-4.59L11 6l-6 6z"]

doubleArrowRight :: Icon
doubleArrowRight = svg [viewBox_ "0 0 24 24"] $ do
  path_ [currentColor, d_ "M6.41 6L5 7.41L9.58 12L5 16.59L6.41 18l6-6z"]
  path_ [currentColor, d_ "m13 6l-1.41 1.41L16.17 12l-4.58 4.59L13 18l6-6z"]

arrowLeft :: Icon
arrowLeft = svg [viewBox_ "0 0 24 24"] $ do
  path_ [currentColor, d_ "M15.41 16.59L10.83 12l4.58-4.59L14 6l-6 6l6 6l1.41-1.41z"]

arrowRight :: Icon
arrowRight = svg [viewBox_ "0 0 24 24"] $ do
  path_ [currentColor, d_ "M8.59 16.59L13.17 12L8.59 7.41L10 6l6 6l-6 6l-1.41-1.41z"]
