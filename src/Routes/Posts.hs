{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Posts (postsRouter, PostsRouter) where

import Components.Content.Header (contentHeader)
import Data.Aeson (decode)
import qualified Data.WC.Category as Cat
import Data.WC.Product (WpPost (..), categories, name)
import Http (getWpResponse)
import Lucid (Html, ToHtml (toHtml), button_, class_, div_, span_, table_, tbody_, td_, th_, thead_, tr_)
import Router (routePage)
import Servant (Get, Headers, Post, addHeader, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXPush)
import State (AppM)

type PostsRouter =
  "posts" :> Get '[HTML] (Html ())
    :<|> "posts" :> Post '[HTML] (Headers '[HXPush] (Html ()))

wcGetPosts :: AppM (Maybe [WpPost])
wcGetPosts = do
  res <- getWpResponse "/products"
  pure $ decode res

content :: AppM (Html ())
content = do
  maybe contentEmpty withPosts <$> wcGetPosts
  where
    wrapper :: Html () -> Html ()
    wrapper h = contentHeader "Posts" "" <> div_ [class_ "w-full overflow-auto"] h
    contentEmpty :: Html ()
    contentEmpty = wrapper "No posts found."
    withPosts :: [WpPost] -> Html ()
    withPosts d =
      wrapper $
        table_ [class_ "w-full caption-bottom text-sm"] $
          thead_
            [class_ "[&amp;_tr]:border-b"]
            ( tr_ [class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted"] $
                th_ [class_ "h-12 px-4 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0"] "Title"
                  <> th_ [class_ "h-12 px-4 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0"] "Category"
                  <> th_ [class_ "h-12 px-4 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0"] "Quantity"
                  <> th_ [class_ "h-12 px-4 align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0 text-right"] "Amount"
                  <> th_ [class_ "h-12 px-4 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0"] ""
            )
            <> tbody_ [class_ "[&amp;_tr:last-child]:border-0"] postList
      where
        postList = foldl1 (<>) $ fmap postItem d

postItem :: WpPost -> Html ()
postItem (WpPost {name = cName, categories = cCategories, price = cPrice}) =
  tr_ [class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted"] $
    td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cName)
      <> td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0 flex gap-1"] (foldl1 (<>) $ fmap categoryItem cCategories)
      <> td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0 text-right"] (toHtml cPrice)
      <> td_
        [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0 text-right"]
        (button_ [class_ "inline-flex items-center justify-center text-sm font-medium ring-offset-background transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50 h-10 px-2 py-1 bg-transparent text-black hover:bg-gray-200 active:bg-gray-300 rounded"] "Edit")
  where
    categoryItem :: Cat.WpCategory -> Html ()
    categoryItem (Cat.WpCategory {Cat.name = cat}) = span_ [class_ "px-2 py-1 bg-red-200 text-red-800 rounded-md"] $ toHtml cat

type GET = AppM (Html ())

get :: GET
get = routePage <$> content

type POST = AppM (Headers '[HXPush] (Html ()))

post :: POST
post = addHeader "/posts" <$> content

postsRouter :: GET :<|> POST
postsRouter = get :<|> post