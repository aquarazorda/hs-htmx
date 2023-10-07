{-# LANGUAGE OverloadedStrings #-}

module Navbar (navBar) where

import Data.Text (Text)
import Lucid
  ( Html,
    ToHtml (toHtml),
    aside_,
    button_,
    class_,
    div_,
    h1_,
    nav_,
  )
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_)

-- <aside class="sticky top-0 h-screen w-56 bg-gray-100 text-gray-800 p-4">
--       <div class="flex items-center mb-4 space-x-1">
--          <img src="/placeholder.svg" width="30" height="30" alt="Company Logo" style="aspect-ratio: 30 / 30; object-fit: cover;">
--          <h1 class="text-lg font-medium">Acme</h1>
--       </div>
--       <nav class="space-y-2">
--          <button class="w-full flex items-center space-x-2 hover:bg-gray-200 active:bg-gray-300 py-2 px-2 rounded-lg text-gray-500">
--             <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class=" w-4 h-4">
--                <path d="m3 9 9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"></path>
--                <polyline points="9 22 9 12 15 12 15 22"></polyline>
--             </svg>
--             <span class="text-sm font-medium">Home</span>
--          </button>
--          <button class="w-full flex items-center space-x-2 bg-gray-200 active:bg-gray-300 py-2 px-2 rounded-lg text-gray-800">
--             <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class=" w-4 h-4">
--                <path d="M21 12V7H5a2 2 0 0 1 0-4h14v4"></path>
--                <path d="M3 5v14a2 2 0 0 0 2 2h16v-5"></path>
--                <path d="M18 12a2 2 0 0 0 0 4h4v-4Z"></path>
--             </svg>
--             <span class="text-sm font-medium">Transactions</span>
--          </button>
--          <button class="w-full flex items-center space-x-2 hover:bg-gray-200 active:bg-gray-300 py-2 px-2 rounded-lg text-gray-500">
--             <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class=" w-4 h-4">
--                <path d="M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2"></path>
--                <circle cx="9" cy="7" r="4"></circle>
--                <path d="M22 21v-2a4 4 0 0 0-3-3.87"></path>
--                <path d="M16 3.13a4 4 0 0 1 0 7.75"></path>
--             </svg>
--             <span class="text-sm font-medium">Accounts</span>
--          </button>
--          <button class="w-full flex items-center space-x-2 hover:bg-gray-200 active:bg-gray-300 py-2 px-2 rounded-lg text-gray-500">
--             <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class=" w-4 h-4">
--                <path d="M2 9a3 3 0 0 1 0 6v2a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-2a3 3 0 0 1 0-6V7a2 2 0 0 0-2-2H4a2 2 0 0 0-2 2Z"></path>
--                <path d="M13 5v2"></path>
--                <path d="M13 17v2"></path>
--                <path d="M13 11v2"></path>
--             </svg>
--             <span class="text-sm font-medium">Tax</span>
--          </button>
--       </nav>
--    </aside>

navItem :: Text -> Text -> Html ()
navItem path text =
  button_
    [ class_ "w-full flex items-center space-x-2 hover:bg-gray-200 active:bg-gray-300 py-2 px-2 rounded-lg text-gray-500",
      hxPost_ path,
      hxSwap_ "innerHTML",
      hxTarget_ "#router-outlet"
    ]
    $ h1_ [class_ "text-sm font-medium"]
    $ toHtml text

navBar :: Html ()
navBar =
  aside_ [class_ "sticky top-0 h-screen w-56 bg-gray-100 text-gray-800 p-4"] $
    div_ [class_ "flex items-center mb-4 space-x-1"] (h1_ [class_ "text-lg font-medium"] "HS-WEB")
      <> nav_
        [class_ "space-y-2"]
        (navItem "/" "Home" <> navItem "/posts" "Posts")
