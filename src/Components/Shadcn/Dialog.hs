{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Dialog (closeModalOnClick, dialog_, dialogHeader_, dialogDescription_, dialogFooter_, dialogTitle_) where

import           Components.Icons      (dialogX)
import           Data.Functor.Identity (Identity)
import           Lucid                 (Attribute, HtmlT, Term (termWith),
                                        button_, class_, div_, id_, span_)
import           Lucid.Base            (Html, makeAttribute)
import           Lucid.Hyperscript     (__)

closeModalOnClick :: Attribute
closeModalOnClick = __ "on click add @data-state='closed' to #dialog-content add @data-state='closed' to #dialog-overlay wait 0.1s remove #dialog-content then remove #dialog-overlay"

overlay :: Html ()
overlay = div_ [
  id_ "dialog-overlay",
  makeAttribute "data-state" "open",
  closeModalOnClick,
  class_ "cursor-pointer fixed inset-0 z-50 bg-background/80 backdrop-blur-sm data-[state=open]:animate-in data-[state=closed]:animate-out data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0"
  ] ""

closeButton :: HtmlT Identity ()
closeButton = button_ [
    id_ "dialog-close-button",
    closeModalOnClick,
    class_ "absolute right-4 top-4 rounded-sm opacity-70 ring-offset-background transition-opacity hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:pointer-events-none data-[state=open]:bg-accent data-[state=open]:text-muted-foreground"
  ] $ do
    dialogX [class_ "h-4 w-4"]
    span_ [class_ "sr-only"] "Close"

dialog_ :: Html () -> Html ()
dialog_ cs = do
  overlay
  div_ [
      id_ "dialog-content",
      makeAttribute "data-state" "open",
      class_ "fixed left-[50%] top-[50%] z-50 grid w-full max-w-lg translate-x-[-50%] translate-y-[-50%] gap-4 border bg-background p-6 shadow-lg duration-200 data-[state=open]:animate-in data-[state=closed]:animate-out data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0 data-[state=closed]:zoom-out-95 data-[state=open]:zoom-in-95 data-[state=closed]:slide-out-to-left-1/2 data-[state=closed]:slide-out-to-top-[48%] data-[state=open]:slide-in-from-left-1/2 data-[state=open]:slide-in-from-top-[48%] sm:rounded-lg md:w-full"
    ] $ do
      cs
      closeButton

dialogHeader_ :: Term arg result => arg -> result
dialogHeader_  = termWith "div" [
  class_ "flex flex-col space-y-1.5 text-center sm:text-left "
  ]

dialogFooter_ :: Term arg result => arg -> result
dialogFooter_  = termWith "div" [
  class_ "flex flex-col-reverse sm:flex-row sm:justify-end sm:space-x-2 "
  ]

dialogTitle_ :: Term arg result => arg -> result
dialogTitle_  = termWith "h2" [
  class_ "text-lg font-semibold leading-none tracking-tight "
  ]

dialogDescription_ :: Term arg result => arg -> result
dialogDescription_  = termWith "p" [
  class_ "text-sm text-muted-foreground "
  ]
