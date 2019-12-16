{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Data.Text.Encoding as E
import Data.Maybe
import Data.Semigroup

type Author = T.Text
type Title  = T.Text
type Html   = T.Text

data Book = Book {
    author :: Author
  , title  :: Title
} deriving (Show)

wrap :: T.Text -> T.Text -> T.Text
wrap tag content = mconcat [open, content, close]
  where open  = "<" <> tag <> ">"
        close = "</" <> tag <> ">"

html  = wrap "html"
hhead  = wrap "head"
htitle = wrap "title"
body  = wrap "body"
p     = wrap "p"
bold  = wrap "strong"
em    = wrap "em"

bookToHtml :: Book -> Html
bookToHtml book = p content
  where t = bold (title book) 
        a = em (author book)    
        content = T.intercalate " " [t, a]

book1 :: Book
book1 = Book {
    title = "The Conspiracy Against the Human Race"
  , author = "Ligotti, Thomas"
}
book2 :: Book
book2 = Book {
    title = "A Short History of Decay"
  , author = "Cioran, Emil"
}
book3 :: Book
book3 = Book {
    title = "The Tears of Eros"
  , author = "Bataille, Georges"
}

booksToHtml :: [Book] -> Html
booksToHtml books = html content
  where title' = htitle "Books"
        head'  = hhead title' <> "<meta charset='utf-8'/>"
        books' = mconcat (map bookToHtml books)
        content = head' <> (body books')

main :: IO ()
main = Tio.writeFile "books.html" (booksToHtml [book1, book2, book3])
