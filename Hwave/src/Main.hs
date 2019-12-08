import Data.List

main :: IO()
main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What the title?"
  title <- getLine
  print "Who is the author?"
  sender <- getLine
  print (makeLetter recipient title sender) 

toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart title   = "Thanks for buying " ++ title ++ ".\n"
fromPart sender  = "From " ++ sender ++ ".\n"

makeLetter r t s = toPart r ++
                   bodyPart t ++
                   fromPart s 

collatz n =
  if a 
  then n - 2
  else 3 * n + 1
  where a = even n

dd n = (\n -> n * 2) n * 2

compLastNames a b = 
  compare n1 n2
  where
    n1 = snd a
    n2 = snd b


subseq start end arr = 
  drop start (take end arr) 

inFirstHalf el arr = 
  elem el (take half arr)
  where half = (length arr) `div` 2

ttail (_:xs) = xs
ttail [] = []

gdc x 0 = abs x
gdc x y = gdc b (mod a b)
  where a = abs x
        b = abs y

ddrop 0 ls = ls
ddrop _ [] = []
ddrop n (_:xs) = pred n `ddrop` xs

llen [] = 0
llen (_:xs) = 1 + llen xs

ttake 0 _ = []
ttake _ [] = []
ttake n (x:xs) = x:rest 
  where rest = pred n `ttake` xs

ccycle [] = error "empty list"
ccycle (x:xs) = first:rest
  where first = x
        rest = ccycle (xs ++ [x])


rreverse [] = []
rreverse (x:xs) = (++) (rreverse xs) [x]

fastFib n _ 0 = n
fastFib n o c = fastFib o (n + o) (pred c)

ffib n = fastFib 0 1 n

mmap _ [] = []
mmap fn (x:xs) = (fn x):(mmap fn xs)
