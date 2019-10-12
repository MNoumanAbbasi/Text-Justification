import Data.List
import Data.Char
data Token = Word String | Blank | HypWord String deriving (Eq,Show)

-- | Converts a word into a token
word2token :: String -> Token
word2token = \word -> Word word

-- | Converts a string into a Line or list of tokens
str2line :: String -> [Token]   -- Line is a list of Token
str2line = \str ->
    map word2token (words str)

-- | Converts a token into a word
token2word :: Token -> String
token2word = \token ->
    case token of
        Word str -> str
        HypWord str -> str ++ "-"
        Blank -> " "

-- | Converts a Line back into a string
line2str :: [Token] -> String
line2str = \line ->
    unwords (map token2word line)

-- main = putStr $ show $ line2str (str2line "He who controls the past controls the future.")
-- | Computes the length of a token
tokLen :: Token -> Int
tokLen = \token ->
    case token of
        Word str -> length str
        HypWord str -> 1 + length str
        Blank -> 1

-- | Computes the length of a Line
lineLen :: [Token] -> Int
lineLen = \line -> 
    let listOfLengths = map tokLen line
    in sum listOfLengths + (length listOfLengths) - 1        
-- main = putStr $ show $ lineLen [Word "He",Word "who",HypWord "con"]

-- | Breaks Line into multiple lengths based on given width
breakLine :: Int -> [Token] -> ([Token], [Token])
breakLine = \w -> \line ->
    case line of
        x:xs | (tokLen x) <= w ->
            let newW = w - tokLen x - 1     -- - 1 for space between spaces
            in (x:(fst (breakLine newW xs)), snd (breakLine newW xs))
        _ -> ([], line)
-- main = putStr $ show $ breakLine 6 [Word "He",Word "who",Word "controls"]

-- | Given parts of a word, produces a pair of all possible ways to break up the word.
mergers :: [String] -> [(String, String)]
mergers = \list ->
    case list of
        x:y:xs -> [(x, concat (y:xs))] ++ mergers ([x ++ y] ++ xs)
        _ -> []
-- main = putStr $ show $ mergers ["co","nt","ro","ls"]

enHyp = [("controls",["co","nt","ro","ls"]), ("future",["fu","tu","re"]),("present",["pre","se","nt"])]
-- | Find the string in a list of pairs of word and its breakups
findHyp4Word :: [(String, [String])] -> String -> [String]
findHyp4Word = \list -> \str ->
    case list of
        [] -> [""]
        (word, breakups):xs | word == str -> breakups
        x:xs -> findHyp4Word xs str

-- | Converts a pair of string into a (HypWord, Word) token pair and adds any punctuation passed
word2HypToken :: String -> (String, String) -> (Token, Token)
word2HypToken = \punc -> \(str1, str2) -> (HypWord str1, Word (str2 ++ punc))

-- | Converts a Word token into its hyphenated pair based on a list passed (Also handles trailing punctuations)
hyphenate :: [(String, [String])] -> Token -> [(Token, Token)]
hyphenate = \hypMap -> \token ->
    case token of       -- first case is if trailing punctuation present and second case is otherwise
        Word w | (not.isAlpha) (last w) -> map (word2HypToken [last w]) (mergers (findHyp4Word hypMap (init w)))
        Word w -> map (word2HypToken "") (mergers (findHyp4Word hypMap w))

-- | Merge line and its hyphenated form (Used as helper functions in lineBreaks)
line2Hyph :: ([Token], [Token]) -> (Token, Token) -> ([Token], [Token])
line2Hyph = \(line1 , line2) -> \(hyp, word) -> (line1 ++ [hyp], [word])

-- | Breaks a line into its hyphenated verison
lineBreaks :: [(String, [String])] -> Int -> [Token] -> [([Token], [Token])]
lineBreaks = \hypMap -> \w -> \line ->
    let x = breakLine w line
    in filter ((<=w).lineLen.fst) (x:(map (line2Hyph x) (hyphenate enHyp (last line))))
-- main = putStr $ show $ lineBreaks enHyp 12 [Word "He",Word "who",Word "controls"]

-- | Insertion helper function (starts is an accumulator)
insertionsHelper :: a -> [a] -> [a] -> [[a]]
insertionsHelper = \c -> \starts -> \str ->
    case str of
        x:xs -> [starts ++ [c] ++ str] ++ insertionsHelper c (starts++[x]) xs
        _ -> [starts ++ [c]]

-- | Inserts a given variable at every possible place in a string
insertions :: a -> [a] -> [[a]]
insertions = \c -> \str -> insertionsHelper c [] str

-- | Removes lines with leading and trailing Blanks from a list
removeUnnBlanks :: [[Token]] -> [[Token]]
removeUnnBlanks = \list ->
    filter ((/=Blank).last) $ filter ((/=Blank).head) list

-- | Remove duplicate lines
removeDups :: [[Token]] -> [[Token]]
removeDups = map head.group

-- | Inserts a given number of blanks into the line and returns all possible insertions     // TODO Maybe insert blanks in a way that they're not inserted in start and end 
insertBlanks :: Int -> [Token] -> [[Token]]
insertBlanks = \n -> \line ->
    case n of
        0 -> [line]
        1 -> removeDups.removeUnnBlanks $ insertions Blank line
        _ -> removeDups.removeUnnBlanks $ concat ((map (insertions Blank)) (insertBlanks (n-1) line))
-- main = putStr $ show $ insertBlanks 2 [Word "He",Word "who",Word "controls"]


-- | Computes the distances between inserted blanks, counted in number of inbetween token
blankDistances :: [Token] -> [Int]
blankDistances = \line ->
    case span (/=Blank) line of
        (first,[]) -> [length first]
        (first, rem) -> [length first] ++ blankDistances (tail rem)
-- main = putStr $ show $ blankDistances [Word "He",Blank,Blank,Word "who",Word "controls"]

-- | Computes the average of values in a list
avg :: [Double] -> Double
avg xs = sum xs / (fromIntegral (length xs))

-- | Computes the variance of values in a list
var :: [Double] -> Double
var xs = avg (map ((^2).subtract (avg xs)) xs)

data Costs = Costs Double Double Double Double deriving (Eq,Show)

-- | Computes cost of introducing blanks in a line
blankCost :: [Token] -> Double
blankCost xs = fromIntegral (length.filter (==Blank) $ xs)

-- | Computes the blankProxCost (no. of tokens minus average blank distances)
blankProxCost :: [Token] -> Double
blankProxCost = \line ->
    case (length.filter (==Blank) $ line) of
        0 -> 0.0              -- in case of 0 blanks
        _ -> fromIntegral (length line) - (avg (map fromIntegral (blankDistances line)))

-- | Computes the cost of having blanks spread unevenly
blankUnevenCost :: [Token] -> Double
blankUnevenCost xs = var (map fromIntegral (blankDistances line))

-- | Computes the cost of hyphenating the last word in a line
hypCost :: [Token] -> Double
hypCost xs = case last xs of
    HypWord x -> 1.0
    _ -> 0.0

-- | Computes the score of a line based on given cost
lineBadness :: Costs -> [Token] -> Double
lineBadness = \(Costs c1 c2 c3 c4) -> \line ->
    c1*(blankCost line) + c2*(blankProxCost line) + c3*(blankUnevenCost line) + c4*(hypCost line)

line = [Word "He",Blank,Word "who",Word "controls"]
cost = Costs 1.0 1.0 1.0 1.0
-- main = putStr $ show $ lineBadness cost line
-- main = putStr $ show $ (blankCost line, blankProxCost line, blankUnevenCost line, hypCost line)

-- | Inserts blanks by first computing no. of blanks required and then inserting them
addBlanks :: [[Token]] -> Int -> [[Token]]
addBlanks = \listOfLines -> \w ->
    case listOfLines of
        [x] -> (insertBlanks (w - lineLen x) x)
        x:xs -> (insertBlanks (w - lineLen x) x) ++ addBlanks xs w

-- -- | Find the index of minimum element in list
minIndex :: [Double] -> Maybe Int
minIndex xs = elemIndex (minimum xs) xs

-- -- | Computes the best line break given the costs, hyphenation map, and the maximum line width
bestLineBreak :: Costs -> [(String, [String])] -> Int -> [Token] -> Maybe [Token]
bestLineBreak = \c -> \hypMap -> \w -> \line ->
    let lineBreakups = lineBreaks hypMap w line
        firsts = map fst lineBreakups
        allLines = addBlanks firsts w
        allCosts = map (lineBadness c) allLines
    in case minIndex allCosts of
        Just x -> Just (allLines !! x)
        Nothing -> Nothing

line2 = [Word"He",Word"who",Word"controls"]
lineBreakups = (lineBreaks enHyp 12 line2)
firsts = map fst lineBreakups
-- main = putStr $ show $ map (lineBadness cost) (addBlanks firsts 12)
main = putStr $ show $ bestLineBreak cost enHyp 12 line2
-- main = putStr $ show $ (map (lineBadness cost) firsts)
-- main = putStr $ show $ zip (zipWith (+) (map fromIntegral (map ((12-).lineLen) firsts)) (map (lineBadness cost) firsts)) firsts

-- main = putStr $ show $ map ((lineBadness cost).fst) lineWithBlanks