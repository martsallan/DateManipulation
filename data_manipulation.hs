import Data.Time.Clock
import Data.Time.Calendar

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return.toGregorian.utctDay

data Data = MyDate String Int Int Int | CommemorativeData String Int Int Int Bool
    deriving(Show)
----------------------

name :: Data -> String
name (MyDate x _ _ _) = x
name (CommemorativeData a _ _ _ _) = a

day :: Data -> Int
day (MyDate _ y _ _) = y
day (CommemorativeData _ b _ _ _) = b

month :: Data -> Int
month (MyDate _ _ z _) = z
month (CommemorativeData _ _ c _ _) = c

year :: Data -> Int
year (MyDate _ _ _ w) = w
year (CommemorativeData _ _ _ d _) = d

holiday :: Data -> Bool
holiday (CommemorativeData _ _ _ _ e) = e

--------------------

compare_dates :: Data -> Data -> Int
compare_dates (MyDate _ x y z) (CommemorativeData _ a b c _) 
    | (x == a) && (y == b) && (z == c) = 0
    | (z == c) && (y == b) && (x > a) = 1
    | (z == c) && (y == b) && (x < a) = -1
    | (z == c) && (y > b) = 1
    | (z == c) && (y < b) = -1
    | (z > c) = 1
    | otherwise = -1

--------------------

add_list :: Data -> [Data] -> [Data]
add_list (CommemorativeData a b c d e) y = y++[(CommemorativeData a b c d e)]

-- remove :: String -> [Data] -> [Data]
-- remove _ [] = []
-- remove x [(CommemorativeData a b c d )]  
--     | (elem x [(CommemorativeData a b c d )] == True) = [filter (/= x) [(CommemorativeData a b c d )]]
--     | otherwise [(CommemorativeData a b c d )]

isHoliday :: Data -> Bool
isHoliday (CommemorativeData _ _ _ _ e) = e

notWorkedHours :: [Data] -> Int
notWorkedHours [] = 0
notWorkedHours (x:xs) 
    | isHoliday x = 8 + notWorkedHours xs
    | otherwise = notWorkedHours xs

----------

toString :: Data -> [Char]
toString (MyDate _ x y z) = "Current date: " ++ show x ++ "/" ++ show y ++ "/" ++ show z
toString (CommemorativeData _ a b c d) = show a ++ "/" ++ show b ++ "/" ++ show c ++ " - is Holiday? " ++ show d

--main--

main::IO()

main = do
  (integer_year, month, day) <- date

  let year = (fromIntegral integer_year)
 
  let actual = MyDate "actual" day month year
  let christmas = CommemorativeData "christmas" 25 12 year True
  let newyear = CommemorativeData "newyear" 31 12 (year+1) True

  putStrLn(show (toString actual))
  putStrLn(show (toString christmas))
  putStrLn(show (toString newyear))

--   -- comparação das datas (MinhaData -> DataComemorativa)

  let comp = compare_dates actual christmas
  putStrLn (show comp)

--   -- adiciona na lista de datas comemorativas (imutabilidade)

  let list = []
  let list2 = add_list christmas list
  let list3 = add_list newyear list2

--   -- gera o num de horasNaoTrabalhadas 

  let hours = notWorkedHours list3
  putStrLn (show hours)

