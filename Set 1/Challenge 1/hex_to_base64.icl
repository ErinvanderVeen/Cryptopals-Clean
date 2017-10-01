implementation module hex_to_base64

import StdEnv
import Text.Encodings.Base64

Start
# bytes = hexToBytes "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
= toBase64 bytes

toBase64 :: [Char] -> String
toBase64 words
# words = toString words
= base64Encode words

hexToBytes :: String -> [Char]
hexToBytes s
# s = fromString s
| isOdd (length s) = abort "Hexadecimal string must have an even number of characters"
# s = map fromHex s
# s = groupPerTwo s
= map toChar s

groupPerTwo :: [a] -> [(a, a)]
groupPerTwo [] = []
groupPerTwo [_ : []] = abort "Cannot group per two if list has odd number of elements"
groupPerTwo [a, b : s] = [(a, b) : groupPerTwo s]

toChar :: (Int, Int) -> Char
toChar (h, l) = fromInt (16 * h + l)

fromHex :: Char -> Int
fromHex 'a' = 10
fromHex 'b' = 11
fromHex 'c' = 12
fromHex 'd' = 13
fromHex 'e' = 14
fromHex 'f' = 15
fromHex c
| isDigit c = digitToInt c
= abort "Character is cannot be converted to an Integer"

