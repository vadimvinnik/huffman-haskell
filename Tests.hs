import Huffman.Tree

alphabet = [' '..'z']

text = "Haskell is a computer programming language. In particular, it is a polymorphically statically typed, lazy, purely functional language, quite different from most other programming languages. The language is named for Haskell Brooks Curry, whose work in mathematical logic serves as a foundation for functional languages. Haskell is based on the lambda calculus, hence the lambda we use as a logo."

stringToCodesAsStrings = itemsToCodes "" ('0':) ('1':) alphabet

codes = stringToCodesAsStrings text
