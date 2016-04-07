module Main where

import Data.Word (Word16)
import qualified Data.Map as M
import Data.Bits
import Test.Hspec

data Wire = Wire String deriving (Show, Ord, Eq)

data Value = Value Word16 deriving (Show)

data Literal = WireL Wire | ValueL Value deriving (Show)

data BinOp = And | Or | LShift | RShift deriving (Show)
data UnOp = Not deriving (Show)

data Expr = LiteralE Literal
          | BinOpE BinOp Literal Literal
          | UnOpE UnOp Literal
            deriving (Show)

data Program = Program (M.Map Wire Expr) deriving Show

mkProgram :: [(Wire, Expr)] -> Program
mkProgram l = Program (M.fromList l)

evalProgram :: Program -> Wire -> Value
evalProgram program@(Program m) wire = case M.lookup wire m of
  Nothing -> error "Wire pas dans program"
  Just expr -> evalExpr program expr

evalExpr :: Program -> Expr -> Value
evalExpr program (LiteralE lit) = evalLiteral program lit
evalExpr program (UnOpE op lit) = evalUnOp op (evalLiteral program lit)
evalExpr program (BinOpE op lit lit') = evalBinOp op (evalLiteral program lit) (evalLiteral program lit')

evalUnOp :: UnOp -> Value -> Value
evalUnOp Not (Value v) = Value (complement v)

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp op (Value v) (Value v') = Value (applyOp op v v')

applyOp :: BinOp -> Word16 -> Word16 -> Word16
applyOp And v v' = v .&. v'
applyOp Or v v' = v .|. v'
applyOp LShift v v' = shiftL v (fromIntegral v')
applyOp RShift v v' = shiftR v (fromIntegral v')

evalLiteral :: Program -> Literal -> Value
evalLiteral _ (ValueL v) = v
evalLiteral program (WireL wire) = evalProgram program wire

{-
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
-}

defaultProgram :: Program
defaultProgram = mkProgram [
  (Wire "x", LiteralE (ValueL (Value 123))),
  (Wire "y", LiteralE (ValueL (Value 456))),
  (Wire "d", BinOpE And (WireL (Wire "x")) (WireL (Wire "y"))),
  (Wire "e", BinOpE Or (WireL (Wire "x")) (WireL (Wire "y"))),
  (Wire "f", BinOpE LShift (WireL (Wire "x")) (ValueL (Value 2))),
  (Wire "g", BinOpE RShift (WireL (Wire "y")) (ValueL (Value 2))),
  (Wire "h", UnOpE Not (WireL (Wire "x"))),
  (Wire "i", UnOpE Not (WireL (Wire "y")))
  ]

eS :: String -> Word16
eS s = let (Value res) = evalProgram defaultProgram (Wire s)
       in res

main :: IO ()
main = hspec $ do
  describe "Test" $ do
    it "works" $ do
      eS "d" `shouldBe` 72
      eS "e" `shouldBe` 507
      eS "f" `shouldBe` 492
      eS "g" `shouldBe` 114
      eS "h" `shouldBe` 65412
      eS "i" `shouldBe` 65079
      eS "x" `shouldBe` 123
      eS "y" `shouldBe` 456

-- Program
-- Partie I (c'est fait) : evaluation
-- Partie II Robustess
-- Partie III Parsing
-- Partie IV Vrai example
