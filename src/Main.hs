module Main where

import Data.Word (Word16)
import qualified Data.Map as M
import Data.Bits
import Test.Hspec
import qualified Text.Parsec as P

data Wire = Wire String deriving (Show, Ord, Eq)

data Value = Value Word16 deriving (Show, Eq)

data Literal = WireL Wire | ValueL Value deriving (Show, Eq)

data BinOp = And | Or | LShift | RShift deriving (Show, Eq)
data UnOp = Not deriving (Show, Eq)

data Expr = LiteralE Literal
          | BinOpE BinOp Literal Literal
          | UnOpE UnOp Literal
            deriving (Show, Eq)

data Program = Program (M.Map Wire Expr) deriving (Show, Eq)
data Solution = Solution (M.Map Wire (Maybe Value)) deriving (Show, Eq)

join' :: Maybe (Maybe a) -> Maybe a
join' Nothing = Nothing
join' (Just v) = v


mkProgram :: [(Wire, Expr)] -> Program
mkProgram l = Program (M.fromList l)

evalProgram :: Program -> Wire -> Maybe Value
evalProgram (Program m) wire = let
  solution = Solution (M.map (evalExpr solution) m)
  res = case M.lookup wire m of
    Nothing -> Nothing
    Just expr -> evalExpr solution expr
  in res

evalExpr :: Solution -> Expr -> Maybe Value
evalExpr solution (LiteralE lit) = evalLiteral solution lit
evalExpr solution (UnOpE op lit) = do
  v <- evalLiteral solution lit
  Just (evalUnOp op v)
evalExpr solution (BinOpE op lit lit') = do
  v <- evalLiteral solution lit
  v' <- evalLiteral solution lit'
  Just (evalBinOp op v v')

evalUnOp :: UnOp -> Value -> Value
evalUnOp Not (Value v) = Value (complement v)

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp op (Value v) (Value v') = Value (applyOp op v v')

applyOp :: BinOp -> Word16 -> Word16 -> Word16
applyOp And v v' = v .&. v'
applyOp Or v v' = v .|. v'
applyOp LShift v v' = shiftL v (fromIntegral v')
applyOp RShift v v' = shiftR v (fromIntegral v')

evalLiteral :: Solution -> Literal -> Maybe Value
evalLiteral _ (ValueL v) = Just v
evalLiteral (Solution solution) (WireL wire) = join' (M.lookup wire solution)

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

exampleString :: String
exampleString = "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"

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

eS :: String -> Maybe Value
eS s = evalProgram defaultProgram (Wire s)

main :: IO ()
main = hspec $ do
  describe "Test" $ do
    it "works" $ do
      eS "d" `shouldBe` Just (Value 72)
      eS "e" `shouldBe` Just (Value 507)
      eS "f" `shouldBe` Just (Value 492)
      eS "g" `shouldBe` Just (Value 114)
      eS "h" `shouldBe` Just (Value 65412)
      eS "i" `shouldBe` Just (Value 65079)
      eS "x" `shouldBe` Just (Value 123)
      eS "y" `shouldBe` Just (Value 456)
      eS "titi" `shouldBe` Nothing

      day7 `shouldReturn` Right (Just (Value 956))
      day7bis `shouldReturn` Right (Just (Value 40149))

  describe "Parsing" $ do
    it "works" $ do
      parseProgram exampleString `shouldBe` (Just defaultProgram)

-- Program
-- Partie I (c'est fait) : evaluation
-- Partie II Robustess.
-- Partie III Parsing
-- Partie IV Vrai example

-- Parsing

parseProgram :: String -> Maybe Program
parseProgram s = case P.parse programParser "TOTO" s of
  Left _ -> Nothing
  Right p -> Just p

type Parser t = P.Parsec String () t

parseInstructions :: Parser [(Wire, Expr)]
parseInstructions = parseInstruction `P.sepBy` (P.string "\n")

parseInstruction :: Parser (Wire, Expr)
parseInstruction = do
  expr <- parseExpr
  _ <- P.string " -> "
  wire <- parseWire

  return $ (wire, expr)

parseExpr :: Parser Expr
parseExpr = P.choice [P.try parseBinOpE, P.try parseUnOpE, P.try parseLiteralE]

parseLiteralE :: Parser Expr
parseLiteralE = do
  lit <- parseLiteral
  return $ LiteralE lit

parseLiteral :: Parser Literal
parseLiteral = P.choice [P.try parseWireL, P.try parseValueL]

parseWireL :: Parser Literal
parseWireL = do
  wire <- parseWire
  return (WireL wire)

parseValueL :: Parser Literal
parseValueL = ValueL <$> parseValue

parseValue :: Parser Value
parseValue = do
  v <- P.many1 (P.oneOf ['0'..'9'])
  return $ Value (read v)

parseNot :: Parser UnOp
parseNot = P.string "NOT" *> return Not

parseAnd, parseOr, parseRShift, parseLShift :: Parser BinOp

parseAnd = P.string "AND" *> return And
parseOr = P.string "OR" *> return Or
parseLShift = P.string "LSHIFT" *> return LShift
parseRShift = P.string "RSHIFT" *> return RShift

parseBinOp :: Parser BinOp
parseBinOp = P.choice [P.try parseAnd, P.try parseOr, P.try parseLShift, P.try parseRShift]

parseUnOp :: Parser UnOp
parseUnOp = P.choice [P.try parseNot]

parseBinOpE :: Parser Expr
parseBinOpE = do
  lit <- parseLiteral
  _ <- P.string " "
  op <- parseBinOp
  _ <- P.string " "
  lit' <- parseLiteral
  return $ BinOpE op lit lit'

parseUnOpE :: Parser Expr
parseUnOpE = do
  op <- parseUnOp
  _ <- P.string " "
  lit <- parseLiteral
  return $ UnOpE op lit

parseWire :: Parser Wire
parseWire = do
  s <- P.many1 (P.oneOf ['a'..'z'])
  return $ Wire s

programParser :: Parser Program
programParser = do
  instr <- parseInstructions
  return $ mkProgram instr

{-
z AND a -> y
NOT a -> z
-}


day7 = do
  content <- readFile "input"
  case parseProgram content of
    Nothing -> return (Left "Unable to parse")
    Just p -> return (Right $ evalProgram p (Wire "a"))

day7bis = do
  content <- readFile "input"
  case parseProgram content of
    Nothing -> return (Left "Unable to parse")
    Just p -> case evalProgram p (Wire "a") of
      Just wireb -> let (Program m) = p
                        f _ = LiteralE (ValueL wireb)
                        p' = Program (M.adjust f (Wire "b") m)
                    in return (Right $ evalProgram p' (Wire "a"))
      Nothing -> return (Right Nothing)
