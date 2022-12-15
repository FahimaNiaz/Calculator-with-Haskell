-- Define a data type for representing the different operations that the calculator can perform
data Operation = Add | Subtract | Multiply | Divide

-- Define a function for evaluating an operation with two operands
evaluate :: Operation -> Float -> Float -> Float
evaluate Add x y = x + y
evaluate Subtract x y = x - y
evaluate Multiply x y = x * y
evaluate Divide x y = x / y

-- Define a function for parsing an operation from a string
parseOperation :: String -> Maybe Operation
parseOperation "+" = Just Add
parseOperation "-" = Just Subtract
parseOperation "*" = Just Multiply
parseOperation "/" = Just Divide
parseOperation _ = Nothing

-- Define a function for performing a calculation with two operands and an operation
calculate :: Float -> Float -> String -> Maybe Float
calculate x y op = do
     op'<- parseOperation op
     return(evaluate op' x y)

-- Define a function for prompting the user for input and performing a calculation
calculator :: IO ()
calculator = do
  putStr "Enter first operand: "
  xStr <- getLine
  let x = read xStr :: Float

  putStr "Enter second operand: "
  yStr <- getLine
  let y = read yStr :: Float

  putStr "Enter operation (+, -, *, /): "
  op <- getLine

  -- Perform the calculation and handle the case where the operation is invalid
  let result = calculate x y op
  case result of
     Just r -> putStrLn (show x ++ op ++ show y ++ " = " ++ show r)
     Nothing -> putStrLn "Invalid operation"

-- Main function
main :: IO ()
main = calculator
