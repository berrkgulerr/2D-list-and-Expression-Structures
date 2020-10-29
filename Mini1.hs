module Mini1 (
    gridMap,
    gridMapIf,
    evalExpr,
    getVars,
    evalDeriv,
    parse -- reexported to allow use
    ) where

import Expression
import Parser

-- Do not modify the module declaration and imports above!
-- Also do not change the function signatures and do not
-- remove the dummy implementations of the functions if
-- you want your code to compile.

-- Feel free to import anything else here
import Data.Maybe
import Data.List



-- gridMap (20 points), map function over grid elements
gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap f y = map (map f) y


-- gridMapIf (20 points), map functions over grid elements 
-- that satisfy the predicate provided as the first arg.
gridMapIf :: (a -> Bool) -> (a -> a) -> [[a]] -> [[a]]
mapIf p f = map (\x -> if p x then f x else x)
gridMapIf b f y = map (mapIf b f) y 

-- evalExpr (20 points), evaluate the expression by
-- substituting (var, value) pairs given in the first arg.
evalExpr :: [(String, Int)] -> ExprV -> Int
eval (Leaf (Constant x)) list = x
eval (Leaf (Variable x)) list = fromJust (lookup x list)
eval (UnaryOperation Minus (Leaf (Constant l))) list = -l
eval (UnaryOperation Minus (Leaf (Variable l))) list = - (fromJust (lookup l list))
eval (BinaryOperation Plus l r) list = (eval l list) + (eval r list)
eval (BinaryOperation Times l r) list = (eval l list) * (eval r list)
evalExpr list x = eval x list

-- getVars (20 points), return the variables contained
-- in the expression in a list (ordered, no duplicates)
getVars :: ExprV -> [String]
add1 list x = if(elem x list) then list else (list ++ [x])  
eval1 (Leaf (Constant x)) list = list
eval1 (Leaf (Variable x)) list = (add1 list x)
eval1 (UnaryOperation Minus (Leaf (Constant l))) list = list
eval1 (UnaryOperation Minus (Leaf (Variable l))) list = (add1 list l)
eval1 (BinaryOperation Plus l r) list = (eval1 l list) `union`  (eval1 r list)
eval1 (BinaryOperation Times l r) list = (eval1 l list) `union` (eval1 r list)
getVars x = sort (eval1 x [])

-- evalDeriv (20 points), evaluate the first derivative
-- with respect to the variable given in the second
-- arg. using (var, value) pairs given in the first arg.
evalDeriv :: [(String, Int)] -> String -> ExprV -> Int
eval2 (Leaf (Constant x)) y = (Leaf (Constant 0))
eval2 (Leaf (Variable x)) y = if(x==y) then (Leaf (Constant 1)) else (Leaf (Constant 0))
eval2 (UnaryOperation Minus x) y = (UnaryOperation Minus (eval2 x y))  --parantez gelebilir
eval2 (BinaryOperation Plus l r) y = (BinaryOperation Plus (eval2 l y) (eval2 r y))
eval2 (BinaryOperation Times l r) y = (BinaryOperation Plus (BinaryOperation Times (eval2 l y) (r)) (BinaryOperation Times (l) (eval2 r y)))
evalDeriv a b c = evalExpr a (eval2 c b)

-- Looks like that's all! 
