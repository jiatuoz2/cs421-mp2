module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = 
    case (H.lookup s env) of
        Just v -> v
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
    in case op of
        "/" -> 
            case v2 of 
                IntVal 0 -> ExnVal "Division by 0"
                _ -> liftIntOp (div) v1 v2
        _ -> case H.lookup op intOps of
                Just f -> liftIntOp f v1 v2
                Nothing -> ExnVal "Not matching operator"

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = 
    case H.lookup op boolOps of
        Just f -> 
            let v1 = eval e1 env
                v2 = eval e2 env
            in liftBoolOp f v1 v2
        Nothing -> ExnVal "Not matching operator"

eval (CompOpExp op e1 e2) env = 
    case H.lookup op compOps of
        Just f -> 
            let v1 = eval e1 env
                v2 = eval e2 env
            in liftCompOp f v1 v2
        Nothing -> ExnVal "Not matching operator"

--- ### If Expressions

eval (IfExp e1 e2 e3) env = 
    case eval e1 env of
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        _ -> ExnVal "Condition is not a Bool"
    
--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    case eval e1 env of
        CloVal params body clenv -> 
            let newenv = zip' params (map' eval args)
                    where zip' [] _ = []
                          zip' _ [] = []
                          zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
                          map' _ [] = []
                          map' f (x:xs) = f x env : map' f xs
            in eval body (H.union (H.fromList newenv) clenv)
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = 
    eval body (H.union (H.fromList (f pairs)) env)
        where f [] = []
              f (x:xs) = (fst x, eval (snd x) env) : f xs

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = 
    let v = eval e env
    in ("", penv, insert var v env)

--- ### Sequencing

exec (SeqStmt xs) penv env = foldl f ("", penv, env) xs
        where f (val', penv', env') x = 
                let (a, b, c) = exec x penv' env'
                in (val' ++ a, b, c)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = 
    case eval e1 env of
        BoolVal True -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        _ -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env = 
    case (H.lookup name penv) of
        Just (ProcedureStmt f params body) -> 
            let env' = H.fromList $ zip params (g args)
                    where g [] = []
                          g (x:xs) = eval x env : g xs
                newenv = H.union env' env
            in exec body penv newenv
        Nothing -> ("Procedure " ++ name ++ " undefined", penv, env)
