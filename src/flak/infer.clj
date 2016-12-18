(ns flak.infer
  (:require
   [flak.monad :as m]
   [flak.type :as t]
   [flak.types :refer :all]))

(t/type Var String)

(t/data Lit (or (LInt Integer)
                (LBool Bool)))

(t/data Binop (or Add Sub Mul Eql))

(t/data Expr (or (Var Var)
                 (App Expr Expr)
                 (Lam Var Expr)
                 (Let Var Expr Expr)
                 (Lit Lit)
                 (If Expr Expr Expr)
                 (Fix Expr)
                 (Op Binop Expr Expr)))

(t/type Decl [String Expr])

(t/data Program (Program [Decl] Expr))

(t/data Unique (Unique c))
(t/type Infer (State Unique))
