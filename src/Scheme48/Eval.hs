{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Eval (readExpr, eval, primitiveBindings) where

import Control.Monad.Except
import System.IO

import Scheme48.Types
import Scheme48.Parsers
import Scheme48.StdLib
import Scheme48.Env

import Text.ParserCombinators.Parsec hiding (spaces)

-- Bindings --
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc' $ IOFunc . IOPrimFunc) ioPrimitives
                                               ++ map (makeFunc' $ PrimitiveFunc . PrimFunc) primitives)
     where makeFunc' constructor (var, func) = (var, constructor func)

-- Evaluation --

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (List [Atom "load", String filename]) =
       load filename >>= liftM last . mapM (eval env)
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", cond, t, f]) =
                     do result <- eval env cond
                        case result of
                             Bool False -> eval env f
                             Bool True -> eval env t
                             _ -> throwError $ TypeMismatch "bool" cond
eval env v@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clauses found in cond expression: " v
  else case head clauses of
            List [Atom "else", expr] -> eval env expr
            List [test, expr] -> eval env $ List [Atom "if", test, expr, List (Atom "cond" : tail clauses)]
            _ -> throwError $ BadSpecialForm "maleformed cond expressioni: " v
eval env v@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clauses or else clause in case expression:" v
  else case head clauses of
            List (Atom "else" : exprs) -> liftM last (mapM (eval env) exprs)
            List (List datums : exprs) -> do
              result <- eval env key
              equality <- mapM  (liftThrows . (\x -> eqv [result, x])) datums
              if Bool True `elem` equality
              then liftM last (mapM (eval env) exprs)
              else eval env $ List (Atom "case" : key : tail clauses)
            _ -> throwError $ BadSpecialForm "maleformed case expression: " v
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List (Atom "define" : List (Atom var : params') : body')) =
     makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
     makeVarArgs varargs env params' body' >>= defineVar env var
eval env (List [Atom "define", Atom var, body']) =
     eval env body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
     makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
     makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
     makeVarArgs varargs env [] body'
eval env (List (func:args)) = do
  func' <- eval env func
  argVals <- mapM (eval env) args
  apply func' argVals
eval env (Atom id') = getVar env id'
eval _ v@(String _) = return v
eval _ v@(Number _) = return v
eval _ v@(Bool _) = return v
eval _ v@(Character _) = return v
eval _ v@(Float _) = return v
eval _ v@(Ratio _) = return v
eval _ v@(Complex _) = return v
eval _ v@(Vector _) = return v
eval _ v@(DottedList _ _) = return v
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc (PrimFunc func)) args = liftThrows $ func args
apply (IOFunc (IOPrimFunc func)) args = func args
apply (Func params' varags' body' closure') args =
  if num params' /= num args && varags' == Nothing
     then throwError $ NumArgs (num params') args
     else (liftIO $ bindVars closure' $ zip params' args) >>= bindVarArgs varags' >>= evalBody
  where remainingArgs = drop (length params') args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body'
        bindVarArgs arg env = case arg of
                                    Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                                    Nothing -> return env
apply form args = liftThrows $ throwError $ NotFunction (show form) (show args)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

-- IO Primatives --

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc _                 = error "impossible"

makePort :: MonadIO m => IOMode -> [LispVal] -> m LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ _ = error "impossible"

closePort :: MonadIO m => [LispVal] -> m LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> ExceptT LispError IO LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc _           = error "impossible"

writeProc :: MonadIO m => [LispVal] -> m LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc _                = error "impossible"

readContents :: MonadIO m => [LispVal] -> m LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents _                 = error "impossible"

load :: FilePath -> ExceptT LispError IO [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll _                 = error "impossible"
