import System.IO
import Control.Monad (when)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)

-- Tipo para operações
data Operacao
  = Soma
  | Subtracao
  | Multiplicacao
  | Divisao
  | RaizQuadrada
  | Potencia
  | LogaritmoNatural
  | Seno
  | Cosseno
  | Sair
  | OpcaoInvalida
  deriving (Eq, Show)

-- Lê a opção do usuário e converte para Operacao
lerOpcao :: IO Operacao
lerOpcao = do
  putStrLn "\n--- CALCULADORA CIENTÍFICA HASKELL ---"
  putStrLn "1. Soma"
  putStrLn "2. Subtração"
  putStrLn "3. Multiplicação"
  putStrLn "4. Divisão"
  putStrLn "5. Raiz Quadrada"
  putStrLn "6. Potência"
  putStrLn "7. Logaritmo Natural"
  putStrLn "8. Seno"
  putStrLn "9. Cosseno"
  putStrLn "0. Sair"
  putStr "Escolha uma opção: "
  opc <- getLine
  return $ case opc of
    "1" -> Soma
    "2" -> Subtracao
    "3" -> Multiplicacao
    "4" -> Divisao
    "5" -> RaizQuadrada
    "6" -> Potencia
    "7" -> LogaritmoNatural
    "8" -> Seno
    "9" -> Cosseno
    "0" -> Sair
    _   -> OpcaoInvalida

-- Lê um número do usuário com mensagem personalizada
lerNumero :: String -> IO (Maybe Double)
lerNumero msg = do
  putStr msg
  input <- getLine
  return (readMaybe input :: Maybe Double)

-- Função principal do programa
main :: IO ()
main = do
  h <- openFile "historico-operacoes.txt" AppendMode
  loop h
  hClose h
  putStrLn "Calculadora encerrada."

-- Loop principal que executa até usuário escolher sair
loop :: Handle -> IO ()
loop h = do
  opc <- lerOpcao
  case opc of
    Soma -> operacaoBinaria h "Soma" (+) >> loop h
    Subtracao -> operacaoBinaria h "Subtração" (-) >> loop h
    Multiplicacao -> operacaoBinaria h "Multiplicação" (*) >> loop h
    Divisao -> operacaoBinariaDivisao h >> loop h
    RaizQuadrada -> operacaoUnariaRaiz h >> loop h
    Potencia -> operacaoBinaria h "Potência" (**) >> loop h
    LogaritmoNatural -> operacaoUnariaLog h >> loop h
    Seno -> operacaoUnariaTrig h "Seno" sin >> loop h
    Cosseno -> operacaoUnariaTrig h "Cosseno" cos >> loop h
    Sair -> return ()
    OpcaoInvalida -> putStrLn "Opção inválida." >> loop h

-- Função para operações com duas entradas e operação binária
operacaoBinaria :: Handle -> String -> (Double -> Double -> Double) -> IO ()
operacaoBinaria h nome op = do
  mx <- lerNumero "Digite o primeiro número: "
  my <- lerNumero "Digite o segundo número: "
  case (mx, my) of
    (Just x, Just y) -> do
      let resultado = op x y
      putStrLn $ "Resultado: " ++ show resultado
      registrar h nome x y resultado
    _ -> putStrLn "Entrada inválida."

-- Tratamento especial para divisão (divisão por zero)
operacaoBinariaDivisao :: Handle -> IO ()
operacaoBinariaDivisao h = do
  mx <- lerNumero "Digite o dividendo: "
  my <- lerNumero "Digite o divisor: "
  case (mx, my) of
    (Just x, Just 0) -> putStrLn "Erro: Divisão por zero."
    (Just x, Just y) -> do
      let resultado = x / y
      putStrLn $ "Resultado: " ++ show resultado
      registrar h "Divisão" x y resultado
    _ -> putStrLn "Entrada inválida."

-- Operação unária: raiz quadrada
operacaoUnariaRaiz :: Handle -> IO ()
operacaoUnariaRaiz h = do
  mx <- lerNumero "Digite o número: "
  case mx of
    Just x | x >= 0 -> do
      let resultado = sqrt x
      putStrLn $ "Resultado: " ++ show resultado
      registrar h "Raiz Quadrada" x 0 resultado
    Just _ -> putStrLn "Erro: Número negativo."
    Nothing -> putStrLn "Entrada inválida."

-- Operação unária: logaritmo natural
operacaoUnariaLog :: Handle -> IO ()
operacaoUnariaLog h = do
  mx <- lerNumero "Digite o número: "
  case mx of
    Just x | x > 0 -> do
      let resultado = log x
      putStrLn $ "Resultado: " ++ show resultado
      registrar h "Logaritmo Natural" x 0 resultado
    Just _ -> putStrLn "Erro: Valor <= 0."
    Nothing -> putStrLn "Entrada inválida."

-- Operação unária trigonométrica
operacaoUnariaTrig :: Handle -> String -> (Double -> Double) -> IO ()
operacaoUnariaTrig h nome f = do
  mx <- lerNumero $ "Digite o ângulo (radianos): "
  case mx of
    Just x -> do
      let resultado = f x
      putStrLn $ "Resultado: " ++ show resultado
      registrar h nome x 0 resultado
    Nothing -> putStrLn "Entrada inválida."

-- Função para registrar operação no arquivo de histórico
registrar :: Handle -> String -> Double -> Double -> Double -> IO ()
registrar h tipo entrada1 entrada2 resultado = do
  timestamp <- getCurrentTime
  let linha = unwords
        [ tipo
        , show entrada1
        , show entrada2
        , show resultado
        , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
        ]
  hPutStrLn h linha
