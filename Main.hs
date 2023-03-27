-- Lista de Exercícios Beatriz Castro e Caio Oliveira
-- Exercício 2.2

verificaString :: String -> Bool
verificaString st = even $ length st

-- Exercício 2.3

reverteString :: [String] -> [String]
reverteString xs = reverse [reverse x | x <- xs]

-- Exercício 2.6

verificaPalindromo :: String -> Bool
verificaPalindromo xs = xs == reverse xs

-- Exercício 3.2

data JokenPo = Pedra | Papel | Tesoura deriving (Show)

pedraPapelTesoura :: JokenPo -> JokenPo -> String
pedraPapelTesoura Pedra Tesoura = "Jogador 1 venceu"
pedraPapelTesoura Pedra Papel = "Jogador 2 venceu"
pedraPapelTesoura Tesoura Papel = "Jogador 1 venceu"
pedraPapelTesoura Tesoura Pedra = "Jogador 2 venceu"
pedraPapelTesoura Papel Pedra = "Jogador 1 venceu"
pedraPapelTesoura Papel Tesoura = "Jogador 2 venceu"
pedraPapelTesoura _ _ = "Empate!"

-- Exercício 3.4

removeVogais :: String -> String
removeVogais str = [c | c <- str, c `notElem` "aeiouAEIOU"]

-- Exercício 3.10

revNum :: String -> Int -> String
revNum s n = reverse (take n s) ++ drop n s
