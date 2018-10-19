import Prop
import Tableaux

main :: IO ()
main = do
    phi <- getContents
    -- putStrLn $ show (read phi :: Phi) -- show formula
    putStrLn $ showTableaux $ tableaux (read phi :: Phi)
