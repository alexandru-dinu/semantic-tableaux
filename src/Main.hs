import Prop
import Tableaux

main :: IO ()
main = do
    phi <- getContents
    putStrLn $ showTableaux $ tableaux (read phi :: Phi)