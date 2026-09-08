module PROJECTV2 where
import Control.Monad.State
import Data.Int

-- State of the machine with registers, counter and program instructions
data StateOfMachine = StateOfMachine Register Counter Program
    deriving(Show)

type Program = [Instructions]

type Register = [(String,Int64)]

type Counter = Int64

-- Supported instructions that can be executed by the machine
data Instructions =
      Mov String String
    | Movc String Int64 
    | Add String String String
    | Addc String Int64
    | Sub String String String
    | Subc String Int64
    | Mul String String String
    | Mulc String Int64
    | Div String String String
    | Divc String Int64
    | Jmp Int64
    | Jal Int64
    | Jif String Int64
    | Jr  String
    deriving(Show)
    
{-
Helper function for getting and setting the registers values

r: Name of the register
v: The value inside the register
rs: The rest of the list

 -}
getRegister :: String -> Register ->  Int64
getRegister r [] = 0
getRegister r ((r2,v):rs)
    | r == r2 = v
    | otherwise = getRegister r rs

setRegister :: String -> Int64 -> Register -> Register
setRegister r v [] = [(r,v)]
setRegister r v ((r2,v2):rs) 
    | r == r2 = ((r,v):rs)
    | otherwise  = ((r2,v2) : setRegister r v rs)

{-
-- Initialize the machine state with default registers and given instructions:
- Default and special register values "rv", "lnk", "r0..r15"
- counter set to 0
- Instructions 
-} 
initializeState ::  Program -> StateOfMachine
initializeState instructions = StateOfMachine initRegisters 0 instructions
    where initRegisters = [("rv",0),("lnk",00)] ++ [(("r" ++ show i),0) | i <- [0..15]] 

{-
Interpets the program by executing all the instructions given in the txt file.
Returns the final value in "rv" register after the execution has finished
-}
interp :: Program -> Int64
interp program = fromIntegral (getRegister "rv" r)
    where
        ((_,StateOfMachine r c i)) = runState  (runExec) (initializeState program )

exec :: Instructions -> State StateOfMachine ()
exec (Mov r0 r1)  = do 
    StateOfMachine r c i <- get 
    put (StateOfMachine (setRegister r1 (getRegister r0 r) r) (c + 1) i)

exec (Movc r0 v) = do 
    StateOfMachine r c i <- get 
    put (StateOfMachine (setRegister r0 v r) (c + 1) i)

exec (Add r0 r1 r2)  = do
    StateOfMachine r c i <- get 
    put (StateOfMachine (setRegister r0 ((getRegister r1 r) + (getRegister r2 r)) r) (c + 1) i)

exec (Addc r0 v)  = do
    StateOfMachine r c i <- get
    put (StateOfMachine (setRegister r0 ((getRegister r0 r) + v) r) (c + 1) i) 

exec (Sub r0 r1 r2) = do
    StateOfMachine r c i <- get 
    put (StateOfMachine (setRegister r0 ((getRegister r1 r) - (getRegister r2 r)) r) (c + 1) i)


exec (Subc r0 v)  = do
    StateOfMachine r c i <- get
    put (StateOfMachine (setRegister r0 ((getRegister r0 r) - v) r) (c + 1) i) 


exec (Mul r0 r1 r2) = do
    StateOfMachine r c i <- get 
    put (StateOfMachine (setRegister r0 ((getRegister r1 r) * (getRegister r2 r)) r) (c + 1) i)


exec (Mulc r0 v)  = do 
    StateOfMachine r c i <- get
    put (StateOfMachine (setRegister r0 ((getRegister r0 r) * v) r) (c + 1) i) 


exec (Div r0 r1 r2)  = do
    StateOfMachine r c i <- get 
    put (StateOfMachine (setRegister r0 ((getRegister r1 r) `div` (getRegister r2 r)) r) (c + 1) i)


exec (Divc r0 v)  = do 
    StateOfMachine r c i <- get
    put (StateOfMachine (setRegister r0 ((getRegister r0 r) `div` v) r) (c + 1) i) 

exec (Jmp v)  = do
    StateOfMachine r c i <- get
    put (StateOfMachine r v i)

-- current line + 1
exec (Jal v)  = do
    StateOfMachine r c i <- get
    put (StateOfMachine (setRegister "lnk" (c+1) r)v i)

exec (Jif r0 v) = do
    StateOfMachine r c i <- get
    put (StateOfMachine r
         (if (getRegister r0 r) /= 0 
          then v 
          else (c + 1))
         i)

exec (Jr r0) = do
    StateOfMachine r c i <- get
    put (StateOfMachine r 
         (if getRegister r0 r >= 0 && getRegister r0 r < fromIntegral (length i) 
          then getRegister r0 r 
          else (c + 1)) 
         i)

{-
Recursively executes the program
-}
runExec :: State StateOfMachine ()
runExec = do
    StateOfMachine r c i <- get
    if c < 0 || fromIntegral c >= length i
        then return ()
        else do
            exec (i !! fromIntegral c)            
            runExec 

{-
Reads a program from a given file and then parses it into a list
-}
loadFromFile :: FilePath -> IO Program
loadFromFile file = do
    contents <- readFile file
    return (map parsefunction (lines contents))

{-
Parses every line from the txt file into instructions
-}
parsefunction :: String -> Instructions
parsefunction lines = 
    case words lines of
        ["mov", r0,r1] -> Mov r0 r1
        ["movc", r0,v] -> Movc r0 (read v)
        ["add",r0,r1,r2] -> Add r0 r1 r2 
        ["addc",r0,v] -> Addc r0 (read v)
        ["sub", r0,r1,r2] -> Sub r0 r1 r2
        ["subc", r0,v] -> Subc r0 (read v)
        ["mul",r0,r1,r2] -> Mul r0 r1 r2
        ["mulc",r0,v] -> Mulc r0 (read v)
        ["div",r0,r1,r2] -> Div r0 r1 r2
        ["divc", r0,v] -> Divc r0 (read v)
        ["jmp", v] -> Jmp (read v)
        ["jal", v] -> Jal (read v)
        ["jif",r0,v] -> Jif r0 (read v)
        ["jr", r0] -> Jr r0
        _ -> error "Invalid instruction"

{-
Runs the program from a given file, interpets it and prints the result of
the "rv" register
-}
runProgram :: FilePath -> IO ()
runProgram file = do
    program <- loadFromFile file
    print (interp program)

main :: IO ()
main = do
    putStrLn "Please enter the name of the file you wish to use:"
    file <- getLine
    runProgram file


{-
A test suite of all instructions to check if it does as intended

Testing of move, arthemtic and jump instructions
-}
-- Move instructions
t1 = interp [Movc "r0" 10, Mov "r0" "rv"] == 10

t2 = interp [Movc "rv" 10] == 10

-- Testing of arithmetic instructions
t3 = interp [Movc "r0" 10, Movc "r1" 10, Add "rv" "r1" "r0"] == 20

t4 = interp [Movc "rv" 10, Addc "rv" 10] == 20

t5 = interp [Movc "r0" 30, Movc "r1" 10, Sub "rv" "r0" "r1"] == 20

t6 = interp [Movc "rv" 30, Subc "rv" 10] == 20

t7 = interp [Movc "r0" 2, Movc "r1" 2, Mul "rv" "r0" "r1"] == 4

t8 = interp [Movc "rv" 30, Mulc "rv" 10] == 300

t9 = interp [Movc "r0" 2, Movc "r1" 2, Div "rv" "r0" "r1"] == 1

t10 = interp [Movc "rv" 30, Divc "rv" 10] == 3

-- Testing of jump functions
t11 = interp [Movc "rv" 10, Jmp 3, Addc "rv" 20, Addc "rv" 30] == 40
t12 = interp [Movc "rv" 10, Jal 3, Movc "rv" 20, Movc "rv" 30, Mov "lnk" "rv"] == 2

-- If register is zero or non-zero for jif
t13 = interp [Movc "r0" 1, Jif "r0" 3, Addc "rv" 20, Addc "rv" 30] == 30
t14 = interp [Movc "r0" 0, Jif "r0" 3, Addc "rv" 20, Addc "rv" 30] == 50

t15 = interp [Movc "r1" 3, Jr "r1", Addc "rv" 20, Addc "rv" 30] == 30


exectestsuite = and [t1, t2, t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15]
