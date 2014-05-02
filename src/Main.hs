import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)

import System.Console.GetOpt
import Control.Monad
import System.IO
import System.Exit
import System.Environment

data Options = Options  { optVerbose    :: Bool
                        , optInput      :: [Int]
                        , optOutput     :: WAVE -> IO () 
                        }

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInput      = [0,0,0] 
                        , optOutput     = do
                              hPutWAVE stdout
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "n" ["number"]
        (ReqArg
            (\arg opt -> return opt { optInput = map(read . (:"")) arg :: [Int] })
            "NUMBER")
        "Phone number (with cat)"
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = do
                                          putWAVEFile arg 
                                          })
            "FILE")
        "Output file"
    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

samplesPS = 8000
bitrate = 16

header = WAVEHeader 1 samplesPS bitrate Nothing

--data Tones = Tones [(Double,Double)]

sound :: Double  -- | Frequency
      -> Int -- | Samples per second
      -> Double -- | Lenght of sound in seconds
      -> Int32 -- | Volume, (maxBound :: Int32) for highest, 0 for lowest
      -> [Int32]
sound freq samples len volume = take (round $ len * (fromIntegral samples)) $ 
                         map (round . (* fromIntegral volume)) $
                         map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..]

sample :: Double -> Double -> [Int32]
sample f1 f2 = zipWith (+) (sound f1 samplesPS lenght (maxBound `div` 8)) (sound f2 samplesPS lenght (maxBound `div` 8))
    where lenght = 0.04

generator :: [(Double,Double)] -> [[Int32]] -- play two tones at once
generator t = map (:[]) $ foldl (++) [] $ [ sample (fst p) (snd p) | p <- t ] 
--generator f = map (:[]) $ zipWith (+) (sound 50 samplesPS 3 (maxBound `div` 2)) (sound 200 samplesPS 3 (maxBound `div` 2))

repDup :: [Int] -> [Int]
repDup [] = []
repDup (x:xs) | xs == [] = x:[]
repDup (x:xs) = if (x == head xs) then x : repN : (repDup $ tail xs) else x : (repDup xs)
     where repN = 10

encode :: [Int] -> [Int]
encode n = cycle $ startN : repDup n 
     where startN = 11 

prepareTones :: [Int] -> [(Double,Double)]
prepareTones n = map (\x->tones!!x) n
    where tones = [(1300,1500),(700,900),(700,1100),(900,1100),(700,1300),(900,1300),(1100,1300),(700,1500),(900,1500),(1100,1500),(1300,1700),(1100,1700)]


main = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    let Options { optVerbose = verbose
                , optInput = input
                , optOutput = output   } = opts

    when verbose (hPutStrLn stderr $ show input )
    output $ WAVE header $ generator $ prepareTones $ take 27 $ encode input


