module Saki.Plugin.Iseng.Gampar (gamparPlugin) where

import Lambdabot.Plugin
import Lambdabot.Util

type Gampar = ModuleT () LB

gamparPlugin :: Module ()
gamparPlugin = newModule
    { moduleCmds = return
        [ (command "gampar")
            { aliases = ["tampar"]
            , help = say "gampar <nick>. Iseng-iseng ngegampar orang."
            , process = gampar
            }
        ]
    }

------------------------------------------------------------------------

gampar :: String -> Cmd Gampar ()
gampar target = do
    targetAsli <- nickAsli (stripSpasiLebih target)
    gamparAcak targetAsli
    where
        stripSpasiLebih         = unwords . words
        nickAsli "gue dong"     = showNick =<< getSender
        nickAsli "diri sendiri" = showNick =<< getLambdabotName
        nickAsli str            = return str

gamparAcak :: String -> Cmd Gampar ()
gamparAcak tgt = say . ($ tgt) =<< random daftarGamparan

daftarGamparan :: [String -> String]
daftarGamparan =
    [(\x -> "/me ngegampar " ++ x)
    ,(\x -> "maaf, lagi nggak nerima order gampar-gamparan")
    ,(\x -> "emang loe siapa, pakai nyuruh-nyuruh segala?!")
    ,(\x -> "/me menuhin twitternya " ++ x ++ " dengan kabar gembira dari kulit manggis")
    ,(\x -> "/me mukul " ++ x ++ " pake gayung")
    ,(\x -> "/me ngehajar " ++ x)
    ,(\x -> "/me nyolok matanya " ++ x)
    ,(\x -> "Salah apa emang " ++ x ++ " sampai harus disakitin?")
    ,(\x -> "*BRAK*, *BRUK*, *GEDUBRAK*, rasain tuh " ++ x ++ "!")
    ,(\_ -> "/me ngaktifin mesin penggamparnya...")
    ,(\x -> "/me nyuruh orangutannya yang pinter buat mukulin " ++ x)
    ,(\x -> "/me mbanting bohlam lampu ke kepalanya " ++ x)
    ,(\x -> "/me mecahin " ++ x ++ " pake batu, biar hancur berkeping-keping, biar ramai, lalu lari ke pantai")
    ,(\x -> "/me ngelemparin beberapa lambdas yang tajam ke " ++ x)
    ,(\x -> "/me sayang " ++ x ++ ", jadi gak ada gampar-gamparan")
    ,(\x -> "/me gak akan pernah ngelukain " ++ x ++ "!")
    ,(\x -> "gampar sendiri gih si " ++ x ++ "!")
    ,(\_ -> "Gak mau. Saya pengen kue aja.")
    ,(\x -> "Mending nggak deh. " ++ x ++ " kliatan ganas.")
    ,(\_ -> "I don't perform such side effects on command!")
    ,(\_ -> "stop nyuruh-nyuruh gue!")
    ,(\x -> "/me ngeribetin " ++ x ++ " pakai bahasa imperatif")
    ,(\x -> "/me nyeburin " ++ x ++ " ke kolam yang penuh buaya")
    ,(\x -> "/me diam-diam ngehapus source codenya " ++ x)
    ,(\x -> "/me naruh bogem mentah di rahangnya " ++ x ++ " dengan santai")
    ,(\x -> "/me ngunci si " ++ x ++ " di dalam Monad")
    ,(\x -> "/me nyubmit alamat emailnya " ++ x ++ " ke lusinan spam lists")
    ,(\x -> "/me nyetak " ++ x ++ " ke cetakan kue, lalu dimasukkan ke oven panas")
    ,(\_ -> "/me bakal ngitung sampai lima...")
    ,(\x -> "/me nusuk " ++ x ++ " pakai C pointer")
    ,(\x -> "/me tiba-tiba dipenuhi hasrat buat nyakitin " ++ x)
    ,(\x -> "Yuk, semuanya bareng-bareng mukulin " ++ x)
    ,(\x -> "/me ndorong " ++ x ++ " dari kursinya")
    ,(\x -> "/me ngelemparin " ++ x ++ " dengan lusinan alat dapur")
    ,(\x -> "/me decomposes " ++ x ++ " into several parts using the Banach-Tarski theorem and reassembles them to get two copies of " ++ x ++ "!")
    ]
