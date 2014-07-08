-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Lambdabot version information
module Lambdabot.Plugin.Core.Version (versionPlugin) where

import Lambdabot.Plugin
import Paths_lambdabot_core (version)
import Data.Version (showVersion)

versionPlugin :: Module ()
versionPlugin = newModule
    { moduleCmds = return
        [ (command "version")
            { help = say $
                "version. Ngeprint versi " ++
                "serta repositori saki-chan"
            , process = const $ do
                say $ "saki-chan " ++ showVersion version
                say "git clone https://github.com/matematikaadit/saki-chan.git"
            }
        ]
    }
