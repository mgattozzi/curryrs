{-# LANGUAGE NamedFieldPuns #-}

module Curryrs.Build (
    curryrsMain
  , curryrsMainWithHooks
  ) where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)
import Data.List (nub, sort)

import GHC.IO.Exception (ioe_type, IOErrorType(InappropriateType))
import Control.Exception (IOException, tryJust)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

import Distribution.Simple hiding (installedUnitId)
import Distribution.Simple.BuildPaths
import Distribution.Simple.Program
import Distribution.Simple.Program.Ar
import Distribution.Verbosity
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Text
import Distribution.InstalledPackageInfo

data LinkerOption = LinkPath FilePath
                  | LinkHs   String
                  | LinkLib  String
  deriving (Show, Eq, Ord)

data DebugRts = NoDebugRts | EventLogRts | DebugRts

data HaskellRts = HaskellRts { threadedRts :: Bool
                             , debugRts    :: DebugRts
                             }

instance Monoid DebugRts where
  mempty = NoDebugRts
  mappend NoDebugRts x = x
  mappend x NoDebugRts = x
  mappend EventLogRts x = x
  mappend x EventLogRts = x
  mappend DebugRts DebugRts = DebugRts

instance Monoid HaskellRts where
  mempty = HaskellRts False NoDebugRts
  mappend (HaskellRts t1 d1) (HaskellRts t2 d2) = HaskellRts (t1 || t2)
                                                             (mappend d1 d2)

curryrsMain = curryrsMainWithHooks simpleUserHooks

curryrsMainWithHooks hooks = defaultMainWithHooks $ hooks
    { buildHook = modBuildHook $ buildHook hooks }

getLibBuildInfo :: (ComponentName, ComponentLocalBuildInfo, a)
                -> Maybe (UnitId, [(UnitId, PackageId)])
getLibBuildInfo ( CLibName
                , LibComponentLocalBuildInfo { componentUnitId
                                             , componentPackageDeps}
                , _
                ) = Just (componentUnitId, componentPackageDeps)
getLibBuildInfo _ = Nothing

modBuildHook x pkg_descr lbi@LocalBuildInfo { buildDir
                                            , compiler
                                            , withPrograms
                                            } hooks flags =
    case exactlyOne $ mapMaybe getLibBuildInfo $ componentsConfigs lbi of
        Nothing -> x pkg_descr lbi hooks flags
        Just (libUnitId, libDeps) -> do
            let cId = getCId compiler
                rts = rtsString $ getRts $ getGhcArgs withPrograms
                linkerOpts = getLinkerOptions rts lbi libDeps
                cargoOpts = map (toCargoOption cId) linkerOpts
                hsLibName = getHSLibraryName libUnitId
                libName = mkLibName libUnitId

            x pkg_descr
              lbi {withVanillaLib = False, withSharedLib = True}
              hooks
              flags

            buildDirFiles <- recurseDirs buildDir
            let objFiles = filter ((".dyn_o"==) . takeExtension) buildDirFiles
            createArLibArchive normal lbi (buildDir </> libName) objFiles

            putStrLn $ "cargo:rustc-link-lib=static=" ++ hsLibName
            putStrLn $ "cargo:rustc-link-search=native=" ++ buildDir
            mapM_ putStrLn cargoOpts

rtsString :: HaskellRts -> String
rtsString HaskellRts { threadedRts, debugRts } = concat
    [ "HSrts"
    , if threadedRts then "_thr" else ""
    , case debugRts of
          EventLogRts -> "_l"
          DebugRts    -> "_debug"
          NoDebugRts  -> ""
    ]

getRts :: [String] -> HaskellRts
getRts = mconcat . map helper
  where
    helper :: String -> HaskellRts
    helper "-threaded" = mempty { threadedRts = True }
    helper "-eventlog" = mempty { debugRts = EventLogRts }
    helper "-debug"    = mempty { debugRts = DebugRts }
    helper _           = mempty

getGhcArgs :: ProgramConfiguration -> [String]
getGhcArgs =
    maybe [] (uncurry (++) . (programDefaultArgs &&& programOverrideArgs)) .
        exactlyOne . filter (("ghc"==) . programId) . mapMaybe snd .
        knownPrograms

toCargoOption :: String -> LinkerOption -> String
toCargoOption _   (LinkPath p) = "cargo:rustc-link-search=native=" ++ p
toCargoOption _   (LinkLib l)  = "cargo:rustc-link-lib=dylib=" ++ l
toCargoOption cId (LinkHs l)   = "cargo:rustc-link-lib=dylib=" ++ l
                                       ++ "-" ++ cId

getCId :: Compiler -> String
getCId Compiler {compilerId = CompilerId flv ver} = display flv ++ display ver

getLinkerOptions :: String -> LocalBuildInfo -> [(UnitId, PackageId)] -> [LinkerOption]
getLinkerOptions rts LocalBuildInfo {installedPkgs} =
    allLinkerOptions rts . getLibDepends installedPkgs

allLinkerOptions :: String -> InstalledPackageIndex -> [LinkerOption]
allLinkerOptions rts =
    nub . sort . concatMap (packageLinkerOptions rts) . allPackages

packageLinkerOptions :: String -> InstalledPackageInfo -> [LinkerOption]
packageLinkerOptions rts InstalledPackageInfo { installedUnitId
                                              , libraryDirs
                                              , hsLibraries
                                              , extraLibraries
                                              } =
    concat [ map LinkPath libraryDirs
           , case installedUnitId of
                 SimpleUnitId (ComponentId "rts") -> [LinkHs rts]
                 _ -> map LinkHs   hsLibraries
           , map LinkLib  extraLibraries
           ]

getLibDepends :: InstalledPackageIndex -> [(UnitId, PackageId)] -> InstalledPackageIndex
getLibDepends iPkgs deps =
    case dependencyClosure iPkgs (map fst deps) of
        Left x -> x

exactlyOne :: [a] -> Maybe a
exactlyOne [x] = Just x
exactlyOne _   = Nothing

recurseDirs :: FilePath -> IO [FilePath]
recurseDirs fp = listDirectory' fp >>=
    maybe (return [fp]) (fmap concat . mapM (recurseDirs . (fp</>)))

listDirectory' :: FilePath -> IO (Maybe [FilePath])
listDirectory' = fmap (either (const Nothing) Just) . tryJust eFilter . listDirectory
  where
    eFilter :: IOException -> Maybe ()
    eFilter ex = case ioe_type ex of
        InappropriateType -> Just ()
        _                 -> Nothing
