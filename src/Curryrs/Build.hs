{-# LANGUAGE NamedFieldPuns #-}

module Curryrs.Build (
    curryrsMain
  , curryrsMainWithHooks
  ) where

import Control.Monad (unless)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (foldl', nub, sort)

import GHC.IO.Exception (ioe_type, IOErrorType(InappropriateType))
import Control.Exception (IOException, tryJust)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

import Distribution.Simple hiding (installedUnitId)
import Distribution.Simple.BuildPaths
import Distribution.Simple.Program.Ar
import Distribution.Verbosity
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Compiler
import Distribution.Text
import Distribution.InstalledPackageInfo

data LinkerOption = LinkPath FilePath
                  | LinkHs   String
                  | LinkLib  String
  deriving (Show, Eq, Ord)

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
                                            } hooks flags =
    case exactlyOne $ mapMaybe getLibBuildInfo $ componentsConfigs lbi of
        Nothing -> x pkg_descr lbi hooks flags
        Just (libUnitId, libDeps) -> do
            let cId = getCId compiler
                linkerOpts = getLinkerOptions lbi libDeps
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

toCargoOption :: String -> LinkerOption -> String
toCargoOption _   (LinkPath p) = "cargo:rustc-link-search=native=" ++ p
toCargoOption _   (LinkLib l)  = "cargo:rustc-link-lib=dylib=" ++ l
toCargoOption cId (LinkHs l)   = "cargo:rustc-link-lib=dylib=" ++ l
                                       ++ "-" ++ cId

getCId :: Compiler -> String
getCId Compiler {compilerId = CompilerId flv ver} = display flv ++ display ver

getLinkerOptions :: LocalBuildInfo -> [(UnitId, PackageId)] -> [LinkerOption]
getLinkerOptions LocalBuildInfo {installedPkgs} =
    allLinkerOptions . getLibDepends installedPkgs

allLinkerOptions :: InstalledPackageIndex -> [LinkerOption]
allLinkerOptions = nub . sort . concatMap packageLinkerOptions . allPackages

packageLinkerOptions :: InstalledPackageInfo -> [LinkerOption]
packageLinkerOptions InstalledPackageInfo { libraryDirs
                                          , hsLibraries
                                          , extraLibraries
                                          } =
    concat [ map LinkPath libraryDirs
           , map LinkHs   hsLibraries
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
