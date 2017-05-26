module Language.Hawk.Compile.Flags where

class HasFlags a where
    flag :: a -> String
    
    flagOn :: a -> String
    flagOn = mkFlagOn . flag

    flagOff :: a -> String
    flagOff = mkFlagOff . flag


mkFlagOn str = "f-" ++ str
mkFlagOff str = "fno-" ++ str

-- Strings for flagging
mkInfoFlag str = "info-" ++ str

infoFileFoundFlag = mkInfoFlag "filefound"
infoDirFoundFlag = mkInfoFlag "dirfound"
infoFreshModuleFoundFlag = mkInfoFlag "freshmodulefound"
infoModulePreservedFlag = mkInfoFlag "modulepreserved"

-- Strings for flagging
mkWarnFlag str = "warn-" ++ str

warnFileIgnoredFlag = mkWarnFlag "fileignored"
warnDirIgnoredFlag = mkWarnFlag "dirignored"
warnSymLinkIgnoredFlag = mkWarnFlag "symlinkignored"
