{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module SrcInfo where
    
type family XSrcInfoAnn idx
type family XSrcInfoLoc idx
data SrcInfoX idx = SrcInfoX (XSrcInfoAnn idx) (XSrcInfoLoc idx)
