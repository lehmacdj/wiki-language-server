module MyPrelude.RestrictedClassyPrelude
  ( module X,
  )
where

import ClassyPrelude as X hiding
  ( MonadReader (..),
    Reader,
    ReaderT (..),
    StringException,
    asks,
    bracket,
    bracketOnError,
    bracketOnError_,
    bracket_,
    catch,
    catchAny,
    catchAnyDeep,
    catchDeep,
    catchIO,
    catchJust,
    catches,
    catchesDeep,
    evaluate,
    evaluateDeep,
    finally,
    handle,
    handleAny,
    handleAnyDeep,
    handleDeep,
    handleIO,
    handleJust,
    impureThrow,
    isAsyncException,
    isSyncException,
    mask,
    mask_,
    onException,
    readFile,
    readFileUtf8,
    runReaderT,
    throwIO,
    throwString,
    throwTo,
    toAsyncException,
    toSyncException,
    try,
    tryAny,
    tryAnyDeep,
    tryDeep,
    tryIO,
    tryJust,
    uninterruptibleMask,
    uninterruptibleMask_,
    withException,
    writeFile,
    writeFileUtf8,
  )
