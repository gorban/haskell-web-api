module HarchWeb.Password.WorkBudget
  ( PasswordWorkBudget,
    PasswordWorkGate,
    defaultPasswordWorkBudget,
    mkPasswordWorkBudget,
    newPasswordWorkGate,
    passwordWorkBudgetKibibytes,
    withPasswordWork,
  )
where

import Control.Exception (finally, mask)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word32)

-- | A positive application-owned cap on simultaneous password-work memory,
-- expressed in Argon2 KiB.  Construct it with 'mkPasswordWorkBudget' so a
-- deployment cannot accidentally configure a gate that admits no work.
newtype PasswordWorkBudget = PasswordWorkBudget Word32

-- | The reference application's 512-MiB shared Argon2 admission capacity.
-- Applications can choose another positive value with 'mkPasswordWorkBudget'.
defaultPasswordWorkBudget :: PasswordWorkBudget
defaultPasswordWorkBudget = PasswordWorkBudget 524288

mkPasswordWorkBudget :: Word32 -> Maybe PasswordWorkBudget
mkPasswordWorkBudget kibibytes
  | kibibytes == 0 = Nothing
  | otherwise = Just (PasswordWorkBudget kibibytes)

passwordWorkBudgetKibibytes :: PasswordWorkBudget -> Word32
passwordWorkBudgetKibibytes (PasswordWorkBudget kibibytes) = kibibytes

-- | State for one owner's Argon2 admissions. The owner creates it explicitly
-- and chooses its lifetime; applications that need one process-wide budget
-- can retain one gate, while tests can isolate a gate for each scenario.
data PasswordWorkGate = PasswordWorkGate
  { passwordWorkCapacity :: Word32,
    passwordWorkInUse :: IORef PasswordWorkInUse
  }

data PasswordWorkInUse = PasswordWorkInUse
  { passwordWorkMemoryInUse :: Word32,
    passwordWorkOperationsInUse :: Word32
  }

newPasswordWorkGate :: PasswordWorkBudget -> IO PasswordWorkGate
newPasswordWorkGate budget =
  PasswordWorkGate (passwordWorkBudgetKibibytes budget) <$> newIORef (PasswordWorkInUse 0 0)

-- | Run one Argon2 operation only when its validated KiB cost fits in the
-- remaining application budget and fewer than eight operations are already
-- active.  The memory check prevents OOM; the operation cap also bounds CPU
-- when a valid hash has a small memory setting but a high iteration count.
-- Admission is immediate rather than queued: overload has a typed,
-- recoverable outcome instead of retaining attacker work and turning a memory
-- limit into unbounded latency. The reservation is released on ordinary
-- completion and asynchronous exception alike.
withPasswordWork :: PasswordWorkGate -> Word32 -> IO value -> IO (Maybe value)
withPasswordWork gate cost action =
  mask $ \restore -> do
    admitted <- acquirePasswordWork gate cost
    if admitted
      then Just <$> (restore action `finally` releasePasswordWork gate cost)
      else pure Nothing

acquirePasswordWork :: PasswordWorkGate -> Word32 -> IO Bool
acquirePasswordWork gate cost =
  atomicModifyIORef' (passwordWorkInUse gate) $ \inUse ->
    if cost <= passwordWorkCapacity gate - passwordWorkMemoryInUse inUse
      && passwordWorkOperationsInUse inUse < maximumPasswordWorkOperations
      then
        ( PasswordWorkInUse
            { passwordWorkMemoryInUse = passwordWorkMemoryInUse inUse + cost,
              passwordWorkOperationsInUse = passwordWorkOperationsInUse inUse + 1
            },
          True
        )
      else (inUse, False)

releasePasswordWork :: PasswordWorkGate -> Word32 -> IO ()
releasePasswordWork gate cost =
  atomicModifyIORef' (passwordWorkInUse gate) $ \inUse ->
    ( PasswordWorkInUse
        { passwordWorkMemoryInUse = passwordWorkMemoryInUse inUse - cost,
          passwordWorkOperationsInUse = passwordWorkOperationsInUse inUse - 1
        },
      ()
    )

maximumPasswordWorkOperations :: Word32
maximumPasswordWorkOperations = 8
