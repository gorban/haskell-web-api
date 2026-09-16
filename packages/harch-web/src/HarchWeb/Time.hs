-- | Unix-epoch instants used for durable security state.
--
-- Decision (PR-S1, 2026-08-23): this is a distinct boundary from the
-- reboot-relative monotonic clock used for request and operation durations.
-- Account verification, sessions, login-attempt throttling, and TOTP all
-- survive a process restart and can be compared across hosts, so their
-- persisted instants use this POSIX-derived type.  The explicit newtype
-- prevents a monotonic @Word64@ from being passed to those boundaries by
-- accident; durations remain ordinary 'Word64' nanoseconds because they are
-- not persisted instants.
module HarchWeb.Time
  ( UnixTimeNanoseconds,
    UnixTimeSeconds,
    addUnixTimeNanoseconds,
    currentUnixTimeNanoseconds,
    unixTimeNanoseconds,
    unixTimeNanosecondsValue,
    unixTimeSeconds,
    unixTimeSecondsFromNanoseconds,
    unixTimeSecondsValue,
  )
where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)

newtype UnixTimeNanoseconds = UnixTimeNanoseconds Word64
  deriving (Bounded, Eq, Ord, Show, Num)

newtype UnixTimeSeconds = UnixTimeSeconds Word64
  deriving (Bounded, Eq, Ord, Show, Num)

-- | Reads a durable POSIX/Unix epoch instant.  It is deliberately unsuitable
-- for measuring elapsed work: wall time can be adjusted, while durable
-- security state must instead remain meaningful across reboots and hosts.
currentUnixTimeNanoseconds :: IO UnixTimeNanoseconds
currentUnixTimeNanoseconds = UnixTimeNanoseconds . floor . (* 1000000000) <$> getPOSIXTime

unixTimeNanoseconds :: Word64 -> UnixTimeNanoseconds
unixTimeNanoseconds = UnixTimeNanoseconds

unixTimeNanosecondsValue :: UnixTimeNanoseconds -> Word64
unixTimeNanosecondsValue (UnixTimeNanoseconds value) = value

unixTimeSeconds :: Word64 -> UnixTimeSeconds
unixTimeSeconds = UnixTimeSeconds

unixTimeSecondsFromNanoseconds :: UnixTimeNanoseconds -> UnixTimeSeconds
unixTimeSecondsFromNanoseconds (UnixTimeNanoseconds value) = UnixTimeSeconds (value `div` 1000000000)

unixTimeSecondsValue :: UnixTimeSeconds -> Word64
unixTimeSecondsValue (UnixTimeSeconds value) = value

addUnixTimeNanoseconds :: UnixTimeNanoseconds -> Word64 -> Maybe UnixTimeNanoseconds
addUnixTimeNanoseconds (UnixTimeNanoseconds instant) duration =
  let result = instant + duration
   in if result < instant
        then Nothing
        else Just (UnixTimeNanoseconds result)
