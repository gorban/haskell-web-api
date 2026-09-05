{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Password
  ( Argon2Iterations,
    Argon2MemoryKib,
    Argon2Parallelism,
    Password,
    PasswordHash (..),
    PasswordHashingPolicy,
    PasswordWorkBudget,
    PasswordWorkGate,
    argon2Iterations,
    argon2MemoryKib,
    argon2Parallelism,
    defaultPasswordWorkBudget,
    defaultPasswordHashingPolicy,
    hashPassword,
    hashPasswordWithSalt,
    mkPassword,
    mkPasswordHashingPolicy,
    mkPasswordWorkBudget,
    newPasswordWorkGate,
    passwordHashText,
    passwordHashWorkKibibytes,
    passwordHashIterations,
    passwordHashMemoryKibibytes,
    passwordHashNeedsRehash,
    passwordHashParallelism,
    passwordWorkBudgetKibibytes,
    readPasswordHash,
    readPasswordHashWithWorkKibibytes,
    verifyPassword,
    withPasswordWork,
  )
where

import Crypto.Error (CryptoFailable (..))
import Crypto.KDF.Argon2 qualified as Argon2
import Crypto.Random.Entropy (getEntropy)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word32, Word64)
import HarchWeb.Password.WorkBudget
  ( PasswordWorkBudget,
    PasswordWorkGate,
    mkPasswordWorkBudget,
    newPasswordWorkGate,
    passwordWorkBudgetKibibytes,
    withPasswordWork,
  )
import HarchWeb.Password.WorkBudget qualified as WorkBudget
import HarchWeb.Security.ConstantTime (constantWorkEquals)
import Text.Read (readMaybe)

newtype Password = Password ByteString.ByteString

-- | The reference application's shared 512-MiB Argon2 admission capacity and
-- eight-operation CPU-concurrency ceiling.
defaultPasswordWorkBudget :: PasswordWorkBudget
defaultPasswordWorkBudget = WorkBudget.defaultPasswordWorkBudget

newtype PasswordHash = PasswordHash Text

data PasswordHashingPolicy = PasswordHashingPolicy
  { passwordHashIterations :: Word32,
    passwordHashMemoryKibibytes :: Word32,
    passwordHashParallelism :: Word32
  }
  deriving (Eq, Show)

defaultPasswordHashingPolicy :: PasswordHashingPolicy
defaultPasswordHashingPolicy =
  PasswordHashingPolicy
    { passwordHashIterations = 3,
      passwordHashMemoryKibibytes = 65536,
      passwordHashParallelism = 1
    }

mkPassword :: Text -> Password
mkPassword = Password . TextEncoding.encodeUtf8

-- | The Argon2 time-cost parameter. Construct with 'argon2Iterations' so a
-- caller cannot transpose it with 'Argon2MemoryKib' or 'Argon2Parallelism' at
-- a 'mkPasswordHashingPolicy' call site.
newtype Argon2Iterations = Argon2Iterations Word32

argon2Iterations :: Word32 -> Argon2Iterations
argon2Iterations = Argon2Iterations

-- | The Argon2 memory-cost parameter, in kibibytes.
newtype Argon2MemoryKib = Argon2MemoryKib Word32

argon2MemoryKib :: Word32 -> Argon2MemoryKib
argon2MemoryKib = Argon2MemoryKib

-- | The Argon2 parallelism (lane count) parameter.
newtype Argon2Parallelism = Argon2Parallelism Word32

argon2Parallelism :: Word32 -> Argon2Parallelism
argon2Parallelism = Argon2Parallelism

-- | Validates both newly configured and persisted Argon2id resource costs at
-- the one policy boundary. The 10-iteration, 256-MiB, and 16-lane ceilings
-- allow a deliberate increase over the production default while preventing
-- stored hashes from turning verification into an unbounded memory, CPU, or
-- lane-allocation request. Raising them is a compatibility decision that needs
-- an accompanying password-rehash plan.
mkPasswordHashingPolicy :: Argon2Iterations -> Argon2MemoryKib -> Argon2Parallelism -> Maybe PasswordHashingPolicy
mkPasswordHashingPolicy (Argon2Iterations iterations) (Argon2MemoryKib memoryKibibytes) (Argon2Parallelism parallelism) =
  case iterations == 0
    || iterations > maximumArgon2Iterations
    || parallelism == 0
    || parallelism > maximumArgon2Parallelism
    || memoryKibibytes > maximumArgon2MemoryKibibytes
    || fromIntegral memoryKibibytes < minimumMemoryKibibytesPerLane * fromIntegral parallelism of
    True -> Nothing
    False -> Just (PasswordHashingPolicy iterations memoryKibibytes parallelism)

maximumArgon2Iterations :: Word32
maximumArgon2Iterations = 10

maximumArgon2MemoryKibibytes :: Word32
maximumArgon2MemoryKibibytes = 262144

maximumArgon2Parallelism :: Word32
maximumArgon2Parallelism = 16

minimumMemoryKibibytesPerLane :: Word64
minimumMemoryKibibytesPerLane = 8

hashPassword :: PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash)
hashPassword policy password = do
  salt <- getEntropy 16
  pure (hashPasswordWithSalt policy salt password)

hashPasswordWithSalt :: PasswordHashingPolicy -> ByteString.ByteString -> Password -> Maybe PasswordHash
hashPasswordWithSalt policy salt (Password password) =
  let hashValue = argon2Hash policy salt password
   in if ByteString.null hashValue
        then Nothing
        else
          Just
            ( PasswordHash
                ( "$argon2id$v=19$m="
                    <> Text.pack (show (passwordHashMemoryKibibytes policy))
                    <> ",t="
                    <> Text.pack (show (passwordHashIterations policy))
                    <> ",p="
                    <> Text.pack (show (passwordHashParallelism policy))
                    <> "$"
                    <> encodeBase64Url (Base64Url.encodeUnpadded salt)
                    <> "$"
                    <> encodeBase64Url (Base64Url.encodeUnpadded hashValue)
                )
            )

passwordHashText :: PasswordHash -> Text
passwordHashText (PasswordHash hashValue) = hashValue

-- | The validated Argon2 memory cost a verification would reserve.  A
-- malformed externally constructed 'PasswordHash' cannot trigger native work,
-- so it has no reservable cost.
passwordHashWorkKibibytes :: PasswordHash -> Maybe Word32
passwordHashWorkKibibytes (PasswordHash storedHash) =
  passwordHashMemoryKibibytes . firstOfThree <$> parsePasswordHash storedHash
  where
    firstOfThree (policy, _, _) = policy

-- | Whether a valid stored hash is uniformly weaker than a replacement
-- policy.  A mixed policy (stronger on one Argon2 cost and weaker on another)
-- is deliberately not replaced: opportunistic migration must never silently
-- lower a stored cost.  The login boundary verifies first and treats a failed
-- best-effort replacement as non-fatal, while its persistence adapter uses a
-- conditional update on the old hash to ensure concurrent successful logins
-- upgrade at most once.
passwordHashNeedsRehash :: PasswordHashingPolicy -> PasswordHash -> Bool
passwordHashNeedsRehash replacementPolicy (PasswordHash storedHash) =
  case parsePasswordHash storedHash of
    Nothing -> False
    Just (storedPolicy, _, _) ->
      policyNotStrongerThan storedPolicy replacementPolicy
        && policyDiffers storedPolicy replacementPolicy

policyNotStrongerThan :: PasswordHashingPolicy -> PasswordHashingPolicy -> Bool
policyNotStrongerThan storedPolicy replacementPolicy =
  passwordHashIterations storedPolicy <= passwordHashIterations replacementPolicy
    && passwordHashMemoryKibibytes storedPolicy <= passwordHashMemoryKibibytes replacementPolicy
    && passwordHashParallelism storedPolicy <= passwordHashParallelism replacementPolicy

policyDiffers :: PasswordHashingPolicy -> PasswordHashingPolicy -> Bool
policyDiffers storedPolicy replacementPolicy = storedPolicy /= replacementPolicy

readPasswordHash :: Text -> Maybe PasswordHash
readPasswordHash = fmap fst . readPasswordHashWithWorkKibibytes

-- | Parse a stored password hash once and retain the validated Argon2 memory
-- cost needed before its verification can begin. Consumers that first accept
-- a textual hash at a trust boundary can use this to carry that evidence into
-- a resource-admission decision without reparsing or admitting malformed
-- input to native Argon2 work.
readPasswordHashWithWorkKibibytes :: Text -> Maybe (PasswordHash, Word32)
readPasswordHashWithWorkKibibytes hashValue = do
  (policy, _, _) <- parsePasswordHash hashValue
  pure (PasswordHash hashValue, passwordHashMemoryKibibytes policy)

verifyPassword :: Password -> PasswordHash -> Bool
verifyPassword (Password password) (PasswordHash storedHash) =
  case parsePasswordHash storedHash of
    Nothing -> False
    Just (policy, salt, expectedHash) ->
      constantWorkEquals expectedHash (argon2Hash policy salt password)

argon2Hash :: PasswordHashingPolicy -> ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString
argon2Hash policy salt password =
  case Argon2.hash (argon2Options policy) password salt 32 of
    CryptoPassed hashValue -> hashValue
    CryptoFailed _ -> ByteString.empty

argon2Options :: PasswordHashingPolicy -> Argon2.Options
argon2Options policy =
  Argon2.defaultOptions
    { Argon2.iterations = passwordHashIterations policy,
      Argon2.memory = passwordHashMemoryKibibytes policy,
      Argon2.parallelism = passwordHashParallelism policy,
      Argon2.variant = Argon2.Argon2id,
      Argon2.version = Argon2.Version13
    }

parsePasswordHash :: Text -> Maybe (PasswordHashingPolicy, ByteString.ByteString, ByteString.ByteString)
parsePasswordHash storedHash = do
  ["", "argon2id", "v=19", parameters, encodedSalt, encodedHash] <- pure (Text.splitOn "$" storedHash)
  [memoryValue, iterationValue, parallelismValue] <- pure (Text.splitOn "," parameters)
  memoryKibibytes <- Text.stripPrefix "m=" memoryValue >>= readWord32
  iterations <- Text.stripPrefix "t=" iterationValue >>= readWord32
  parallelism <- Text.stripPrefix "p=" parallelismValue >>= readWord32
  policy <- mkPasswordHashingPolicy (argon2Iterations iterations) (argon2MemoryKib memoryKibibytes) (argon2Parallelism parallelism)
  salt <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedSalt))
  hashValue <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedHash))
  if ByteString.length salt < 16 || ByteString.length hashValue /= 32
    then Nothing
    else Just (policy, salt, hashValue)

readWord32 :: Text -> Maybe Word32
readWord32 = readMaybe . Text.unpack

encodeBase64Url :: ByteString.ByteString -> Text
encodeBase64Url = TextEncoding.decodeUtf8
