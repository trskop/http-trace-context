-- |
-- Module:      Network.HTTP.TraceContext.Internal
-- Description: Internal definitions for Trace Context HTTP headers
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Internal definitions for Trace Context HTTP headers. By depending on this
-- module you're depending on library internals that may change even with patch
-- releases. Use with caution.
module Network.HTTP.TraceContext.Internal
    (
      RawTraceRequest(..)
    , parseRawTraceRequest
    , normaliseTrceState
    )
  where

import Data.Either (Either)
import Data.Eq (Eq)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Show (Show)

import Control.DeepSeq (NFData)
import Data.Attoparsec.ByteString ()
import Data.ByteString (ByteString)


-- | Low-level representation of @traceparent@ and @tracestate@ headers. Idea
-- here is to provide intermediate representation that is close to the actual
-- representation. Module "Network.HTTP.TraceContext" then provides more user
-- friendly representation. This should allow us to easily extend\/optimise
-- in the future without breaking API too much.
--
-- Another benefit, of having this data type, is that in case of proxies we may
-- want to do as little parsing and interpretation as possible while being able
-- to log the relevant information. This data type should be used for such
-- purposes instead of the user-friendly one. Rendering (of this data type) is
-- not required in case of pass-through proxies as the values should be passed
-- unmodified. Only exception to that is "restart tracing" scenario.
--
-- Simplified grammar of @traceparent@ and @tracestate@ headers in ABNF
-- (Augmented Backus-Naur Form notation
-- <https://tools.ietf.org/html/rfc5234 RFC5234>):
--
-- > traceparent = version "-" trace-id "-" parent-id "-" trace-flags
-- > tracestate = key "=" value 0*31("," key "=" value )
--
-- Please, be aware that the above grammar is oversimplification intended to
-- help to understand this data type. Trace Context standard needs to be
-- consulted for details.
--
-- Reason why values from @traceparent@ and @tracestate@ values are combined is
-- because even systems that do not use @tracestate@ value are required to pass
-- it along to subsequent calls and it SHOULD be in a normalised form.
data RawTraceRequest = RawTraceRequest
    { rawVersion :: Word8
    -- ^ Trace Context standard version as declared in the @traceparent@ header
    -- value.

    , rawTraceId :: ByteString
    -- ^ Value of trace ID as it was specified in the @traceparent@ header
    -- value. It is a sequence of 32 hexadecimal digits (@0-f@) encoding
    -- 16-byte array.

    , rawParentId :: ByteString
    -- ^ Value of parent ID as it was specified in the @traceparent@ header
    -- value. It is a sequence of 16 hexadecimal digits (@0-f@) encoding
    -- 8-byte array.

    , rawFlags :: Word8
    -- ^ Raw portion of @traceparent@ header.

    , rawState :: ByteString
    -- ^ Raw value of vendor-specific tracing information, which is a list of
    -- @key=value@ pairs separated by commas.  Value is treated as opaque
    -- unless understood by the system.

    , rawStatePairs :: [(ByteString, ByteString)]
    -- ^ Raw list of parsed @key=value@ entries from 'rawState'.
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

parseRawTraceRequest
    :: ByteString
    -- ^ Value of @traceparent@ environment variable.
    -> Maybe ByteString
    -- ^ Value of @tracestate@ environment variable. 'Nothing' if not received.
    -> Either String RawTraceRequest
parseRawTraceRequest =
    -- TODO:
    --
    -- 1. Parse @traceparent@, if that fails we won't even attempt to parse
    --    @tracestate@ header, as is mandated by Trace Context standard.
    --
    -- 2. Parse but don't normalise @tracestate@ header. Be aware that empty
    --    values in various forms and lots of whitespace should be accepted.
    parseRawTraceRequest -- TODO!

-- | Normalise value of @tracestate@ (apply limits) as described in:
-- <https://w3c.github.io/trace-context/#combined-header-value>
-- and <https://w3c.github.io/trace-context/#tracestate-limits>
normaliseTrceState :: RawTraceRequest -> RawTraceRequest
normaliseTrceState =
    normaliseTrceState -- TODO!
    -- The logic described by the standard is pretty complicated and we may
    -- want to implement the most important bits of it first:
    --
    -- * Prune empty values as their purpose is to simplify generation.
    -- * Apply 32 entries limitation.
    -- * One item per key limitation.
    --
    -- Things for later:
    --
    -- * Maximum size limit. (Make it configurable?)
    -- * Logic for pruning entries when maximum size has been reached. This
    --   requires starting with long entries first. See standard.
