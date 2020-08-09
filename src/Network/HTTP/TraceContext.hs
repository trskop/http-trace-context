-- |
-- Module:      Network.HTTP.TraceContext
-- Description: Parsing and rendering of Trace Context HTTP headers
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Parsing and rendering of Trace Context HTTP headers for distributed tracing.
--
-- This implementation is based on W3C standard:
--
-- * <https://www.w3.org/TR/trace-context-1/ Trace Context Level 1>
--
-- * <https://w3c.github.io/trace-context/ Trace Context Level 2 (Draft)>
module Network.HTTP.TraceContext
    (
    -- $docs

    -- * Trace Format Version
      TraceFormatVersion
    , traceFormatVersion00
    , renderTraceFormatVersionBuilder

    -- * Trace Flags
    , RawTraceFlags
    , TraceFlags(..)
    , emptyTraceFlags
    , parseRawTraceFlags
    , renderRawTraceFlags
    , renderTraceFlagsBuilder

    -- * Trace ID
    , TraceId(..)
    , renderTraceIdBuilder

    -- * Parent ID
    , ParentId(..)
    , renderParentIdBuilder

    -- * Request Trace Headers
    , TraceData(..)
    , traceData00

    -- ** @traceparent@
    , hTraceParent
    , renderTraceParentHeader
    , renderTraceParent
    , renderTraceParentBuilder

    -- ** @tracestate@
    , hTraceState
    , renderTraceStateHeader
    , renderTraceState
    , renderTraceStateBuilder

    -- * Response Trace Headers

    -- ** @traceresponse@
    , hTraceResponse
    , TraceResponse(..)
    , renderTraceResponseHeader
    , renderTraceResponse
    , renderTraceResponseBuilder
    )
  where

import Data.Bits ((.&.))
import Data.Bool (Bool(False))
import Data.Eq (Eq, (/=))
import Data.Function (($), (.))
import Data.Functor (fmap)
import qualified Data.List as List (intercalate, take)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (mconcat, mempty)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Show (Show)

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Builder as ByteString (Builder)
import Data.ByteString.Builder as ByteString.Builder
    ( byteString
    , char7
    , toLazyByteString
    , word8HexFixed
    )
import Data.ByteString.Lazy as ByteString (toStrict)
import Network.HTTP.Types.Header (Header, HeaderName)

import Network.HTTP.TraceContext.Internal ()


-- Notes:
--
-- Most of the tracing functionality should be either a Wai middleware or done
-- by a proxy. Service itself should be handling the passing of it to
-- subsequent calls as is or modified.
--
-- If implemented solely on the server side these steps should be done:
--
-- 1. Parse @traceparent@ and @tracestate@ headers. If @traceparent@ header is
--    not present then responsibility of the service to "restart tracing". In
--    other words it needs to start a new tracing tree and notify the caller
--    about this fact in @traceresponse@ (step 5).
--
-- 2. Interpret trace ID and trace state. Interpretation of these depends on a
--    tracing system that is in use.
--
-- 3. Log tracing information if desired. Depends on multiple factors, but in
--    large systems only random samples or explicitly requested trees are
--    logged (sampled).
--
-- 4. If the call is not terminating in the service then @traceparent@ and
--    @tracestate@ need to be passed to subsequent call. Modified or unmodified,
--    depending on a use case.
--
-- 5. Set appropriate @traceresponse@ so that it's sent to the caller.

-- | Trace Context specification version encoded as a value between 0 (0x00)
-- and 255 (0xff).  Value 255 (0xff) is forbidden and value 0 (0x00) is the
-- version of current standard.
--
-- <https://www.w3.org/TR/trace-context-1/#version>
-- <https://w3c.github.io/trace-context/#version-0>
type TraceFormatVersion = Word8

-- | Version of currently defined Trace Context format is @00@.
traceFormatVersion00 :: TraceFormatVersion
traceFormatVersion00 = 0x00

renderTraceFormatVersionBuilder :: TraceFormatVersion -> ByteString.Builder
renderTraceFormatVersionBuilder = ByteString.Builder.word8HexFixed

-- | Trace flags are an 8-bit field where individual bits may have meaning when
-- set.
--
-- <https://www.w3.org/TR/trace-context-1/#trace-flags>
-- <https://w3c.github.io/trace-context/#trace-flags-0>
type RawTraceFlags = Word8

data TraceFlags = TraceFlags
    { sampled :: Bool
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Parse trace flags bit field into a structure. No error reporting as
-- unknown bits are ignored when set. This also means that higher level code
-- needs to check that the value is not @0xff@.
parseRawTraceFlags :: RawTraceFlags -> TraceFlags
parseRawTraceFlags flags = TraceFlags
    { sampled = (flags .&. flagSampledMask) /= 0
    }

renderRawTraceFlags :: TraceFlags -> RawTraceFlags
renderRawTraceFlags TraceFlags{sampled} =
    if sampled
        then flagSampledMask
        else 0

renderTraceFlagsBuilder :: TraceFlags -> ByteString.Builder
renderTraceFlagsBuilder =
    ByteString.Builder.word8HexFixed . renderRawTraceFlags

flagSampledMask :: RawTraceFlags
flagSampledMask = 0x01

-- | Value of 'TraceFlags' with all flags off.
--
-- Invariants:
--
-- >>> parseRawTraceFlags 0x00 == emptyTraceFlags
-- True
-- >>> renderRawTraceFlags emptyTraceFlags == 0x00
-- True
emptyTraceFlags :: TraceFlags
emptyTraceFlags = TraceFlags{sampled = False}

-- | Trace ID is an ID of the whole trace forest and is used to uniquely
-- identify a distributed trace through a system. It is represented as 32
-- hexadecimal digits (@0-f@, lowercase only), i.e. 16 bytes; it SHOULD be
-- globally unique and cannot be all zero (@00000000000000000000000000000000@),
-- which is considered an invalid value.
newtype TraceId = TraceId
    { traceId :: ByteString
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Render trace ID portion of trace headers.  It relies on:
--
-- * Input is sequence of hexadecimal digits (@0-f@).
-- * Input is exactly 32 hexadecimal digits long.
--
-- Trace Context standard dictates that these values SHOULD be randomly
-- generated to improve sampling and avoid security issues.
renderTraceIdBuilder :: TraceId -> ByteString.Builder
renderTraceIdBuilder TraceId{traceId} =
    ByteString.Builder.byteString traceId
    -- TODO: We need to make sure that traceId is 32 nibbles long. To avoid
    -- complex error handling we could do truncation as the value needs to be
    -- random generated (per standard) and we could pad it with zeros if it's
    -- shorter. The only corner case left is when it's empty since all zeros is
    -- invalid value. We could generate it as that will make subsequent user
    -- fail on parsing and we would (hopefully) get logs about it.
    --
    -- We could avoid a lot of the above by moving the newtype definition into
    -- Internal module.

-- | Request ID is the ID of the request as known by the caller, it is also
-- known under the name "span ID", where a span represents the execution of a
-- client request. It is represented as 16 hexadecimal digits (@0-f@, lowercase
-- only, i.e. 8 bytes. Zero value (@0000000000000000@) is considered invalid.
newtype ParentId = ParentId
    { parentId :: ByteString
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Render parent ID (also known as span ID) portion of trace headers.  It
-- relies on:
--
-- * Input is sequence of hexadecimal digits (@0-f@).
-- * Input is exactly 16 hexadecimal digits long.
--
-- Trace Context standard dictates that these values SHOULD be randomly
-- generated to improve sampling and avoid security issues.
renderParentIdBuilder :: ParentId -> ByteString.Builder
renderParentIdBuilder ParentId{parentId} =
    ByteString.Builder.byteString parentId
    -- TODO: We need to make sure that parentId is 16 nibbles long. To avoid
    -- complex error handling we could do truncation as the value needs to be
    -- random generated (per standard) and we could pad it with zeros if it's
    -- shorter. The only corner case left is when it's empty since all zeros is
    -- invalid value. We could generate it as that will make subsequent user
    -- fail on parsing and we would (hopefully) get logs about it.
    --
    -- We could avoid a lot of the above by moving the newtype definition into
    -- Internal module.

-- {{{ Request ----------------------------------------------------------------

-- | Structured representation of @traceparent@ and @tracestate@ headers.
--
-- Reason why values from @traceparent@ and @tracestate@ values are combined is
-- because even systems that do not use @tracestate@ value are required to pass
-- it along to subsequent calls and it SHOULD be in a normalised form.
--
-- Be aware of the following:
--
-- * 'Eq' instance is derived and compares all fields as they are. It may not
--   be the most appropriate in all cases.
--
-- * 'Show' instance is derived and renders 'TraceData' as a Haskell data
--   type. To get representation that goes into the HTTP headers use various
--   @renderTraceParent*@ and @renderTraceState*@ functions instead.
data TraceData = TraceData
    { version :: TraceFormatVersion
    -- ^ Trace Context standard version as declared in the @traceparent@ header
    -- value.

    , traceId :: TraceId
    -- ^ Value of trace ID as it was specified in the @traceparent@ header
    -- value. It is a sequence of 32 hexadecimal digits (@0-f@) encoding
    -- 16-byte array.

    , parentId :: ParentId
    -- ^ Value of parent ID as it was specified in the @traceparent@ header
    -- value. It is a sequence of 16 hexadecimal digits (@0-f@) encoding
    -- 8-byte array.

    , flags :: TraceFlags
    -- ^ Parsed flags portion of @traceparent@ header value.  See 'TraceFlags'
    -- for more information.

    , state :: [(ByteString, ByteString)]
    -- ^ Vendor-specific tracing information is a list of @key=value@ pairs
    -- that are treated as opaque unless understood by the system.
    --
    -- Specification dictates that:
    --
    -- * ordering needs to be preserved,
    -- * empty values are allowed
    -- * and only one entry per key is allowed (caveat below applies).
    --
    -- Specification dictates that if new value of @key@ is added it overrides
    -- the existing value. If we follow
    -- <https://w3c.github.io/trace-context/#combined-header-value section 3.3.3 Combined Header Value>
    -- of the specification to it's logical conclusion then the last value (in
    -- the list) overwrites the previous existing definition.  Since this field
    -- contains "raw" @tracestate@ value it is up to user to handle it
    -- correctly when inserting\/modifying value.
    --
    -- TODO: Use newtypes and maybe a different data structure to guarantee
    -- that the value has the correct format.
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Smart constructor for 'TraceData' with 'TraceFormatVersion' @0x00@. If new
-- trace format is introduced in the future then the data type may be extended,
-- relying instead on this constructor will allow easier transition.
traceData00 :: TraceId -> ParentId -> TraceData
traceData00 traceId parentId = TraceData
    { version = traceFormatVersion00
    , traceId
    , parentId
    , flags = emptyTraceFlags
    , state = []
    }

{-
parseTraceParent
    :: ByteString
    -- ^ Raw value of @traceparent@ header.
    -> Either String TraceData
parseTraceParent traceparent =
-}

-- | Example of a @traceparent@ header value:
--
-- > traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-d75597dee50b0cac-00
renderTraceParentHeader :: TraceData -> Header
renderTraceParentHeader traceData = (hTraceParent, renderTraceParent traceData)

renderTraceParent :: TraceData -> ByteString
renderTraceParent =
    ByteString.toStrict
    . ByteString.Builder.toLazyByteString
    . renderTraceParentBuilder

renderTraceParentBuilder :: TraceData -> ByteString.Builder
renderTraceParentBuilder TraceData{version, traceId, parentId, flags} = mconcat
    [ renderTraceFormatVersionBuilder version
    , dash
    , renderTraceIdBuilder traceId
    , dash
    , renderParentIdBuilder parentId
    , dash
    , renderTraceFlagsBuilder flags
    ]
  where
    dash = ByteString.Builder.char7 '-'

renderTraceStateHeader :: TraceData -> Header
renderTraceStateHeader traceData = (hTraceState, renderTraceState traceData)

renderTraceState :: TraceData -> ByteString
renderTraceState =
    ByteString.toStrict
    . ByteString.Builder.toLazyByteString
    . renderTraceStateBuilder

renderTraceStateBuilder :: TraceData -> ByteString.Builder
renderTraceStateBuilder TraceData{state} =
    -- This is a very naive way of implementing it. Some things to improve:
    --
    -- * There is an overall size limit as well as number of entries and it
    --   needs to be taken into account.
    --
    -- * The whole algorithm is naive and could benefit from a one pass fold.
    --   Needs performance tests to evaluate as there may be fusion rules.
    concatList . fmap renderKeyValue $ List.take maxEntries state
  where
    -- Standard specified that at most 32 entries are allowed.
    maxEntries = 32

    renderKeyValue :: (ByteString, ByteString) -> [ByteString.Builder]
    renderKeyValue (key, value) =
        [ ByteString.Builder.byteString key
        , ByteString.Builder.char7 '='
        , ByteString.Builder.byteString value
        ]

    -- Standard says that space SHOULD follow comma.
    concatList :: [[ByteString.Builder]] -> ByteString.Builder
    concatList =
        mconcat . List.intercalate [ByteString.Builder.byteString ", "]

-- | Trace Context HTTP request header name @traceparent@ (yes, all lowercase
-- per specification).
--
-- <https://w3c.github.io/trace-context/#traceparent-header>
hTraceParent :: HeaderName
hTraceParent = "traceparent"

-- | Trace Context HTTP request header name @tracestate@ (yes, all lowercase
-- per specification).
--
-- <https://w3c.github.io/trace-context/#tracestate-header>
hTraceState :: HeaderName
hTraceState = "tracestate"

-- }}} Request ----------------------------------------------------------------

-- {{{ Response ---------------------------------------------------------------

-- |
--
-- > traceresponse = version "-" [trace-id] "-" [proposed-parent-id] "-" [trace-flags]
data TraceResponse = TraceResponse
    { version :: TraceFormatVersion
    , traceId :: Maybe TraceId
    -- ^ Value SHOULD be omitted when callee uses the same trace ID as caller.
    --
    -- If @traceparent@ header was missing or failed to parse then 'traceId'
    -- SHOULD be specified.
    , proposedParentId :: Maybe ParentId
    -- ^ Value SHOULD be omitted when callee uses the same parent ID as caller.
    --
    -- If @traceparent@ header was missing or failed to parse then 'traceId'
    -- SHOULD be specified.
    , traceFlags :: Maybe TraceFlags
    -- ^ Value SHOULD be omitted if callee hadn't changed them. For example
    -- callee may have made a decision to sample this trace forest.
    }

renderTraceResponseBuilder :: TraceResponse -> ByteString.Builder
renderTraceResponseBuilder TraceResponse{..} = mconcat
    [ renderTraceFormatVersionBuilder version
    , ByteString.Builder.char7 '-'
    -- TODO: We need to make sure that traceId consists of 32 nibbles. See
    , maybe mempty renderTraceIdBuilder traceId
    , ByteString.Builder.char7 '-'
    , maybe mempty renderParentIdBuilder proposedParentId
    , ByteString.Builder.char7 '-'
    , maybe mempty renderTraceFlagsBuilder traceFlags
    ]

renderTraceResponse :: TraceResponse -> ByteString
renderTraceResponse =
    ByteString.toStrict
    . ByteString.Builder.toLazyByteString
    . renderTraceResponseBuilder

renderTraceResponseHeader :: TraceResponse -> Header
renderTraceResponseHeader r = (hTraceResponse, renderTraceResponse r)

-- | Trace Context HTTP response header name @traceresponse@ (yes, all
-- lowercase per specification).
--
-- <https://w3c.github.io/trace-context/#traceresponse-header>
hTraceResponse :: HeaderName
hTraceResponse = "traceresponse"

-- }}} Response ---------------------------------------------------------------

-- $docs
--
-- TODO:
--
-- * Describe difference between 'TraceId' and 'ParentId'
--
-- * Describe algorithms for passing tracing information along to subsequent
--   calls: i) no mutation, ii) mutating 'ParentId'.
--
-- * Describe what 'sampled' is used for and how it can be used. Don't go into
--   too much detail as it's system specific.
--
-- * Describe process of "restarting tracing" and when it is necessary. This
--   should include information about @traceresponse@ header as it's very
--   useful in this scenario.
--
-- * Describe logging requirements for tracing to be useful. Describing some
--   important bits of OpenTracing and pointing to its documentation may be the
--   best approach.
