-- |
-- Module:      Main
-- Description: Unit tests for HTTP Trace Context library
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Unit tests for HTTP Trace Context library.
module Main
    ( main
    )
  where

import Data.Bool (Bool(True))
import Data.Maybe (Maybe(Just, Nothing))
import System.IO (IO)

--import Data.CallStack (HasCallStack)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Network.HTTP.TraceContext


main :: IO ()
main = defaultMain (testGroup "Network.HTTP.TraceContext.Tests" tests)

tests :: [TestTree]
tests =
    [ testGroup "parseRawTraceFlags"
        [ testCase "0x00" do
            parseRawTraceFlags 0x00 @?= emptyTraceFlags

        , testCase "0x01" do
            parseRawTraceFlags 0x01 @?= emptyTraceFlags{sampled = True}

        , testCase "0x0f" do
            parseRawTraceFlags 0x0f @?= emptyTraceFlags{sampled = True}
        ]

    , testGroup "renderRawTraceFlags"
        [ testCase "emptyTraceFlags" do
            renderRawTraceFlags emptyTraceFlags @?= 0x00

        , testCase "emptyTraceFlags{sampled = True}" do
            renderRawTraceFlags emptyTraceFlags{sampled = True} @?= 0x01
        ]

    , testGroup "renderTraceParent"
        [ testCase "00-4bf92f3577b34da6a3ce929d0e0e4736-d75597dee50b0cac-00" do
            let actual = renderTraceParent TraceData
                    { version = traceFormatVersion00
                    , traceId = TraceId "4bf92f3577b34da6a3ce929d0e0e4736"
                    , parentId = ParentId "d75597dee50b0cac"
                    , flags = emptyTraceFlags
                    , state = [] -- Actually ignored by renderTraceParent
                    }

            -- Example taken from:
            -- https://w3c.github.io/trace-context/#tail-sampling
            actual @?= "00-4bf92f3577b34da6a3ce929d0e0e4736-d75597dee50b0cac-00"

        , testCase "00-4bf92f3577b34da6a3ce929d0e0e4736-d75597dee50b0cac-01" do
            let actual = renderTraceParent TraceData
                    { version = traceFormatVersion00
                    , traceId = TraceId "4bf92f3577b34da6a3ce929d0e0e4736"
                    , parentId = ParentId "d75597dee50b0cac"
                    , flags = emptyTraceFlags{sampled = True}
                    , state = [] -- Actually ignored by renderTraceParent
                    }

            -- Example based on the one from:
            -- https://w3c.github.io/trace-context/#tail-sampling
            actual @?= "00-4bf92f3577b34da6a3ce929d0e0e4736-d75597dee50b0cac-01"
        ]

    , testGroup "renderTraceResponse"
        [ testCase "00---01" do
            let actual = renderTraceResponse TraceResponse
                    { version = traceFormatVersion00
                    , traceId = Nothing
                    , proposedParentId = Nothing
                    , traceFlags = Just emptyTraceFlags{sampled = True}
                    }

            -- Example taken from:
            -- https://w3c.github.io/trace-context/#tail-sampling
            actual @?= "00---01"

        -- So called "restarted trace":
        , testCase "00-1baad25c36c11c1e7fbd6d122bd85db6--01" do
            let actual = renderTraceResponse TraceResponse
                    { version = traceFormatVersion00
                    , traceId =
                        Just (TraceId "1baad25c36c11c1e7fbd6d122bd85db6")
                    , proposedParentId = Nothing
                    , traceFlags = Just emptyTraceFlags{sampled = True}
                    }

            -- Example taken from:
            -- https://w3c.github.io/trace-context/#restarted-trace
            actual @?= "00-1baad25c36c11c1e7fbd6d122bd85db6--01"


        , testCase "00-4bf92f3577b34da6a3ce929d0e0e4736-d75597dee50b0cac-01" do
            let actual = renderTraceResponse TraceResponse
                    { version = traceFormatVersion00
                    , traceId =
                        Just (TraceId "4bf92f3577b34da6a3ce929d0e0e4736")
                    , proposedParentId = Just (ParentId "d75597dee50b0cac")
                    , traceFlags = Just emptyTraceFlags{sampled = True}
                    }

            -- Example taken from:
            -- https://w3c.github.io/trace-context/#web-browser
            actual @?= "00-4bf92f3577b34da6a3ce929d0e0e4736-d75597dee50b0cac-01"
        ]
    ]
