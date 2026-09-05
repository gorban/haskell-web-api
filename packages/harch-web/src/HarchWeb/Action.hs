-- | Stable authoring facade for declarative client actions.
--
-- Decision record (PR-F5, 2026-09-05): retain one public action surface while
-- private owners separate field decoding, validated endpoint declarations,
-- and trusted mount adaptation. The named mount adapter replaces a positional
-- mapping bundle without adding a second action router or weakening the
-- validated-endpoint invariant.
module HarchWeb.Action
  ( ActionCodec,
    ActionCodecError (..),
    ActionCodecMountAdapter (..),
    ActionDecoder,
    ActionEndpoint,
    ActionMethod (..),
    ActionPath,
    ClientActionDecodeResult (..),
    ClientActionIdempotencyKey,
    ClientActionParseError (..),
    ClientActionPayload (..),
    FieldValue,
    FormField,
    action,
    actionCodec,
    actionEndpointMetadata,
    actionEndpointTarget,
    actionMethod,
    actionMethodText,
    actionPath,
    actionWithMetadata,
    combineActionCodecs,
    declaredActionEndpointMetadata,
    decodeAction,
    delete,
    deleteAt,
    emptyActionCodec,
    exactlyOne,
    formField,
    get,
    getAt,
    mapActionCodec,
    methodAt,
    mountActionCodecAtPrefix,
    optional,
    parseField,
    patch,
    patchAt,
    post,
    postAt,
    prefixActionCodecByContext,
    publicAction,
    put,
    putAt,
    required,
    singleActionCodec,
    singleActionCodecWithMetadata,
    singleOrDefault,
    staticActionEndpointMetadata,
    staticActionPath,
    textValue,
  )
where

import HarchWeb.Action.Codec
import HarchWeb.Action.Field
import HarchWeb.Action.Mount
