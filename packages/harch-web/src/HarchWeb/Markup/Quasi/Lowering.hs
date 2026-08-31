{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Lowers parsed nodes and typed component properties. Native-attribute
-- interpretation lives in 'HarchWeb.Markup.Quasi.AttributeLowering'.
module HarchWeb.Markup.Quasi.Lowering
  ( lowerNodes,
  )
where

import Data.Functor ((<&>))
import Data.List (find, intercalate)
import HarchWeb.Markup.Implementation qualified as Impl
import HarchWeb.Markup.Quasi.AttributeLowering (lowerAttribute)
import HarchWeb.Markup.Quasi.LoweringSupport
  ( failAt,
    parseExpression,
    textLiteral,
  )
import HarchWeb.Markup.Quasi.Parser
  ( MarkupAttribute (..),
    MarkupNode (..),
    Position,
  )
import Language.Haskell.TH
  ( Con (..),
    Dec (..),
    Exp (..),
    Info (..),
    Name,
    Q,
    Type (..),
    lookupValueName,
    mkName,
    nameBase,
    reify,
  )

lowerNodes :: [MarkupNode] -> Q Exp
lowerNodes nodes = traverse lowerNode nodes <&> (AppE (VarE 'Impl.fragment) . ListE)

lowerNode :: MarkupNode -> Q Exp
lowerNode node =
  case node of
    NativeNode position tagConstructor isVoid attributes children ->
      lowerNativeNode position tagConstructor isVoid attributes children
    ComponentNode position componentName attributes children ->
      lowerComponentNode position componentName attributes children
    RegionNode position expressionSource ->
      AppE (VarE 'Impl.regionHtml) <$> parseExpression position expressionSource
    InterpolationNode position expressionSource ->
      AppE (VarE 'Impl.toHtml) <$> parseExpression position expressionSource
    LiteralNode literal -> pure (AppE (VarE 'Impl.text) (textLiteral literal))

lowerNativeNode :: Position -> String -> Bool -> [MarkupAttribute] -> [MarkupNode] -> Q Exp
lowerNativeNode position tagConstructor isVoid attributes children = do
  tagExpression <- resolveNativeTag position tagConstructor
  attributeExpressions <- traverse lowerAttribute attributes
  childExpressions <- traverse lowerNode children
  if isVoid
    then pure (AppE (AppE (VarE 'Impl.voidElement) tagExpression) (ListE attributeExpressions))
    else
      pure
        ( AppE
            (AppE (AppE (VarE 'Impl.element) tagExpression) (ListE attributeExpressions))
            (ListE childExpressions)
        )

lowerComponentNode :: Position -> String -> [MarkupAttribute] -> [MarkupNode] -> Q Exp
lowerComponentNode position componentName attributes nestedChildren = do
  componentExpression <- parseExpression position componentName
  componentProperties <- lowerComponentProperties position componentName attributes
  childrenExpression <- lowerComponentChildren attributes nestedChildren
  let componentWithProperties =
        case componentProperties of
          NamedComponentProperties propertiesExpression -> AppE componentExpression propertiesExpression
          PositionalComponentProperties propertiesExpressions ->
            foldl AppE componentExpression propertiesExpressions
  pure (AppE componentWithProperties childrenExpression)

data ComponentProperties
  = NamedComponentProperties Exp
  | PositionalComponentProperties [Exp]

lowerComponentProperties :: Position -> String -> [MarkupAttribute] -> Q ComponentProperties
lowerComponentProperties componentPosition componentName attributes =
  case propsAttributes of
    [] ->
      NamedComponentProperties
        <$> lowerNamedComponentProperties componentPosition componentName namedAttributes
    [propsAttribute] ->
      case namedAttributes of
        [] -> lowerPositionalComponentProperties propsAttribute
        firstNamedAttribute : _ ->
          failAt
            (markupAttributePosition firstNamedAttribute)
            "props cannot be combined with named component properties"
    _ : duplicatePropsAttribute : _ ->
      failAt (markupAttributePosition duplicatePropsAttribute) "duplicate props component property"
  where
    propsAttributes = filter ((== "props") . markupAttributeName) attributes
    namedAttributes =
      filter
        ( \attribute ->
            let attributeName = markupAttributeName attribute
             in attributeName /= "props" && attributeName /= "children"
        )
        attributes

lowerNamedComponentProperties :: Position -> String -> [MarkupAttribute] -> Q Exp
lowerNamedComponentProperties componentPosition componentName attributes = do
  namedProperties <- traverse lowerNamedComponentProperty attributes
  rejectDuplicateComponentProperties namedProperties
  componentProps <- reifyComponentProps componentPosition componentName
  case componentProps of
    NullaryComponentProps propsConstructorName -> do
      rejectUnknownComponentProperties namedProperties []
      pure (ConE propsConstructorName)
    RecordComponentProps propsConstructorName fields -> do
      rejectUnknownComponentProperties namedProperties (map fst fields)
      rejectMissingComponentProperties componentPosition namedProperties (map fst fields)
      pure
        ( RecConE
            propsConstructorName
            [ (fieldName, propertyExpression)
            | (fieldLabel, fieldName) <- fields,
              Just (_, (_, propertyExpression)) <- [find ((== fieldLabel) . fst) namedProperties]
            ]
        )

lowerNamedComponentProperty :: MarkupAttribute -> Q (String, (Position, Exp))
lowerNamedComponentProperty attribute =
  case attribute of
    LiteralAttribute position attributeName literal ->
      pure (componentPropertyName attributeName, (position, textLiteral literal))
    ExpressionAttribute position attributeName expressionSource -> do
      expression <- parseExpression position expressionSource
      pure (componentPropertyName attributeName, (position, expression))
    FlagAttribute position attributeName ->
      failAt
        position
        ("component property " <> componentPropertyName attributeName <> " requires a quoted literal or {...} expression")

componentPropertyName :: String -> String
componentPropertyName attributeName =
  case attributeName of
    "aria-label" -> "ariaLabel"
    _ -> attributeName

lowerPositionalComponentProperties :: MarkupAttribute -> Q ComponentProperties
lowerPositionalComponentProperties attribute =
  case attribute of
    ExpressionAttribute position _ expressionSource -> do
      expression <- parseExpression position expressionSource
      case expression of
        ListE values -> pure (PositionalComponentProperties values)
        _ -> failAt position "props requires an inline list expression such as props={[first, second]}"
    _ -> failAt (markupAttributePosition attribute) "props requires an inline list expression"

lowerComponentChildren :: [MarkupAttribute] -> [MarkupNode] -> Q Exp
lowerComponentChildren attributes nestedChildren =
  case childrenAttributes of
    [] -> ListE <$> traverse lowerNode nestedChildren
    [ExpressionAttribute position _ expressionSource]
      | null nestedChildren -> parseExpression position expressionSource
      | otherwise -> failAt position "children cannot be combined with nested markup"
    [attribute] -> failAt (markupAttributePosition attribute) "children requires a {...} expression"
    _ : duplicateChildrenAttribute : _ ->
      failAt (markupAttributePosition duplicateChildrenAttribute) "duplicate children component property"
  where
    childrenAttributes = filter ((== "children") . markupAttributeName) attributes

data ComponentProps
  = NullaryComponentProps Name
  | RecordComponentProps Name [(String, Name)]

reifyComponentProps :: Position -> String -> Q ComponentProps
reifyComponentProps componentPosition componentName = do
  maybeComponentFunctionName <- lookupValueName componentName
  case maybeComponentFunctionName of
    Nothing -> failAt componentPosition ("could not resolve component function " <> componentName)
    Just componentValueName -> do
      componentInfo <- reify componentValueName
      case componentInfo of
        VarI _ componentType _ ->
          case firstFunctionArgumentType componentType of
            Nothing -> failAt componentPosition "components must take a props datatype as their first argument"
            Just propsTypeName -> do
              propsInfo <- reify propsTypeName
              case propsInfo of
                TyConI declaration ->
                  componentPropsFromDeclaration componentPosition (nameBase propsTypeName) declaration
                _ -> failAt componentPosition "component props must be a datatype"
        _ -> failAt componentPosition "components must be ordinary functions"

firstFunctionArgumentType :: Type -> Maybe Name
firstFunctionArgumentType componentType =
  case componentType of
    ForallT _ _ nestedType -> firstFunctionArgumentType nestedType
    AppT (AppT ArrowT propsType) _ -> typeConstructorName propsType
    SigT nestedType _ -> firstFunctionArgumentType nestedType
    ParensT nestedType -> firstFunctionArgumentType nestedType
    _ -> Nothing

typeConstructorName :: Type -> Maybe Name
typeConstructorName typeValue =
  case typeValue of
    ConT typeName -> Just typeName
    AppT nestedType _ -> typeConstructorName nestedType
    SigT nestedType _ -> typeConstructorName nestedType
    ParensT nestedType -> typeConstructorName nestedType
    _ -> Nothing

componentPropsFromDeclaration :: Position -> String -> Dec -> Q ComponentProps
componentPropsFromDeclaration componentPosition propsName declaration =
  case declaration of
    DataD _ _ _ _ constructors _ -> componentPropsFromConstructors componentPosition propsName constructors
    NewtypeD _ _ _ _ constructor _ -> componentPropsFromConstructors componentPosition propsName [constructor]
    _ -> failAt componentPosition ("expected " <> propsName <> " to be a props datatype")

componentPropsFromConstructors :: Position -> String -> [Con] -> Q ComponentProps
componentPropsFromConstructors componentPosition propsName constructors =
  case find ((== unqualifiedPropsName) . nameBase . componentConstructorName) constructors of
    Just (NormalC propsConstructorName []) -> pure (NullaryComponentProps propsConstructorName)
    Just (RecC propsConstructorName fields) ->
      pure (RecordComponentProps propsConstructorName [(nameBase fieldName, fieldName) | (fieldName, _, _) <- fields])
    Just _ -> failAt componentPosition (propsName <> " must use a nullary or record constructor")
    Nothing -> failAt componentPosition ("expected a " <> propsName <> " constructor")
  where
    unqualifiedPropsName = reverse (takeWhile (/= '.') (reverse propsName))

componentConstructorName :: Con -> Name
componentConstructorName constructor =
  case constructor of
    NormalC name _ -> name
    RecC name _ -> name
    InfixC _ name _ -> name
    ForallC _ _ nestedConstructor -> componentConstructorName nestedConstructor
    GadtC (name : _) _ _ -> name
    RecGadtC (name : _) _ _ -> name
    GadtC [] _ _ -> mkName ""
    RecGadtC [] _ _ -> mkName ""

rejectDuplicateComponentProperties :: [(String, (Position, Exp))] -> Q ()
rejectDuplicateComponentProperties properties =
  case duplicateProperty of
    Nothing -> pure ()
    Just (_, (position, _)) -> failAt position "duplicate component property"
  where
    duplicateProperty =
      find
        (\(propertyName, _) -> length (filter ((== propertyName) . fst) properties) > 1)
        properties

rejectUnknownComponentProperties :: [(String, (Position, Exp))] -> [String] -> Q ()
rejectUnknownComponentProperties properties fieldNames =
  case find (\(propertyName, _) -> propertyName `notElem` fieldNames) properties of
    Nothing -> pure ()
    Just (propertyName, (position, _)) -> failAt position ("unknown component property " <> propertyName)

rejectMissingComponentProperties :: Position -> [(String, (Position, Exp))] -> [String] -> Q ()
rejectMissingComponentProperties componentPosition properties fieldNames =
  case filter (`notElem` map fst properties) fieldNames of
    [] -> pure ()
    missingProperties ->
      failAt componentPosition ("missing component properties: " <> intercalate ", " missingProperties)

markupAttributeName :: MarkupAttribute -> String
markupAttributeName attribute =
  case attribute of
    LiteralAttribute _ attributeName _ -> attributeName
    ExpressionAttribute _ attributeName _ -> attributeName
    FlagAttribute _ attributeName -> attributeName

markupAttributePosition :: MarkupAttribute -> Position
markupAttributePosition attribute =
  case attribute of
    LiteralAttribute position _ _ -> position
    ExpressionAttribute position _ _ -> position
    FlagAttribute position _ -> position

-- | Mirrors 'HarchWeb.Markup.Quasi.Parser.nativeTagConstructors' set. The
-- parser only produces a native tag constructor string that is a key here;
-- the 'Nothing' case is defensive drift detection.
nativeTagNames :: [(String, Name)]
nativeTagNames =
  [ ("anchorTag", 'Impl.anchorTag),
    ("breakTag", 'Impl.breakTag),
    ("buttonTag", 'Impl.buttonTag),
    ("codeTag", 'Impl.codeTag),
    ("divTag", 'Impl.divTag),
    ("dialogTag", 'Impl.dialogTag),
    ("formTag", 'Impl.formTag),
    ("headingOneTag", 'Impl.headingOneTag),
    ("headingTwoTag", 'Impl.headingTwoTag),
    ("horizontalRuleTag", 'Impl.horizontalRuleTag),
    ("imageTag", 'Impl.imageTag),
    ("inputTag", 'Impl.inputTag),
    ("labelTag", 'Impl.labelTag),
    ("listItemTag", 'Impl.listItemTag),
    ("listTag", 'Impl.listTag),
    ("mainTag", 'Impl.mainTag),
    ("metaTag", 'Impl.metaTag),
    ("optionTag", 'Impl.optionTag),
    ("paragraphTag", 'Impl.paragraphTag),
    ("sectionTag", 'Impl.sectionTag),
    ("selectTag", 'Impl.selectTag),
    ("spanTag", 'Impl.spanTag)
  ]

resolveNativeTag :: Position -> String -> Q Exp
resolveNativeTag position tagConstructor =
  case lookup tagConstructor nativeTagNames of
    Just name -> pure (VarE name)
    Nothing -> failAt position ("unsupported native element " <> tagConstructor)
