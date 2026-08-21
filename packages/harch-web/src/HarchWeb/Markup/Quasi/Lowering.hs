{-# LANGUAGE TemplateHaskellQuotes #-}

module HarchWeb.Markup.Quasi.Lowering
  ( lowerNodes,
  )
where

import Data.Functor ((<&>))
import Data.List (find, intercalate)
import Data.String (fromString)
import Data.Text qualified as Text
import HarchWeb.Markup.Implementation qualified as Impl
import HarchWeb.Markup.Quasi.Parser
  ( MarkupAttribute (..),
    MarkupNode (..),
    Position (..),
  )
import Language.Haskell.Meta.Parse qualified as Meta
import Language.Haskell.TH
  ( Con (..),
    Dec (..),
    Exp (..),
    Info (..),
    Lit (..),
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
    then
      pure (AppE (AppE (VarE 'Impl.voidElement) tagExpression) (ListE attributeExpressions))
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
    DataD _ _ _ _ constructors _ ->
      componentPropsFromConstructors componentPosition propsName constructors
    NewtypeD _ _ _ _ constructor _ ->
      componentPropsFromConstructors componentPosition propsName [constructor]
    _ -> failAt componentPosition ("expected " <> propsName <> " to be a props datatype")

componentPropsFromConstructors :: Position -> String -> [Con] -> Q ComponentProps
componentPropsFromConstructors componentPosition propsName constructors =
  case find ((== unqualifiedPropsName) . nameBase . componentConstructorName) constructors of
    Just (NormalC propsConstructorName []) -> pure (NullaryComponentProps propsConstructorName)
    Just (RecC propsConstructorName fields) ->
      pure
        ( RecordComponentProps
            propsConstructorName
            [(nameBase fieldName, fieldName) | (fieldName, _, _) <- fields]
        )
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

lowerAttribute :: MarkupAttribute -> Q Exp
lowerAttribute markupAttribute =
  case markupAttribute of
    LiteralAttribute position attributeName literal ->
      lowerLiteralAttribute position attributeName literal
    ExpressionAttribute position attributeName expressionSource ->
      lowerExpressionAttribute position attributeName expressionSource
    FlagAttribute position attributeName -> lowerFlagAttribute position attributeName

lowerLiteralAttribute :: Position -> String -> String -> Q Exp
lowerLiteralAttribute position attributeName literal =
  case attributeName of
    "id" -> applyNamed 'Impl.elementId [applyNamedPure 'Impl.literalElementId [textLiteral literal]]
    "for" -> applyNamed 'Impl.labelFor [applyNamedPure 'Impl.literalElementId [textLiteral literal]]
    "class" -> failAt position "class requires an interpolated CssClass expression"
    -- 'href' takes a 'SafeUrl', not 'Text': a quoted literal in markup
    -- source is the template author's own text, validated once at compile
    -- time through 'SafeUrl'\'s 'IsString' instance (the same trust level
    -- 'fromStringLiteral' already gives a literal Haskell string), not
    -- through the plain-'Text' 'textLiteral' every other text attribute uses.
    "href" -> lowerTextAttribute position attributeName (fromStringLiteral literal)
    _ -> lowerTextAttribute position attributeName (textLiteral literal)

lowerExpressionAttribute :: Position -> String -> String -> Q Exp
lowerExpressionAttribute position attributeName expressionSource = do
  expression <- parseExpression position expressionSource
  case attributeName of
    "id" -> applyNamed 'Impl.elementId [expression]
    "for" -> applyNamed 'Impl.labelFor [expression]
    "class" -> applyNamed 'Impl.className [expression]
    _ -> lowerTextAttribute position attributeName expression

lowerFlagAttribute :: Position -> String -> Q Exp
lowerFlagAttribute position attributeName =
  case attributeName of
    "required" -> pure (VarE 'Impl.required)
    _
      | Just suffix <- dataAttributeSuffix attributeName -> applyNamed 'Impl.dataFlag [fromStringLiteral suffix]
      | otherwise -> failAt position ("unsupported boolean attribute " <> attributeName)

lowerTextAttribute :: Position -> String -> Exp -> Q Exp
lowerTextAttribute position attributeName valueExpression =
  case lookup attributeName textAttributeConstructors of
    Just constructorName -> applyNamed constructorName [valueExpression]
    Nothing
      | Just suffix <- dataAttributeSuffix attributeName ->
          -- The suffix always comes from the static attribute name parsed
          -- from markup source (never from an interpolated expression), so
          -- it goes through 'DataAttributeSuffix'\'s 'IsString' instance
          -- via 'fromStringLiteral' regardless of whether the *value* half
          -- was a literal or an interpolated expression.
          applyNamed 'Impl.dataAttribute [fromStringLiteral suffix, valueExpression]
      | otherwise -> failAt position ("unsupported attribute " <> attributeName)

-- | Decision (BS, 2026-08-21, per @docs/design-guidance.md@'s
-- extend-vs-new-abstraction rule and missing-framework-capability protocol):
-- see @docs/design-guidance.md@'s \"Follow-up decision — BS\" for the full
-- record, including the module-cycle restructuring this required.
--
-- Every framework identifier below is resolved through a quoted 'Name'
-- ('Impl.foo'), not 'mkName', so a splice site's own local bindings can
-- never shadow it: 'mkName' produces a dynamically bound, unqualified name
-- resolved against whatever is in scope where @[harch| ... |]@ appears, so
-- a component with a parameter named @value@, @name@, or @method@ would
-- otherwise silently rebind the framework's own constructor at every splice
-- site that parameter is visible — usually an inscrutable type error, and in
-- the worst case (a module defining its own non-escaping @text :: Text ->
-- Html@) an escaping bypass with no diagnostic. A component's own name
-- ('lowerComponentNode', 'reifyComponentProps') is deliberately NOT resolved
-- this way: it is the intended open extension point, meant to resolve
-- against whatever the splice site itself has in scope.
textAttributeConstructors :: [(String, Name)]
textAttributeConstructors =
  [ ("action", 'Impl.formAction),
    ("aria-label", 'Impl.ariaLabel),
    ("aria-live", 'Impl.ariaLive),
    ("autocomplete", 'Impl.autocomplete),
    ("href", 'Impl.href),
    ("inputmode", 'Impl.inputMode),
    ("maxlength", 'Impl.maxLength),
    ("method", 'Impl.method),
    ("minlength", 'Impl.minLength),
    ("name", 'Impl.name),
    ("role", 'Impl.role),
    ("type", 'Impl.inputType),
    ("value", 'Impl.value)
  ]

-- | Mirrors 'HarchWeb.Markup.Quasi.Parser.nativeTagConstructors'\' set of
-- constructor-name strings; the parser only ever produces a
-- 'HarchWeb.Markup.Quasi.Parser.MarkupNode' native-tag constructor string
-- that is a key here; 'resolveNativeTag'\'s 'Nothing' case exists as a
-- defensive fallback if the two tables were ever to drift, not because a
-- real parse can reach it today.
nativeTagNames :: [(String, Name)]
nativeTagNames =
  [ ("anchorTag", 'Impl.anchorTag),
    ("breakTag", 'Impl.breakTag),
    ("buttonTag", 'Impl.buttonTag),
    ("codeTag", 'Impl.codeTag),
    ("divTag", 'Impl.divTag),
    ("formTag", 'Impl.formTag),
    ("headingOneTag", 'Impl.headingOneTag),
    ("headingTwoTag", 'Impl.headingTwoTag),
    ("horizontalRuleTag", 'Impl.horizontalRuleTag),
    ("imageTag", 'Impl.imageTag),
    ("inputTag", 'Impl.inputTag),
    ("labelTag", 'Impl.labelTag),
    ("listItemTag", 'Impl.listItemTag),
    ("listTag", 'Impl.listTag),
    ("metaTag", 'Impl.metaTag),
    ("optionTag", 'Impl.optionTag),
    ("paragraphTag", 'Impl.paragraphTag),
    ("sectionTag", 'Impl.sectionTag),
    ("selectTag", 'Impl.selectTag)
  ]

resolveNativeTag :: Position -> String -> Q Exp
resolveNativeTag position tagConstructor =
  case lookup tagConstructor nativeTagNames of
    Just name -> pure (VarE name)
    Nothing -> failAt position ("unsupported native element " <> tagConstructor)

dataAttributeSuffix :: String -> Maybe String
dataAttributeSuffix attributeName =
  case splitAt 5 attributeName of
    ("data-", suffix)
      | not (null suffix) -> Just suffix
    _ -> Nothing

applyNamed :: Name -> [Exp] -> Q Exp
applyNamed name arguments = pure (applyNamedPure name arguments)

applyNamedPure :: Name -> [Exp] -> Exp
applyNamedPure name = foldl AppE (VarE name)

parseExpression :: Position -> String -> Q Exp
parseExpression position expressionSource =
  case Meta.parseExp expressionSource of
    Left message -> failAt position ("invalid Haskell expression: " <> message)
    Right expression -> pure expression

textLiteral :: String -> Exp
textLiteral literal = AppE (VarE 'Text.pack) (LitE (StringL literal))

-- | Like 'textLiteral', but for a validated newtype ('DataAttributeSuffix',
-- 'SafeUrl') reached through its 'IsString' instance instead of building a
-- plain 'Text' value: the generated code reads the same as a template
-- author writing the string literal directly with 'OverloadedStrings'.
fromStringLiteral :: String -> Exp
fromStringLiteral literal = AppE (VarE 'fromString) (LitE (StringL literal))

failAt :: Position -> String -> Q a
failAt (Position line column) message =
  fail ("harch:" <> show line <> ":" <> show column <> ": " <> message)
