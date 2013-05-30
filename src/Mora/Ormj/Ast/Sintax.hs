module Mora.Ormj.Ast.Sintax where

-- 1
data Ormj = Ormj  PackageDeclaration   ImportDeclarations  TypeDeclarations
                  deriving Show

-- 2-  definicion completa
data PackageDeclaration = PackageDeclaration Annotations PackageName
                         | NilPackageDeclaration
                         deriving Show
                                                     
type Annotations = [Annotation]
sem_Annotations_Cons = (:)
sem_Annotations_Nil = []
                             
data Annotation = Annotation TypeName TypeAnnotation
                                deriving Show
                                
data TypeAnnotation = MarkerAnnotation
                                        | NormalAnnotation  ElementValuePairs
                                        | SingleElementAnnotation  ElementValue
                                        deriving Show
                                                                                                         
data TypeName = TypeName Identifier TypeName
                          | NilTypeName
                           deriving Show
                           
type ElementValuePairs = [ElementValuePair]
sem_ElementValuePairs_Cons = (:)
sem_ElementValuePairs_Nil = []
                                                
data ElementValuePair = ElementValuePair Identifier ElementValue
                                                deriving Show                                   

data ElementValue = ElementValueConditional ConditionalExpression
                                  | ElementValueAnnotation Annotation
                                  | ElementValueEVArrayInitializer ElementValueArrayInitializer
                                  deriving Show

-- *****************************************
-- Los siguientes dataTypes representan la estructutra de Conditional expresion 
-- *****************************************
data ConditionalExpression = ConditionalExpr ConditionalOrExpression
                                                   | ConditionalExprComb ConditionalOrExpression Expression ConditionalExpression
                                                   deriving Show
                                                        
data ConditionalOrExpression = ConditionalOrExpression :||: ConditionalOrExpression
                                                         | ConditionalOrExpression :&&: ConditionalOrExpression
                                                         | ConditionalOrExpression :|:  ConditionalOrExpression
                                                         | ConditionalOrExpression :^:  ConditionalOrExpression
                                                         | ConditionalOrExpression :&:  ConditionalOrExpression
                                                         | ConditionalOrExpression :==: ConditionalOrExpression
                                                         | ConditionalOrExpression :!=: ConditionalOrExpression
                                                         | ConditionalOrExpression :<: ConditionalOrExpression
                                                     | ConditionalOrExpression :>: ConditionalOrExpression
                                                     | ConditionalOrExpression :<=: ConditionalOrExpression
                                                     | ConditionalOrExpression :>=: ConditionalOrExpression
                                                     | ConditionalOrExpression :<<: ConditionalOrExpression
                                                     | ConditionalOrExpression :>>: ConditionalOrExpression
                                                     | ConditionalOrExpression :>>>: ConditionalOrExpression
                                                     | ConditionalOrExpression :+: ConditionalOrExpression
                                                     | ConditionalOrExpression :-: ConditionalOrExpression
                                                     | ConditionalOrExpression :*: ConditionalOrExpression
                                                     | ConditionalOrExpression :/: ConditionalOrExpression
                                                     | ConditionalOrExpression :%: ConditionalOrExpression
                                                     | ConditionalOrExpressionUnaryExpression UnaryExpression
                                                     | ConditionalOrExpressionIntanceOf UnaryExpression Type
                                                         deriving Show
                                                         
--                                           | ConditionalOrExpressionIntanceOf UnaryExpression ZReferenceType

-- type ZReferenceType = [ReferenceTypeOrArrayType]
-- sem_ZReferenceType_Cons = (:)
-- sem_ZReferenceType_Nil = []

-- data ReferenceTypeOrArrayType = ZReferenceType ReferenceType
--                                                        | InstanceOfZArrayType Type   
--                                                        deriving Show
-- sem_ReferenceTypeOrArrayType_ZReferenceType = ZReferenceType
-- sem_ReferenceTypeOrArrayType_InstanceOfZArrayType = InstanceOfZArrayType

data UnaryExpression = UnaryExpressionPreIncrementExpression UnaryExpression -- ++ UnaryExpression
                                         | UnaryExpressionPreDecrementExpression UnaryExpression -- -- UnaryExpression
                                         | UnExpMas UnaryExpression -- :+:
                                         | UnExpMenos UnaryExpression -- :-:
                                         | PostExpPrimaryPostfixZ Primary ZPostfixExpression                                                                                                                                                                                                                                                                              
                                         | PostfixExpressionPrimary Primary
                                         | Pestan UnaryExpression -- :~:
                                         | Admiracion  UnaryExpression -- :!:
                                         deriving Show
                                         
--                                       | UnNotPlusCastExpression Type UnaryExpression
                                                                                                 
{- data UnaryExpressionNotPlusMinus = UnNotPlus PostfixExpression
                                                                 | Pestan UnaryExpression -- :~:
                                                                 | Admiracion  UnaryExpression -- :!:
                                                                 | UnNotPlusCastExpression Type UnaryExpression
                                                                 deriving Show 
                                                                                                                                 
data PostfixExpression = PostExpPrimaryPostfixZ Primary ZPostfixExpression                                                                                                                                                                                                                                                                                
                                           | PostfixExpressionPrimary Primary
                                           deriving Show
-}      
{-                                 
data PostfixExpression = PostExpPrimaryPostfixZ Primary ZPostfixExpression                                                                                                                                                                                                                                                                                
                                           | PostfixExpressionPrimary Primary
                                           deriving Show                                           -}
                                                                                   
type ZPostfixExpression = [ZPostfixExp]
sem_ZPostfixExpression_Cons = (:)
sem_ZPostfixExpression_Nil = []

data ZPostfixExp = PostIncrement
                                 | PostDecrement
                                 deriving Show
sem_ZPostfixExp_PostIncrement = PostIncrement            
sem_ZPostfixExp_PostDecrement = PostDecrement
                                           
{- data Primary = PrimNoNewArray PrimaryNoNewArray
                         | PrimArrayCreationExpression ArrayCreationExpression
                         | PrimNoNewArrayZ PrimaryNoNewArray ZPrimary
                         | PrimArrayCreationExpressionZ ArrayCreationExpression ZPrimary 
                         deriving Show -}
                         
-- Para Modificacion de  ArrayCreationExpression
data Primary = PrimNoNewArray PrimaryNoNewArray  -- OR ArrayCreationExpression
                         | PrimNoNewArrayZ PrimaryNoNewArray ZPrimary -- OR ArrayCreationExpression
                         deriving Show                   
                         
{- data ZPrimary = PrimaryNoNewArrayFieldAccess      Identifier
                           | PrimarynoNewArrayMethodInvocation NonWildTypeArguments Identifier ArgumentList
                           | PrimaryNoNewArrayFieldAccessZ      Identifier           ZPrimary
                           | PrimarynoNewArrayMethodInvocationZ NonWildTypeArguments Identifier ArgumentList ZPrimary
                           deriving Show  -}

type ZPrimary = [PrimaryNNAArrayFieldAccessOrMethodInvocation] 
sem_ZPrimary_Cons = (:)
sem_ZPrimary_Nil  = []

data PrimaryNNAArrayFieldAccessOrMethodInvocation =  PrimaryNoNewArrayFieldAccess      Identifier
                                                                                              | PrimarynoNewArrayMethodInvocation  NonWildTypeArguments Identifier ArgumentList
                                                                                              | PrimaryConstructor  NonWildTypeArguments 
                                                                                                  deriving Show

sem_PrimaryNNAArrayFieldAccessOrMethodInvocation_PrimaryNoNewArrayFieldAccess = PrimaryNoNewArrayFieldAccess
sem_PrimaryNNAArrayFieldAccessOrMethodInvocation_PrimarynoNewArrayMethodInvocation = PrimarynoNewArrayMethodInvocation
sem_PrimaryNNAArrayFieldAccessOrMethodInvocation_PrimaryConstructor = PrimaryConstructor                                                                                                  
-- 27
data PrimaryNoNewArray = PrimaryNoNewArray PrimaryNNA ZPrimaryNoNewArray
                                           deriving Show
                                           
-- sem_PrimaryNoNewArray_PrimaryNoNewArray = PrimaryNoNewArray -- PrimaryNNA ZPrimaryNoNewArray
data PrimaryNNA = PrimNNALiteral_IntegerLiteral_DecimalIntegerLiteral String
                            | PrimNNALiteral_IntegerLiteral_HexIntegerLiteral     String
                            | PrimNNALiteral_IntegerLiteral_OctalIntegerLiteral   String
                            | PrimNNALiteral_FloatingPointLiteral_DecimalFloatingPointLiteral String
                            | PrimNNALiteral_FloatingPointLiteral_HexadecimalFloatingPointLiteral String
                            | PrimNNALiteral_BooleanLiteral Bool
                            | PrimNNALiteral_CharacterLiteral String
                            | PrimNNALiteral_StringLiteral String
                                | PrimNNALiteral_NullLiteral
                                | PrimNNATypeClassPrimitiveType PrimitiveType TypeZ  ---                                | PrimNNATypeClass Type
                                | PrimNNATypeClassReferenceTypeClassIOT Identifier TypeArguments PrimNNAClassOrInterfaceType --TypeZ 
                                | PrimNNATypeClassReferenceTypeTypeVariable Identifier TypeZ
                                | PrimNNAVoid
                                | PrimNNAThis
                                | PrimNNAClassName TypeName -- Antes Identifier
                                | PrimNNAParExp           Expression
                                | UnNotPlusCastExpression Expression UnaryExpression -- ANTES Type        UnaryExpression  -- CONTROLAR COND CONTEXTO
                                | PrimNNAClassInstanceCreationExpression TypeArguments ClassOrInterfaceType ArgumentList  -- Antes ->                           | PrimNNAClassIns ClassInstanceCreationExpression                               
                                | ArrayCreationExpressionPrimitiveType PrimitiveType DimExprs                      -- Dims -- ARRAYCreationExpression Aplicar Cond Contexto
                                | ArrayCreationExpressionClassOrInterf ClassOrInterfaceType DimExprs               -- Dims -- ARRAYCreationExpression Aplicar Cond Contexto
                                | ArrayCreationExpressionArrInitialPrim PrimitiveType Dims ArrayInitializer          -- ARRAYCreationExpression Aplicar Cond Contexto
                                | ArrayCreationExpressionArrInitialClass ClassOrInterfaceType Dims ArrayInitializer  -- ARRAYCreationExpression Aplicar Cond Contexto
                                | PrimNNAFieldAccessPrim   Primary Identifier
                                | PrimNNAFieldAccessSuper   Identifier
                                | PrimNNAFieldAccessClassName   TypeName  Identifier  --- Antes Identifier
                                | PrimNNAMethodInvocationMN     TypeName ArgumentList  -- Antes MethodName
                            | PrimNNAMethodInvocationSuper  NonWildTypeArguments Identifier ArgumentList
                                | PrimNNAMethodInvocationClassN TypeName NonWildTypeArguments Identifier ArgumentList  -- TypeNAme ANtes Identifier CONTROLAR EN COND CONTEXTO
                                | PrimNNAMethodInvocationTypeN  TypeName NonWildTypeArguments Identifier ArgumentList
                                | PrimNNAArrayAccessExprName    TypeName Expression -- Antes ExpressionName
                                | PostfixExpressionExpressionName TypeName -- AQUI IMPLEMENTAR COND CONTEXTO
                                deriving Show
                                
--                              | PostExpNamePostfixZ Identifiers -- ZPostfixExpression -- AQUI IMPLEMENTAR COND CONTEXTO                               
                                
--                              | PrimNNATypeClassReferenceTypeClassIOT Identifier TypeArguments PrimNNAClassOrInterfaceType --TypeZ 
--                              | PrimNNATypeClassReferenceTypeTypeVariable TypeVariable TypeZ
                                
data PrimNNAClassOrInterfaceType = PrimNNAClassOrInterfaceType  Identifier TypeArguments PrimNNAClassOrInterfaceType
                                                                 | NilPrimNNAClassOrInterfaceType
                                                                 | TypeZPrimNNAClassOrInterfaceType TypeZ
                                                                 deriving Show
                                                                 
sem_PrimNNAClassOrInterfaceType_PrimNNAClassOrInterfaceType = PrimNNAClassOrInterfaceType
sem_PrimNNAClassOrInterfaceType_NilPrimNNAClassOrInterfaceType = NilPrimNNAClassOrInterfaceType
sem_PrimNNAClassOrInterfaceType_TypeZPrimNNAClassOrInterfaceType = TypeZPrimNNAClassOrInterfaceType
-- sem_PrimaryNNA_PrimNNALiteral    = PrimNNALiteral -- Literal
sem_PrimaryNNA_PrimNNALiteral_IntegerLiteral_DecimalIntegerLiteral = PrimNNALiteral_IntegerLiteral_DecimalIntegerLiteral
sem_PrimaryNNA_PrimNNALiteral_IntegerLiteral_HexIntegerLiteral     = PrimNNALiteral_IntegerLiteral_HexIntegerLiteral
sem_PrimaryNNA_PrimNNALiteral_IntegerLiteral_OctalIntegerLiteral   = PrimNNALiteral_IntegerLiteral_OctalIntegerLiteral
sem_PrimaryNNA_PrimNNALiteral_FloatingPointLiteral_DecimalFloatingPointLiteral = PrimNNALiteral_FloatingPointLiteral_DecimalFloatingPointLiteral
sem_PrimaryNNA_PrimNNALiteral_FloatingPointLiteral_HexadecimalFloatingPointLiteral = PrimNNALiteral_FloatingPointLiteral_HexadecimalFloatingPointLiteral
sem_PrimaryNNA_PrimNNALiteral_BooleanLiteral = PrimNNALiteral_BooleanLiteral
sem_PrimaryNNA_PrimNNALiteral_CharacterLiteral = PrimNNALiteral_CharacterLiteral
sem_PrimaryNNA_PrimNNALiteral_StringLiteral    = PrimNNALiteral_StringLiteral
sem_PrimaryNNA_PrimNNALiteral_NullLiteral      = PrimNNALiteral_NullLiteral
sem_PrimaryNNA_PrimNNATypeClassPrimitiveType =  PrimNNATypeClassPrimitiveType
sem_PrimaryNNA_PrimNNATypeClassReferenceTypeClassIOT =  PrimNNATypeClassReferenceTypeClassIOT
sem_PrimaryNNA_PrimNNATypeClassReferenceTypeTypeVariable = PrimNNATypeClassReferenceTypeTypeVariable

-- AQUI APLICAR CONDICIONES DE CONTEXTO para ArrayCreationExpression
sem_PrimaryNNA_ArrayCreationExpressionPrimitiveType =  ArrayCreationExpressionPrimitiveType
sem_PrimaryNNA_ArrayCreationExpressionClassOrInterf = ArrayCreationExpressionClassOrInterf
sem_PrimaryNNA_ArrayCreationExpressionArrInitialPrim = ArrayCreationExpressionArrInitialPrim 
sem_PrimaryNNA_ArrayCreationExpressionArrInitialClass = ArrayCreationExpressionArrInitialClass
------------------------------------------------------------------------------------------------

sem_PrimaryNNA_PrimNNAVoid               = PrimNNAVoid
sem_PrimaryNNA_PrimNNAThis               = PrimNNAThis
sem_PrimaryNNA_PrimNNAClassName  = PrimNNAClassName -- ClassName
sem_PrimaryNNA_PrimNNAParExp     = PrimNNAParExp -- Expression

sem_PrimaryNNA_UnNotPlusCastExpression = UnNotPlusCastExpression

sem_PrimaryNNA_PrimNNAClassInstanceCreationExpression    = PrimNNAClassInstanceCreationExpression -- ClassInstanceCreationExpression
-- sem_PrimaryNNA_PrimNNAFieldAcc        = PrimNNAFieldAcc -- FieldAccess

sem_PrimaryNNA_PrimNNAFieldAccessSuper  = PrimNNAFieldAccessSuper
sem_PrimaryNNA_PrimNNAFieldAccessClassName = PrimNNAFieldAccessClassName

-- sem_PrimaryNNA_PrimNNAMethIn  = PrimNNAMethIn -- MethodInvocation
sem_PrimaryNNA_PrimNNAMethodInvocationMN = PrimNNAMethodInvocationMN
sem_PrimaryNNA_PrimNNAMethodInvocationSuper= PrimNNAMethodInvocationSuper
sem_PrimaryNNA_PrimNNAMethodInvocationClassN = PrimNNAMethodInvocationClassN 
sem_PrimaryNNA_PrimNNAMethodInvocationTypeN = PrimNNAMethodInvocationTypeN
-- sem_PrimaryNNA_PrimNNAArrayAccessExprName = PrimNNAArrayAccessExprName 

sem_PrimaryNNA_PrimNNAArrayAccessExprName = PrimNNAArrayAccessExprName -- ExpressionName Expression
-- sem_PrimaryNNA_PostExpNamePostfixZ = PostExpNamePostfixZ -- AQUI IMPLEMENTAR COND CONTEXTO
sem_PrimaryNNA_PostfixExpressionExpressionName = PostfixExpressionExpressionName -- AQUI IMPLEMENTAR COND CONTEXTO

                                           
-- *
type ZPrimaryNoNewArray = [ZPrimaryOrExpression]
sem_ZPrimaryNoNewArray_Cons = (:)
sem_ZPrimaryNoNewArray_Nil  = []

data ZPrimaryOrExpression = ZPOEExpressionDeArrayAccess Expression
                                                  | ZPOEZPrimary ZPrimary
                                                  deriving Show

sem_ZPrimaryOrExpression_ZPOEExpressionDeArrayAccess = ZPOEExpressionDeArrayAccess
sem_ZPrimaryOrExpression_ZPOEZPrimary   = ZPOEZPrimary
                                                                        


{- data Type = TypePrimitiveType PrimitiveType
                  | TypeReferenceType ReferenceType
                  | TypePrimitiveTypeArrayType PrimitiveType TypeZ
                  | TypeReferenceTypeArrayType ReferenceType TypeZ
                  deriving Show
                  -}
-- 29             
data Type = TypePrimitiveType PrimitiveOrRefereceType TypeZ
                  deriving Show
                  
sem_Type_TypePrimitiveType = TypePrimitiveType 
data PrimitiveOrRefereceType = TypePrimitiveNumericType_TypeIntegral_Byte
                                            | TypePrimitiveNumericType_TypeIntegral_Short
                                            | TypePrimitiveNumericType_TypeIntegral_Int
                                            | TypePrimitiveNumericType_TypeIntegral_Long
                                            | TypePrimitiveNumericType_TypeIntegral_Char
                                            | TypePrimitiveNumericType_TypeFloating_Float
                                            | TypePrimitiveNumericType_TypeFloating_Double
                                                        | TypePrimitivePrimitivetypeBoolean
                                                        | TypeReferece   ReferenceType 
                                                        deriving Show
                                                        
sem_PrimitiveOrRefereceType_TypePrimitivePrimitivetypeBoolean = TypePrimitivePrimitivetypeBoolean
sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Byte = TypePrimitiveNumericType_TypeIntegral_Byte
sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Short = TypePrimitiveNumericType_TypeIntegral_Short
sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Int = TypePrimitiveNumericType_TypeIntegral_Int
sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Long = TypePrimitiveNumericType_TypeIntegral_Long
sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Char = TypePrimitiveNumericType_TypeIntegral_Char
sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeFloating_Float = TypePrimitiveNumericType_TypeFloating_Float
sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeFloating_Double = TypePrimitiveNumericType_TypeFloating_Double
                                                        
                                                        
-- sem_PrimitiveOrRefereceType_TypePrimitive = TypePrimitive -- PrimitiveType
sem_PrimitiveOrRefereceType_TypeReferece  = TypeReferece   -- ReferenceType 
                                                                
{- data TypeZ = ArrayTypeZ TypeZ
           | ArrayType
           deriving Show -}
-- *
type TypeZ = [ArrayType]  -- en referenceType, el ArrayType que era recursivo
data ArrayType = ArrayType
                           deriving Show
sem_TypeZ_Cons =(:)
sem_TypeZ_Nil = []
sem_ArrayType_ArrayType = ArrayType

data PrimitiveType =  NumericType_TypeIntegral_Byte
                                     | NumericType_TypeIntegral_Short
                                     | NumericType_TypeIntegral_Int
                                     | NumericType_TypeIntegral_Long
                                     | NumericType_TypeIntegral_Char
                                     | NumericType_TypeFloating_Float
                                     | NumericType_TypeFloating_Double
                                         | PrimitivetypeBoolean
                                        deriving Show             

sem_PrimitiveType_PrimitivetypeBoolean = PrimitivetypeBoolean
sem_PrimitiveType_NumericType_TypeIntegral_Byte = NumericType_TypeIntegral_Byte
sem_PrimitiveType_NumericType_TypeIntegral_Short = NumericType_TypeIntegral_Short
sem_PrimitiveType_NumericType_TypeIntegral_Int = NumericType_TypeIntegral_Int
sem_PrimitiveType_NumericType_TypeIntegral_Long = NumericType_TypeIntegral_Long
sem_PrimitiveType_NumericType_TypeIntegral_Char = NumericType_TypeIntegral_Char
sem_PrimitiveType_NumericType_TypeFloating_Float = NumericType_TypeFloating_Float
sem_PrimitiveType_NumericType_TypeFloating_Double = NumericType_TypeFloating_Double
                                 
-- 34 -                                         
data ReferenceType = ReferenceTypeClassOrInterfaceType  Identifier TypeArguments ZClassOrInterfaceType
                               deriving Show 
--                                 | ReferenceTypeT Identifier
        
        
-- 35 -
-- data ClassOrInterfaceType = ClassOrInterfaceType  TypeName TypeArguments ZClassOrInterfaceType
data ClassOrInterfaceType = ClassOrInterfaceType  Identifier TypeArguments ZClassOrInterfaceType
                                                  deriving Show
                                                  
sem_ClassOrInterfaceType_ClassOrInterfaceType = ClassOrInterfaceType

type ZClassOrInterfaceType = [ZCOITTypeDeclSpecifier]                                           
sem_ZClassOrInterfaceType_Cons = (:)
sem_ZClassOrInterfaceType_Nil = []

data ZCOITTypeDeclSpecifier = ZCOITTypeDeclSpecifier Identifier TypeArguments
                                                        deriving Show
sem_ZCOITTypeDeclSpecifier_ZCOITTypeDeclSpecifier = ZCOITTypeDeclSpecifier

-- data ClassType = ClassType TypeDeclSpecifier TypeArguments
--                         deriving Show
                           
-- data TypeDeclSpecifier = TypeDeclSpecifierTN TypeName
  --                                       | TypeDeclSpecifierCI ClassOrInterfaceType Identifier
--                                         deriving Show

-- data TypeDeclSpecifier = TypeDeclSpecifierTN TypeName
--                                         deriving Show
                                           
data TypeArguments = TypeArgumentsC1 ActualTypeArgumentList
                                   | TypeArgumentsC2 ActualTypeArgumentList
                                   | TypeArgumentsC3 ActualTypeArgumentList
                                   | TypeArgumentsC0 ActualTypeArgumentList
                                   | NilTypeArguments
                                        deriving Show
                                        
{- data ActualTypeArgumentList = ActualTypeArgumentList ActualTypeArgument ActualTypeArgumentList
                                                        | NilActualTypeArgumentList
                                                        deriving Show
                                                        -}
type ActualTypeArgumentList = [ActualTypeArgument]
sem_ActualTypeArgumentList_Cons = (:)
sem_ActualTypeArgumentList_Nil  = []
                                                        
data ActualTypeArgument = ActualTypeArgumentWildCard WildcardBounds
                                                | ActualTypeReferenceType Type
                                                deriving Show
                                                
-- data WildCard = WildCard WildcardBounds
--                         deriving Show

--  Aqui le precede las palabras reservadas extends y super
{- data WildcardBounds = WilcardBoundsExtends ReferenceType
                    | WilcardBoundsSuper ReferenceType
                    | WilcardBoundsExtendsArrayType Type
                    | WilcardBoundsSuperArrayType Type
                                        | NilwildcardBounds 
                                        deriving Show -}
                                        
data WildcardBounds =  WilcardBoundsExtendsReferenceType Type
                    | WilcardBoundsSuperReferenceType Type
                                        | NilwildcardBounds 
                                        deriving Show                                   
                                        
                                        
-- data InterfaceType = InterfaceType TypeDeclSpecifier TypeArguments
--                                 deriving Show
-- type TypeVariable = Identifier                                                                               
-- data ArrayType = ArrayType Type
--                         deriving Show                           
-- type ClassName = Identifier

-- 47
data Expression = ExpressionConditionalExpr     ConditionalOrExpression
                                | ExpressionConditionalExprComb ConditionalOrExpression Expression ConditionalExpression
                                | ExpressionAssignment          ConditionalOrExpression AssignmentOperator Expression
                                deriving Show
                                
-- data Expression = Expression AssignmentExpression
--                              deriving Show
                                
-- data AssignmentExpression = AssignmentExpressionCE ConditionalExpression
--                                                | AssignmentExpressionAss Assignment
--                                                deriving Show
                                                  
-- data Assignment =  Assignment LeftHandSide AssignmentOperator Expression -- AssignmentExpression
--                              deriving Show
                                
data AssignmentOperator = AssignmentOp -- =
                                                | AssignmentPlus -- *=
                                                | AssignmentDiv -- /=
                                                | AssignmentMod -- %=
                                                | AssignmentAdd -- +=
                                                | AssignmentMin -- -=
                                                | AssignmentMinShifShift  -- <<=
                                                | AssignmentMayShitfShift -- >>=
                                                | AssignmentMayShiftShiftShift -- >>>=
                                                | AssignmentAndSingle -- &=
                                                | AssignmentCincun -- ^=
                                                | AssignmentOrSingle -- |=  
                                                deriving Show
                                
-- data LeftHandSide = LeftHandSideExpName Identifiers -- Antes ExpressionName
--                                | LeftHandSidePrimary Primary -- Identifier
--                            deriving Show
                                        
{- data ExpressionName = ExpressionName Identifier
                                        | ExpressionNameAmb AmbiguousName Identifier
                                        deriving Show
-}

type Identifiers = [Identifier]
sem_Identifiers_Cons = (:)
sem_Identifiers_Nil  = []

{- data AmbiguousName = AmbiguousName Identifier AmbiguousName
                                   | NilAmbiguousName
                                        deriving Show
        
type AmbiguousName = [Identifier]

sem_AmbiguousName_Cons = (:)
sem_AmbiguousName_Nil = []
-}
-- type AmbiguousTye = [ Identifier ]
-- sem_Ambiguous_AmbiguousName = (:)
-- sem_Ambiguous_NilAmbiguousName = []

data FieldAccess = FieldAccessPrim Primary Identifier
                                 | FieldAccessSuper Identifier
                                 | FieldAccessClassName Identifier Identifier
                                 deriving Show
                                 
data ArrayAccess = ArrayAccessExpName  Identifiers Expression -- Antes ExpressionName
                                 | ArrayAccessPrimNNA PrimaryNoNewArray Expression
                                 deriving Show
                                 
-- data ClassInstanceCreationExpression = ClassInstanceCreationExpression TypeArguments ClassOrInterfaceType ArgumentList
--                                                                       deriving Show
                                                                         
--                                                                       | ClassInstanceCreationExpressionTA TypeArguments ClassOrInterfaceType 
--                                                                       | ClassInstanceCreationExpressionAL  ClassOrInterfaceType ArgumentList
--                                                                       | ClassInstanceCreationExpression  ClassOrInterfaceType

{- data ArgumentList = ArgumentList Expression ArgumentList
                                  | NilArgumentList
                                  deriving Show
-}
type ArgumentList = [Expression]
sem_ArgumentList_Cons = (:)
sem_ArgumentList_Nil = []
                                  
{- data MethodInvocation = MethodInvocationMN MethodName ArgumentList
                                          | MethodInvocationSuper NonWildTypeArguments Identifier ArgumentList
                                          | MethodInvocationClassN ClassName NonWildTypeArguments Identifier ArgumentList
                                          | MethodInvocationTypeN TypeName NonWildTypeArguments Identifier ArgumentList
                                          deriving Show
--                                        | MethodInvocationPrim Primary NonWildTypeArguments Identifier ArgumentList   
-}

{- data MethodName = MethodName Identifier 
                                | MethodNameAmbiguous AmbiguousName Identifier
                                deriving Show
                                -}
-- type MethodName = [Identifier]
-- sem_MethodName_Cons = (:)
-- sem_MethodName_Nil  = []
                                
data NonWildTypeArguments = NonWildTypeArgumentsC1 ReferenceTypeList
                                                  | NonWildTypeArgumentsC2 ReferenceTypeList
                                                  | NonWildTypeArgumentsC3 ReferenceTypeList
                                                  | NonWildTypeArgumentsC0 ReferenceTypeList
                                                  | NilNonWildTypeArguments
                                                  deriving Show
                                                  
{- data ReferenceTypeList = ReferenceTypeList ReferenceType ReferenceTypeList
                                           | ReferenceTypeListArrayType Type ReferenceTypeList
                                           |NilReferenceTypeList
                                           deriving Show
                                           -}
                                           
type ReferenceTypeList = [Type ]
sem_ReferenceTypeList_Cons = (:)
sem_ReferenceTypeList_Nil  = []
{- data ReferenceTypeOrType = ReferenceTypeList ReferenceType
                                                 | ReferenceTypeListArrayType   Type
                                                 deriving Show 
sem_ReferenceTypeOrType_ReferenceTypeList = ReferenceTypeList
sem_ReferenceTypeOrType_ReferenceTypeListArrayType = ReferenceTypeListArrayType -}
                                           
                                           
{- data ArrayCreationExpression = ArrayCreationExpressionPrimitiveType PrimitiveType DimExprs                      -- Dims
                                                         | ArrayCreationExpressionClassOrInterf ClassOrInterfaceType DimExprs               -- Dims
                                                         | ArrayCreationExpressionArrInitialPrim PrimitiveType Dims ArrayInitializer
                                                         | ArrayCreationExpressionArrInitialClass ClassOrInterfaceType Dims ArrayInitializer
                                                         deriving Show -}
                                                         
data DimExprs = DimExprs Expression DimExprs
                          | NilDimExprsDims Dims
                          | NilDimExprs
                          deriving Show
                          
sem_DimExprs_DimExprs        = DimExprs
sem_DimExprs_NilDimExprsDims = NilDimExprsDims
sem_DimExprs_NilDimExprs     = NilDimExprs

-- data DimExpr = DimExpr Expression
--                       deriving Show
                         
{- data Dims = Corchetes
                  | Dims ZDims
                  | NilDims
                  deriving Show
                  
data ZDims = ZCorchetes ZDims
                   | NilZDims
                   deriving Show
-}
{-vdata Dims = Dims Dims
                  | NilDims
                  deriving Show
                  
sem_Dims_Dims    = Dims -- Dims
sem_Dims_NilDims =  NilDims
-}
type Dims = [ () ]

sem_Dims_Dims      = (:)
sem_Dims_NilDims   =  []


data ArrayInitializer = ArrayInitializer VariableInitializers
                                          | NilArrayInitializers
                                          deriving Show
                                          
{- data VariableInitializers = VariableInitializers VariableInitializer VariableInitializers
                                                  | NilVariableInitializers
                                                  deriving Show
                                                  -}
type VariableInitializers = [VariableInitializer]                                                 
sem_VariableInitializers_Cons = (:)
sem_VariableInitializers_Nil = []
                                                  
data VariableInitializer = VariableInitializerExp Expression 
                                                 | VariableInitializerArr ArrayInitializer
                                                 deriving Show
                                                 
data ElementValueArrayInitializer = ElementValueArrayInitializer ElementValues
                                                                  | NilElementValueArrayinitializer
                                                                  deriving Show
                                                                  
type ElementValues = [ElementValue]
sem_ElementValues_Cons = (:)
sem_ElementValues_Nil = []
                                        
type PackageName = [Identifier]
sem_PackageName_Cons = (:)
sem_PackageName_Nil = []
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
type Identifier  = String 
-- *****************************************
-- FIN Los siguientes dataTypes representan la estructutra de Conditional expresion 
-- *****************************************

-- -------------------------------------------------------------------------
-- Definicion de ImportDeclarations
-- -------------------------------------------------------------------------
data ImportDeclarations = ImportDeclarations ImportDeclaration ImportDeclarations
            | NilImportDeclarations
            deriving Show
            
data ImportDeclaration = SingleTypeImportDeclaration TypeName
                                           | TypeImportOnDemandDeclaration PackageOrTypeName
                                           | SingleStaticImportDeclaration TypeName
                                           | StaticImportOnDemandDeclaration TypeName
                                           deriving Show

data PackageOrTypeName = NilPackageOrTypeName
                                           | PackageOrTypeName Identifier PackageOrTypeName
                                           deriving Show
                                           
-- -------------------------------------------------------------------------
-- FIN Definicion de ImportDeclarations
-- -------------------------------------------------------------------------
-- -------------------------------------------------------------------------
-- INICIO Definicion de TypeDeclarations
-- -------------------------------------------------------------------------
type TypeDeclarations = [TypeDeclaration]

sem_TypeDeclarations_Nil = []
sem_TypeDeclarations_Cons = (:)
                                                
data TypeDeclaration = TypeDeclarationClassDeclarationNormalCD    Modifiers Identifier TypeParameters Super Interfaces ClassBodyDeclarations
                        | TypeDeclarationClassDeclarationEnumD                Modifiers Identifier Interfaces EnumBody
                        | TypeDeclarationInterfaceDeclarationNormalInterfaceD Modifiers Identifier TypeParameters ExtendsInterfaces ListInterfaceMemberDeclaration
                        | TypeDeclarationInterfaceDeclarationAnnotationTypeD  Modifiers Identifier  ListAnnotationTypeElementDeclaration
                        | TypeDeclarationSemiColon
                        deriving Show
                        
type ListInterfaceMemberDeclaration = [InterfaceMemberDeclaration]
sem_ListInterfaceMemberDeclaration_Nil = []
sem_ListInterfaceMemberDeclaration_Cons = (:)

type ListAnnotationTypeElementDeclaration = [AnnotationTypeElementDeclaration]
sem_ListAnnotationTypeElementDeclaration_Nil = []
sem_ListAnnotationTypeElementDeclaration_Cons = (:)

sem_TypeDeclaration_TypeDeclarationClassDeclarationNormalCD             = TypeDeclarationClassDeclarationNormalCD 
sem_TypeDeclaration_TypeDeclarationClassDeclarationEnumD                = TypeDeclarationClassDeclarationEnumD
sem_TypeDeclaration_TypeDeclarationInterfaceDeclarationNormalInterfaceD = TypeDeclarationInterfaceDeclarationNormalInterfaceD
sem_TypeDeclaration_TypeDeclarationInterfaceDeclarationAnnotationTypeD  = TypeDeclarationInterfaceDeclarationAnnotationTypeD
sem_TypeDeclaration_TypeDeclarationSemiColon                            = TypeDeclarationSemiColon
                         
data Modifiers = Modifiers Modifier Modifiers
                           | NilModifiers 
                           deriving Show
                                        
data Modifier = ModifierAnnotation Annotation
                 | ModifierPublic
                 | ModifierProtected
                 | ModifierPrivate
                 | ModifierAbstract
                 | ModifiersStatic
                 | ModifierFinal
                 | ModifierStrictfp
                 | FieldModifierTransient
                         | FieldModifierVolatile
                         | MethodModifierSynchronized
                         | MethodModifierNative
                 deriving Show

data TypeParameters = TypeParametersC1 TypeParameterList
                                        | TypeParametersC2 TypeParameterList
                                        | TypeParametersC3 TypeParameterList
                                        | TypeParametersC0 TypeParameterList                                    
                                        | NilTypeParameters
                                        deriving Show
                                        
type TypeParameterList = [TypeParameter]
sem_TypeParameterList_Cons = (:) 
sem_TypeParameterList_Nil = []                     
                                           
data TypeParameter = TypeParameterBound  Identifier TypeBound
                                   | TypeParameter       Identifier
                                   deriving Show
                                   
data TypeBound = TypeBound ClassOrInterfaceType TypeBound
                           | NilAdditionalBoundList
                           deriving Show
                           
data Super = Super ClassOrInterfaceType
                   | NilSuper
                        deriving Show
                        
data Interfaces = Interfaces InterfaceTypeList
                                | NilInterfaces
                                deriving Show
                                
type InterfaceTypeList = [ClassOrInterfaceType] 
sem_InterfaceTypeList_Cons = (:)
sem_InterfaceTypeList_Nil = []
                                                                                                                   
type ClassBodyDeclarations = [ClassBodyDeclaration]
sem_ClassBodyDeclarations_Cons = (:)
sem_ClassBodyDeclarations_Nil =[]
                                                                                         
data ClassBodyDeclaration = ClassBodyDeclClassMemberDeclFieldDeclaration                     Modifiers Type  VariableDeclarators
                                                  | ClassBodyDeclClassMemberDeclMethodDeclaration                    Modifiers TypeParameters ResultType MethodDeclarator  Throws MethodBody
                                                  | ClassBodyDeclClassMemberDeclClassDeclarationNormalCD             Modifiers Identifier TypeParameters Super Interfaces ClassBodyDeclarations
                                                  | ClassBodyDeclClassMemberDeclClassDeclarationEnumD                Modifiers Identifier Interfaces EnumBody
                                                  | ClassBodyDeclClassMemberDeclInterfaceDeclarationNormalInterfaceD Modifiers Identifier TypeParameters ExtendsInterfaces ListInterfaceMemberDeclaration
                                                  | ClassBodyDeclClassMemberDeclInterfaceDeclarationAnnotationTypeD  Modifiers Identifier  ListAnnotationTypeElementDeclaration
                                                  | ClassBodyDeclClassMemberDeclSemiColon
                                                  | ClassBodyInstanceInitializer BlockStatements -- Block -- InstanceInitializer
                                                  | ClassBodyStaticInitializer BlockStatements -- StaticInitializer
                                                  | ClassBodyConstructorDeclaration Modifiers TypeParameters Identifier FormalParameterList Throws ConstructorBody
                                                  | ClassBodyConstructorDeclarationNoFormalParList Modifiers TypeParameters Identifier Throws ConstructorBody
                                                  deriving Show
        
sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclFieldDeclaration = ClassBodyDeclClassMemberDeclFieldDeclaration
sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclMethodDeclaration = ClassBodyDeclClassMemberDeclMethodDeclaration
sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclClassDeclarationNormalCD = ClassBodyDeclClassMemberDeclClassDeclarationNormalCD
sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclClassDeclarationEnumD = ClassBodyDeclClassMemberDeclClassDeclarationEnumD 
sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclInterfaceDeclarationNormalInterfaceD = ClassBodyDeclClassMemberDeclInterfaceDeclarationNormalInterfaceD
sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclInterfaceDeclarationAnnotationTypeD = ClassBodyDeclClassMemberDeclInterfaceDeclarationAnnotationTypeD
sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclSemiColon  = ClassBodyDeclClassMemberDeclSemiColon
sem_ClassBodyDeclaration_ClassBodyInstanceInitializer = ClassBodyInstanceInitializer  -- InstanceInitializer
sem_ClassBodyDeclaration_ClassBodyStaticInitializer = ClassBodyStaticInitializer  -- StaticInitializer
sem_ClassBodyDeclaration_ClassBodyConstructorDeclaration = ClassBodyConstructorDeclaration
sem_ClassBodyDeclaration_ClassBodyConstructorDeclarationNoFormalParList = ClassBodyConstructorDeclarationNoFormalParList

                                          
type VariableDeclarators =  [VariableDeclarator]
sem_VariableDeclarators_Cons = (:)
sem_VariableDeclarators_Nil  = []
                                             
data VariableDeclarator = VariableDeclaratorId VariableDeclaratorId
                                                | VariableDeclaratorIdAsig VariableDeclaratorId VariableInitializer
                                                deriving Show
                                                
data VariableDeclaratorId = VarDeclaratorId Identifier
                                                  | VarDeclaratorIdVDZ Identifier VariableDeclatatorIdZ
                                                  deriving Show                                           

data VariableDeclatatorIdZ = VarDeclaratorIdCorchete
                                                   | VarDeclaratorIdZ VariableDeclatatorIdZ
                                              deriving Show                                           
                                                                                                            
                                        
data ResultType = ResultTypeType Type
                                | ResultTypeVoid
                                deriving Show
                                
data MethodDeclarator = MethodDeclaratorFormalPL Identifier FormalParameterList
                                          | MethodDeclaratorSingle Identifier
                                          deriving Show
                                          
data FormalParameterList = FormalParameterListFormal VariableModifiers Type VariableDeclaratorId FormalParameterList
                                                 | FormalParameterListLast   VariableModifiers Type VariableDeclaratorId 
                                                 | FormalParameterListNil
                                                 deriving Show
                                                 
data VariableModifiers = VariableModifiers VariableModifier VariableModifiers
                                           | NilVariableModifiers
                                           deriving Show
                                           
data VariableModifier = VariableModifierFinal
                                          | VariableModifierAnnotation Annotation
                                          deriving Show
                                                  
data Throws = Throws ExceptionTypeList
                    | NilThrows
                        deriving Show
                        
type ExceptionTypeList = [ExceptionType]
sem_ExceptionTypeList_Cons = (:)
sem_ExceptionTypeList_Nil  = []
                                           
data ExceptionType = ExceptionTypeClassType ClassOrInterfaceType
                                   | ExceptionTypeTypeVariable Identifier
                                   deriving Show
                                   
data MethodBody = MethodBodyBlock BlockStatements
                            | MethodBodySemiColon 
                            deriving Show
                                           
type BlockStatements = [BlockStatement]
sem_BlockStatements_Cons = (:)
sem_BlockStatements_Nil = []
                                         
data BlockStatement = BlockStatementLocalVariableDeclarationStatement      Modifiers Type VariableDeclarators
                                        | BlockStatementClassDeclarationNormalClassDeclaration Modifiers Identifier TypeParameters Super Interfaces ClassBodyDeclarations
                                        | BlockStatementClassDeclarationEnumDeclaration        Modifiers Identifier Interfaces EnumBody
                                    | BlockStatementStatement Statement
                                    deriving Show
                                                                                          
data Statement = StatementLabeled Identifier Statement  -- LabeledStatement
                           | StatementIf Expression Statement -- IfThenStatement
                           | StatementIfElse Expression Statement Statement -- IfThenElseStatement
                           | StatementWhile Expression Statement   -- WhileStatement
                           | StatementFor ForStatement
                           | SWTSBlock BlockStatements
                           | SWTSEmptyStatement
                           | SWTSExpressionStatement Expression
                           | SWTSAssertStatementCond  Expression
                           | SWTSAssertStatementCondEx  Expression ConditionalExpression
                           | SWTSSwitchStatement Expression SwitchBlock
                           | SWTSDoStatement Statement Expression
                           | SWTSBreakStatement
                           | SWTSBreakStatementId Identifier
                           | SWTSNilContinueStatement
                           | SWTSContinueStatement Identifier
                           | SWTSReturnStatement Expression
                           | SWTSNilReturnStatement 
                           | SWTSynchronizedStatement Expression BlockStatements
                           | SWTTrhowStatement Expression
                           | SWTTryStatement BlockStatements Catches
                           | SWTTryStatementFinally  BlockStatements Catches BlockStatements -- el ultimo Block es FinallyBlock
                           deriving Show
                           
-- data StatementExpression = StatExpressionAssign Assignment
--                                               | StatExpressionConditionalExpression ConditionalExpression 
--                                               deriving Show                                           
                                                 
-- sem_StatementExpression_StatExpressionAssign= StatExpressionAssign -- Assignment
--sem_StatementExpression_StatExpressionConditionalExpression = StatExpressionConditionalExpression -- ConditionalExpression 
                                                 
data SwitchBlock = SwitchBlockAll SwitchBlockStatementGroups SwitchLabels
                                 | SwitchBlockLabels  SwitchLabels
                                 | SwitchBlockGroups SwitchBlockStatementGroups
                                 | NilSwitchBlock 
                                 deriving Show
                                                                                                
type SwitchBlockStatementGroups = [SwitchBlockStatementGroup]                                                           
sem_SwitchBlockStatementGroups_Cons = (:)
sem_SwitchBlockStatementGroups_Nil = []

data SwitchBlockStatementGroup = SwitchBlockStatementGroup SwitchLabels BlockStatements
                                                                deriving Show

type SwitchLabels = [SwitchLabel]
sem_SwitchLabels_Cons = (:)
sem_SwitchLabels_Nil  = []                                 
                                   
data SwitchLabel = SwitchLabelConstant Expression
                                 | SwitchLabelEnum         Identifier
                                 | SwitchLabelDefault
                                 deriving Show
                                                                                                                           
data ForInit = ForInitStaExp StatementExpressionList
                         | ForInitLocalVar VariableModifiers Type VariableDeclarators -- LocalVariableDeclaration
                         | NilForInit
                         deriving Show
                         
data StatementExpressionList = StatementExpressionList Expression StatementExpressionList
                                                         | NilStatementExpressionList
                                                         deriving Show
                                                         
data ForUpdate = ForUpdate StatementExpressionList
                           | NilForUpdate
                           deriving Show
                           
data ForStatement = ForStatementBasicForStatementAll           ForInit Expression ForUpdate Statement
                                  | ForStatementBasicForStatementNoExp         ForInit ForUpdate Statement
                                  | ForStatementEnhancedForStatement           VariableModifiers Type Identifier Expression Statement
                                  | ForStatementEnhancedForStatementNoVarModif Type Identifier Expression Statement
                                  deriving Show
                                  
data ExtendsInterfaces = ExtendsInterfaceType ClassOrInterfaceType ExtendsInterfaces
                      | NilExtendsInterfaces
                      deriving Show

data InterfaceMemberDeclaration = InterfaceMemberDeclarationConstant                                                                     Modifiers Type VariableDeclarators
                                | InterfaceMemberDeclarationAbstract                                                                     Modifiers TypeParameters ResultType MethodDeclarator Throws                                
                                                | InterfaceMemberDeclTypeDeclarationClassDeclarationNormalCD                     Modifiers Identifier TypeParameters Super Interfaces ClassBodyDeclarations
                                                                | InterfaceMemberDeclTypeDeclarationClassDeclarationEnumD                Modifiers Identifier Interfaces EnumBody
                                                                | InterfaceMemberDeclTypeDeclarationInterfaceDeclarationNormalInterfaceD Modifiers Identifier TypeParameters ExtendsInterfaces ListInterfaceMemberDeclaration
                                                                | InterfaceMemberDeclTypeDeclarationInterfaceDeclarationAnnotationTypeD  Modifiers Identifier  ListAnnotationTypeElementDeclaration
                                                                | InterfaceMemberDeclTypeDeclarationSemiColon
                                deriving Show                                

data AnnotationTypeElementDeclaration = AnnTypeElemDeclAbstract                                            Modifiers Type Identifier DefaultValue
                                      | AnnTypeElemDeclConstant                                            Modifiers Type VariableDeclarators
                                      | AnnTypeElemDeclTypeDeclarationClassDeclarationNormalCD             Modifiers Identifier TypeParameters Super Interfaces ClassBodyDeclarations
                                                                          | AnnTypeElemDeclTypeDeclarationClassDeclarationEnumD                Modifiers Identifier Interfaces EnumBody
                                                                          | AnnTypeElemDeclTypeDeclarationInterfaceDeclarationNormalInterfaceD Modifiers Identifier TypeParameters ExtendsInterfaces ListInterfaceMemberDeclaration
                                                                          | AnnTypeElemDeclTypeDeclarationInterfaceDeclarationAnnotationTypeD  Modifiers Identifier  ListAnnotationTypeElementDeclaration
                                                                          | AnnTypeElemDeclTypeDeclarationSemiColon
                                      deriving Show                                      
 
data DefaultValue = DefaultValue ElementValue
                                  | NilDefaultValue
                  deriving Show

data EnumBody = EnumBody EnumConstants EnumBodyDeclarations
              deriving Show

data EnumConstants = EnumConstants EnumConstant EnumConstants
                   | NilEnumConstants
                   deriving Show

data EnumConstant = EnumConstantAll Annotations Identifier ArgumentList ClassBodyDeclarations  -- Arguments por ArgumentList y ()
                                  | EnumConstantArgL Annotations Identifier ArgumentList 
                                  | EnumConstantClasB Annotations Identifier ClassBodyDeclarations
                                  | EnumConstantNothing Annotations Identifier
                   deriving Show

-- Pensar en eliminar o colocar un semicolon
data EnumBodyDeclarations = EnumBodyDeclarations ClassBodyDeclarations
                                                  | NilEnumBodyDeclarations
                          deriving Show

data StaticInitializer = StaticInitializer BlockStatements
                       deriving Show

data ConstructorBody = ConstructorBody ExplicitConstructorInvocation BlockStatements
                      deriving Show

data ExplicitConstructorInvocation = ExplConsInvThis NonWildTypeArguments ArgumentList
                                   | ExplConsInvSuper NonWildTypeArguments ArgumentList
                                   | ExplConsInvPrimary Primary ArgumentList -- NonWildTypeArguments ArgumentList
                                   | NilExplixitConsInv
                                   deriving Show

-- -------------------------------------------------------------------------
-- FIN Definicion de TypeDeclarations
-- -------------------------------------------------------------------------
type Catches = [CatchClause]

data CatchClause = CatchClause VariableModifiers Type VariableDeclaratorId BlockStatements
                  deriving Show