module Parser where

import UU.Parsing
import Sintax
import Scanner
import Semantic
import IntegrationScannerParser

import UU.Scanner.Position
-- 1
pOrmj =  sem_Ormj_Ormj <$> pPackageDeclaration <*> pImportDeclarations <*> pTypeDeclarations
-- -----------------------------------------------------------------------------------
-- Parser of PackageDeclaration - pAnnotations
-- ----------------------------------------------------------------------------------------
--  2 - OK
pPackageDeclaration = sem_PackageDeclaration_PackageDeclaration <$> pAnnotations <*  pKeyWord "package" <*> pPackageName <* pSpecialSimbol ";"
                   <|> pSucceed sem_PackageDeclaration_NilPackageDeclaration
-- 3                   
pAnnotations = pFoldr(sem_Annotations_Cons, sem_Annotations_Nil) pAnnotation

-- 4
pAnnotation = sem_Annotation_Annotation <$ pSpecialSimbol "@"<*> pTypeName <*> pTypeAnnotation

-- AQUI REVISAR SI SE OPTIMIZA OJO OJO OJO
pTypeAnnotation = pSucceed sem_TypeAnnotation_MarkerAnnotation
                           <|> r <**> ( (\ss evp -> stan evp )  <$$> evps <|> (\ss ev -> stas ev) <$$> ev  )

r = pSpecialSimbol "("
stan = sem_TypeAnnotation_NormalAnnotation
stas = sem_TypeAnnotation_SingleElementAnnotation
evps  = pElementValuePairs  <* pSpecialSimbol ")"
ev =  pElementValue <* pSpecialSimbol ")" 

-- 6 OK
pTypeName = sem_TypeName_TypeName <$> pIdentifier <*> pTypeName'
pTypeName' = pSucceed sem_TypeName_NilTypeName
                  <|> sem_TypeName_TypeName <$ pSpecialSimbol "." <*> pIdentifier <*> pTypeName'                  

-- 7
pElementValuePairs = pFoldrSep(sem_ElementValuePairs_Cons, sem_ElementValuePairs_Nil) (pSpecialSimbol ",") pElementValuePair

-- 8 - OK                                                          
pElementValuePair = sem_ElementValuePair_ElementValuePair <$> pIdentifier <* pSpecialSimbol "=" <*> pElementValue                                               -- 9 - OK

pElementValue  = sem_ElementValue_ElementValueConditional <$> pConditionalExpression
                          <|> sem_ElementValue_ElementValueAnnotation <$> pAnnotation  -- siempre empieza con @
                          <|> sem_ElementValue_ElementValueEVArrayInitializer <$> pElementValueArrayInitializer -- siempre empieza con llave
                          
-- 10
pConditionalExpression = (\z fz -> fz z) <$> pConditionalOrExpression <*> pZ
pZ  =   (\e ce z -> sem_ConditionalExpression_ConditionalExprComb z e ce) <$ pOperator "?" <*> pExpression <* pOperator ":" <*> pConditionalExpression 
   <|> pSucceed (\z -> sem_ConditionalExpression_ConditionalExpr z)

-- 11 AQUI REVISAR EL INSTANCEOF
pConditionalOrExpression =  foldr pGen pFactor [ orExp, andExp, orIncl, orExcl, andSingle, eqs,rels, shift, adss, muls ]

pGen ops p  =  pChainl (foldr1 (<|>) (map f ops)) p
   where f (s,c)     = const c  <$> pOperator s
                                    <|> const c  <$> pTokMayor s 
         
orExp  = [("||",sem_ConditionalOrExpression_Or)] 
andExp = [("&&",sem_ConditionalOrExpression_And)] 
orIncl = [("|",sem_ConditionalOrExpression_OrEx)]
andSingle = [("&",sem_ConditionalOrExpression_AndIc)]
orExcl = [("^",sem_ConditionalOrExpression_AndEx)]
eqs = [("==",sem_ConditionalOrExpression_Eq),("!=",sem_ConditionalOrExpression_Dist)]
rels = [("<",sem_ConditionalOrExpression_Men),("<=",sem_ConditionalOrExpression_MenQ),(">=", sem_ConditionalOrExpression_MayQ), (">",sem_ConditionalOrExpression_May)]
shift = [(">>>",sem_ConditionalOrExpression_DDer),(">>",sem_ConditionalOrExpression_Der),("<<",sem_ConditionalOrExpression_Izq)]
adss = [("+", sem_ConditionalOrExpression_Add),("-", sem_ConditionalOrExpression_Res)] 
muls = [("*", sem_ConditionalOrExpression_Mul),("/",sem_ConditionalOrExpression_Div),("%",sem_ConditionalOrExpression_Mod)]




{-
orExp  = [("||",sem_ConditionalOrExpression_Or)] 
andExp = [("&&",sem_ConditionalOrExpression_And)] 
orIncl = [("|",sem_ConditionalOrExpression_OrEx)]
andSingle = [("&",sem_ConditionalOrExpression_AndIc)]
orExcl = [("^",sem_ConditionalOrExpression_AndEx)]
eqs = [("==",sem_ConditionalOrExpression_Eq),("!=",sem_ConditionalOrExpression_Dist)]
rels = [("<",sem_ConditionalOrExpression_Men),("<=",sem_ConditionalOrExpression_MenQ),(">=", sem_ConditionalOrExpression_MayQ), (">",sem_ConditionalOrExpression_May)]
shift = [(">>>",sem_ConditionalOrExpression_DDer),(">>",sem_ConditionalOrExpression_Der),("<<",sem_ConditionalOrExpression_Izq)]
adss = [("+", sem_ConditionalOrExpression_Add),("-", sem_ConditionalOrExpression_Res)] 
muls = [("*", sem_ConditionalOrExpression_Mul),("/",sem_ConditionalOrExpression_Div),("%",sem_ConditionalOrExpression_Mod)]
-}

           
pFactor = (\pu f -> f pu) <$> pUnaryExpression <*> pFactor'

-- AQUI REALIZAR UN CONTROL DE CONTEXTO Y PRUEBAS
pFactor' = pSucceed (\u -> sem_ConditionalOrExpression_ConditionalOrExpressionUnaryExpression u)
           <|>  (\t u -> sem_ConditionalOrExpression_ConditionalOrExpressionIntanceOf u t) <$ pKeyWord "instanceof" <*>  pType
           
-- pZReferenceType = pKeyWord "instanceof" *> pFoldr1 (sem_ZReferenceType_Cons, sem_ZReferenceType_Nil) pReferenceTypeOrArrayType
-- pZReferenceType = pKeyWord "instanceof" *> pFoldr1 (sem_ZReferenceType_Cons, sem_ZReferenceType_Nil) pReferenceTypeOrArrayType
-- pReferenceTypeOrArrayType =  sem_ReferenceTypeOrArrayType_InstanceOfZArrayType <$> pType <* pSpecialSimbol "[" <*  pSpecialSimbol "]"
--                      <|> sem_ReferenceTypeOrArrayType_ZReferenceType          <$> pReferenceType

-- 21 - OK
pUnaryExpression = sem_UnaryExpression_UnaryExpressionPreIncrementExpression  <$ pOperator "++"  <*> pUnaryExpression   -- PreIncrement
                                <|> sem_UnaryExpression_UnaryExpressionPreDecrementExpression <$ pOperator "--"  <*> pUnaryExpression  -- PreDecrement
                                <|> sem_UnaryExpression_UnExpMas                              <$ pOperator "+" <*> pUnaryExpression
                                <|> sem_UnaryExpression_UnExpMenos                            <$ pOperator "-" <*> pUnaryExpression
                                <|> sem_UnaryExpression_Pestan                                <$ pOperator "~" <*> pUnaryExpression 
                                <|> sem_UnaryExpression_Admiracion                            <$ pOperator "!" <*> pUnaryExpression
                                <|> pPrimary         <**> ( (sem_PostfixExpression_PostExpPrimaryPostfixZ <$$> pZPostfixExpression) <|> (pSucceed sem_PostfixExpression_PostfixExpressionPrimary))  -- PostfixExpression
                                
-- 24 - OK
-- AQUI CONTROLAR CONDICION DE CONTEXTO PARA CAST
{- pUnaryExpressionNotPlusMinus = sem_UnaryExpressionNotPlusMinus_Pestan                     <$ pOperator "~" <*> pUnaryExpression 
                                                    <|> sem_UnaryExpressionNotPlusMinus_Admiracion <$ pOperator "!" <*> pUnaryExpression
                                                        <|> sem_UnaryExpressionNotPlusMinus_UnNotPlusCastExpression <$ pSpecialSimbol "(" <*> pType <* pSpecialSimbol ")" <*> pUnaryExpression
                                                    <|> sem_UnaryExpressionNotPlusMinus_UnNotPlus <$> pPostfixExpression
-}

-- 25 
-- pPostfixExpression = pPrimary         <**> ( (sem_PostfixExpression_PostExpPrimaryPostfixZ <$$> pZPostfixExpression) <|> (pSucceed sem_PostfixExpression_PostfixExpressionPrimary))
--                                <|> pIdentifiers <**> (( sem_PostfixExpression_PostExpNamePostfixZ    <$$> pZPostfixExpression) <|> (pSucceed sem_PostfixExpression_PostfixExpressionExpressionName))

-- Optimizado, aqui se refleja PostIncrementExpression y PostDecrementExpression
pZPostfixExpression     = pFoldr1 (sem_ZPostfixExpression_Cons,sem_ZPostfixExpression_Nil) pZPostfixExp
pZPostfixExp = sem_ZPostfixExp_PostIncrement <$ pOperator "++"
                        <|> sem_ZPostfixExp_PostDecrement <$ pOperator "--"
                                 
-- 26                   
pPrimary = pPrimaryNoNewArray <**> ( (pSucceed sem_Primary_PrimNoNewArray) <|> ( sem_Primary_PrimNoNewArrayZ <$$> pZPrimary))
--              <|> pArrayCreationExpression <**> ((pSucceed sem_Primary_PrimArrayCreationExpression) <|> (sem_Primary_PrimArrayCreationExpressionZ <$$> pZPrimary)) 

-- *****************************************************************************
-- ZPrimary Optimizado ---------------------------------------------------------
-- *****************************************************************************
pZPrimary 
  =  pFoldr1(sem_ZPrimary_Cons, sem_ZPrimary_Nil) pPrimaryNNAArrayFieldAccessOrMethodInvocation
 
pPrimaryNNAArrayFieldAccessOrMethodInvocation 
  = pSpecialSimbol "." *> pPrimaryNNAArrayFieldAccessOrMethodInvocation'
pPrimaryNNAArrayFieldAccessOrMethodInvocation' = (\nw fcora -> fcora nw) <$> pNonWildTypeArguments <*> pPrimaryConstructorOrArrayMethod
                                              <|> sem_PrimaryNNAArrayFieldAccessOrMethodInvocation_PrimaryNoNewArrayFieldAccess <$> pIdentifier
 
pPrimaryConstructorOrArrayMethod = (\ i al nw -> sem_PrimaryNNAArrayFieldAccessOrMethodInvocation_PrimarynoNewArrayMethodInvocation nw i al ) <$> pIdentifier <* pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")"
                                                            <|> pSucceed (\nw -> sem_PrimaryNNAArrayFieldAccessOrMethodInvocation_PrimaryConstructor nw) 
-- *****************************************************************************
-- FIN - ZPrimary Optimizado ---------------------------------------------------
-- *****************************************************************************
                           
-- 27 - Optimizado
pPrimaryNoNewArray = sem_PrimaryNoNewArray_PrimaryNoNewArray <$> pPrimaryNNA <*> pZPrimaryNoNewArray

pPrimaryNNA =  sem_PrimaryNNA_PrimNNALiteral_IntegerLiteral_DecimalIntegerLiteral                 <$> pDecimalIntegerLiteral
                   <|> sem_PrimaryNNA_PrimNNALiteral_IntegerLiteral_HexIntegerLiteral                     <$> pHexIntegerLiteral
                   <|> sem_PrimaryNNA_PrimNNALiteral_IntegerLiteral_OctalIntegerLiteral                   <$> pOctalIntegerLiteral
                   <|> sem_PrimaryNNA_PrimNNALiteral_FloatingPointLiteral_DecimalFloatingPointLiteral     <$> pDecimalFloatingPointLiteral
                   <|> sem_PrimaryNNA_PrimNNALiteral_FloatingPointLiteral_HexadecimalFloatingPointLiteral <$> pHexadecimalFloatingPointLiteral
                   <|> sem_PrimaryNNA_PrimNNALiteral_BooleanLiteral                                       <$> pBooleanLiteral "true"
                   <|> sem_PrimaryNNA_PrimNNALiteral_BooleanLiteral                                       <$> pBooleanLiteral "false"
                   <|> sem_PrimaryNNA_PrimNNALiteral_CharacterLiteral                                     <$> pCharacterLiteral  
                   <|> sem_PrimaryNNA_PrimNNALiteral_StringLiteral                                        <$> pStringLiteral
                   <|> sem_PrimaryNNA_PrimNNALiteral_NullLiteral                                          <$ pNullLiteral "null"
                   <|> sem_PrimaryNNA_PrimNNAVoid                                                             <$ pKeyWord "void" <* pSpecialSimbol "." <* pKeyWord "class"
                   <|> sem_PrimaryNNA_PrimNNAThis                                                             <$ pKeyWord "this"                   
                   <|> (\f -> f)                                                                          <$ pSpecialSimbol "(" <*> pParExprOrCastExpression               
                   <|> (\f -> f)                                                                          <$ pKeyWord "super" <* pSpecialSimbol "." <*> pPrimaryNNAiV
                   <|>  sem_PrimaryNNA_PrimNNATypeClassPrimitiveType                                      <$> pPrimitiveType <*> pTypeZ  <* pSpecialSimbol "." <* pKeyWord "class" 
                   <|> (\ids f -> f ids)                                                                  <$> pTypeName <*> pPrimaryNNAv
                   <|> (\i f -> f i)                                                                      <$> pIdentifier <*> pPrimaryNNA'
                   <|> (\f -> f)                                                                          <$ pKeyWord "new" *> pArrayCreationExpressionOrClassInstanceCreationExpression
                  
-- pIdentifiers     = pFoldr1Sep (sem_Identifiers_Cons, sem_Identifiers_Nil) (pSpecialSimbol ".") pIdentifier             
                   
                   
-- AQUI REALIZAR PRUEBAS PARA VERIFICAR LA AFECTACION DE pClassOrInterfaceType
pArrayCreationExpressionOrClassInstanceCreationExpression =  pPrimitiveType <**>  (( sem_ArrayCreationExpression_ArrayCreationExpressionPrimitiveType <$$> pDimExprs ) <|> ( (\ a b c -> sem_ArrayCreationExpression_ArrayCreationExpressionArrInitialPrim c b a) <$$> pDims <*> pArrayInitializer ))
                                                <|>  pClassOrInterfaceType <**> (( sem_ArrayCreationExpression_ArrayCreationExpressionClassOrInterf <$$> pDimExprs ) <|> ( (\a b c -> sem_ArrayCreationExpression_ArrayCreationExpressionArrInitialClass c b a) <$$>  pDims <*> pArrayInitializer))
                                                <|>  sem_PrimaryNNA_PrimNNAClassInstanceCreationExpression <$> pTypeArguments <*> pClassOrInterfaceType <* pSpecialSimbol "(" <*>  pArgumentList <* pSpecialSimbol ")" -- Antes -> <$> pClassInstanceCreationExpression            
                                                                                                
-- Type.class se desglosa en varios                
-- AQUI CONTROLAR CON VARIAS CONDICIONES DE CONTEXTO EL CASO CAST
pParExprOrCastExpression = (\e f -> f e)  <$>  pExpression <* pSpecialSimbol ")" <*> pParExprOrCastExpression'
                   
pParExprOrCastExpression' =  pSucceed (\e -> sem_PrimaryNNA_PrimNNAParExp e)
                                             <|> (\u e ->  sem_PrimaryNNA_UnNotPlusCastExpression e u)  <$>  pUnaryExpression -- AQUI CONTROLAR CONDICIONES DE CONTEXTO PARA CAST

pPrimaryNNA' =   (\t i -> sem_PrimaryNNA_PrimNNATypeClassReferenceTypeTypeVariable i t)  <$>  pTypeZ   <* pSpecialSimbol "." <* pKeyWord "class"
                   <|>   (\a p i -> sem_PrimaryNNA_PrimNNATypeClassReferenceTypeClassIOT i a p ) <$>  pTypeArguments <*> pPrimNNAClassOrInterfaceType  -- .class esta a continuacion
                   
-- pPrimaryNNA'' = (\f -> f)                                   <$ pKeyWord "super" <* pSpecialSimbol "." <*> pPrimaryNNA'''
--                   <|> (\i -> sem_PrimaryNNA_PrimNNAClassName i )  <$ pKeyWord "this"                    
                   
pPrimaryNNA''' =  (\n i a e -> sem_PrimaryNNA_PrimNNAMethodInvocationClassN e n i a )  <$> pNonWildTypeArguments <*> pIdentifier <* pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")" 
                      <|> (\i e -> sem_PrimaryNNA_PrimNNAFieldAccessClassName e i )            <$> pIdentifier             
                   
pPrimaryNNAiV = sem_PrimaryNNA_PrimNNAMethodInvocationSuper <$>  pNonWildTypeArguments <*> pIdentifier <* pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")" 
                   <|> sem_PrimaryNNA_PrimNNAFieldAccessSuper       <$>  pIdentifier
                   
pPrimaryNNAv  =  (\a ids -> sem_PrimaryNNA_PrimNNAMethodInvocationMN ids a)  <$ pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")" 
                 <|> (\ e ids -> sem_PrimaryNNA_PrimNNAArrayAccessExprName ids e)    <$ pSpecialSimbol "[" <*> pExpression <* pSpecialSimbol "]"                  
                 <|> pSucceed (\ids -> sem_PrimaryNNA_PostfixExpressionExpressionName ids) -- AQUI APLICAR COND CONTEXTO
                 <|> (\f -> f)                                                       <$ pSpecialSimbol "." <*> pPrimaryNNAv'
                 
pPrimaryNNAv' = (\nwt i al tn -> sem_PrimaryNNA_PrimNNAMethodInvocationTypeN tn nwt i al) <$>  pNonWildTypeArguments1 <*> pIdentifier <* pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")"   
                     <|> (\i  -> sem_PrimaryNNA_PrimNNAClassName i )  <$ pKeyWord "this"                             -- AQUI CONTROLAR QUE LA COND DE CONTEXTO ADMITA UN SOLO ID DE TYPENAME
                     <|>  (\f ->  f)                                  <$ pKeyWord "super" <* pSpecialSimbol "." <*> pPrimaryNNA''' -- -- AQUI CONTROLAR QUE LA COND DE CONTEXTO ADMITA UN SOLO ID DE TYPENAME

--               <|> (\ zpe ids -> sem_PrimaryNNA_PostExpNamePostfixZ ids zpe) <$> pZPostfixExpression -- AQUI APLICAR COND CONTEXTO             

pPrimNNAClassOrInterfaceType  = (\f -> f) <$ pSpecialSimbol "." <*> pPrimNNAClassOrInterfaceType'
                                <|> sem_PrimNNAClassOrInterfaceType_TypeZPrimNNAClassOrInterfaceType        <$> pTypeZ <* pSpecialSimbol "." <* pKeyWord "class"
pPrimNNAClassOrInterfaceType'  = (\i ta co ->  sem_PrimNNAClassOrInterfaceType_PrimNNAClassOrInterfaceType i ta co ) <$> pIdentifier <*> pTypeArguments <*> pPrimNNAClassOrInterfaceType
                                        <|> (sem_PrimNNAClassOrInterfaceType_NilPrimNNAClassOrInterfaceType)           <$ pKeyWord "class"

pZPrimaryNoNewArray = pFoldr(sem_ZPrimaryNoNewArray_Cons, sem_ZPrimaryNoNewArray_Nil) pZPrimaryOrExpression
pZPrimaryOrExpression = sem_ZPrimaryOrExpression_ZPOEExpressionDeArrayAccess <$ pSpecialSimbol "[" <*> pExpression <* pSpecialSimbol "]"
                                         <|> sem_ZPrimaryOrExpression_ZPOEZPrimary <$> pZPrimary
                                         
-- 28 -  OK
pType =  sem_Type_TypePrimitiveType <$> pPrimitiveOrRefereceType <*> pTypeZ

pPrimitiveOrRefereceType =  sem_PrimitiveOrRefereceType_TypePrimitivePrimitivetypeBoolean  <$ pKeyWord "boolean"
                          <|> sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Byte   <$ pKeyWord "byte"
                          <|> sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Short  <$ pKeyWord "short"
                          <|> sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Int    <$ pKeyWord "int"
                          <|> sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Long   <$ pKeyWord "long"
                          <|> sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeIntegral_Char   <$ pKeyWord "char"
                          <|> sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeFloating_Float  <$ pKeyWord "float"
                          <|> sem_PrimitiveOrRefereceType_TypePrimitiveNumericType_TypeFloating_Double <$ pKeyWord "double"                     
                      <|> sem_PrimitiveOrRefereceType_TypeReferece          <$> pReferenceType 

pTypeZ = pFoldr (sem_TypeZ_Cons, sem_TypeZ_Nil) pArrayType
pArrayType = sem_ArrayType_ArrayType <$ pSpecialSimbol "[" <* pSpecialSimbol "]"
         
-- OK
pPrimitiveType =  sem_PrimitiveType_PrimitivetypeBoolean            <$ pKeyWord "boolean"
                          <|> sem_PrimitiveType_NumericType_TypeIntegral_Byte   <$ pKeyWord "byte"
                          <|> sem_PrimitiveType_NumericType_TypeIntegral_Short  <$ pKeyWord "short"
                          <|> sem_PrimitiveType_NumericType_TypeIntegral_Int    <$ pKeyWord "int"
                          <|> sem_PrimitiveType_NumericType_TypeIntegral_Long   <$ pKeyWord "long"
                          <|> sem_PrimitiveType_NumericType_TypeIntegral_Char   <$ pKeyWord "char"
                          <|> sem_PrimitiveType_NumericType_TypeFloating_Float  <$ pKeyWord "float"
                          <|> sem_PrimitiveType_NumericType_TypeFloating_Double <$ pKeyWord "double"

-- OK
pReferenceType  = sem_ReferenceType_ReferenceTypeClassOrInterfaceType <$> pIdentifier <*> pTypeArguments <*> pZClassOrInterfaceType
-- pReferenceType' =  (\ta z i -> sem_ReferenceType_ReferenceTypeClassOrInterfaceType i ta z) <$> pTypeArguments <*> pZClassOrInterfaceType
-- <|> pSucceed (\i -> sem_ReferenceType_ReferenceTypeT i)

pClassOrInterfaceType = sem_ClassOrInterfaceType_ClassOrInterfaceType <$>   pIdentifier <*> pTypeArguments <*> pZClassOrInterfaceType

pZClassOrInterfaceType     = pFoldr(sem_ZClassOrInterfaceType_Cons,sem_ZClassOrInterfaceType_Nil) pZCOITTypeDeclSpecifier
pZCOITTypeDeclSpecifier    = sem_ZCOITTypeDeclSpecifier_ZCOITTypeDeclSpecifier <$ pSpecialSimbol "." <*> pIdentifier <*> pTypeArguments
                                         
-- OK                             
-- pTypeArguments = sem_TypeArguments_TypeArguments <$ pOperator "<" <*> pActualTypeArgumentList <* pOperator ">"
--                        <|> pSucceed sem_TypeArguments_NilTypeArguments
pTypeArguments = (\al f -> f al) <$ pOperator "<" <*> pActualTypeArgumentList <*> pTypeArguments'
                          <|> pSucceed sem_TypeArguments_NilTypeArguments                         

pTypeArguments' =  (\al -> sem_TypeArguments_TypeArgumentsC3 al) <$ pTokMayor ">>>"
                           <|> (\al -> sem_TypeArguments_TypeArgumentsC2 al) <$ pTokMayor ">>"
                           <|> (\al -> sem_TypeArguments_TypeArgumentsC1 al) <$ pTokMayor ">"
--                         <|>  pSucceed (\al ->sem_TypeArguments_TypeArgumentsC0 al)

pActualTypeArgumentList = pFoldr1Sep (sem_ActualTypeArgumentList_Cons,sem_ActualTypeArgumentList_Nil) (pSpecialSimbol ",") pActualTypeArgument

-- OK                                           
pActualTypeArgument = sem_ActualTypeArgument_ActualTypeArgumentWildCard <$ pOperator "?" <*> pWildcardBounds
                                   <|> sem_ActualTypeArgument_ActualTypeReferenceType   <$> pType -- <* pSpecialSimbol "[" <*pSpecialSimbol "]"
                                                                   
-- pWildCard = sem_WildCard_WildCard <$ pOperator "?" <*> pWildcardBounds                                                                                                         

{- pWildcardBounds 
  = pKeyWord "extends" <**> (( (\ss -> sem_WildcardBounds_WilcardBoundsExtends ) <$$> pReferenceType) <|> ( (\ ss -> sem_WildcardBounds_WilcardBoundsExtendsArrayType) <$$> pType ))  -- <* pSpecialSimbol "[" <* pSpecialSimbol "]"
 <|> pKeyWord "super"  <**> (((\kw -> sem_WildcardBounds_WilcardBoundsSuper) <$$> pReferenceType ) <|> ((\kw ->  sem_WildcardBounds_WilcardBoundsSuperArrayType) <$$> pType  ))       -- <* pSpecialSimbol "[" <* pSpecialSimbol "]"                            
 <|> pSucceed sem_WildcardBounds_NilwildcardBounds -}
 
pWildcardBounds  =  sem_WildcardBounds_WilcardBoundsExtendsReferenceType <$ pKeyWord "extends" <*>  pType 
                <|>  sem_WildcardBounds_WilcardBoundsSuperReferenceType <$ pKeyWord "super"  <*>  pType 
                <|> pSucceed sem_WildcardBounds_NilwildcardBounds 
 
pExpression =  (\ce f -> f ce) <$> pConditionalOrExpression <*> pExpression'
pExpression' = (\e ce z -> sem_Expression_ExpressionConditionalExprComb z e ce) <$ pOperator "?" <*> pExpression <* pOperator ":" <*> pConditionalExpression  
                        <|> (\op e z -> sem_Expression_ExpressionAssignment z op e)         <$> pAssignmentOperator <*> pExpression
                    <|> pSucceed (\ce -> sem_Expression_ExpressionConditionalExpr ce)
                   
pAssignmentOperator = sem_AssignmentOperator_AssignmentOp                  <$ pSpecialSimbol "="
                                   <|> sem_AssignmentOperator_AssignmentPlus               <$ pOperator "*="
                                   <|> sem_AssignmentOperator_AssignmentDiv                <$ pOperator "/="
                                   <|> sem_AssignmentOperator_AssignmentMod                <$ pOperator "%="
                                   <|> sem_AssignmentOperator_AssignmentAdd                <$ pOperator "+="
                                   <|> sem_AssignmentOperator_AssignmentMin                <$ pOperator "-="
                                   <|> sem_AssignmentOperator_AssignmentMinShifShift       <$ pOperator "<<="
                                   <|> sem_AssignmentOperator_AssignmentMayShitfShift      <$ pOperator ">>="
                                   <|> sem_AssignmentOperator_AssignmentMayShiftShiftShift <$ pOperator ">>>="
                                   <|> sem_AssignmentOperator_AssignmentAndSingle          <$ pOperator "&="
                                   <|> sem_AssignmentOperator_AssignmentCincun             <$ pOperator "^="
                                   <|> sem_AssignmentOperator_AssignmentOrSingle           <$ pOperator "|="
                                                   

-- AQUI CONTROLAR CON CONDICIONES DE CONTEXTO  a partir de PostfixExpression
-- pLeftHandSide = sem_LeftHandSide_LeftHandSideExpName  <$> pIdentifiers 

-- AQUI AL MENOS 1
pArgumentList = pFoldrSep (sem_ArgumentList_Cons, sem_ArgumentList_Nil) (pSpecialSimbol ",") pExpression                          
                         
-- pIdentifiers     = pFoldr1Sep (sem_Identifiers_Cons, sem_Identifiers_Nil) (pSpecialSimbol ".") pIdentifier
                   
-- pNonWildTypeArguments = sem_NonWildTypeArguments_NonWildTypeArguments <$ pOperator "<" <*> pReferenceTypeList <* pOperator ">"
--                                        <|> pSucceed sem_NonWildTypeArguments_NilNonWildTypeArguments
pNonWildTypeArguments = (\tl f -> f tl ) <$ pOperator "<" <*> pReferenceTypeList <*> pNonWildTypeArguments'
                                          <|> pSucceed sem_NonWildTypeArguments_NilNonWildTypeArguments

pNonWildTypeArguments' =  (\tl -> sem_NonWildTypeArguments_NonWildTypeArgumentsC3 tl) <$ pTokMayor ">>>"
                                          <|> (\tl -> sem_NonWildTypeArguments_NonWildTypeArgumentsC2 tl) <$ pTokMayor ">>"
                                          <|> (\tl -> sem_NonWildTypeArguments_NonWildTypeArgumentsC1 tl)  <$ pTokMayor ">"
--                                        <|> pSucceed (\tl -> sem_NonWildTypeArguments_NonWildTypeArgumentsC0 tl)

-- Al menos 1
-- pNonWildTypeArguments1 = sem_NonWildTypeArguments_NonWildTypeArguments <$ pOperator "<" <*> pReferenceTypeList <* pOperator ">"
pNonWildTypeArguments1 = (\tl f -> f tl) <$ pOperator "<" <*> pReferenceTypeList <*> pNonWildTypeArguments1'

pNonWildTypeArguments1' = (\ tl -> sem_NonWildTypeArguments_NonWildTypeArgumentsC3 tl) <$ pTokMayor ">>>"
                                          <|> (\ tl -> sem_NonWildTypeArguments_NonWildTypeArgumentsC2 tl) <$ pTokMayor ">>"
                                          <|>  (\ tl -> sem_NonWildTypeArguments_NonWildTypeArgumentsC1 tl) <$ pTokMayor ">"
--                                        <|>  pSucceed (\ tl -> sem_NonWildTypeArguments_NonWildTypeArgumentsC0 tl)                                      
-- OJO OPTIMIZAR ESTO
pReferenceTypeList = pFoldr1Sep (sem_ReferenceTypeList_Cons,sem_ReferenceTypeList_Nil) (pSpecialSimbol ",") pType
-- pReferenceTypeOrType = sem_ReferenceTypeOrType_ReferenceTypeList <$> pReferenceType
--                                      <|> sem_ReferenceTypeOrType_ReferenceTypeListArrayType <$> pType -- <* pSpecialSimbol "[" <* pSpecialSimbol "]"

{- pArrayCreationExpression = pKeyWord "new" *> pArrayCreationExpression'
pArrayCreationExpression' =  pPrimitiveType <**>  (( sem_ArrayCreationExpression_ArrayCreationExpressionPrimitiveType <$$> pDimExprs ) <|> ( (\ a b c -> sem_ArrayCreationExpression_ArrayCreationExpressionArrInitialPrim c b a) <$$> pDims <*> pArrayInitializer ))
                                                <|>  pClassOrInterfaceType <**> (( sem_ArrayCreationExpression_ArrayCreationExpressionClassOrInterf <$$> pDimExprs ) <|> ( (\a b c -> sem_ArrayCreationExpression_ArrayCreationExpressionArrInitialClass c b a) <$$>  pDims <*> pArrayInitializer)) -}
                                            
pDimExprs = sem_DimExprs_DimExprs <$ pSpecialSimbol "[" <*> pExpression <* pSpecialSimbol "]" <*> pDimExprs
             <|> sem_DimExprs_NilDimExprsDims <$> pDims
             <|> pSucceed sem_DimExprs_NilDimExprs
         
pDims  =  (sem_Dims_Dims ()) <$ pSpecialSimbol "[" <* pSpecialSimbol "]" <*> pDims'
pDims' =  (sem_Dims_Dims ()) <$ pSpecialSimbol "[" <* pSpecialSimbol "]" <*> pDims'
     <|> pSucceed sem_Dims_NilDims

pArrayInitializer = sem_ArrayInitializer_ArrayInitializer <$ pSpecialSimbol "{" <*> pVariableInitializers <* pSpecialSimbol "}"

pVariableInitializers = pFoldrSep (sem_VariableInitializers_Cons,sem_VariableInitializers_Nil) (pSpecialSimbol ",") pVariableInitializer
                                          
pVariableInitializer = sem_VariableInitializer_VariableInitializerExp <$> pExpression 
                                        <|> sem_VariableInitializer_VariableInitializerArr <$> pArrayInitializer
                                                   
pElementValueArrayInitializer = sem_ElementValueArrayInitializer_ElementValueArrayInitializer <$ pSpecialSimbol "{" <*>  pElementValues <* pSpecialSimbol "}"  

pElementValues = pFoldrSep(sem_ElementValues_Cons,sem_ElementValues_Nil) (pSpecialSimbol ",") pElementValue

pPackageName = pFoldr1Sep(sem_PackageName_Cons,sem_PackageName_Nil) (pSpecialSimbol ".") pIdentifier

-- ----------------------------------------------------------------------------------------
-- FIN Parser of PackageDeclaration - pAnnotations
-- ----------------------------------------------------------------------------------------

-- ----------------------------------------------------------------------------------------              
-- Parser of ImportDeclarations
-- ----------------------------------------------------------------------------------------
-- 77
pImportDeclarations =   sem_ImportDeclarations_ImportDeclarations <$ pKeyWord "import" <*> pImportDeclaration  <*> pImportDeclarations
                                   <|> pSucceed sem_ImportDeclarations_NilImportDeclarations

-- 78
pImportDeclaration = sem_ImportDeclaration_SingleTypeImportDeclaration <$> pTypeName <* pSpecialSimbol ";"
                                  <|> sem_ImportDeclaration_TypeImportOnDemandDeclaration <$> pPackageOrTypeName <* pSpecialSimbol "." <* pOperator "*" <* pSpecialSimbol ";"
                                  <|> pKeyWord "static" *> pTypeName <**> (( (\tn ss -> sem_ImportDeclaration_SingleStaticImportDeclaration tn) <$$> pSpecialSimbol ";" ) <|> ( (\tn ss -> sem_ImportDeclaration_StaticImportOnDemandDeclaration tn ) <$$> pSpecialSimbol "." <* pOperator "*" <* pSpecialSimbol ";" ) )

-- 81
pPackageOrTypeName = sem_PackageOrTypeName_PackageOrTypeName <$> pIdentifier <*> pPackageOrTypeName'
pPackageOrTypeName' = sem_PackageOrTypeName_PackageOrTypeName <$ pSpecialSimbol "." <*> pIdentifier <*> pPackageOrTypeName'
                                   <|> pSucceed sem_PackageOrTypeName_NilPackageOrTypeName

-- ----------------------------------------------------------------------------------------------                                        
-- FIN Parser of ImportDeclarations
-- ----------------------------------------------------------------------------------------------

-- ----------------------------------------------------------------------------------------------                                        
-- INICIO Parser of TypeDeclarations
-- ----------------------------------------------------------------------------------------------
-- 84
pTypeDeclarations = pFoldr (sem_TypeDeclarations_Cons,sem_TypeDeclarations_Nil)pTypeDeclaration
                                 
pTypeDeclaration = (\cm f -> f cm) <$> pModifiers <*> pTypeDeclaration'
                                <|> sem_TypeDeclaration_TypeDeclarationSemiColon <$ pSpecialSimbol ";"
pTypeDeclaration' =   (\a b c d e f    -> sem_TypeDeclaration_TypeDeclarationClassDeclarationNormalCD f a b c d e)              <$ pKeyWord "class" <*> pIdentifier <*> pTypeParameters <*> pSuper <*> pInterfaces <* pSpecialSimbol "{" <*> pClassBodyDeclarations <* pSpecialSimbol "}"
                                  <|> (\a b c f        -> sem_TypeDeclaration_TypeDeclarationClassDeclarationEnumD f a b c)                     <$ pKeyWord "enum" <*> pIdentifier <*> pInterfaces <*> pEnumBody
                                  <|> (\i l f          -> sem_TypeDeclaration_TypeDeclarationInterfaceDeclarationAnnotationTypeD f i l )        <$ pSpecialSimbol "@" <* pKeyWord "interface" <*> pIdentifier <* pSpecialSimbol "{" <*> (pFoldr (sem_ListAnnotationTypeElementDeclaration_Cons,sem_ListAnnotationTypeElementDeclaration_Nil) pAnnotationTypeElementDeclaration) <* pSpecialSimbol "}"
                                  <|> (\i tp e  imd f  -> sem_TypeDeclaration_TypeDeclarationInterfaceDeclarationNormalInterfaceD f i tp e imd )<$ pKeyWord "interface" <*> pIdentifier <*> pTypeParameters <*> pExtendsInterfaces <* pSpecialSimbol "{" <*> pFoldr (sem_ListInterfaceMemberDeclaration_Cons,sem_ListInterfaceMemberDeclaration_Nil) pInterfaceMemberDeclaration <* pSpecialSimbol "}"
                                  
-- NO CAMBIAR A pList
pModifiers = sem_Modifiers_Modifiers <$> pModifier <*> pModifiers
                           <|> pSucceed sem_Modifiers_NilModifiers
                           
-- pClassModifiers = pList pClassModifier XX                       
pModifier =  sem_Modifier_ModifierPublic    <$ pKeyWord "public"
                 <|> sem_Modifier_ModifierProtected <$ pKeyWord "protected"
                 <|> sem_Modifier_ModifierPrivate   <$ pKeyWord "private"
                 <|> sem_Modifier_ModifierAbstract  <$ pKeyWord "abstract"
                 <|> sem_Modifier_ModifiersStatic   <$ pKeyWord "static"
                 <|> sem_Modifier_ModifierFinal     <$ pKeyWord "final"
                 <|> sem_Modifier_ModifierStrictfp  <$ pKeyWord "strictfp"
                 <|> sem_Modifier_FieldModifierVolatile  <$ pKeyWord "volatile"
                 <|> sem_Modifier_FieldModifierTransient  <$ pKeyWord "transient"
                 <|> sem_Modifier_MethodModifierSynchronized <$ pKeyWord "synchronized"
                 <|> sem_Modifier_MethodModifierNative <$ pKeyWord "native"
                 <|> sem_Modifier_ModifierAnnotation <$> pAnnotation
                          
-- pTypeParameters = sem_TypeParameters_TypeParameters <$ pOperator "<" <*>  pTypeParameterList <* pOperator ">" 
--                         <|> pSucceed sem_TypeParameters_NilTypeParameters


pTypeParameters = (\pl f -> f pl) <$ pOperator "<" <*>  pTypeParameterList <*> pTypeParameters'
                           <|> pSucceed sem_TypeParameters_NilTypeParameters                       

pTypeParameters' = (\ pl  -> sem_TypeParameters_TypeParametersC3 pl ) <$ pTokMayor ">>>"
                           <|> (\ pl  -> sem_TypeParameters_TypeParametersC2 pl ) <$ pTokMayor ">>"     
                           <|> (\ pl  -> sem_TypeParameters_TypeParametersC1 pl ) <$ pTokMayor ">" 
--                         <|>  pSucceed (\ pl  -> sem_TypeParameters_TypeParametersC1 pl )
                                                   
pTypeParameterList = pFoldr1Sep (sem_TypeParameterList_Cons, sem_TypeParameterList_Nil) (pSpecialSimbol ",")  pTypeParameter                          
                          
pTypeParameter = (\tv ftv -> ftv tv) <$> pIdentifier <*> pZTypeBound
pZTypeBound =  (\tb tv -> sem_TypeParameter_TypeParameterBound tv tb)<$>  pTypeBound
                        <|> pSucceed (\ftp -> sem_TypeParameter_TypeParameter ftp)

pTypeBound = sem_TypeBound_TypeBound <$ pKeyWord "extends" <*> pClassOrInterfaceType <*> pAdditionalBoundList
pAdditionalBoundList = sem_TypeBound_TypeBound <$ pOperator "&" <*> pClassOrInterfaceType <*> pAdditionalBoundList
                                        <|> pSucceed sem_TypeBound_NilAdditionalBoundList
                                        
pSuper = sem_Super_Super <$ pKeyWord "extends" <*> pClassOrInterfaceType 
      <|> pSucceed sem_Super_NilSuper
      
pInterfaces = sem_Interfaces_Interfaces <$ pKeyWord "implements" <*> pInterfaceTypeList
                   <|> pSucceed sem_Interfaces_NilInterfaces
                   
pInterfaceTypeList = pFoldr1Sep (sem_InterfaceTypeList_Cons,sem_InterfaceTypeList_Nil) (pSpecialSimbol ",") pClassOrInterfaceType
                                   
pClassBodyDeclarations = pFoldr (sem_ClassBodyDeclarations_Cons, sem_ClassBodyDeclarations_Nil) pClassBodyDeclaration
                                          
pClassBodyDeclaration =  sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclSemiColon <$ pSpecialSimbol ";"
                                         <|> sem_ClassBodyDeclaration_ClassBodyInstanceInitializer          <$ pSpecialSimbol "{" <*> pBlockStatements <* pSpecialSimbol "}"  -- pBlock -- pInstanceInitializer
                                         <|> sem_ClassBodyDeclaration_ClassBodyStaticInitializer            <$ pKeyWord "static" <* pSpecialSimbol "{" <*> pBlockStatements <* pSpecialSimbol "}"  -- pBlock -- pStaticInitializer
                                         <|> (\cm f -> f cm)                                                <$> pModifiers <*> pClassMemberDeclaration'
pClassMemberDeclaration' =   (\a b c d e f    -> sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclClassDeclarationNormalCD f a b c d e)        <$ pKeyWord "class" <*> pIdentifier <*> pTypeParameters <*> pSuper <*> pInterfaces <* pSpecialSimbol "{" <*> pClassBodyDeclarations <* pSpecialSimbol "}"
                                  <|> (\a b c f        -> sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclClassDeclarationEnumD f a b c)                      <$ pKeyWord "enum" <*> pIdentifier <*> pInterfaces <*> pEnumBody
                                  <|> (\i l f          -> sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclInterfaceDeclarationAnnotationTypeD f i l )         <$ pSpecialSimbol "@" <* pKeyWord "interface" <*> pIdentifier <* pSpecialSimbol "{" <*> (pFoldr (sem_ListAnnotationTypeElementDeclaration_Cons,sem_ListAnnotationTypeElementDeclaration_Nil) pAnnotationTypeElementDeclaration) <* pSpecialSimbol "}"
                                  <|> (\i tp e  imd f  -> sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclInterfaceDeclarationNormalInterfaceD f i tp e imd ) <$ pKeyWord "interface" <*> pIdentifier <*> pTypeParameters <*> pExtendsInterfaces <* pSpecialSimbol "{" <*> pFoldr (sem_ListInterfaceMemberDeclaration_Cons,sem_ListInterfaceMemberDeclaration_Nil) pInterfaceMemberDeclaration <* pSpecialSimbol "}"
                                  <|> (\t vd f -> sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclFieldDeclaration f t vd)                                    <$> pType <*> pVariableDeclarators <* pSpecialSimbol  ";"
                                  <|> (\tp f -> f tp)                                                                                                              <$> pTypeParameters <*> pClassMemberDeclarationConstructorOrMethod

pClassMemberDeclarationConstructorOrMethod = (\rt md t mb tp m ->  sem_ClassBodyDeclaration_ClassBodyDeclClassMemberDeclMethodDeclaration m tp rt md t mb) <$> pResultType <*> pMethodDeclarator <*> pThrows <*> pMethodBody
                                                                                  <|> (\i f -> f i) <$> pIdentifier <* pSpecialSimbol "(" <*> pZConstructorDeclarator
                                                                                  
pZConstructorDeclarator =  (\   t cb  i tp m -> sem_ClassBodyDeclaration_ClassBodyConstructorDeclarationNoFormalParList m tp i t cb) <$ pSpecialSimbol ")" <*> pThrows <*> pConstructorBody 
                                           <|> (\fp t cb  i tp m -> sem_ClassBodyDeclaration_ClassBodyConstructorDeclaration m tp i fp t cb )            <$> pFormalParameterList <* pSpecialSimbol ")" <*> pThrows <*> pConstructorBody                                          
                                                                                  
pVariableDeclarators = pFoldr1Sep (sem_VariableDeclarators_Cons, sem_VariableDeclarators_Nil) (pSpecialSimbol ",") pVariableDeclarator
                                        
pVariableDeclarator = (\vdi fvdi -> fvdi vdi) <$> pVariableDeclaratorId <*> pZVariableDeclarator
pZVariableDeclarator = (\vi vdi -> sem_VariableDeclarator_VariableDeclaratorIdAsig vdi vi) <$ pSpecialSimbol "=" <*> pVariableInitializer 
                    <|> pSucceed (\vdi -> sem_VariableDeclarator_VariableDeclaratorId vdi)
pVariableDeclaratorId   = (\i fvd -> fvd i) <$> pIdentifier <*> pZVariableDeclaratorIdZ
pZVariableDeclaratorIdZ = (\vd i -> sem_VariableDeclaratorId_VarDeclaratorIdVDZ i vd) <$> pVariableDeclaratorIdZ
                       <|> pSucceed (\i -> sem_VariableDeclaratorId_VarDeclaratorId i) 
pVariableDeclaratorIdZ   = (\fvd -> fvd)  <$ pSpecialSimbol "[" <* pSpecialSimbol "]" <*> pVariableDeclaratorIdZFi
pVariableDeclaratorIdZFi = (\vd -> sem_VariableDeclatatorIdZ_VarDeclaratorIdZ vd)  <$> pVariableDeclaratorIdZ
                                            <|> pSucceed (sem_VariableDeclatatorIdZ_VarDeclaratorIdCorchete)
                                          
pResultType = sem_ResultType_ResultTypeVoid <$ pKeyWord "void"
                   <|> sem_ResultType_ResultTypeType <$> pType

pMethodDeclarator  =  sem_MethodDeclarator_MethodDeclaratorFormalPL <$> pIdentifier <* pSpecialSimbol "(" <*> pFormalParameterList <* pSpecialSimbol ")"

pFormalParameterList = (\vm t f -> f vm t ) <$> pVariableModifiers <*> pType <*> pFormalParameterList'
                                        <|> pSucceed sem_FormalParameterList_FormalParameterListNil     
pFormalParameterList' = (\vdi vm t ->  sem_FormalParameterList_FormalParameterListLast vm t vdi)          <$ pSpecialSimbol "." <* pSpecialSimbol "." <* pSpecialSimbol "." <*> pVariableDeclaratorId   
                                        <|> (\vdi fpl vm t -> sem_FormalParameterList_FormalParameterListFormal vm t vdi fpl) <$> pVariableDeclaratorId <*> pFormalParameterList''

pFormalParameterList'' =  (\f -> f) <$ pSpecialSimbol "," <*> pFormalParameterList
                                          <|> pSucceed sem_FormalParameterList_FormalParameterListNil   
                                        
-- debe haber al menos 1 -> NO
-- pVariableModifiers  = sem_VariableModifiers_VariableModifiers <$> pVariableModifier <*> pVariableModifiers'
pVariableModifiers = sem_VariableModifiers_VariableModifiers <$> pVariableModifier <*> pVariableModifiers
                                   <|> pSucceed sem_VariableModifiers_NilVariableModifiers
-- pVariableModifiers = pList1 pVariableModifier                                   

pVariableModifier = sem_VariableModifier_VariableModifierFinal <$ pKeyWord "final" 
                                 <|> sem_VariableModifier_VariableModifierAnnotation <$> pAnnotation
                                 
pThrows = sem_Throws_Throws <$ pKeyWord "throws" <*> pExceptionTypeList
           <|> pSucceed sem_Throws_NilThrows
           
pExceptionTypeList = pFoldr1Sep (sem_ExceptionTypeList_Cons, sem_ExceptionTypeList_Nil) (pSpecialSimbol ",") pExceptionType
                                   
pExceptionType =  sem_ExceptionType_ExceptionTypeTypeVariable <$> pIdentifier
                          <|> sem_ExceptionType_ExceptionTypeClassType <$> pClassOrInterfaceType
                          
pMethodBody = sem_MethodBody_MethodBodyBlock       <$ pSpecialSimbol "{" <*> pBlockStatements <* pSpecialSimbol "}" -- pBlock 
                    <|> sem_MethodBody_MethodBodySemiColon <$ pSpecialSimbol ";"
                    
pBlockStatements = pFoldr (sem_BlockStatements_Cons,sem_BlockStatements_Nil) pBlockStatement                            
-- para el caso de que debe haber al menos 1                            
pBlockStatements' = pFoldr1 (sem_BlockStatements_Cons,sem_BlockStatements_Nil) pBlockStatement
                                
pBlockStatement =  (\cm f -> f cm)                                                   <$> pModifiers <*> pBlockStatement'        
                           <|> sem_BlockStatement_BlockStatementStatement                        <$> pStatement

pBlockStatement' =  (\a b c d e f   -> sem_BlockStatement_BlockStatementClassDeclarationNormalClassDeclaration f a b c d e)  <$ pKeyWord "class" <*> pIdentifier <*> pTypeParameters <*> pSuper <*> pInterfaces <* pSpecialSimbol "{" <*> pClassBodyDeclarations <* pSpecialSimbol "}"
                                <|> (\a b c f       -> sem_BlockStatement_BlockStatementClassDeclarationEnumDeclaration f a b c)             <$ pKeyWord "enum" <*> pIdentifier <*> pInterfaces <*> pEnumBody
                                <|> (\t vd f        -> sem_BlockStatement_BlockStatementLocalVariableDeclarationStatement f t vd )          <$>  pType <*> pVariableDeclarators <* pSpecialSimbol ";"

pStatement =  sem_Statement_StatementLabeled                     <$> pIdentifier <* pOperator ":" <*> pStatement  -- LabeledStatement
                  <|> (\e s f -> f e s)                                  <$ pKeyWord "if" <* pSpecialSimbol "(" <*> pExpression <* pSpecialSimbol ")" <*> pStatement <*> pElse
                  <|> sem_Statement_StatementWhile                       <$ pKeyWord "while" <* pSpecialSimbol "(" <*> pExpression <* pSpecialSimbol ")" <*> pStatement  -- pWhileStatement
                  <|> sem_Statement_SWTSBlock                            <$ pSpecialSimbol "{" <*> pBlockStatements <* pSpecialSimbol "}" -- pBlock
                  <|> sem_Statement_SWTSEmptyStatement                   <$ pSpecialSimbol ";"                                                               
                  <|> (\e f -> f e)                                      <$ pKeyWord "assert" <*> pExpression <*> pZAssertStatement
                  <|> sem_Statement_SWTSSwitchStatement                  <$ pKeyWord "switch" <* pSpecialSimbol "(" <*> pExpression <* pSpecialSimbol ")" <*> pSwitchBlock
                  <|> sem_Statement_SWTSDoStatement                      <$ pKeyWord "do" <*> pStatement <* pKeyWord "while" <* pSpecialSimbol "(" <*> pExpression <* pSpecialSimbol ")" <* pSpecialSimbol ";"
                  <|> (\fb -> fb)                                        <$ pKeyWord "break" <*> pZBreakStatement
                  <|> (\f -> f)                                          <$ pKeyWord "continue" <*> pZContinueStatement
                  <|> (\f -> f)                                          <$ pKeyWord "return" <*> pZReturnStatement
                  <|> sem_Statement_SWTSynchronizedStatement             <$ pKeyWord "synchronized" <* pSpecialSimbol "(" <*> pExpression <* pSpecialSimbol ")" <* pSpecialSimbol "{" <*> pBlockStatements <* pSpecialSimbol "}"  -- pBlock 
                  <|> sem_Statement_SWTThrowStatement                    <$ pKeyWord "throw" <*> pExpression <* pSpecialSimbol ";" 
                  <|> (\b fts -> fts b )                                 <$ pKeyWord "try" <* pSpecialSimbol "{" <*> pBlockStatements <* pSpecialSimbol "}"  <*> pZTryStatement
                  <|> sem_Statement_StatementFor                         <$> pForStatement                
                  <|> sem_Statement_SWTSExpressionStatement              <$> pExpression <*  pSpecialSimbol ";" 
                  --  reemplazado  <|> sem_StatementWithoutTrailingSubstatement_SWTSExpressionStatement  <$> pExpressionStatement

pElse = pSucceed  sem_Statement_StatementIf             -- IfThenStatement
         <|> (\selse e sthen ->   sem_Statement_StatementIfElse e sthen selse) <$ pKeyWord "else" <*> pStatement   -- pIfThenElseStatement
                  
pZAssertStatement = (\ ce e -> sem_Statement_SWTSAssertStatementCondEx e ce) <$  pOperator ":" <*> pConditionalExpression <* pSpecialSimbol ";"
                                <|> (\e     -> sem_Statement_SWTSAssertStatementCond e )     <$  pSpecialSimbol ";"
                                
pZContinueStatement = (\i -> sem_Statement_SWTSContinueStatement i ) <$> pIdentifier <* pSpecialSimbol ";"
                                  <|> sem_Statement_SWTSNilContinueStatement <$ pSpecialSimbol ";"
                                  
pZTryStatement =  (\l b -> sem_Statement_SWTTryStatement b l) <$> (pList1 pCatchClause)         
              <|> (\l f b -> sem_Statement_SWTTryStatementFinally b l f) <$> (pList pCatchClause) <* pKeyWord "finally" <* pSpecialSimbol "{" <*> pBlockStatements <* pSpecialSimbol "}"  -- pBlock
                                                                                                     
pZReturnStatement  = (\e -> sem_Statement_SWTSReturnStatement e)    <$> pExpression <* pSpecialSimbol ";"
                                 <|> sem_Statement_SWTSNilReturnStatement <$ pSpecialSimbol ";"

-- pStatementExpression = sem_StatementExpression_StatExpressionAssign <$> pAssignment -- <* pSpecialSimbol ";"
--                                      <|> sem_StatementExpression_StatExpressionConditionalExpression <$> pConditionalExpression -- <* pSpecialSimbol ";"
                                
-- revisar orden, optimizar
pSwitchBlock  = (\fs -> fs)<$ pSpecialSimbol "{" <*> pZSwitchBlock
pZSwitchBlock = (\ls fs -> fs ls)                            <$> pFoldr1 (sem_SwitchBlockStatementGroups_Cons, sem_SwitchBlockStatementGroups_Nil) pSwitchBlockStatementGroup <*> pZZSwitchBlock
             <|> (\l -> sem_SwitchBlock_SwitchBlockLabels l) <$> pFoldr1 (sem_SwitchLabels_Cons,sem_SwitchLabels_Nil) pSwitchLabel <* pSpecialSimbol "}"
             <|> (sem_SwitchBlock_NilSwitchBlock)            <$ pSpecialSimbol "}"
pZZSwitchBlock = (\ll ls -> sem_SwitchBlock_SwitchBlockAll ls ll ) <$> pFoldr1 (sem_SwitchLabels_Cons,sem_SwitchLabels_Nil) pSwitchLabel <* pSpecialSimbol "}"
              <|> (\l -> sem_SwitchBlock_SwitchBlockGroups l)      <$ pSpecialSimbol "}" 

pSwitchBlockStatementGroup = sem_SwitchBlockStatementGroup_SwitchBlockStatementGroup <$> pFoldr1 (sem_SwitchLabels_Cons,sem_SwitchLabels_Nil) pSwitchLabel <*> pBlockStatements'
                    
pSwitchLabel =  (\fsl -> fsl) <$ pKeyWord "case" <*> pZSwitchLabel
                    <|> sem_SwitchLabel_SwitchLabelDefault <$ pKeyWord "default" <* pOperator ":"
pZSwitchLabel = (\e -> sem_SwitchLabel_SwitchLabelConstant e) <$> pExpression <* pOperator ":" 
             <|> (\i -> sem_SwitchLabel_SwitchLabelEnum i)    <$> pIdentifier <* pOperator ":" 
                    
pZBreakStatement = (sem_Statement_SWTSBreakStatement) <$  pSpecialSimbol ";"
                <|> (\i -> sem_Statement_SWTSBreakStatementId i) <$> pIdentifier <* pSpecialSimbol ";"
                           
pForInit = sem_ForInit_ForInitStaExp <$> pStatementExpressionList
        <|> sem_ForInit_ForInitLocalVar <$> pVariableModifiers <*> pType <*> pVariableDeclarators -- <* pSpecialSimbol ";" --pLocalVariableDeclaration
        <|> pSucceed sem_ForInit_NilForInit
        
pStatementExpressionList = sem_StatementExpressionList_StatementExpressionList <$> pExpression <*> pStatementExpressionList'
pStatementExpressionList' = sem_StatementExpressionList_StatementExpressionList <$ pSpecialSimbol "," <*> pExpression <*> pStatementExpressionList'
                                                <|> pSucceed sem_StatementExpressionList_NilStatementExpressionList
                                                                                                
pForUpdate = sem_ForUpdate_ForUpdate <$> pStatementExpressionList
                  <|> pSucceed sem_ForUpdate_NilForUpdate

pForStatement = (\ffs -> ffs) <$ pKeyWord "for" <* pSpecialSimbol "(" <*> pZForStatement
pZForStatement = (\fi f -> f fi)                                             <$> pForInit <* pSpecialSimbol ";" <*> pZBasicForStatement
                         <|> sem_ForStatement_ForStatementEnhancedForStatement           <$> pVariableModifiers <*> pType <*> pIdentifier <* pOperator ":" <*> pExpression <* pSpecialSimbol ")" <*> pStatement
                         <|> sem_ForStatement_ForStatementEnhancedForStatementNoVarModif <$> pType <*> pIdentifier <* pOperator ":" <*> pExpression <* pSpecialSimbol ")" <*> pStatement 

pZBasicForStatement = (\e fu s fi  -> sem_ForStatement_ForStatementBasicForStatementAll fi e fu s) <$> pExpression <* pSpecialSimbol ";" <*> pForUpdate <* pSpecialSimbol ")" <*> pStatement
                                  <|> (\fu s fi -> sem_ForStatement_ForStatementBasicForStatementNoExp fi fu s)    <$ pSpecialSimbol ";" <*> pForUpdate <* pSpecialSimbol ")" <*> pStatement
                                  
pExtendsInterfaces = sem_ExtendsInterfaces_ExtendsInterfaceType <$ pKeyWord "extends" <*> pClassOrInterfaceType <*> pExtendsInterfaces'
                                  <|> pSucceed sem_ExtendsInterfaces_NilExtendsInterfaces
pExtendsInterfaces' = sem_ExtendsInterfaces_ExtendsInterfaceType <$ pSpecialSimbol "," <*> pClassOrInterfaceType <*> pExtendsInterfaces'                                  
                                  <|> pSucceed sem_ExtendsInterfaces_NilExtendsInterfaces

pInterfaceMemberDeclaration = (\cm f -> f cm) <$> pModifiers <*> pInterfaceMemberDeclaration'
                                <|> sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationSemiColon <$ pSpecialSimbol ";"
pInterfaceMemberDeclaration' =   (\a b c d e f    -> sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationClassDeclarationNormalCD f a b c d e)   <$ pKeyWord "class" <*> pIdentifier <*> pTypeParameters <*> pSuper <*> pInterfaces <* pSpecialSimbol "{" <*> pClassBodyDeclarations <* pSpecialSimbol "}"
                                  <|> (\a b c f        -> sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationClassDeclarationEnumD f a b c)                     <$ pKeyWord "enum" <*> pIdentifier <*> pInterfaces <*> pEnumBody
                                  <|> (\i l f          -> sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationInterfaceDeclarationAnnotationTypeD f i l )        <$ pSpecialSimbol "@" <* pKeyWord "interface" <*> pIdentifier <* pSpecialSimbol "{" <*> (pFoldr (sem_ListAnnotationTypeElementDeclaration_Cons,sem_ListAnnotationTypeElementDeclaration_Nil) pAnnotationTypeElementDeclaration) <* pSpecialSimbol "}"
                                  <|> (\i tp e  imd f  -> sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationInterfaceDeclarationNormalInterfaceD f i tp e imd )<$ pKeyWord "interface" <*> pIdentifier <*> pTypeParameters <*> pExtendsInterfaces <* pSpecialSimbol "{" <*> pFoldr (sem_ListInterfaceMemberDeclaration_Cons,sem_ListInterfaceMemberDeclaration_Nil) pInterfaceMemberDeclaration <* pSpecialSimbol "}"
                                  <|> (\t vd f         -> sem_InterfaceMemberDeclaration_InterfaceMemberDeclarationConstant f t vd)                                           <$> pType <*> pVariableDeclarators <* pSpecialSimbol ";"
                                  <|> (\tp rt d t f    -> sem_InterfaceMemberDeclaration_InterfaceMemberDeclarationAbstract f tp rt d t)                                      <$> pTypeParameters <*> pResultType <*> pMethodDeclarator <*> pThrows

pAnnotationTypeElementDeclaration = (\cm f -> f cm) <$> pModifiers <*> pAnnotationTypeElementDeclaration'
                                <|> sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationSemiColon <$ pSpecialSimbol ";"
                                
pAnnotationTypeElementDeclaration' =   (\a b c d e f    -> sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationClassDeclarationNormalCD f a b c d e) <$ pKeyWord "class" <*> pIdentifier <*> pTypeParameters <*> pSuper <*> pInterfaces <* pSpecialSimbol "{" <*> pClassBodyDeclarations <* pSpecialSimbol "}"
                                  <|> (\a b c f        -> sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationClassDeclarationEnumD f a b c)                         <$ pKeyWord "enum" <*> pIdentifier <*> pInterfaces <*> pEnumBody
                                  <|> (\i l f          -> sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationInterfaceDeclarationAnnotationTypeD f i l )            <$ pSpecialSimbol "@" <* pKeyWord "interface" <*> pIdentifier <* pSpecialSimbol "{" <*> (pFoldr (sem_ListAnnotationTypeElementDeclaration_Cons,sem_ListAnnotationTypeElementDeclaration_Nil) pAnnotationTypeElementDeclaration) <* pSpecialSimbol "}"
                                  <|> (\i tp e  imd f  -> sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationInterfaceDeclarationNormalInterfaceD f i tp e imd )    <$ pKeyWord "interface" <*> pIdentifier <*> pTypeParameters <*> pExtendsInterfaces <* pSpecialSimbol "{" <*> pFoldr (sem_ListInterfaceMemberDeclaration_Cons,sem_ListInterfaceMemberDeclaration_Nil) pInterfaceMemberDeclaration <* pSpecialSimbol "}"
                                  <|> (\t f        ->  f t)                                                                                                                         <$> pType <*> pAnnotationTypeElementDeclaration''
                                  
pAnnotationTypeElementDeclaration''= (\vd t f          -> sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclConstant f t vd)                                      <$> pVariableDeclarators <* pSpecialSimbol ";"
                                                 <|> (\i d t f         -> sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclAbstract f t i d)                                     <$> pIdentifier <* pSpecialSimbol "(" <* pSpecialSimbol ")" <*> pDefaultValue <* pSpecialSimbol ";" 

pDefaultValue = sem_DefaultValue_DefaultValue <$ pKeyWord "default" <*> pElementValue 
                         <|> pSucceed sem_DefaultValue_NilDefaultValue
                         
pEnumBody = (\ec feb -> feb ec)  <$ pSpecialSimbol "{" <*>  pEnumConstants <*> pZEnumBody        
pZEnumBody = (\ed ec -> sem_EnumBody_EnumBody ec ed ) <$ pSpecialSimbol "," <*> pEnumBodyDeclarations <* pSpecialSimbol "}"
                 <|> (\ed ec -> sem_EnumBody_EnumBody ec ed ) <$> pEnumBodyDeclarations <* pSpecialSimbol "}"

-- Se queda tal cual por problemas de comas
pEnumConstants = sem_EnumConstants_EnumConstants <$>  pEnumConstant <*> pEnumConstants'
                         <|> pSucceed sem_EnumConstants_NilEnumConstants  -- Debe existir caso Succeed                   
pEnumConstants' = sem_EnumConstants_EnumConstants <$ pSpecialSimbol "," <*> pEnumConstant <*> pEnumConstants'
                           <|> pSucceed sem_EnumConstants_NilEnumConstants
                           
pEnumConstant = (\a i f -> f a i ) <$> pAnnotations <*> pIdentifier <*> pZEnumConstant
pZEnumConstant = (\al fec a i  -> fec a i al) <$ pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")" <*> pZZEnumConstant
                        <|> (\cb  a i -> sem_EnumConstant_EnumConstantClasB a i cb) <$ pSpecialSimbol "{" <*> pClassBodyDeclarations <* pSpecialSimbol "}"
                        <|> pSucceed (\i -> sem_EnumConstant_EnumConstantNothing i)
pZZEnumConstant = (\cl al  a i -> sem_EnumConstant_EnumConstantAll  al a i cl) <$ pSpecialSimbol "{" <*> pClassBodyDeclarations <* pSpecialSimbol "}"
                           <|> pSucceed (\al a i -> sem_EnumConstant_EnumConstantArgL al a i)

pEnumBodyDeclarations = sem_EnumBodyDeclarations_EnumBodyDeclarations <$ pSpecialSimbol ";" <*> pClassBodyDeclarations
                                         <|> pSucceed sem_EnumDeclaration_NilEnumBodyDeclarations

pConstructorBody = sem_ConstructorBody_ConstructorBody <$ pSpecialSimbol "{" <*> pExplicitConstructorInvocation <*> pBlockStatements <* pSpecialSimbol "}"  

pExplicitConstructorInvocation = (\nw f -> f  nw)   <$> pNonWildTypeArguments <*> pZExplicitConstructorInvocation
                                                          <|> sem_ExplicitConstructorInvocation_ExplConsInvPrimary <$> pPrimary <* pKeyWord "super" <* pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")" <* pSpecialSimbol ";"
                                                          <|> pSucceed sem_ExplicitConstructorInvocation_NilExplixitConsInv
                                                          
pZExplicitConstructorInvocation = (\ al nw -> sem_ExplicitConstructorInvocation_ExplConsInvThis nw al )  <$ pKeyWord "this" <* pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")" <* pSpecialSimbol ";"
                                                          <|> (\ al nw -> sem_ExplicitConstructorInvocation_ExplConsInvSuper nw al) <$ pKeyWord "super" <* pSpecialSimbol "(" <*> pArgumentList <* pSpecialSimbol ")" <* pSpecialSimbol ";"
                                                          
pCatchClause = sem_CatchClause_CatchClause <$ pKeyWord "catch" <* pSpecialSimbol "(" <*> pVariableModifiers <*> pType <*> pVariableDeclaratorId <* pSpecialSimbol ")" <* pSpecialSimbol "{" <*> pBlockStatements <* pSpecialSimbol "}"  -- pBlock

-- --------------------------------------------------------------------------------------------                  
-- FIN Parser of TypeDeclarations
-- ---------------------------------------------------------------------------------------------
parser nombre = do 
                                   entrada   <- readFile nombre
                                   let sel   = classify entrada (initPos nombre)
                                   resultado <- parseIO pOrmj sel
                                   putStrLn( show resultado)    