module Mora.Ormj.Ast.Semantic where

import Mora.Ormj.Ast.Sintax

-- tomar en cuenta comentarios
sem_Ormj_Ormj = Ormj
-- sem_Ormj_NilOrmj = NilOrmj -- no se considera el caso Nil
                  
-- definicion completa
sem_PackageDeclaration_PackageDeclaration = PackageDeclaration
sem_PackageDeclaration_NilPackageDeclaration =  NilPackageDeclaration
                         
-- -------------------------------------------------------------------
-- definicion de Annotations
-- ------------------------------------------------------------------
sem_Annotation_Annotation = Annotation -- TypeName TypeAnnotation
sem_TypeAnnotation_MarkerAnnotation        = MarkerAnnotation
sem_TypeAnnotation_NormalAnnotation        = NormalAnnotation  -- ElementValuePairs
sem_TypeAnnotation_SingleElementAnnotation = SingleElementAnnotation  -- ElementValue
                                 
sem_TypeName_TypeName    = TypeName
sem_TypeName_NilTypeName = NilTypeName
                           
sem_ElementValuePair_ElementValuePair = ElementValuePair

sem_ElementValue_ElementValueConditional        = ElementValueConditional
sem_ElementValue_ElementValueAnnotation         = ElementValueAnnotation
sem_ElementValue_ElementValueEVArrayInitializer = ElementValueEVArrayInitializer
                                                                                           
sem_ConditionalExpression_ConditionalExpr = ConditionalExpr -- ConditionalOrExpression
sem_ConditionalExpression_ConditionalExprComb = ConditionalExprComb -- ConditionalOrExpression Expression ConditionalExpression
                                                        
sem_ConditionalOrExpression_Or                        = (:||:)
sem_ConditionalOrExpression_And                       = (:&&:)
sem_ConditionalOrExpression_OrEx                      = (:|:)
sem_ConditionalOrExpression_AndEx                     = (:^:)
sem_ConditionalOrExpression_AndIc                     = (:&:)
sem_ConditionalOrExpression_Eq                        = (:==:)
sem_ConditionalOrExpression_Dist                      = (:!=:)
-- sem_ConditionalOrExpression_Men                                                = (:<:) -- Cambio desde Aqui --
-- sem_ConditionalOrExpression_ConditionalRelationalExpression = ConditionalRelationalExpression

sem_ConditionalOrExpression_Men  = (:<:)
sem_ConditionalOrExpression_May  = (:>:)
sem_ConditionalOrExpression_MenQ = (:<=:)
sem_ConditionalOrExpression_MayQ = (:>=:)
sem_ConditionalOrExpression_Izq  = (:<<:)
sem_ConditionalOrExpression_Der  = (:>>:)
sem_ConditionalOrExpression_DDer = (:>>>:)
sem_ConditionalOrExpression_Add  = (:+:)
sem_ConditionalOrExpression_Res  = (:-:)
sem_ConditionalOrExpression_Mul  = (:*:)
sem_ConditionalOrExpression_Div  = (:/:)
sem_ConditionalOrExpression_Mod  = (:%:)

sem_ConditionalOrExpression_ConditionalOrExpressionUnaryExpression = ConditionalOrExpressionUnaryExpression
sem_ConditionalOrExpression_ConditionalOrExpressionIntanceOf = ConditionalOrExpressionIntanceOf

sem_UnaryExpression_UnaryExpressionPreIncrementExpression = UnaryExpressionPreIncrementExpression
sem_UnaryExpression_UnaryExpressionPreDecrementExpression = UnaryExpressionPreDecrementExpression
sem_UnaryExpression_UnExpMas                                              = UnExpMas
sem_UnaryExpression_UnExpMenos                                                = UnExpMenos
-- sem_UnaryExpression_UnNotPlus                             = UnNotPlus
sem_UnaryExpression_Pestan                                = Pestan
sem_UnaryExpression_Admiracion                            = Admiracion
sem_UnaryExpression_UnNotPlusCastExpression               = UnNotPlusCastExpression
sem_UnaryExpression_PostExpPrimaryPostfixZ                = PostExpPrimaryPostfixZ
sem_UnaryExpression_PostfixExpressionPrimary              = PostfixExpressionPrimary


sem_PostfixExpression_PostExpPrimaryPostfixZ = PostExpPrimaryPostfixZ                                                                                                                                                                                                                                                                             
-- sem_PostfixExpression_PostExpNamePostfixZ     = PostExpNamePostfixZ
sem_PostfixExpression_PostfixExpressionPrimary = PostfixExpressionPrimary
-- sem_PostfixExpression_PostfixExpressionExpressionName = PostfixExpressionExpressionName

sem_Primary_PrimNoNewArray               = PrimNoNewArray
-- sem_Primary_PrimArrayCreationExpression  = PrimArrayCreationExpression
sem_Primary_PrimNoNewArrayZ              = PrimNoNewArrayZ
-- sem_Primary_PrimArrayCreationExpressionZ = PrimArrayCreationExpressionZ

sem_PrimaryNoNewArray_PrimaryNoNewArray     = PrimaryNoNewArray

{-- 28
sem_Literal_IntegerLiteral_DecimalIntegerLiteral                 = IntegerLiteral_ecimalIntegerLiteral
sem_Literal_IntegerLiteral_HexIntegerLiteral                     = IntegerLiteral_HexIntegerLiteral
sem_Literal_IntegerLiteral_OctalIntegerLiteral                   = IntegerLiteral_OctalIntegerLiteral
sem_Literal_FloatingPointLiteral_DecimalFloatingPointLiteral     = FloatingPointLiteral_DecimalFloatingPointLiteral
sem_Literal_FloatingPointLiteral_HexadecimalFloatingPointLiteral = FloatingPointLiteral_HexadecimalFloatingPointLiteral
sem_Literal_BooleanLiteral                                       = BooleanLiteral
sem_Literal_CharacterLiteral                                     = CharacterLiteral
sem_Literal_StringLiteral                                        = StringLiteral
sem_Literal_NullLiteral                                          = NullLiteral
-}


                                                
-- sem_ReferenceType_ReferenceTypeT = ReferenceTypeT
sem_ReferenceType_ReferenceTypeClassOrInterfaceType = ReferenceTypeClassOrInterfaceType

                                          
sem_TypeArguments_TypeArgumentsC1 = TypeArgumentsC1
sem_TypeArguments_TypeArgumentsC2 = TypeArgumentsC2
sem_TypeArguments_TypeArgumentsC3 = TypeArgumentsC3
sem_TypeArguments_TypeArgumentsC0 = TypeArgumentsC0
sem_TypeArguments_NilTypeArguments = NilTypeArguments
                                        
sem_ActualTypeArgument_ActualTypeArgumentWildCard = ActualTypeArgumentWildCard
sem_ActualTypeArgument_ActualTypeReferenceType    = ActualTypeReferenceType
                                                
-- sem_WildCard_WildCard = WildCard

{- sem_WildcardBounds_WilcardBoundsExtends           = WilcardBoundsExtends
sem_WildcardBounds_WilcardBoundsSuper             = WilcardBoundsSuper
sem_WildcardBounds_WilcardBoundsExtendsArrayType  = WilcardBoundsExtendsArrayType
sem_WildcardBounds_WilcardBoundsSuperArrayType    = WilcardBoundsSuperArrayType
sem_WildcardBounds_NilwildcardBounds                      = NilwildcardBounds 
-}
sem_WildcardBounds_WilcardBoundsExtendsReferenceType = WilcardBoundsExtendsReferenceType
sem_WildcardBounds_WilcardBoundsSuperReferenceType   = WilcardBoundsSuperReferenceType
sem_WildcardBounds_NilwildcardBounds                 = NilwildcardBounds                                        


sem_Expression_ExpressionConditionalExpr     = ExpressionConditionalExpr 
sem_Expression_ExpressionConditionalExprComb = ExpressionConditionalExprComb 
sem_Expression_ExpressionAssignment                      = ExpressionAssignment 

                                
-- sem_Assignment_Assignment = Assignment --  LeftHandSide AssignmentOperator AssignmentExpression
                                
sem_AssignmentOperator_AssignmentOp = AssignmentOp
sem_AssignmentOperator_AssignmentPlus = AssignmentPlus -- *=
sem_AssignmentOperator_AssignmentDiv = AssignmentDiv -- /=
sem_AssignmentOperator_AssignmentMod = AssignmentMod -- %=
sem_AssignmentOperator_AssignmentAdd = AssignmentAdd -- +=
sem_AssignmentOperator_AssignmentMin = AssignmentMin -- -=
sem_AssignmentOperator_AssignmentMinShifShift = AssignmentMinShifShift  -- <<=
sem_AssignmentOperator_AssignmentMayShitfShift = AssignmentMayShitfShift -- >>=
sem_AssignmentOperator_AssignmentMayShiftShiftShift = AssignmentMayShiftShiftShift -- >>>=
sem_AssignmentOperator_AssignmentAndSingle = AssignmentAndSingle -- &=
sem_AssignmentOperator_AssignmentCincun = AssignmentCincun -- ^=
sem_AssignmentOperator_AssignmentOrSingle = AssignmentOrSingle -- |=  
                                                                                
-- sem_LeftHandSide_LeftHandSideExpName               = LeftHandSideExpName
-- sem_LeftHandSide_LeftHandSideFieldAccess           = LeftHandSideFieldAccess
-- sem_LeftHandSide_LeftHandSidePrimary    = LeftHandSidePrimary
-- sem_LeftHandSide_LeftHandSideArrayAccessExpName         = LeftHandSideArrayAccessExpName
-- sem_LeftHandSide_LeftHandSideArrayAccessPrimaryNNA = LeftHandSideArrayAccessPrimaryNNA

sem_FieldAccess_FieldAccessPrim = FieldAccessPrim
sem_FieldAccess_FieldAccessSuper = FieldAccessSuper
sem_FieldAccess_FieldAccessClassName = FieldAccessClassName
                                 
sem_ArrayAccess_ArrayAccessExpName = ArrayAccessExpName
sem_ArrayAccess_ArrayAccessPrimNNA = ArrayAccessPrimNNA
                                  
-- sem_MethodInvocation_MethodInvocationMN = MethodInvocationMN
-- sem_MethodInvocation_MethodInvocationSuper = MethodInvocationSuper
-- sem_MethodInvocation_MethodInvocationClassN = MethodInvocationClassN
-- sem_MethodInvocation_MethodInvocationTypeN = MethodInvocationTypeN
                                
sem_NonWildTypeArguments_NonWildTypeArgumentsC1 = NonWildTypeArgumentsC1
sem_NonWildTypeArguments_NonWildTypeArgumentsC2 = NonWildTypeArgumentsC2
sem_NonWildTypeArguments_NonWildTypeArgumentsC3 = NonWildTypeArgumentsC3
sem_NonWildTypeArguments_NonWildTypeArgumentsC0 = NonWildTypeArgumentsC0
sem_NonWildTypeArguments_NilNonWildTypeArguments = NilNonWildTypeArguments
                                                  
sem_ArrayCreationExpression_ArrayCreationExpressionPrimitiveType   = ArrayCreationExpressionPrimitiveType
sem_ArrayCreationExpression_ArrayCreationExpressionClassOrInterf   = ArrayCreationExpressionClassOrInterf
sem_ArrayCreationExpression_ArrayCreationExpressionArrInitialPrim  = ArrayCreationExpressionArrInitialPrim
sem_ArrayCreationExpression_ArrayCreationExpressionArrInitialClass = ArrayCreationExpressionArrInitialClass
                                                                          
sem_ArrayInitializer_ArrayInitializer = ArrayInitializer
                                                  
sem_VariableInitializer_VariableInitializerExp = VariableInitializerExp
sem_VariableInitializer_VariableInitializerArr = VariableInitializerArr
                                        
sem_ElementValueArrayInitializer_ElementValueArrayInitializer = ElementValueArrayInitializer

-- -------------------------------------------------------------------------------------------
-- FIN Definicion de Annotations 
-- -------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------
-- Definicion de ImportDeclarations
-- -------------------------------------------------------------------------------------------
sem_ImportDeclarations_ImportDeclarations = ImportDeclarations
sem_ImportDeclarations_NilImportDeclarations = NilImportDeclarations

sem_ImportDeclaration_SingleTypeImportDeclaration = SingleTypeImportDeclaration
sem_ImportDeclaration_TypeImportOnDemandDeclaration = TypeImportOnDemandDeclaration
sem_ImportDeclaration_SingleStaticImportDeclaration = SingleStaticImportDeclaration
sem_ImportDeclaration_StaticImportOnDemandDeclaration = StaticImportOnDemandDeclaration
                                           
sem_PackageOrTypeName_NilPackageOrTypeName = NilPackageOrTypeName
sem_PackageOrTypeName_PackageOrTypeName    = PackageOrTypeName
-- -------------------------------------------------------------------------------------------
-- FIN Definicion de ImportDeclarations
-- -------------------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------------------
-- Definicion de TypeDeclarations
-- ------------------------------------------------------------------------------------------
sem_Modifiers_Modifiers     = Modifiers
sem_Modifiers_NilModifiers  = NilModifiers
                                        
sem_Modifier_ModifierAnnotation     = ModifierAnnotation
sem_Modifier_ModifierPublic         = ModifierPublic
sem_Modifier_ModifierProtected      = ModifierProtected
sem_Modifier_ModifierPrivate        = ModifierPrivate
sem_Modifier_ModifierAbstract       = ModifierAbstract
sem_Modifier_ModifiersStatic        = ModifiersStatic
sem_Modifier_ModifierFinal          = ModifierFinal
sem_Modifier_ModifierStrictfp       = ModifierStrictfp
sem_Modifier_FieldModifierTransient = FieldModifierTransient
sem_Modifier_FieldModifierVolatile  = FieldModifierVolatile
sem_Modifier_MethodModifierSynchronized = MethodModifierSynchronized
sem_Modifier_MethodModifierNative = MethodModifierNative
                           
sem_TypeParameters_TypeParametersC1 = TypeParametersC1
sem_TypeParameters_TypeParametersC2 = TypeParametersC2
sem_TypeParameters_TypeParametersC3 = TypeParametersC3
sem_TypeParameters_TypeParametersC0 = TypeParametersC0
sem_TypeParameters_NilTypeParameters =NilTypeParameters

sem_TypeParameter_TypeParameterBound = TypeParameterBound
sem_TypeParameter_TypeParameter      = TypeParameter
                                      
sem_TypeBound_TypeBound = TypeBound
sem_TypeBound_NilAdditionalBoundList = NilAdditionalBoundList

sem_Super_Super = Super
sem_Super_NilSuper = NilSuper
                        
sem_Interfaces_Interfaces = Interfaces
sem_Interfaces_NilInterfaces = NilInterfaces
                                        
                                                                   
sem_VariableDeclarator_VariableDeclaratorId     = VariableDeclaratorId
sem_VariableDeclarator_VariableDeclaratorIdAsig = VariableDeclaratorIdAsig
                                                                        
sem_VariableDeclaratorId_VarDeclaratorId  = VarDeclaratorId
sem_VariableDeclaratorId_VarDeclaratorIdVDZ = VarDeclaratorIdVDZ
sem_VariableDeclatatorIdZ_VarDeclaratorIdCorchete = VarDeclaratorIdCorchete
sem_VariableDeclatatorIdZ_VarDeclaratorIdZ        = VarDeclaratorIdZ

                                                                                
sem_ResultType_ResultTypeType = ResultTypeType
sem_ResultType_ResultTypeVoid = ResultTypeVoid
                                
sem_MethodDeclarator_MethodDeclaratorFormalPL = MethodDeclaratorFormalPL
sem_MethodDeclarator_MethodDeclaratorSingle   = MethodDeclaratorSingle
                                          
sem_FormalParameterList_FormalParameterListLast    = FormalParameterListLast
sem_FormalParameterList_FormalParameterListFormal = FormalParameterListFormal
sem_FormalParameterList_FormalParameterListNil = FormalParameterListNil
                                                                                                 
sem_VariableModifiers_VariableModifiers    = VariableModifiers
sem_VariableModifiers_NilVariableModifiers = NilVariableModifiers
                                           
sem_VariableModifier_VariableModifierFinal      = VariableModifierFinal
sem_VariableModifier_VariableModifierAnnotation = VariableModifierAnnotation                                      
                                                                                          
sem_Throws_Throws = Throws
sem_Throws_NilThrows = NilThrows
                        
sem_ExceptionType_ExceptionTypeClassType = ExceptionTypeClassType
sem_ExceptionType_ExceptionTypeTypeVariable = ExceptionTypeTypeVariable                            
                                   
sem_MethodBody_MethodBodyBlock     = MethodBodyBlock
sem_MethodBody_MethodBodySemiColon = MethodBodySemiColon 
                                                    
sem_BlockStatement_BlockStatementLocalVariableDeclarationStatement    = BlockStatementLocalVariableDeclarationStatement
sem_BlockStatement_BlockStatementStatement = BlockStatementStatement
sem_BlockStatement_BlockStatementClassDeclarationNormalClassDeclaration = BlockStatementClassDeclarationNormalClassDeclaration
sem_BlockStatement_BlockStatementClassDeclarationEnumDeclaration = BlockStatementClassDeclarationEnumDeclaration
                                                          
sem_Statement_StatementLabeled = StatementLabeled
sem_Statement_StatementIf          = StatementIf
sem_Statement_StatementIfElse  = StatementIfElse
sem_Statement_StatementWhile   = StatementWhile
sem_Statement_StatementFor         = StatementFor
                           
sem_Statement_SWTSBlock                            = SWTSBlock
sem_Statement_SWTSEmptyStatement                   = SWTSEmptyStatement
sem_Statement_SWTSExpressionStatement              = SWTSExpressionStatement
sem_Statement_SWTSAssertStatementCondEx            = SWTSAssertStatementCondEx
sem_Statement_SWTSAssertStatementCond              = SWTSAssertStatementCond
sem_Statement_SWTSSwitchStatement                      = SWTSSwitchStatement
sem_Statement_SWTSDoStatement                          = SWTSDoStatement
sem_Statement_SWTSBreakStatement                           = SWTSBreakStatement
sem_Statement_SWTSBreakStatementId                     = SWTSBreakStatementId
sem_Statement_SWTSNilContinueStatement             = SWTSNilContinueStatement
sem_Statement_SWTSContinueStatement                = SWTSContinueStatement
sem_Statement_SWTSReturnStatement                  = SWTSReturnStatement
sem_Statement_SWTSNilReturnStatement               = SWTSNilReturnStatement
sem_Statement_SWTSynchronizedStatement             = SWTSynchronizedStatement
sem_Statement_SWTThrowStatement                    =    SWTTrhowStatement
sem_Statement_SWTTryStatement                      = SWTTryStatement
sem_Statement_SWTTryStatementFinally               = SWTTryStatementFinally

sem_SwitchBlock_SwitchBlockAll    = SwitchBlockAll
sem_SwitchBlock_SwitchBlockLabels = SwitchBlockLabels
sem_SwitchBlock_NilSwitchBlock    =     NilSwitchBlock 
sem_SwitchBlock_SwitchBlockGroups = SwitchBlockGroups
                                                                                                
sem_SwitchBlockStatementGroup_SwitchBlockStatementGroup = SwitchBlockStatementGroup                                                             
                                                                
sem_SwitchLabel_SwitchLabelConstant = SwitchLabelConstant
sem_SwitchLabel_SwitchLabelEnum         = SwitchLabelEnum
sem_SwitchLabel_SwitchLabelDefault      = SwitchLabelDefault
                                          
sem_ForInit_ForInitStaExp   = ForInitStaExp
sem_ForInit_ForInitLocalVar = ForInitLocalVar
sem_ForInit_NilForInit      = NilForInit
                         
sem_StatementExpressionList_StatementExpressionList    = StatementExpressionList
sem_StatementExpressionList_NilStatementExpressionList = NilStatementExpressionList
                                                          
sem_ForUpdate_ForUpdate    = ForUpdate
sem_ForUpdate_NilForUpdate = NilForUpdate

sem_ForStatement_ForStatementBasicForStatementAll           = ForStatementBasicForStatementAll 
sem_ForStatement_ForStatementBasicForStatementNoExp         = ForStatementBasicForStatementNoExp 
sem_ForStatement_ForStatementEnhancedForStatement           = ForStatementEnhancedForStatement  
sem_ForStatement_ForStatementEnhancedForStatementNoVarModif = ForStatementEnhancedForStatementNoVarModif 

sem_ExtendsInterfaces_ExtendsInterfaceType = ExtendsInterfaceType
sem_ExtendsInterfaces_NilExtendsInterfaces  = NilExtendsInterfaces

sem_InterfaceMemberDeclaration_InterfaceMemberDeclarationConstant                                     = InterfaceMemberDeclarationConstant 
sem_InterfaceMemberDeclaration_InterfaceMemberDeclarationAbstract                                     = InterfaceMemberDeclarationAbstract
sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationClassDeclarationNormalCD             = InterfaceMemberDeclTypeDeclarationClassDeclarationNormalCD
sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationClassDeclarationEnumD                = InterfaceMemberDeclTypeDeclarationClassDeclarationEnumD 
sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationInterfaceDeclarationNormalInterfaceD = InterfaceMemberDeclTypeDeclarationInterfaceDeclarationNormalInterfaceD 
sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationInterfaceDeclarationAnnotationTypeD  = InterfaceMemberDeclTypeDeclarationInterfaceDeclarationAnnotationTypeD  
sem_InterfaceMemberDeclaration_InterfaceMemberDeclTypeDeclarationSemiColon                                                        = InterfaceMemberDeclTypeDeclarationSemiColon

sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationClassDeclarationNormalCD             = AnnTypeElemDeclTypeDeclarationClassDeclarationNormalCD
sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationClassDeclarationEnumD                = AnnTypeElemDeclTypeDeclarationClassDeclarationEnumD
sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationInterfaceDeclarationNormalInterfaceD = AnnTypeElemDeclTypeDeclarationInterfaceDeclarationNormalInterfaceD
sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationInterfaceDeclarationAnnotationTypeD  = AnnTypeElemDeclTypeDeclarationInterfaceDeclarationAnnotationTypeD
sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclTypeDeclarationSemiColon                            = AnnTypeElemDeclTypeDeclarationSemiColon
sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclAbstract                                            = AnnTypeElemDeclAbstract
sem_AnnotationTypeElementDeclaration_AnnTypeElemDeclConstant                                            = AnnTypeElemDeclConstant
                                       
sem_DefaultValue_DefaultValue = DefaultValue
sem_DefaultValue_NilDefaultValue = NilDefaultValue

sem_EnumDeclaration_NilEnumBodyDeclarations = NilEnumBodyDeclarations

sem_EnumBody_EnumBody              = EnumBody

sem_EnumConstants_EnumConstants    = EnumConstants
sem_EnumConstants_NilEnumConstants = NilEnumConstants
                   
sem_EnumConstant_EnumConstantAll = EnumConstantAll -- Annotations Identifier ArgumentList ClassBody  -- Arguments por ArgumentList y ()
sem_EnumConstant_EnumConstantArgL = EnumConstantArgL -- Annotations Identifier ArgumentList 
sem_EnumConstant_EnumConstantClasB = EnumConstantClasB -- Annotations Identifier ClassBody
sem_EnumConstant_EnumConstantNothing = EnumConstantNothing -- Annotations Identifier

sem_EnumBodyDeclarations_EnumBodyDeclarations = EnumBodyDeclarations

sem_ConstructorBody_ConstructorBody = ConstructorBody

sem_ExplicitConstructorInvocation_ExplConsInvThis    = ExplConsInvThis
sem_ExplicitConstructorInvocation_ExplConsInvSuper   = ExplConsInvSuper
sem_ExplicitConstructorInvocation_ExplConsInvPrimary = ExplConsInvPrimary
sem_ExplicitConstructorInvocation_NilExplixitConsInv = NilExplixitConsInv

sem_CatchClause_CatchClause = CatchClause
-- -------------------------------------------------------------------------------------------
-- Fin de TypeDeclarations
-- -------------------------------------------------------------------------------------------