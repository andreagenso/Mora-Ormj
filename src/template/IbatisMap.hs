module IbatisMap where
{- Plantilla IbatisMap.aa
        Conjunto de instrucciones a ser generadas en IbatisMap
-}

-- Encabezado de los documentos SqlMap
headIbatisMap :: String
headIbatisMap = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?> \n"
                                ++ "<!DOCTYPE sqlMap"
                                ++ "  PUBLIC \"-//ibatis.apache.org//DTD SQL Map 2.0//EN\""
                                ++ "  \"http://ibatis.apache.org/dtd/sql-map-2.dtd\">"

-- Cuerpo del documento XML                             
sqlMapIbatisMapIni :: String -> String
sqlMapIbatisMapIni namesp = "<sqlMap namespace=\"" ++ namesp ++ "\">"

sqlMapIbatisMapEnd :: String
sqlMapIbatisMapEnd = "</sqlMap>"

-- FORMA EXTENSA :

-- Cache Model
cacheModelIbatisMapIni :: String -> String -> String
cacheModelIbatisMapIni id typ = "<cacheModel id=\"" ++ id ++ "\" type=\"" ++ typ ++ "\">"

cacheModelIbatisMapFlush :: String -> String
cacheModelIbatisMapFlush hour = "<flushInterval hours=\"" ++ hour ++"\"/>"


cacheModelIbatisMapEnd :: String
cacheModelIbatisMapEnd = "</cacheModel>"

-- usado como parte  de CacheModel
propertyIbatisMap :: String -> String -> String
propertyIbatisMap name value = "<property name=\"" ++ name ++ "\" value=\"" ++ value ++ "\" />"

-- Alias en IbatisMap
aliasIbatisMap :: String -> String -> String
aliasIbatisMap alias typ = "<typeAlias alias=\"" ++ alias ++ "\" type=\"" ++ typ ++ "\" />"

-- ParameterMap de IbatisMap
parameterMapIbatisMapIni :: String -> String -> String
parameterMapIbatisMapIni id clas = "<parameterMap id=\"" ++ id ++ "\" class=\"" ++ clas ++ "\">"

parameterMapIbatisMapEnd :: String
parameterMapIbatisMapEnd = "</parameterMap>"

parameterMapIbatisMapParameter :: String -> String
parameterMapIbatisMapParameter prop = "<parameter property=\"" ++ prop ++ "\"/>"

-- Result Map de IbatisMap
resultMapIbatisMapIni :: String -> String -> String
resultMapIbatisMapIni id clas = "<resultMap id=\"" ++ id ++ "\" class=\"" ++ clas ++"\">"

resultMapIbatisMapEnd :: String
resultMapIbatisMapEnd = "</resultMap>"

resultMapIbatisMapResult :: String -> String -> String
resultMapIbatisMapResult prop col = "\t <result property=\"" ++ prop ++ "\" column=\"" ++ col ++ "\"/>"

-- Mostrar declaraciones SQL, basado en el manual, pagina 18
-- estructura de dato, que representa el encabezado de la instruccion Sql.
data InsSqlIni = InsSqlIni SqlIni Attributes

data SqlIni = Select    String
                        | Insert    String
                        | Statement String
                        | Update    String
                        | Delete    String
                        | Procedure String
                        
data Attributes = Attributes Attribute Attributes
                                | NilAttributes
                                
data Attribute = ParameterClass String 
                           | ResultClass    String
                           | ParameterMap   String
                           | ResultMap      String
                           | CacheModel     String
                           | ResultSetType  String
                           | FetchSize      String
                           | XmlResultName  String
                           | RemapResults   String
                           | Timeout        String              

data SqlEnd = SelectE
                        | InsertE
                        | StatementE
                        | UpdateE
                        | DeleteE
                        | ProcedureE


instance Show InsSqlIni where
        show (InsSqlIni sqlIni ats) = show sqlIni ++ " " ++ show ats ++ "> \n"

instance Show SqlIni where
        show ( Select    id ) = "<select id="    ++ show id
        show ( Insert    id ) = "<insert id="    ++ show id
        show ( Statement id ) = "<select id="    ++ show id
        show ( Update    id ) = "<update id="    ++ show id
        show ( Delete    id ) = "<delete id="    ++ show id
        show ( Procedure id ) = "<procedure id=" ++ show id

instance Show Attributes where
        show (Attributes at ats ) = show at ++ " "
        show (NilAttributes     ) = ""

instance Show Attribute where
        show ( ParameterClass value ) = "parameterClass=" ++ "\"" ++ value ++ "\""
        show ( ResultClass    value ) = "resultClass="    ++ "\"" ++ value ++ "\""
        show ( ParameterMap   value ) = "parameterMap="   ++ "\"" ++ value ++ "\""
        show ( ResultMap      value ) = "resultMap="      ++ "\"" ++ value ++ "\""
        show ( CacheModel     value ) = "cacheModel="     ++ "\"" ++ value ++ "\""
        show ( ResultSetType  value ) = "resultSetType="  ++ "\"" ++ value ++ "\""
        show ( FetchSize      value ) = "fetchSize="      ++ "\"" ++ value ++ "\""
        show ( XmlResultName  value ) = "xmlResultName="  ++ "\"" ++ value ++ "\""
        show ( RemapResults   value ) = "remapResults="   ++ "\"" ++ value ++ "\""
        show ( Timeout        value ) = "timeout="        ++ "\"" ++ value ++ "\""

instance Show SqlEnd where
        show SelectE    = "</select>"
        show InsertE    = "</insert>"
        show StatementE = "</statement>"
        show UpdateE    = "</update>"
        show DeleteE    = "</delete>"
        show ProcedureE = "</procedure>"


-- Principio de la declaracion
statementIbatisMapIni :: InsSqlIni -> String
statementIbatisMapIni isi = show isi
-- En el Ag hay que armar esta estructura para que se pueda mostar
ej = statementIbatisMapIni (InsSqlIni (Select "getProduct") (Attributes (ParameterMap "xyz") NilAttributes))

statementIbatisMapEnd :: SqlEnd -> String
statementIbatisMapEnd se = show se

-- El resto de las instrucciones se enfoca netamente en SQL

-- Esto entra en otro modulo que lleva por nombre : SqlMapIbatisMap
-- PENSAR CUAL ES LA MEJOR MANERA DE MOSTRAR UNA CONSULTA SQL