module IbatisSettings where

{- Plantilla IbatisSettings.
        Conjunto de instrucciones a ser generadas en IbatisSettings -}

-- NOTA : revisar que este encabezado del Manual,sea el mismo del de los ejemplos.
headIbatisSet :: String
headIbatisSet = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" ++ "\n"  ++ "<!DOCTYPE sqlMapConfig"
                                ++ "  PUBLIC \"-//ibatis.apache.org//DTD SQL Map Config 2.0//EN\""
                                ++ " \"http://ibatis.apache.org/dtd/sql-map-config-2.dtd\">"
                                
-- Configuracion de SqlMapConfig.
sqlMapConfigIni :: String
sqlMapConfigIni = "<sqlMapConfig>"

sqlMapConfigEnd :: String
sqlMapConfigEnd = "</sqlMapConfig>"

-- Comentarios
-- Opcional
commentIbatisSet :: String -> String
commentIbatisSet c = "<!-- " ++ c ++ " -->"

-- Properties del SqlMap
propertiesIbatisSet :: String -> String
propertiesIbatisSet p = "<properties resource=\"" ++ p ++ "\" />"

-- Settings del SqlMap
-- NOTA, revisar si True  es igual a true en Ibatis.
-- Opcional
-- settingsIbatisSet :: Bool -> Bool -> Bool -> Int -> Int -> Int -> Bool -> Int -> String
settingsIbatisSet :: String -> String -> String -> String -> String -> String -> String -> String -> String
settingsIbatisSet "" "" "" "" "" "" "" dst = "<settings"
                                                                                                ++ "\n defaultStatementTimeout=\"" ++ show dst ++ "\""
                                                                                                ++ "\n />"

settingsIbatisSet "" "" "" "" "" "" usn dst = "<settings"
                                                                                                ++ "\n useStatementNamespaces=\""  ++ show usn ++ "\""
                                                                                                ++ "\n defaultStatementTimeout=\"" ++ show dst ++ "\""
                                                                                                ++ "\n />"

settingsIbatisSet "" "" "" "" "" mt usn dst = "<settings"
                                                                                                ++ "\n maxTransactions=\""         ++ show mt  ++ "\""
                                                                                                ++ "\n useStatementNamespaces=\""  ++ show usn ++ "\""
                                                                                                ++ "\n defaultStatementTimeout=\"" ++ show dst ++ "\""
                                                                                                ++ "\n />"

settingsIbatisSet "" "" "" "" ms mt usn dst = "<settings"
                                                                                                ++ "\n maxSessions=\""             ++ show ms  ++ "\""
                                                                                                ++ "\n maxTransactions=\""         ++ show mt  ++ "\""
                                                                                                ++ "\n useStatementNamespaces=\""  ++ show usn ++ "\""
                                                                                                ++ "\n defaultStatementTimeout=\"" ++ show dst ++ "\""
                                                                                                ++ "\n />"

settingsIbatisSet "" "" "" mr ms mt usn dst = "<settings"
                                                                                                ++ "\n maxRequests=\""             ++ show mr  ++ "\""
                                                                                                ++ "\n maxSessions=\""             ++ show ms  ++ "\""
                                                                                                ++ "\n maxTransactions=\""         ++ show mt  ++ "\""
                                                                                                ++ "\n useStatementNamespaces=\""  ++ show usn ++ "\""
                                                                                                ++ "\n defaultStatementTimeout=\"" ++ show dst ++ "\""
                                                                                                ++ "\n />"

settingsIbatisSet "" "" lle mr ms mt usn dst = "<settings"
                                                                                                ++ "\n lazyLoadingEnabled=\""      ++ show lle ++ "\""
                                                                                                ++ "\n maxRequests=\""             ++ show mr  ++ "\""
                                                                                                ++ "\n maxSessions=\""             ++ show ms  ++ "\""
                                                                                                ++ "\n maxTransactions=\""         ++ show mt  ++ "\""
                                                                                                ++ "\n useStatementNamespaces=\""  ++ show usn ++ "\""
                                                                                                ++ "\n defaultStatementTimeout=\"" ++ show dst ++ "\""
                                                                                                ++ "\n />"

settingsIbatisSet "" ee lle mr ms mt usn dst = "<settings"
                                                                                                ++ "\n enhancementEnabled=\""      ++ show ee  ++ "\""
                                                                                                ++ "\n lazyLoadingEnabled=\""      ++ show lle ++ "\""
                                                                                                ++ "\n maxRequests=\""             ++ show mr  ++ "\""
                                                                                                ++ "\n maxSessions=\""             ++ show ms  ++ "\""
                                                                                                ++ "\n maxTransactions=\""         ++ show mt  ++ "\""
                                                                                                ++ "\n useStatementNamespaces=\""  ++ show usn ++ "\""
                                                                                                ++ "\n defaultStatementTimeout=\"" ++ show dst ++ "\""
                                                                                                ++ "\n />"

settingsIbatisSet ce ee lle mr ms mt usn dst = "<settings"
                                                                                                ++ "\n cacheModelsEnabled=\""      ++ show ce  ++ "\""
                                                                                                ++ "\n enhancementEnabled=\""      ++ show ee  ++ "\""
                                                                                                ++ "\n lazyLoadingEnabled=\""      ++ show lle ++ "\""
                                                                                                ++ "\n maxRequests=\""             ++ show mr  ++ "\""
                                                                                                ++ "\n maxSessions=\""             ++ show ms  ++ "\""
                                                                                                ++ "\n maxTransactions=\""         ++ show mt  ++ "\""
                                                                                                ++ "\n useStatementNamespaces=\""  ++ show usn ++ "\""
                                                                                                ++ "\n defaultStatementTimeout=\"" ++ show dst ++ "\""
                                                                                                ++ "\n />"

-- Property, usado dentro de otras instrucciones
propertyIbatisSet :: String -> String -> String
propertyIbatisSet name value = "<property name=\"" ++ name ++ "\" value=\"" ++ value ++ "\"/>"

-- Result Object Factory del SqlMap
-- Opcional
resultObjectFactoryIbatisSetIni :: String -> String
resultObjectFactoryIbatisSetIni typ = "<resultObjectFactory type=\"" ++ typ ++ "\" >"


resultObjectFactoryIbatisSetEnd :: String
resultObjectFactoryIbatisSetEnd = "</resultObjectFactory>"

-- Alias
-- Opcional
-- Existen alias predefinidos para : Transaction Manager :
-- JDBC     - com.ibatis.sqlmap.engine.transaction.jdbc.JdbcTransactionConfig
-- JTA      - com.ibatis.sqlmap.engine.transaction.jta.JtaTransactionConfig
-- EXTERNAL - com.ibatis.sqlmap.engine.transaction.external.ExternalTransactionConfig
-- DataSource :
-- SIMPLE   - com.ibatis.sqlmap.engine.datasource.SimpleDataSourceFactory
-- DBCP     - com.ibatis.sqlmap.engine.datasource.DbcpDataSourceFactory
-- JNDI     - com.ibatis.sqlmap.engine.datasource.JndiDataSourceFactory
typeAliasIbatisSet :: String -> String -> String
typeAliasIbatisSet alias typ = "<typeAlias alias=\"" ++ alias ++ "\" type=\"" ++ typ ++ "\"/>"

-- Configuracion del DataSource usando el SimpleDataSource 
-- JDBC (SIMPLE, DBCP, JNDI)
-- JTA (JNDI)
-- EXTERNAL
transactionManagerIbatisSetIni :: String -> String
transactionManagerIbatisSetIni typ = "<transactionManager type=\"" ++ typ ++"\" >"

transactionManagerIbatisSetEnd :: String
transactionManagerIbatisSetEnd = "</transactionManager>"

-- DataSource, parte del TransactionManager
dataSourceIbatisSetIni :: String -> String
dataSourceIbatisSetIni typ = "<dataSource type=\"" ++ typ ++ "\">"

-- SqlMap, para incluir un Sql Map en la configuracion
sqlMapIbatisSet :: String -> String
sqlMapIbatisSet resource = "<sqlMap resource=\"" ++ resource ++ "\" />"