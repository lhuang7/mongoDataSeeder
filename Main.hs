{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, MultiParamTypeClasses, FlexibleInstances ,DeriveDataTypeable #-}

module Main where 
import Data.Text
import Data.Maybe
import Prelude
import MongoDataSeeder 
import Data.Yaml
import Control.Parallel.Strategies
import qualified Debug.Trace as Bug
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy as BL
import Control.Monad.State.Strict
import Control.Applicative
import System.Environment
import Control.Concurrent.Spawn
import qualified Data.List as L



testInsertId = InsertId 343 Random

testSinInsertId = InsertId 343 (SineWave 3.0 3.7)

testStep = 23000
testStart = SeedTime Now
testStop  = SeedTime (Offset 22000)
testInsertDoc = InsertDoc testSinInsertId testStep testStart testStop

javaProto = "[{\"step\":23.0,\"stop\":{\"sType\":{\"Offset\":22000}},\"start\":{\"sType\":{\"Now\":[]}},\"iId\":{\"iType\":{\"Random\":[]},\"pid\":343}}]"

testMongoDoc =  MongoDBConfig "localHost" "onping_production" "onping_tag_history" 33 True javaProto

main :: IO () 
main = do 
  args <- getArgs
  rslt <- decodeFile.L.head $ args:: IO (Maybe MongoDBConfig)
  docLst <- return.fromMaybe [] $ (rslt >>= \x -> let t1 = ( mInsertProto $ x) 
                                                      t2 = parseInsertParameters
                                                  in Bug.traceShow t1 (t2 t1))
  validDocList <- sequence $ returnValidData <$> docLst
  fmtDocList   <- sequence $ formatIDoc <$>  validDocList 
  print fmtDocList     
  tell   <- ( parRunStateIO rslt fmtDocList) >>=(\x -> return x)
  putStr ("Done: \n" )
-- stateFnl <- sequence $ (\x -> runStateT runInserter (return x)).(\x ->
-- ((FDInsert x (fromJust rslt)),(FDInsert x (fromJust rslt)))) <$>  fmtDocList :: IO [(DocState,IOState)]
-- stateFnl <- sequence $ runStateIO rslt <$> fmtDocList

-- fnl <- sequence $ (\(a,b) -> b ) <$> stateFnl :: IO [InsertState]

--  encodeFile "mongoDB.yml" testMongoDoc
--  print $ fnl
--  print $ A.encode testInsertDoc s

parRunStateIO rslt pDoc =  parMapIO (runStateIO rslt ) pDoc


                                                           
runStateIO  :: Maybe MongoDBConfig -> InsertDoc -> IO (MongoDataSeeder.DocState, MongoDataSeeder.IOState)
runStateIO rslt = (\x -> runStateT runInserter (return x)).(\x -> ((FDInsert x (fromJust rslt)),(FDInsert x (fromJust rslt)))) 


parseInsertParameters :: U.ByteString -> Maybe [InsertDoc]
parseInsertParameters strn = A.decode.BL.fromStrict $ strn
        
eitherParseInsertParameters :: U.ByteString -> Either String [InsertDoc]
eitherParseInsertParameters = A.eitherDecode.BL.fromStrict

{-| Example YML File

--------------------------------------------------

mDatabase: onping_production
mHost: localHost
mDelay: 1000000
mPrint: True
mCollection: onping_tag_history
mInsertProto: ! '[{"step":15.0,"stop":{"sType":{"Offset":100}},"start":{"sType":{"Now":[]}},"iId":{"iType":{"Random":[]},"pid":343}}
                 ,{"step":15.0,"stop":{"sType":{"Offset":100}},"start":{"sType":{"Now":[]}},"iId":{"iType":{"Random":[]},"pid":344}}  
		 ,{"step":15.0,"stop":{"sType":{"Offset":100}},"start":{"sType":{"Now":[]}},"iId":{"iType":{"Random":[]},"pid":345}}	      	 
		 ,{"step":15.0,"stop":{"sType":{"Offset":200}},"start":{"sType":{"Offset":101}},"iId":{"iType":{"Constant":1},"pid":345}}
		 ]'

--------------------------------------------------
Notice that you can use the offsets to create Event signatures like a value starting after a certain amount of time.  
In addtion to "Now" and "Offset" you can specify specific date Ranges with "Date" 



  
--}






--  rslt    <- return $ decode.BL.fromStrict.U.fromString.L.head $ args :: IO (Maybe MongoDBConfig)
  



         
