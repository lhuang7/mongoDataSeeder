{-# LANGUAGE BangPatterns,TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, MultiParamTypeClasses, FlexibleInstances ,DeriveGeneric #-}

module MongoDataSeeder where 

import Data.String
import Data.Text
import System.IO
import GHC.Generics
import Data.Typeable
import Data.Data 
import Control.Parallel.Strategies
import Data.Time
import Text.Show
import Data.Yaml
import System.Random
import Data.Bool
import Data.Maybe
import qualified Debug.Trace as Bug
import Prelude --(Ord,($),Show,Eq,(>),(<),(/=),(==), (.),Double,Int,undefined)
import Control.Monad
import Control.Monad.State.Strict 
import Control.Applicative
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.Aeson.Bson as A2B
import qualified Data.Aeson as A

import Database.MongoDB

-- | Configure MongoDB


data MongoDBConfig = MongoDBConfig { 
     mHost ::   Text 
    ,mDatabase ::   Text 
    ,mCollection ::  Text
    ,mDelay      :: Int
    ,mPrint      :: Bool
    ,mInsertProto :: B.ByteString
    }
  deriving (Generic,Show,Eq)

instance FromJSON MongoDBConfig
instance ToJSON MongoDBConfig






{-| The Mongo Data Seeder inserts records based on a very simple query language 
    at set intervals (in milliseconds) using the first argument on the command LINE
    to set up the database connection and the second argument to set up the query

    The value given at the command line is the initial value you want to put in the query 
    You can also choose whether this should be a "constant" never changing value or 
    change according to some very simple rules right now just (Constant).  
    the after each insertion the query is changed to show that it will insert a different value 
    in the query on the next pass according to your predefined rule.   

    input your command line string as "{\"insert\":{\"iName\":<valueName>
                                                   , \"iType\":<valueType> }
                                        ,\"step\":<stepSize>
                                        ,\"start\":<startTime>}
                                        ,\"stop\" :<stopTime>}"

    valueName is just the name given to the value 
    valueType can be {\"constant\":<Double> } or "Random" 
              where <Double> is a constant that will be repeated over and over
    stepSize is in milliseconds
    startTime and StopTime can be "{sType:now}
                                   {sType:{\"date\", sVal:<UTCDateTime>} }
                                   {sType:{\"offset\":<Integer>}}"


EX: 

"{\"step\":23.0
,\"stop\":{\"sType\":{\"Offset\":22000}}
,\"start\":{\"sType\":{\"Now\":[]}}
,\"insert\":{\"iType\":{\"Random\":[]},\"iName\":\"Test\"}}"

example inserts a record

|-}




-- |Fully defined insert Data contains a Mongo DB config 
-- | and an insertDocument 
data FDInsert = FDInsert {getInsert::InsertDoc ,getMcfg::MongoDBConfig}
                deriving (Show, Eq)
type InitialDoc = FDInsert 
type DocState   = FDInsert
type InsertState = (InitialDoc , DocState)

type IOState = IO InsertState

newStateGen :: IOState -> (IOState -> IOState)
newStateGen !m = (\x -> m)


runInserter :: StateT IOState IO DocState
runInserter = do 
  iDocState <- get 
  insertDocs@(iDoc,cDoc) <- liftIO $ iDocState 
  case done insertDocs of 
    True -> return.snd $ insertDocs
    False -> do 
      rslt <- liftIO $ (threadDelay (mDelay.getMcfg $ iDoc))>>maybePrint cDoc>>insertData (snd insertDocs) -- Run the Insert function and return an updated state
      withStateT (newStateGen $ return (iDoc,rslt)) runInserter 


maybePrint d 
    |(mPrint.getMcfg $ d) = print (getInsert d) >> putStr "\n"
    | otherwise = return () 

done :: InsertState -> Bool
done ( _ ,FDInsert cDoc _) = let isDate (Date l) = Just l
                                 isDate _ = Nothing
                             in  (isDate.sType.start $ cDoc) >= (isDate.sType.stop $ cDoc) -- Nice oneline check because of mutation of state

      

insertData :: DocState -> IO DocState
insertData d@(FDInsert iDoc mdbCFG) = do 
  pipe <-runIOE $ connect (host.unpack.mHost $ mdbCFG)
  rnd <- randomRIO (0,100) 
  fI     <- mkInsert d --returns a function that can pass a random number generated from IO 
  e    <- access pipe UnconfirmedWrites (mDatabase mdbCFG) (fI rnd)
  strt  <- asDate.start $ iDoc
  close pipe
  return $ FDInsert (InsertDoc (iId iDoc) (step iDoc) (SeedTime (Date (addUTCTime (realToFrac.step $ iDoc) strt ))) (stop iDoc) ) mdbCFG  


mkInsert (FDInsert iDoc mdbCFG) =  let 
    isDate (Date l) = l
    commonInsert v =  insert (mCollection mdbCFG) ["pid" =: (pid.iId $ iDoc) , "val" =: v, "time" =: (isDate.sType.start $ iDoc)] 
    mkValue :: ValueType -> (Double -> Double)
    mkValue  (Constant x) = (\_ -> x) 
    mkValue  Random = (\x -> x) 
    mkValue  (SineWave a x) = (\t -> a * sin(x * (t)))
    in  return $ commonInsert.(mkValue.iType.iId $ iDoc) 



                                           


asDate (SeedTime (Date n)) = return n 
asDate _ = guard (False) >> getCurrentTime 
  
       

formatIDoc ( InsertDoc { 
               iId = a
             , step = b 
             , start = c
             , stop = d

             } ) = do 
  newC <- (formatSeedType.sType $ c)
  newD <- (formatSeedType.sType $ d)
  return $ InsertDoc a b (SeedTime newC) (SeedTime newD)



-- | formatSeedTime changes the Seed Time into appropriate UTC Dates
formatSeedType :: SeedType -> IO SeedType
formatSeedType Now = getCurrentTime >>= (\t-> return $ Date t) 
formatSeedType (Offset a) = do 
  time <- getCurrentTime
  return $ Date $ addUTCTime (realToFrac a) time 
formatSeedType x = return x
  
           
             
                   
                   
  

returnValidData :: InsertDoc -> IO InsertDoc
returnValidData doc = (guard (step doc >= 15.0)) >> validateStartStop (start doc) (stop doc) >> return doc


  
validateStartStop :: SeedTime -> SeedTime -> IO ()
validateStartStop strt stp = guard (stp /= strt) >> guard ( sType stp /= Now ) >> 
                          invalidStartStop (sType strt) (sType stp) 

invalidStartStop :: SeedType -> SeedType -> IO ()
invalidStartStop (Date strtDate) (Date stpDate) = do 
  guard (strtDate < stpDate)
invalidStartStop (Now) (Date stpDate) = do 
  time <- getCurrentTime 
  guard (time < stpDate)
invalidStartStop (Offset strt ) (Offset stp) = guard (strt < stp) 
invalidStartStop _ _ = return ()




makeValidInsert doc = undefined

data InsertId = InsertId { 
       pid :: Int
      ,iType :: ValueType
}
 deriving (Generic,Show,Eq)



instance ToJSON InsertId
instance FromJSON InsertId

type Amplitude = Double 
type Frequency = Double

data ValueType = Constant !Double | Random | SineWave !Amplitude !Frequency
  deriving (Generic,Show,Eq)

instance ToJSON ValueType
instance FromJSON ValueType

type Seconds = Double

data SeedTime = SeedTime { 
      sType :: SeedType 
}
 deriving (Generic,Show,Ord,Eq)

instance ToJSON SeedTime
instance FromJSON SeedTime

data SeedType = Now | Date !UTCTime| Offset !Double
 deriving (Generic,Show,Eq,Ord)

instance ToJSON SeedType 
instance FromJSON SeedType

data InsertDoc =InsertDoc { 
      iId    :: InsertId
     ,step   :: Seconds
     ,start  :: SeedTime
     ,stop   :: SeedTime
    }
 deriving (Generic,Show,Eq)



instance ToJSON InsertDoc
instance FromJSON InsertDoc
