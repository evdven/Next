-- Next Copyright AFAS Software BV - 2017

module NextKpi where

import qualified Data.List as List
import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock
import Data.Maybe

import Next

-- Stock
data Stock = Stock {entity:: EntityInstance, stockAmount:: Integer} deriving (Show)

-- physical stock
physicalStockAtT:: [EventInstance] -> Date -> [Stock]
physicalStockAtT es t = deduceStock (filter (beforeClosed t getActualEndDate) es)

-- economic stock
economicStockAtT:: [EventInstance] -> Date -> [Stock]
economicStockAtT es t = deduceStock (List.nub ((filter (beforeClosed t getPlannedTodoDate ) es) ++ (filter (beforeClosed t getActualTodoDate) es)))

-- countStock
countStock:: EntityInstance -> [EventInstance] -> Stock
countStock ent es = Stock{entity = ent, stockAmount = ((getTotalAmount (getInEvents (filter (samesubject ent) es))) - (getTotalAmount (getOutEvents (filter (samesubject ent) es))))}

-- stock
deduceStock:: [EventInstance] -> [Stock]
deduceStock es = [ countStock ent es | ent <- getEntities es]

{- KPI : average duration of done events that change of ownership calculations - grouped by direction -}
{-function finds events that change of ownership and calculates the average duration-}

kpiAvgDurChgOwDone:: [EventInstance] -> [Float]
kpiAvgDurChgOwDone es = getAverageDurationEventsChangeOwnershipDone (getEventsChangeOwnershipGrouped es)

getEventsChangeOwnershipGrouped :: [EventInstance] -> [[EventInstance]]
getEventsChangeOwnershipGrouped es = List.groupBy (\x y -> evtInstDirection x == evtInstDirection y) (getEventsChangeOwnership es)

-- helper functions
getEventsChangeOwnership:: [EventInstance] -> [EventInstance]
getEventsChangeOwnership es = filter ( isdone ) (filter (ischangeownership) es)

getAverageDurationEventsChangeOwnershipDone :: [[EventInstance]] -> [Float]
getAverageDurationEventsChangeOwnershipDone es =  map calculateAverageDuration (es)


calculateAverageDuration :: [EventInstance] -> Float
calculateAverageDuration es =  realToFrac( sum (map calculateDuration (es) ) ) / List.genericLength es

calculateDuration ::  EventInstance -> Int
calculateDuration e = durationInDays (getActualBeginDate e) (getActualEndDate e)

durationInDays ::  Date -> Date -> Int
durationInDays begin end = fromInteger( Time.diffDays (Time.fromGregorian (getyear end) (getmonthint end) (getdayint end)) (Time.fromGregorian (getyear begin) (getmonthint begin) (getdayint begin)) )

eventsinperiod :: [EventInstance] -> Date -> Date -> [EventInstance]
eventsinperiod es pbegin pend = filter (inperiod pbegin getAdminBeginDate pend getAdminEndDate ) es

calculatePercentage :: Float -> Float -> Float
calculatePercentage newValue oldValue = if oldValue /=0 then (newValue / oldValue) *100 else 0

kpiAvgDurChgOwDone_compare_with_same_period_last_year::[EventInstance] -> Date -> Date -> Date -> Date -> [(Float,Float, String)]
kpiAvgDurChgOwDone_compare_with_same_period_last_year es dt1 dt2 dt3 dt4 = zip3 kpiAvgDurChgOwDonethisperiod kpiAvgDurChgOwDoneprevperiod [ ( show  $ (( calculatePercentage (kpiAvgDurChgOwDonethisperiod!!0) (kpiAvgDurChgOwDoneprevperiod!!0)) - 100 )) ++ " %" , (show  $ ( (calculatePercentage (kpiAvgDurChgOwDonethisperiod!!1) (kpiAvgDurChgOwDoneprevperiod!!1)) - 100 ) )  ++ " %"  ]
                                                  where kpiAvgDurChgOwDonethisperiod = kpiAvgDurChgOwDone (eventsinperiod es dt1 dt2)
                                                        kpiAvgDurChgOwDoneprevperiod = kpiAvgDurChgOwDone (eventsinperiod es dt3 dt4)

{- KPI : percentage-events followed by agreement , e.g., sales offer -> sales order -}

kpiFromEventToAgreementPercentage:: [EventInstance] -> [AgreementInstance] -> Float
kpiFromEventToAgreementPercentage es ags = calculatePercentage (List.genericLength(getEventsFollowAgreement  ags)) (List.genericLength(getEventsMayFollowAgreement es))
