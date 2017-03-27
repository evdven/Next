-- Next Formalization v1 AFAS Software BV - 2017
-- DSR: formalisations based on LaTeX document

import qualified Data.List as List
import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock

------------------------------------------- OEM
------------ Possible values
-- data Options = Agreement | Regular | DirIn | DirOut | Always | Optional | Never | Bot |  Asset | TradeItem | Person | Organisation | Good | Time | Given deriving (Eq, Show)
data EntType = Person | Organisation | Good | Time | Given | EntBot deriving (Eq, Show)
data EvtStyle = Agreement | Regular deriving (Eq, Show)
data Direction = DirIn | DirOut deriving (Eq, Show)
data OEMEvtOptions = Always | Optional | Never | OptBot deriving (Eq, Show)
data OEMTypeOfOwnership = Asset | OwnershipOptional | TradeItem | OwnershipBot deriving (Eq, Show)
data Status = ToDo | Started | Done deriving (Eq, Show)

------------ OEMEntity
data OEMEntity = OEMEntity {oemEntname :: String, oemEntType :: EntType} deriving (Eq, Show)

------------ OEMRole
data OEMActorStyle = OEMActorStyle {oemActorEnttype:: EntType, oemActorEntity:: OEMEntity} deriving (Eq, Show)
data OEMRole = OEMRole {oemRoleName :: String, oemActStyle :: [OEMActorStyle]} deriving (Eq, Show)

------------ OEMEvent
-- TODO incoming edges, outgoing edges, grouping, hasagreement, beginend date
-- unal : to handle agreements i updated OEMEvent.oemEvtSubject characteristic as optional using Maybe
data OEMEvent = OEMEvent {oemEvtName :: String, oemEvtStyle:: EvtStyle, oemhasParty :: Bool, oemEvtParty :: OEMRole, oemhasSubject :: Bool, oemEvtSubject :: Maybe OEMRole, oemhasAmount :: Bool, oemhasValue :: Bool, oemEvtDirection :: Direction, oemEvtChangeOfOwnership :: OEMEvtOptions, oemEvtTypeOfOwnership :: OEMTypeOfOwnership, oemEvtPaymentRequired :: OEMEvtOptions} deriving (Eq, Show)


------------------------------------------- LAM

------------ LAMEntity
data LAMEntity = LAMEntity {lamEntname :: String, lamEntType :: EntType} deriving (Eq, Show)

------------ LAMRole
data LAMActorStyle = LAMActorStyle {lamActorEnttype:: EntType, lamActorEntity:: LAMEntity} deriving (Eq, Show)
data LAMRole = LAMRole {lamRoleName :: String, lamActStyle :: [LAMActorStyle]} deriving (Eq, Show)

------------ LAMEvent
-- TODO incoming edges, outgoing edges, grouping, hasagreement, beginend date
data LAMEvent = LAMEvent {lamEvtName :: String, lamEvtStyle:: EvtStyle, lamhasParty :: Bool, lamEvtParty :: LAMRole, lamhasSubject :: Bool, lamEvtSubject :: LAMRole, lamhasAmount :: Bool, lamhasValue :: Bool, lamEvtDirection :: Direction, lamEvtChangeOfOwnership :: OEMEvtOptions, lamEvtTypeOfOwnership :: OEMTypeOfOwnership, lamEvtPaymentRequired :: OEMEvtOptions} deriving (Eq, Show)

------------------------------------------- Instance
-- date = dd, mm, yyyy
type Date = (Integer, Integer, Integer)
-- follows the index of the status
type TimeStamps = [Date]

emptyDateFirst :: Date
emptyDateFirst = (1, 1, 1970)
emptyDateLast :: Date
emptyDateLast = (1, 1, 2100)

data EntityInstance = EntityInstance {entRef :: LAMEntity, entInstName :: String, entInstType :: EntType, entInstCreate :: Date, entInstDelete :: Date} deriving (Eq, Show)
data RoleInstance = RoleInstance {rolRef:: LAMRole, rolInstName :: String, actInstStyle :: [LAMActorStyle], rolEnt :: EntityInstance, rolInstCreate :: Date, rolInstDelete :: Date} deriving (Eq, Show)
-- TODO elements are missing: incoming edges, outgoing edges, agreement, begin end date
data EventInstance = EventInstance {evtRef :: LAMEvent, evtInstName :: String, evtInstStyle :: EvtStyle, evtInstHasParty :: Bool, evtInstParty :: RoleInstance, evtInstHasSubject :: Bool, evtInstSubject :: RoleInstance, evtInstHasAmount :: Bool, evtInstHasValue :: Bool, evtInstDirection :: Direction, evtInstChangeOfOwnership :: OEMEvtOptions, evtInstTypeOfOwnership :: OEMTypeOfOwnership, evtInstPaymentRequired :: OEMEvtOptions, evtInstPlanned :: TimeStamps, evtInstActual :: TimeStamps, evtInstAdministrative :: TimeStamps, evtInstAmount :: Integer, evtInstValue :: [Double]} deriving (Eq, Show)


------------------------------------------- Model
personEntity = LAMEntity {lamEntname = "Person", lamEntType = Person}
customerStyle = LAMActorStyle {lamActorEnttype = Person, lamActorEntity = personEntity}
roleCustomer = LAMRole {lamRoleName = "Customer", lamActStyle = [customerStyle]}
roleSupplier = LAMRole {lamRoleName = "Supplier", lamActStyle = [customerStyle]}

goodEntity = LAMEntity {lamEntname = "Good", lamEntType = Good}
productStyle = LAMActorStyle {lamActorEnttype = Good, lamActorEntity = goodEntity}
roleProduct = LAMRole {lamRoleName = "Product", lamActStyle = [productStyle]}

-- unal : before we used paymentRequired as a separator between invoice and delivery. If we say paymentRequired=Always for a delivery then there will be no difference between it and an invoice
deliveryEvent = LAMEvent {lamEvtName = "Delivery", lamEvtStyle = Regular, lamhasParty = True, lamEvtParty = roleCustomer, lamhasSubject = True, lamEvtSubject = roleProduct, lamhasAmount = True, lamhasValue = True, lamEvtDirection = DirOut, lamEvtChangeOfOwnership = Always, lamEvtTypeOfOwnership = TradeItem, lamEvtPaymentRequired = Never}

goodsReceiveEvent = LAMEvent {lamEvtName = "GoodsReceive", lamEvtStyle = Regular, lamhasParty = True, lamEvtParty = roleSupplier, lamhasSubject = True, lamEvtSubject = roleProduct, lamhasAmount = True, lamhasValue = True, lamEvtDirection = DirIn, lamEvtChangeOfOwnership = Always, lamEvtTypeOfOwnership = TradeItem, lamEvtPaymentRequired = Never}

salesInvoiceEvent = LAMEvent {lamEvtName = "SalesInvoice", lamEvtStyle = Regular, lamhasParty = True, lamEvtParty = roleCustomer, lamhasSubject = True, lamEvtSubject = roleProduct, lamhasAmount = True, lamhasValue = True, lamEvtDirection = DirOut, lamEvtChangeOfOwnership = Never, lamEvtTypeOfOwnership = TradeItem, lamEvtPaymentRequired = Always}

------------------------------------------- Run-time
person1 = EntityInstance{entRef = personEntity, entInstName = "Alice", entInstType = Person, entInstCreate = (1, 1, 1988), entInstDelete = emptyDateLast}
person2 = EntityInstance{entRef = personEntity, entInstName = "Bob", entInstType = Person, entInstCreate = (15, 2, 1988), entInstDelete = emptyDateLast}

customer1 = RoleInstance{rolRef = roleCustomer, rolInstName = "Customer", actInstStyle = [customerStyle], rolEnt = person1, rolInstCreate = (1, 1, 2016), rolInstDelete = emptyDateLast}
customer2 = RoleInstance{rolRef = roleCustomer, rolInstName = "Customer", actInstStyle = [customerStyle], rolEnt = person2, rolInstCreate = (1, 6, 2016), rolInstDelete = emptyDateLast}

supplier1 = RoleInstance{rolRef = roleSupplier, rolInstName = "Supplier", actInstStyle = [customerStyle], rolEnt = person1, rolInstCreate = (1, 1, 2016), rolInstDelete = emptyDateLast}

good1 = EntityInstance{entRef = goodEntity, entInstName = "Fiets", entInstType = Good, entInstCreate = (1, 1, 2016), entInstDelete = emptyDateLast}
good2 = EntityInstance{entRef = goodEntity, entInstName = "Auto", entInstType = Good, entInstCreate = (15, 2, 2016), entInstDelete = emptyDateLast}

product1 = RoleInstance{rolRef = roleProduct, rolInstName = "Product", actInstStyle = [productStyle], rolEnt = good1, rolInstCreate = (1, 1, 2016), rolInstDelete = emptyDateLast}
product2 = RoleInstance{rolRef = roleProduct, rolInstName = "Product", actInstStyle = [productStyle], rolEnt = good2, rolInstCreate = (15, 2, 2016), rolInstDelete = emptyDateLast}

delivery1a = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery1a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(1, 1, 2017), (5, 1, 2017), (10, 2, 2017)], evtInstActual = [(3, 1, 2017), (10, 2, 2017), (16, 3, 2017)], evtInstAdministrative = [(1, 1, 2017), (5, 1, 2017), (10, 2, 2017)], evtInstAmount = 5, evtInstValue = [100, 500]}

delivery1b = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery1b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(11, 3, 2017), (15, 3, 2017), (20, 5, 2017)], evtInstActual = [(13, 3, 2017), (20, 5, 2017), (26, 6, 2017)], evtInstAdministrative = [(11, 3, 2017), (15, 3, 2017), (20, 5, 2017)], evtInstAmount = 3, evtInstValue = [100, 500]}

delivery2a = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery2a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer2, evtInstHasSubject = True, evtInstSubject = product2, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(1, 2, 2016), (5, 2, 2016), (10, 3, 2016)], evtInstActual = [(3, 2, 2016), (10, 3, 2016), (30, 3, 2016)], evtInstAdministrative = [(1, 2, 2016), (5, 2, 2016), (10, 3, 2016)], evtInstAmount = 5, evtInstValue = [1000, 5000]}

delivery2b = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery2b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer2, evtInstHasSubject = True, evtInstSubject = product2, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(11, 5, 2016), (15, 5, 2016), (20, 6, 2016)], evtInstActual = [(13, 5, 2016), (20, 6, 2016), (30, 7, 2016)], evtInstAdministrative = [(11, 5, 2016), (15, 5, 2016), (20, 6, 2016)], evtInstAmount = 7, evtInstValue = [1000, 5000]}

delivery3a = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery3a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer2, evtInstHasSubject = True, evtInstSubject = product2, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(1, 5, 2016), (5, 5, 2016), (10, 6, 2016)], evtInstActual = [], evtInstAdministrative = [(1, 5, 2016), (5, 5, 2016), (10, 6, 2016)], evtInstAmount = 7, evtInstValue = [1000, 5000]}

delivery3b = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery3b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer2, evtInstHasSubject = True, evtInstSubject = product2, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [], evtInstActual = [], evtInstAdministrative = [(1, 5, 2016), (5, 5, 2016), (10, 6, 2016)], evtInstAmount = 7, evtInstValue = [1000, 5000]}

goodReceive1a = EventInstance{evtRef = goodsReceiveEvent, evtInstName = "goodReceive1a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(1, 8, 2017), (5, 8, 2017), (10, 9, 2017)], evtInstActual = [(5, 8, 2017), (10, 9, 2017), (16, 10, 2017)], evtInstAdministrative = [(1, 8, 2017), (5, 8, 2017), (10, 9, 2017)], evtInstAmount = 100, evtInstValue = [100, 500]}

goodReceive1b = EventInstance{evtRef = goodsReceiveEvent, evtInstName = "goodReceive1b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(11, 8, 2017), (15, 8, 2017), (20, 9, 2017)], evtInstActual = [(15, 8, 2017), (10, 9, 2017), (26, 10, 2017)], evtInstAdministrative = [(11, 8, 2017), (15, 8, 2017), (20, 9, 2017)], evtInstAmount = 100, evtInstValue = [100, 500]}

goodReceive2a = EventInstance{evtRef = goodsReceiveEvent, evtInstName = "goodReceive2a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(10, 8, 2016), (20, 9, 2016), (30, 10, 2016)], evtInstActual = [(10, 8, 2016), (20, 9, 2016), (30, 10, 2016)], evtInstAdministrative = [(10, 8, 2016), (20, 9, 2016), (30, 10, 2016)], evtInstAmount = 100, evtInstValue = [100, 500]}

goodReceive2b = EventInstance{evtRef = goodsReceiveEvent, evtInstName = "goodReceive2b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = [(10, 8, 2016), (20, 9, 2016), (30, 10, 2016)], evtInstActual = [(10, 8, 2016), (20, 9, 2016), (30, 10, 2016)], evtInstAdministrative = [(10, 8, 2016), (20, 9, 2016), (30, 10, 2016)], evtInstAmount = 100, evtInstValue = [100, 500]}

salesInvoice1a = EventInstance{evtRef = salesInvoiceEvent, evtInstName = "salesInvoice1a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Never, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Always, evtInstPlanned = [(1, 8, 2016), (5, 8, 2016), (10, 9, 2016)], evtInstActual = [(5, 8, 2016), (10, 9, 2016), (16, 10, 2016)], evtInstAdministrative = [(1, 8, 2016), (5, 8, 2016), (10, 9, 2016)], evtInstAmount = 100, evtInstValue = [100, 500]}

persons = [person1, person2]
goods = [good1, good2]
roles = [customer1, customer2, product1, product2, supplier1]
events = [delivery1a, delivery1b, delivery2a, delivery2b, delivery3a, delivery3b, goodReceive1a, goodReceive1b, goodReceive2a, goodReceive2b]

-- Stock
data Stock = Stock {entity:: EntityInstance, stockAmount:: Integer} deriving (Show)

get1st (a, b, c) = a
get2nd (a, b, c) = b
get3rd (a, b, c) = c

-- getday
getday:: Date -> Integer
getday d = get1st d
-- getyear
getyear:: Date -> Integer
getyear d = get3rd d

-- getmonth
getmonth:: Date -> Integer
getmonth d = get2nd d

-- Data.Time.diffDays requires Int type, so I added these functions
getdayint::Date -> Int
getdayint d = fromInteger(getday d)

getmonthint:: Date -> Int
getmonthint d = fromInteger(getmonth d)

getyearint:: Date -> Int
getyearint d = fromInteger(getyear d)

getDirEvents:: [EventInstance] -> Direction -> [EventInstance]
getDirEvents es d = [e | e<- es, evtInstDirection e == d]

getInEvents es = getDirEvents es DirIn
getOutEvents es = getDirEvents es DirOut

getAmount:: EventInstance -> Integer
getAmount e = evtInstAmount e

getTotalAmount:: [EventInstance] -> Integer
getTotalAmount es = sum (map getAmount es)

-- getSubjectInstance
getSubjectInstance:: EventInstance -> EntityInstance
getSubjectInstance e = rolEnt (evtInstSubject e)

-- getEntities
getEntities:: [EventInstance] -> [EntityInstance]
getEntities es = List.nub [getSubjectInstance e |e <- es]

-- samesubject
samesubject:: EntityInstance -> EventInstance -> Bool
samesubject ent evt = getSubjectInstance evt == ent

-- countStock
countStock:: EntityInstance -> [EventInstance] -> Stock
countStock ent es = Stock{entity = ent, stockAmount = ((getTotalAmount (getInEvents (filter (samesubject ent) es))) - (getTotalAmount (getOutEvents (filter (samesubject ent) es))))}

-- stock
deduceStock:: [EventInstance] -> [Stock]
deduceStock es = [ countStock ent es | ent <- getEntities es]

-- planned
getPlannedDate:: EventInstance -> TimeStamps
getPlannedDate e = evtInstPlanned e

getPlannedStartDate:: EventInstance -> Date
getPlannedStartDate e = (getPlannedDate e) !! 1

getPlannedEndDate:: EventInstance -> Date
getPlannedEndDate e = (getPlannedDate e) !!2

-- actual
getActualDate:: EventInstance -> TimeStamps
getActualDate e = evtInstActual e

getActualStarDate::EventInstance -> Date
getActualStarDate e =  (getActualDate e) !! 1

getActualEndDate::EventInstance -> Date
getActualEndDate e =  (getActualDate e) !! 2

-- administrative
getAdminDate:: EventInstance -> TimeStamps
getAdminDate e = evtInstAdministrative e

getAdminCreateDate:: EventInstance -> Date
getAdminCreateDate e = (getAdminDate e) !! 0

-- do something with the first and last dates
-- before
beforeOpen:: Date -> (EventInstance -> TimeStamps) -> Int -> EventInstance -> Bool
beforeOpen (d, m, y) func int evt = if(getyear ((func evt)!!int) < y) then True
else if (getyear ((func evt)!!int) == y && getmonth ((func evt)!!int) < m) then True
else if (getyear ((func evt)!!int) == y && getmonth ((func evt)!!int) == m && getday ((func evt)!!int) < d) then True
else False

beforeClosed:: Date -> (EventInstance -> TimeStamps) -> Int -> EventInstance -> Bool
beforeClosed (d, m, y) func int evt = if(getyear ((func evt)!!int) < y) then True
else if (getyear ((func evt)!!int) == y && getmonth ((func evt)!!int) < m) then True
else if (getyear ((func evt)!!int) == y && getmonth ((func evt)!!int) == m && getday ((func evt)!!int) <= d) then True
else False

-- after
afterOpen:: Date -> (EventInstance -> TimeStamps) -> Int -> EventInstance -> Bool
afterOpen (d, m, y) func int evt = not (beforeClosed (d, m, y) func int evt)

afterClosed:: Date -> (EventInstance -> TimeStamps) -> Int -> EventInstance -> Bool
afterClosed (d, m, y) func int evt = not (beforeOpen (d, m, y) func int evt)

-- in period
inperiod:: Date -> (EventInstance -> TimeStamps) -> Int -> Date -> (EventInstance -> TimeStamps) -> Int -> EventInstance -> Bool
inperiod t1 func1 int1 t2 func2 int2 evt = afterClosed t1 func1 int1 evt && beforeClosed t2 func2 int2 evt

-- is done
isdone:: EventInstance -> Bool
isdone e = if List.genericLength (getActualDate e) == 3 then True else False

-- is planned
isplanned:: EventInstance -> Bool
isplanned e = if List.genericLength (getPlannedDate e) > 0 then True else False

-- changes of ownership
ischangeownership:: EventInstance -> Bool
ischangeownership e = if evtInstChangeOfOwnership e == Always then True else False

-- physical stock
physicalStockAtT:: [EventInstance] -> Date -> [Stock]
physicalStockAtT es t = deduceStock (filter (beforeClosed t getActualDate 2) es) 

-- economic stock
economicStockAtT:: [EventInstance] -> Date -> [Stock]
economicStockAtT es t = deduceStock (List.nub ((filter (beforeClosed t getPlannedDate 0) es) ++ (filter (beforeClosed t getActualDate 0) es)))

{- KPI : averageDurationOfDoneEventsThatChangeOfOwnership calculations -}
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
calculateDuration e = durationInDays (getActualStarDate e) (getActualEndDate e)

durationInDays ::  Date -> Date -> Int
durationInDays start end = fromInteger( Time.diffDays (Time.fromGregorian (getyear end) (getmonthint end) (getdayint end)) (Time.fromGregorian (getyear start) (getmonthint start) (getdayint start)) )

eventsinperiod :: [EventInstance] -> Date -> Date -> [EventInstance]
eventsinperiod es pstart pend = filter (inperiod pstart getAdminDate 1  pend getAdminDate 2 )es

calculatePercentage :: Float -> Float -> Float
calculatePercentage newValue oldValue = if oldValue /=0 then (newValue / oldValue) *100 else 0

kpiAvgDurChgOwDone_compare_with_same_period_last_year::[EventInstance] -> Date -> Date -> Date -> Date -> [(Float,Float, String)]
kpiAvgDurChgOwDone_compare_with_same_period_last_year es dt1 dt2 dt3 dt4 = zip3 kpiAvgDurChgOwDonethisperiod kpiAvgDurChgOwDoneprevperiod [ ( show  $ (calculatePercentage (kpiAvgDurChgOwDonethisperiod!!0) (kpiAvgDurChgOwDoneprevperiod!!0)) ) ++ " %" , (show  $ (calculatePercentage (kpiAvgDurChgOwDonethisperiod!!1) (kpiAvgDurChgOwDoneprevperiod!!1)))  ++ " %"  ]
                                                  where kpiAvgDurChgOwDonethisperiod = kpiAvgDurChgOwDone (eventsinperiod es dt1 dt2) 
                                                        kpiAvgDurChgOwDoneprevperiod = kpiAvgDurChgOwDone (eventsinperiod es dt3 dt4)
p1Start = (1, 1, 2017)
p1End = (31, 12, 2017)
p2Start = (1, 1, 2016)
p2End = (31, 12, 2016)

kpiValue = kpiAvgDurChgOwDone_compare_with_same_period_last_year events p1Start p1End p2Start p2End
