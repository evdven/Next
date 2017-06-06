-- Next Copyright AFAS Software BV - 2017
-- DSR: formalisations based on LaTeX document
module Next where

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
data Status = ToDo | Doing | Done deriving (Eq, Show)

------------ OEMEntity
data OEMEntity = OEMEntity {oemEntname :: String, oemEntType :: EntType} deriving (Eq, Show)

------------ OEMRole
data OEMActorStyle = OEMActorStyle {oemActorEnttype:: EntType, oemActorEntity:: OEMEntity} deriving (Eq, Show)
data OEMRole = OEMRole {oemRoleName :: String, oemActStyle :: [OEMActorStyle]} deriving (Eq, Show)

------------ OEMEvent
-- TODO incoming edges, outgoing edges, grouping, hasagreement, beginend date
-- unal : to handle agreements i updated OEMEvent.oemEvtSubject characteristic as optional using Maybe
data OEMEvent = OEMEvent {oemEvtName :: String, oemEvtStyle:: EvtStyle, oemEvtHasParty :: Bool, oemEvtParty :: OEMRole, oemEvtHasSubject :: Bool, oemEvtSubject :: OEMRole, oemEvtHasAmount :: Bool, oemEvtHasValue :: Bool, oemEvtDirection :: Direction, oemEvtChangeOfOwnership :: OEMEvtOptions, oemEvtTypeOfOwnership :: OEMTypeOfOwnership, oemEvtPaymentRequired :: OEMEvtOptions} deriving (Eq, Show)

data OEMAgreement = OEMAgreement {oemAgrName :: String, oemAgrStyle:: EvtStyle, oemAgrHasParty :: Bool, oemAgrParty :: OEMRole, oemAgrHasAmount :: Bool, oemAgrHasValue :: Bool, oemAgrFormer :: Maybe OEMEvent, oemAgrLatter :: OEMEvent} deriving (Eq, Show)
------------------------------------------- LAM

------------ LAMEntity
data LAMEntity = LAMEntity {lamEntname :: String, lamEntType :: EntType} deriving (Eq, Show)

------------ LAMRole
data LAMActorStyle = LAMActorStyle {lamActorEnttype:: EntType, lamActorEntity:: LAMEntity} deriving (Eq, Show)
data LAMRole = LAMRole {lamRoleName :: String, lamActStyle :: [LAMActorStyle]} deriving (Eq, Show)

------------ LAMEvent
-- TODO incoming edges, outgoing edges, grouping, hasagreement, beginend date
data LAMEvent = LAMEvent {lamEvtName :: String, lamEvtStyle:: EvtStyle, lamEvtHasParty :: Bool, lamEvtParty :: LAMRole, lamEvtHasSubject :: Bool, lamEvtSubject :: LAMRole, lamEvtHasAmount :: Bool, lamEvtHasValue :: Bool, lamEvtDirection :: Direction, lamEvtChangeOfOwnership :: OEMEvtOptions, lamEvtTypeOfOwnership :: OEMTypeOfOwnership, lamEvtPaymentRequired :: OEMEvtOptions} deriving (Eq, Show)

data LAMAgreement = LAMAgreement {lamAgrName :: String, lamAgrStyle:: EvtStyle, lamAgrHasParty :: Bool, lamAgrParty :: LAMRole, lamAgrHasAmount :: Bool, lamAgrHasValue :: Bool, lamAgrFormer :: Maybe LAMEvent, lamAgrLatter :: LAMEvent} deriving (Eq, Show)

type Date = (Integer, Integer, Integer)
-- follows the index of the status (ToDo, Start, Done)
-- For an event instance in plannedDate fied ToDo date is mandatory.
-- Status (lifecyle) of an event instance will be determined based on Start and Done date values in plannedDate and actualDate fields.
data Timing = Timing {todo :: Date,  begin :: Date, end :: Date} deriving (Eq, Show)


-- Helper functions

emptyDateFirst :: Date
emptyDateFirst = (1, 1, 1970)
emptyDateLast :: Date
emptyDateLast = (1, 1, 2100)
emptyTiming = Timing {todo=emptyDateFirst, begin=emptyDateFirst, end=emptyDateFirst}


data EntityInstance = EntityInstance {entRef :: LAMEntity, entInstName :: String, entInstType :: EntType, entInstCreate :: Date, entInstDelete :: Date} deriving (Eq, Show)
data RoleInstance = RoleInstance {rolRef:: LAMRole, rolInstName :: String, actInstStyle :: [LAMActorStyle], rolEnt :: EntityInstance, rolInstCreate :: Date, rolInstDelete :: Date} deriving (Eq, Show)
-- TODO elements are missing: incoming edges, outgoing edges, agreement, begin end date
data EventInstance = EventInstance {evtRef :: LAMEvent, evtInstName :: String, evtInstStyle :: EvtStyle, evtInstHasParty :: Bool, evtInstParty :: RoleInstance, evtInstHasSubject :: Bool, evtInstSubject :: RoleInstance, evtInstHasAmount :: Bool, evtInstHasValue :: Bool, evtInstDirection :: Direction, evtInstChangeOfOwnership :: OEMEvtOptions, evtInstTypeOfOwnership :: OEMTypeOfOwnership, evtInstPaymentRequired :: OEMEvtOptions, evtInstPlanned :: Timing, evtInstActual :: Timing, evtInstAdministrative :: Timing, evtInstAmount :: Integer, evtInstValue :: [Double]} deriving (Eq, Show)

data AgreementInstance = AgreementInstance {agrRef :: LAMAgreement, agrInstName :: String, agrInstStyle :: EvtStyle, agrInstHasParty :: Bool, agrInstParty :: RoleInstance, agrInstHasAmount :: Bool, agrInstHasValue :: Bool, agrInstPlanned :: Timing, agrInstActual :: Timing, agrInstAdministrative :: Timing, agrInstAmount :: Integer, agrInstValue :: [Double], agrInstFormer :: Maybe EventInstance, agrInstLatter :: EventInstance} deriving (Eq, Show)


-- Helper functions

get1st (a, b, c) = a
get2nd (a, b, c) = b
get3rd (a, b, c) = c

-- getday
getday:: Date -> Integer
getday d = get1st d

-- getmonth
getmonth:: Date -> Integer
getmonth d = get2nd d

-- getyear
getyear:: Date -> Integer
getyear d = get3rd d

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

-- planned

getPlannedDate:: EventInstance -> Timing
-- there can be three dates in this field (ToDo, Start, Done)
-- ToDo date is mandatory for the plannedDate field
getPlannedDate e = evtInstPlanned e

getPlannedTodoDate:: EventInstance -> Date
getPlannedTodoDate e = todo (getPlannedDate e)

getPlannedBeginDate:: EventInstance -> Date
getPlannedBeginDate e = begin (getPlannedDate e)

getPlannedEndDate:: EventInstance -> Date
getPlannedEndDate e = end (getPlannedDate e)

-- actual
getActualDate:: EventInstance -> Timing
-- there can be three dates in this field (ToDo, Start, Done)
getActualDate e = evtInstActual e

getActualTodoDate::EventInstance -> Date
getActualTodoDate e = todo (getActualDate e)

getActualBeginDate::EventInstance -> Date
getActualBeginDate e = begin (getActualDate e)

getActualEndDate::EventInstance -> Date
getActualEndDate e = end (getActualDate e)

-- administrative
getAdminDate:: EventInstance -> Timing
getAdminDate e = evtInstAdministrative e

getAdminTodoDate:: EventInstance -> Date
getAdminTodoDate e = todo (getAdminDate e)

getAdminBeginDate:: EventInstance -> Date
getAdminBeginDate e = begin (getAdminDate e)

getAdminEndDate:: EventInstance -> Date
getAdminEndDate e = end (getAdminDate e)

-- do something with the first and last dates
-- before
beforeOpen:: Date -> (EventInstance -> Date) -> EventInstance -> Bool
beforeOpen (d, m, y) func evt = if( (getyear (func evt)) < y ) then True
else if ( (getyear (func evt)) == y && (getmonth (func evt)) < m ) then True
else if ( (getyear (func evt)) == y && (getmonth (func evt)) == m && (getday (func evt)) < d ) then True
else False

beforeClosed:: Date -> (EventInstance -> Date) -> EventInstance -> Bool
beforeClosed (d, m, y) func evt = if( (getyear (func evt)) < y ) then True
else if ( (getyear (func evt)) == y && (getmonth (func evt)) < m ) then True
else if ( (getyear (func evt)) == y && (getmonth (func evt)) == m && (getday (func evt)) <= d ) then True
else False

-- after
afterOpen:: Date -> (EventInstance -> Date) -> EventInstance -> Bool
afterOpen (d, m, y) func evt = not (beforeClosed (d, m, y) func evt)

afterClosed:: Date -> (EventInstance -> Date) -> EventInstance -> Bool
afterClosed (d, m, y) func evt = not (beforeOpen (d, m, y) func evt)

-- in period
inperiod:: Date -> (EventInstance -> Date) -> Date -> (EventInstance -> Date) -> EventInstance -> Bool
inperiod t1 func1 t2 func2 evt = afterClosed t1 func1 evt && beforeClosed t2 func2 evt



-- is done
isdone:: EventInstance -> Bool
isdone e = (getActualEndDate e) > emptyDateFirst

-- is planned
isplanned:: EventInstance -> Bool
--isplanned e = if List.genericLength( map(getPlannedTodoDate e) ) == 3 then True else False
isplanned e = (getPlannedTodoDate e) > emptyDateFirst

-- is started
isstarted:: EventInstance -> Bool
--isstarted e = if List.genericLength( map( getPlannedBeginDate e) ) == 3 then True else False
isstarted e = (getPlannedBeginDate e)  > emptyDateFirst

-- changes of ownership
ischangeownership :: EventInstance -> Bool
ischangeownership e = if evtInstChangeOfOwnership e == Always then True else False

-- payment required
ispaymentrequired :: EventInstance -> Bool
ispaymentrequired e = if evtInstPaymentRequired e == Always then True else False

hasFormer :: AgreementInstance -> Bool
hasFormer a = (agrInstFormer a) /= Nothing

hasLatter :: AgreementInstance -> Bool
hasLatter a = (agrInstLatter a) /= Nothing

getEventsFollowAgreement :: [AgreementInstance] -> [Maybe EventInstance]
getEventsFollowAgreement ags = [agrInstFormer a | a<- ags, ((hasFormer a) == True)]

getEventsMayFollowAgreement :: [EventInstance] -> [EventInstance]
getEventsMayFollowAgreement es = filter ( not . ispaymentrequired ) (filter (not . ischangeownership ) es)