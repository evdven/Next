-- Next Formalization v1 AFAS Software BV - 2017
-- eve: initial setup. meta -> model -> instance

import qualified Data.List as List

data Status = ToDo | Done deriving (Eq, Show)
type Date = (Integer, Integer, Integer)

-- meta model defs
data Role = Role {rname :: String} deriving (Show)
data Direction = DirIn | DirOut deriving (Eq, Show)
data Event = Event {name :: String, party :: Role, subject :: Role, direction :: Direction} deriving (Show)

--data Component = Role {rname :: String} deriving (Show)

-- model defs
roles :: [Role]
roles = [Role {rname = "Customer"}, Role {rname = "SalesProduct"}]

events :: [Event]
events = [Event {name = "SalesOrder", party = Role "Customer", subject = Role "SalesProduct", direction = DirOut}]

-- instance defs
data RoleInstance = RoleInstance {value :: String} deriving (Eq, Show)
data EventInstance = EventInstance {startDate :: Date, status :: Status, iparty :: RoleInstance, isubject :: RoleInstance, idirection :: Direction, iamount :: Integer} deriving (Show)

-- Stock
data Stock = Stock {entity:: RoleInstance, stockAmount:: Integer} deriving (Show)

-- sample population
pops = [(Event {name = "SalesOrder", party = Role "Customer", subject = Role "SalesProduct", direction = DirOut}, [EventInstance {startDate = (1, 1, 2017), status = Done, iparty = RoleInstance {value="Alice"}, isubject = RoleInstance {value = "Merida X0 Carbon"}, idirection = DirOut, iamount = 5}, EventInstance {startDate = (1, 2, 2017), status = ToDo, iparty = RoleInstance {value="Bob"}, isubject = RoleInstance {value = "Merida X0 Carbon"}, idirection = DirOut, iamount = 5}, EventInstance {startDate = (1, 3, 2017), status = ToDo, iparty = RoleInstance {value="Charles"}, isubject = RoleInstance {value = "Merida X0 Carbon"}, idirection = DirOut, iamount = 5}])]


pop:: String -> [EventInstance]
pop e = snd (head pops) -- hack

-- Generic helper functions on events
-- An event instance is complete if it's done
complete :: EventInstance -> Bool
complete e 
    | status e == Done = True
    | otherwise = False

completedeventsinstances is = filter complete is
notcompletedeventsinstances is = filter (not . complete) is

get1st (a, b, c) = a
get2nd (a, b, c) = b
get3rd (a, b, c) = c

-- getday
getday:: EventInstance -> Integer
getday e = get1st (startDate e)

-- getmonth
getmonth:: EventInstance -> Integer
getmonth e = get2nd (startDate e)

-- getyear
getyear:: EventInstance -> Integer
getyear e = get3rd (startDate e)

-- before
-- before:: [EventInstance] -> Date -> [EventInstance]
-- before (((d1, m1, y1), _):es) (d2, m2, y2) = 
-- list comprehension

getDirEvents es d = [e | e<- es, idirection e == d]

getInEvents es = getDirEvents es DirIn
getOutEvents es = getDirEvents es DirOut

getAmount:: EventInstance -> Integer
getAmount e = iamount e

getTotalAmount:: [EventInstance] -> Integer
getTotalAmount es = sum (map getAmount es)

getSubjectInstance:: EventInstance -> RoleInstance
getSubjectInstance e = isubject e

getEntities:: [EventInstance] -> [RoleInstance]
getEntities es = List.nub [isubject e |e <- es]

-- samesubject
samesubject:: RoleInstance -> EventInstance -> Bool
samesubject r e = getSubjectInstance e == r

-- countStock
countStock:: RoleInstance -> [EventInstance] -> Stock
countStock r es = Stock{entity = r, stockAmount = ((getTotalAmount (getInEvents (filter (samesubject r) es))) - (getTotalAmount (getOutEvents (filter (samesubject r) es))))}

-- stock
deduceStock:: [EventInstance] -> [Stock]
deduceStock es = [ countStock ent es | ent <- getEntities es]

-- before
before:: Date -> EventInstance -> Bool
before (d, m, y) e = if(getyear e < y) then True
else if (getyear e == y && getmonth e < m) then True
else if (getyear e == y && getmonth e == m && getday e <= d) then True
else False

-- after
after:: Date -> EventInstance -> Bool
after (d, m, y) e = not (before (d, m, y) e)

-- in period
inperiod:: Date -> Date -> EventInstance -> Bool
inperiod t1 t2 e = after t1 e && before t2 e

-- Generic helper functions on time (cleanup with elem)
-- inperiod

-- stock at t1
stockAtT:: [EventInstance] -> Date -> [Stock]
stockAtT es t = deduceStock (filter (before t) es)

-- stock in period (delta)
stockInPeriodDelta:: [EventInstance] -> Date -> Date -> [Stock]
stockInPeriodDelta es t1 t2 = deduceStock (filter (inperiod t1 t2) es)



---completedevents :: [EventType] -> [EventType]
--completedevents events = [e | e <- events, status e == Done]				
--eventstodo events = [e | e <- events, status e == ToDo]				

