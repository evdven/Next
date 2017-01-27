-- Next Formalization v1 AFAS Software BV - 2017
-- eve: initial setup. meta -> model -> instance

data Status = ToDo | Done deriving (Eq, Show)
type Date = (Integer, Integer, Integer)

-- meta model defs
data Role = Role {rname :: String} deriving (Show)
data Event = Event {name :: String, party :: Role, subject :: Role} deriving (Show)

--data Component = Role {rname :: String} deriving (Show)

-- model defs
roles :: [Role]
roles = [Role {rname = "Customer"}, Role {rname = "SalesProduct"}]

events :: [Event]
events = [Event {name = "SalesOrder", party = Role "Customer", subject = Role "SalesProduct"}]

-- instance defs
data RoleInstance = RoleInstance {value :: String} deriving (Show)
data EventInstance = EventInstance {startDate :: Date, status :: Status, iparty :: RoleInstance, isubject :: RoleInstance} deriving (Show)

-- sample population
pops = [(Event {name = "SalesOrder", party = Role "Customer", subject = Role "SalesProduct"}, [EventInstance {startDate = (1, 1, 2017), status = Done, iparty = RoleInstance {value="Alice"}, isubject = RoleInstance {value = "Merida X0 Carbon"}}, EventInstance {startDate = (1, 2, 2017), status = ToDo, iparty = RoleInstance {value="Bob"}, isubject = RoleInstance {value = "Merida X0 Carbon"}}, EventInstance {startDate = (1, 3, 2017), status = ToDo, iparty = RoleInstance {value="Charles"}, isubject = RoleInstance {value = "Merida X0 Carbon"}}])]


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
	
	
-- samesubject
	
	
-- Generic helper functions on time (cleanup with elem)
-- inperiod



---completedevents :: [EventType] -> [EventType]
--completedevents events = [e | e <- events, status e == Done]				
--eventstodo events = [e | e <- events, status e == ToDo]				

