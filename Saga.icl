module Saga

import Character

import iTasks
import StdBool
import Text.HTML

import StdDebug

characters :: SimpleSDSLens [Character]
characters = sharedStore "characters" []

Start :: *World -> *World
Start world = startEngine (setup >>| loginAndManageWork "Saga" Nothing (Just (Text "Welcome to Saga. Erin's DnD Management System")) False) world
where
	worklist :: [Workflow]
	worklist =
		[ restrictedTransientWorkflow "Manage users" "Manage users" ["Dungeon Master"] manageUsers
		]

setup :: Task ()
setup = createDMIfNonExist >>| installWorkflows worklist
where
	worklist =
		[ restrictedTransientWorkflow "Manage users" "Manage users" ["Dungeon Master"] manageUsers
		, restrictedTransientWorkflow "View All Characters" "Give a combat centric overview of all characters" ["Dungeon Master"] viewCharacters
		, transientWorkflow "Characters" "View/Edit all my characters" viewCharacters
		]

isRole :: User String -> Bool
isRole (AuthenticatedUser _ roles _) role = isMember role roles

DMTask :: Task ()
DMTask = editCharacters

playerTask :: Task ()
playerTask = viewCharacters

createDMIfNonExist :: Task ()
createDMIfNonExist = get (usersWithRole "Dungeon Master") >>- (\dms
	| isEmpty dms = trace_n "Create DM" createDMAccount
	| otherwise = return ()
	)
where
	createDMAccount :: Task ()
	createDMAccount = createUser
		{ UserAccount
		| credentials =
			{ username = Username "DM"
			, password = Password "erin"
			}
		, title = Just "Dungeon Master"
		, roles = ["Dungeon Master"]
		} @ \_ -> ()

editCharacters :: Task ()
editCharacters = updateSharedInformation (Title "Characters") [] characters @ \_ -> ()

viewCharacters :: Task ()
viewCharacters = viewSharedInformation (Title "Characters") [] characters @ \_ -> ()
