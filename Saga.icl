module Saga

import Character

from Data.Func import $
from StdFunctions import flip
import StdBool
import Text.HTML
import iTasks

import StdDebug

characters :: SimpleSDSLens [Character]
characters = sharedStore "characters" []

Start :: *World -> *World
Start world = startEngine (setup >>| loginAndManageWork "Saga" Nothing (Just (Text "Welcome to Saga. Erin's DnD Management System")) False) world

setup :: Task ()
setup = createDMIfNonExist >>| installWorkflows worklist
where
	worklist =
		[ restrictedTransientWorkflow "Manage users" "Manage users" ["Dungeon Master"] manageUsers
		, restrictedTransientWorkflow "View All Characters" "Give a combat centric overview of all characters" ["Dungeon Master"] viewCharacters
		, transientWorkflow "Characters" "View/Edit all my characters" $ forever editCharacters
		]

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
editCharacters =
	    enterChoiceWithShared (Title "Select Character") [ChooseFromGrid snd] (mapRead withIndexes characters)
	>>*
		[ OnAction (Action "Edit Character") $ hasValue $ \(i, c) ->
			    editCharacter c
			>>* [ OnAction (Action "Save Character") $ hasValue $ \c -> upd (updateAt i c) characters ]
		, OnAction (Action "New Character") $ always $
			    editCharacter gDefault{|*|}
			>>* [ OnAction (Action "Save Character") $ hasValue $ \c -> upd (insertAt 0 c) characters ]
		]
	@ \_ -> ()

viewCharacters :: Task ()
viewCharacters = viewSharedInformation (Title "Characters") [] characters @ \_ -> ()

withIndexes :: ([a] -> [(Int,a)])
withIndexes = zip2 [0..]
