implementation module Saga.Character

import iTasks

gEditor{|Character|} = bijectEditorValue toTuple fromTuple characterEditor
where
	characterEditor = tabset4
		(gEditor{|*|} <<@ Title "Characteristics")
		(gEditor{|*|} <<@ Title "Skills")
		(gEditor{|*|} <<@ Title "Combat")
		(gEditor{|*|} <<@ Title "Roleplay")

	toTuple :: Character -> (Characteristics, SkillStats, CombatStats, RoleplayStats)
	toTuple {characteristics, skills, combat, roleplay} = (characteristics, skills, combat, roleplay)

	fromTuple :: (Characteristics, SkillStats, CombatStats, RoleplayStats) -> Character
	fromTuple (ch, s, c, r) = {Character | characteristics = ch, skills = s, combat = c, roleplay = r}

derive gEditor Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats
derive gText Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats, Character
derive JSONEncode Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats, Character
derive JSONDecode Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats, Character
derive gEq Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats, Character
