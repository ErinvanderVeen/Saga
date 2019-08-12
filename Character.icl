implementation module Character

import iTasks

editCharacter :: Character -> Task Character
editCharacter c = updateInformation (Title "Test") [UpdateUsing toTuple (const fromTuple) characterEditor] c
where
	characterEditor = tabset4 gEditor{|*|} gEditor{|*|} gEditor{|*|} gEditor{|*|}

toTuple :: Character -> (Characteristics, SkillStats, CombatStats, RoleplayStats)
toTuple {characteristics, skills, combat, roleplay} = (characteristics, skills, combat, roleplay)

fromTuple :: (Characteristics, SkillStats, CombatStats, RoleplayStats) -> Character
fromTuple (ch, s, c, r) = {Character | characteristics = ch, skills = s, combat = c, roleplay = r}

derive class iTask Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats, Character
derive gDefault Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats, Character
