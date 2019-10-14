module House (rhyme) where

verbs :: [String]
verbs =   ["that ate", "that killed", "that worried", "that tossed", "that milked",
           "that kissed", "that married", "that woke", "that kept", "that belonged to"]

subjects :: [String]
subjects =  ["the malt", "the rat", "the cat", "the dog",
             "the cow with the crumpled horn",
             "the maiden all forlorn",
             "the man all tattered and torn",
             "the priest all shaven and shorn",
             "the rooster that crowed in the morn",
             "the farmer sowing his corn",
             "the horse and the hound and the horn"]


verse :: String -> Int -> String
verse _ 0 = "This is the house that Jack built.\n"
verse poem index = poem <> "\n"
                        <> concatMap verse' (verbSubjects index)
                        <> "that lay in the house that Jack built.\n"
    where
        verse' (v, s) = v <> " " <> s <> "\n"
        -- feels wrong reversing the vebs and subjects every iteration
        verbSubjects i = zip ("This is" : (reverse . take (i - 1)) verbs) ((reverse . take i) subjects)

rhyme :: String
rhyme = foldl verse "" [0..(length subjects)]
