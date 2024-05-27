(deftemplate node
  (slot name)
  (slot question)
  (slot B)
  (slot E)
  (slot A)
  (slot C)
  (slot R)
  (slot next-question)
)

(deftemplate typeValue
  (slot typeName)
  (slot value)
  (slot vacationType)
)

(deffacts initial
 (node (name Q1) (question "Question 1") (B "Beach") (E "Exploration") (A "Adventure") (C "Cultural") (R "Pampering") (next-question Q2))
 (node (name Q2) (question "Question 2") (B "Relaxing") (E "Historical") (A "Thrilling") (C "Educational") (R "Rejuvenating") (next-question Q3))
 (node (name Q3) (question "Question 3") (B "Sunny") (E "Nature") (A "Extreme Sports") (C "Museums") (R "Tranquil") (next-question Q4))
 (node (name Q4) (question "Question 4") (B "Tropical") (E "Urban Exploration") (A "Hiking") (C "Art") (R "Serene") (next-question Q5))
 (node (name Q5) (question "Question 5") (B "Resort") (E "Backpacking") (A "Rafting") (C "Food and Wine") (R "Yoga Retreat") (next-question Q6))
 (node (name Q6) (question "Question 6") (B "Spa") (E "Mountains") (A "Bungee Jumping") (C "Festivals") (R "Meditation") (next-question Q7))
 (node (name Q7) (question "Question 7") (B "Cruise") (E "Deserts") (A "Skydiving") (C "Theater") (R "Hot Springs") (next-question Q8))
 (node (name Q8) (question "Question 8") (B "Island Hopping") (E "Jungle Trek") (A "Rock Climbing") (C "Historical Sites") (R "Hammock Time") (next-question Q9))
 (node (name Q9) (question "Question 9") (B "Luxury") (E "Camping") (A "Diving") (C "Architectural Tours") (R "Slow Travel") (next-question Q10))
 (node (name Q10) (question "Question 10") (B "Sunbathing") (E "Wildlife Safari") (A "Skiing") (C "Cultural Events") (R "Unplugging") (next-question nil))

 (typeValue (typeName B) (value 0) (vacationType "Beach Lover"))
 (typeValue (typeName E) (value 0) (vacationType "Explorer"))
 (typeValue (typeName A) (value 0) (vacationType "Adventurer"))
 (typeValue (typeName C) (value 0) (vacationType "Culture Enthusiast"))
 (typeValue (typeName R) (value 0) (vacationType "Relaxation Seeker")) 
 (currentQ Init)
)

(defrule InitialPersonList
  ?Q <- (currentQ Init)
=>
  (retract ?Q)
  (assert (currentQ Q1))
)

(deffunction Value-One-Option(?option)
  (printout t ?option " (select 1...5) "crlf) 
  (bind ?answer (read))
  (while (and (integerp ?answer)(or (< ?answer 1) (> ?answer 5))) 
   (printout t ?option " (select 1...5) "crlf) 
   (bind ?answer (read)))
  (return ?answer))

(defrule askQ
  ?f1 <- (currentQ ?currentNode)
  (node (name ?currentNode) (question ?Q) (B ?B) (E ?E) (A ?A) (C ?C) (R ?R)
  (next-question ?nextNode))
  ?fB <- (typeValue (typeName B) (value ?BVal))
  ?fE <- (typeValue (typeName E) (value ?EVal))
  ?fA <- (typeValue (typeName A) (value ?AVal))
  ?fC <- (typeValue (typeName C) (value ?CVal))
  ?fR <- (typeValue (typeName R) (value ?RVal))
=>
  (printout t ?Q ") put the number '5' next to the word that best describes you in that line; a '4' next to the word that describes you next best; a '3' to the next best word, a '2' to the next best word, and a '1' by the word that least describes you in that line" crlf)
  (printout t "___" ?B crlf)
  (printout t "___" ?E crlf)
  (printout t "___" ?A crlf)
  (printout t "___" ?C crlf)
  (printout t "___" ?R crlf) 
  (printout t "*********************" crlf)

  (bind ?BAns (Value-One-Option ?B))
  (bind ?EAns (Value-One-Option ?E))
  (bind ?AAns (Value-One-Option ?A))
  (bind ?CAns (Value-One-Option ?C))
  (bind ?RAns (Value-One-Option ?R))

  (if (neq ?nextNode ?currentNode) then
    (modify ?fB (value (+ ?BVal ?BAns)))
    (modify ?fE (value (+ ?EVal ?EAns)))
    (modify ?fA (value (+ ?AVal ?AAns)))
    (modify ?fC (value (+ ?CVal ?CAns)))
    (modify ?fR (value (+ ?RVal ?RAns)))
  )

  (retract ?f1)
  (assert (currentQ ?nextNode))
)

(defrule findAns
  ?f1 <- (currentQ nil)
  ?fB <- (typeValue (typeName B) (value ?bVal))
  ?fE <- (typeValue (typeName E) (value ?eVal))
  ?fA <- (typeValue (typeName A) (value ?aVal))
  ?fC <- (typeValue (typeName C) (value ?cVal))
  ?fR <- (typeValue (typeName R) (value ?rVal))
=>
  (retract ?f1)
  (assert (currentQ sort))
  (assert (vacationList ?fB ?fE ?fA ?fC ?fR))

  (printout t "Results: " crlf)
  (printout t "Beach Lover: " ?bVal crlf)
  (printout t "Explorer: " ?eVal crlf)
  (printout t "Adventurer: " ?aVal crlf)
  (printout t "Culture Enthusiast: " ?cVal crlf)
  (printout t "Relaxation Seeker: " ?rVal crlf) 
)
