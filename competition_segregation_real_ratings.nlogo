extensions [csv array]

globals [
  RATINGS_F_SOURCE ; Source list of female ratings
  RATINGS_M_SOURCE ; Source list of male ratings
  NUM_PLAYERS ; Total number of players in the simulation
  ;WOMEN_FRACTION ; Fraction of the population that is female
  ;TEST_W_FRACTION ; Fraction of the women population that will be given specific characteristics being tested
  MEAN_RATING_MEN ; Initial average rating of male population
  MEAN_RATING_WOMEN ; Initial average rating of female population
  SD_RATING_MEN ; Initial standard deviation of male players' rating distribution
  SD_RATING_WOMEN ; Initial standard deviation of female players' rating distribution
  NUM_PLAYERS_TOURNAMENT ; Number of players participating in a tournament
  ROUNDS ; Total number of rounds that a tournament has
  ;MAX_BENEFIT ; Maximum learning benefit from a game
  ;IDEAL_CHALLENGE ; Ideal rating difference (challenge) from which a player learns the most
  ;BENEFIT_SPREAD ; Spread of the benefit-vs-challenge curve
  NUM_LEARNING_GAMES ; Number of tha past games that are taken into account for total learning
  ;SEGREGATION_PREFERENCE ; For the treatment group of female players, probability they will choose to play in a women's tournament over an open one
  PLAYERS_RETIRE?
  GAMES_TO_RETIRE
]

turtles-own [ ; Each turtle is a player
  rating ; Elo rating of the player
  sex
  group ; Any group we want to assign players to for experimenting purposes
  subgroup ;
  segregationPreference ; In case of female players, probability they will choose to play in a women's tournament over an open one
  k ; K-factor of the Elo system
  learning ; Represents learning
  benefit_history
  current_tournament
  current_tournament_type
  current_tournament_wins
  opponent_history
  has_opponent?
  games_played
  prev_rating
  last_challenge
  positive_of_last_challenge
  last_opponent_sex
  last_opponent_group
]

to setup
  clear-all
  reset-ticks
  set RATINGS_F_SOURCE csv:from-file "ratings_f.csv"
  set RATINGS_M_SOURCE csv:from-file "ratings_m.csv"
;  set NUM_PLAYERS 1369
  ;set WOMEN_FRACTION 0.20
  ;set TEST_W_FRACTION 0.5
  set MEAN_RATING_MEN 2000
  set MEAN_RATING_WOMEN 1850
  set SD_RATING_MEN 200
  set SD_RATING_WOMEN 200
  set NUM_PLAYERS_TOURNAMENT 100
  set ROUNDS 8
  ;set MAX_BENEFIT 50
  ;set IDEAL_CHALLENGE 100
  ;set BENEFIT_SPREAD 80
  set NUM_LEARNING_GAMES 30
;  set SEGREGATION_PREFERENCE 0
  set PLAYERS_RETIRE? false
  set GAMES_TO_RETIRE 1000
  ask patches [
    ifelse random-float 1 <= WOMEN_FRACTION [ ; New female turtle
      sprout 1 [
        set sex "W"
        set rating getStartingRating sex
        ;set color getColor rating sex
        set k get-k-factor rating
        ifelse random-float 1 < TEST_W_FRACTION [ ; Female player will be part of the test group of women
          set segregationPreference SEGREGATION_PREFERENCE ; This is how much she is interested in playing women's tournaments
          set group "test"
        ]
        [
          set segregationPreference 1 ; Probability that a female player chooses to play in a women-only tournament
          set group "segregated"
        ]
        set learning 0 ; Initial total learning
        set last_challenge 0
        set positive_of_last_challenge 0
        set benefit_history [0 0]
        set size 0.9
        set shape "triangle"
        set games_played random GAMES_TO_RETIRE
      ]
    ]
    [ ; Else, new male turtle
      sprout 1 [
        set sex "M"
        set rating getStartingRating sex
        ;set color getColor rating sex
        set k get-k-factor rating
        set segregationPreference 0  ; For redundance. Male players dont- get to choose to play a women-only tournament anyway
        set learning 0 ; Initial total learning
        set last_challenge 0
        set positive_of_last_challenge 0
        set benefit_history [0 0]
        ;set size 0.8
        ;set shape "circle"
        set games_played random GAMES_TO_RETIRE
      ]
    ]
  ]
  ; Mark players for a CONTROL GROUP
  let upper_cut max [rating] of turtles with [group = "test"]
  ask turtles with [sex = "M" and rating < upper_cut] [
    set group "control"
  ]
  ask turtles with [group = "control" and rating > upper_cut - 200] [
    set subgroup "top control"
  ]
  ask turtles with [group = "test" and rating > upper_cut - 200] [
    set subgroup "top test"
  ]
  ask turtles with [group = "segregated" and rating > upper_cut - 200] [
    set subgroup "top segregated"
  ]
  update-plots
end

to go
  assign-tournaments
  play-tournaments
  if PLAYERS_RETIRE? [
   renew-players
  ]
  tick
end

to assign-tournaments
 let i 1
 let j 1
 let w_tournament_index 1
 let w_counter 0
 let o_tournament_index 1
 let o_counter 0
 foreach reverse sort-on [rating] turtles [
    player -> ask player [
      set current_tournament_wins 0
      set opponent_history []
      set has_opponent? false
      ifelse sex = "W" [
        ; Player is female, so we check her preference for segregation before assigning her a tournament
        ifelse random-float 1 <= segregationPreference [
          ; assign W player to women's tournament
          set current_tournament w_tournament_index
          set current_tournament_type "women's"
          set w_counter w_counter + 1 ; 1 more player in the current women tournament
          if w_counter = NUM_PLAYERS_TOURNAMENT [
            set w_tournament_index w_tournament_index + 1 ; Create new women tournament
            set w_counter 0 ; new female tournament has zero players
          ]
        ]
        [ ; assign W player to open tournament
          set current_tournament o_tournament_index
          set current_tournament_type "open"
          set o_counter o_counter + 1 ; One more player in the current open tournament
          if o_counter = NUM_PLAYERS_TOURNAMENT [
            set o_tournament_index o_tournament_index + 1 ; Create new open tournament
            set o_counter 0 ; New open tournament has zero players
          ]
        ]
      ]
      [ ; player is not female, so we assign him to an open tournament
        set current_tournament o_tournament_index
        set current_tournament_type "open"
        set o_counter o_counter + 1 ; One more player in the current open tournament
        if o_counter = NUM_PLAYERS_TOURNAMENT [
          set o_tournament_index o_tournament_index + 1 ; Create new open tournament
          set o_counter 0 ; New open tournament has zero players
        ]
      ]
    ]
  ]
  ; If the last women's tournament is too small, join the two last women's tournaments
  if w_counter < NUM_PLAYERS_TOURNAMENT / 2 [
    ask turtles with [ (current_tournament = w_tournament_index) and (current_tournament_type = "women's") ] [
      set current_tournament current_tournament - 1
    ]
  ]
  ; If the last open tournament is too small, join the two last open tournaments
  if o_counter < NUM_PLAYERS_TOURNAMENT / 2 [
    ask turtles with [ (current_tournament = o_tournament_index) and (current_tournament_type = "open") ] [
      set current_tournament current_tournament - 1
    ]
  ]
end

to play-tournaments
  let w_tournaments max [current_tournament] of turtles with [current_tournament_type = "women's"]
  let o_tournaments max [current_tournament] of turtles with [current_tournament_type = "open"]
  foreach n-values w_tournaments [ i -> i + 1] [ ; play every women's tournament
   num_tournament -> play-one-tournament num_tournament "women's"
  ]
  foreach n-values o_tournaments [ i -> i + 1] [ ; play each open tournament
  num_tournament -> play-one-tournament num_tournament "open"
  ]
end

to play-one-tournament [num_tournament tournament_type]
  let round_number 1
  let players []
  while [round_number <= ROUNDS] [
    ifelse round_number = 1 [
      set players reverse sort-on [rating] turtles with [ (current_tournament = num_tournament) and (current_tournament_type = tournament_type) ]
    ]
    [
      set players reverse sort-on [current_tournament_wins] turtles with [ (current_tournament = num_tournament) and (current_tournament_type = tournament_type) ]
    ]
    foreach players [player -> ask player [set has_opponent? false]]
    if remainder length players 2 = 1 [
      ask last players [
        set has_opponent? true
        set current_tournament_wins current_tournament_wins + 1
        set opponent_history lput nobody opponent_history
;        show round_number
;        show players
;        foreach players [player -> ask player [show word current_tournament_wins opponent_history]]
      ]
      ;set players but-last players
    ]
    let tournament_size length players
    let i 0
    let playerA nobody
    let playerB nobody
    while [i < (tournament_size - 1)] [;show i show item i players
      ask item i players [
        let fallback nobody
        let fallback_index -1
        ifelse not has_opponent? [
          set playerA item i players
          let j i + 1
;          show item j players show j
          let checked? false
          let opponent_found? false
          while [ (not opponent_found?) and (j < tournament_size) ] [
;            show "playerA"
;            show playerA
;            show "looking at"
;            show item j players
            if not [has_opponent?] of item j players [
              ifelse member? playerA [opponent_history] of item j players [ ; Player A is already in the history of player B
                if fallback = nobody [
                  set fallback item j players
                  set fallback_index j
                ]
                if not checked? [
                  set i j
                  set checked? true
                ]
              ]
              [ ; Player A is not in player B's history
                if not checked? [
                  set i j + 1
                ]
                set playerB item j players
                ; PLAY GAME
;                show (word "round: " round_number "   i: " i "   j: " j "   checked?: "checked?)
;                show players
;                foreach players [player -> ask player [show word current_tournament_wins opponent_history]]
                play-game playerA playerB
                set opponent_found? true
                set has_opponent? true
                set opponent_history lput playerB opponent_history
                ;set color getColor rating sex
                ask item j players [
                  set has_opponent? true
                  set opponent_history lput playerA opponent_history
                  ;set color getColor rating sex
                ]
                ;wait 0.03
              ]
            ]
            set j j + 1
          ]
          if not opponent_found? and (j >= tournament_size) [
            set i fallback_index + 1
            set playerB fallback
            ; PLAY GAME
;            show (word "round: " round_number "   i: " i "   j: " j "   checked?: "checked?)
;            show players
;            foreach players [player -> ask player [show word current_tournament_wins opponent_history]]
            play-game playerA playerB
;            if fallback = nobody [inspect playerA show [opponent_history] of playerA show i]
            set opponent_found? true
            set has_opponent? true
            set opponent_history lput playerB opponent_history
            ;set color getColor rating sex
            ask fallback [
              set has_opponent? true
              set opponent_history lput playerA opponent_history
              ;set color getColor rating sex
            ]
          ]
        ]
        [ set i i + 1 ] ; else, player already has opponent, move pointer in list
      ]
    ]
    set round_number round_number + 1
  ]
end

to play-game [playerA playerB]
  if playerB = nobody [inspect playerA show playerA]
  let I_A [rating] of playerA + mean ([benefit_history] of playerA)
  let I_B [rating] of playerB + mean ([benefit_history] of playerB)
  let benefit_A precision (MAX_BENEFIT * exp (-(( ([rating] of playerB - [rating] of playerA - IDEAL_CHALLENGE) / BENEFIT_SPREAD ) ^ 2)) ) 3
  let benefit_B precision (MAX_BENEFIT * exp (-(( ([rating] of playerA - [rating] of playerB - IDEAL_CHALLENGE) / BENEFIT_SPREAD ) ^ 2)) ) 3
  ;show (word [rating] of playerA "  " [rating] of playerB "  " benefit_A)
  ; FALTA agregar el learning a la historia

  ask playerA [
    set prev_rating rating
    set last_challenge [rating] of playerB - rating
    ifelse last_challenge > 0 [
      set positive_of_last_challenge last_challenge
    ]
    [ ;last game was not a positive challege
      set positive_of_last_challenge 0
    ]
    set last_opponent_sex [sex] of playerB
    set last_opponent_group [group] of playerB
  ]
  ask playerB [
    set prev_rating rating
    set last_challenge [rating] of playerA - rating
    ifelse last_challenge > 0 [
      set positive_of_last_challenge last_challenge
    ]
    [ ;last game was not a positive challege
      set positive_of_last_challenge 0
    ]
    set last_opponent_sex [sex] of playerA
    set last_opponent_group [group] of playerA
  ]

  ; FALTA incluir el aprendizaje en la predicción del desarrollo de la partida

  let expected_A precision (1 / (1 + (10 ^ ((I_B - I_A) / 400)))) 2 ; This is the expected score in the game taking learning into account
  let expected_A_fide precision (1 / (1 + (10 ^ ((([rating] of playerB) - ([rating] of playerA)) / 400)))) 2 ; This is the expected score for A in the game as seen from FIDE: only rating difference taken into account, no learning
  ifelse random-float 1 <= expected_A [ ;player A won (this takes into account learning benefits)
    ask playerA [
      set rating rating + k * ( 1 - expected_A_fide ) ; new rating only depends on game result and rating differences, not learning
      set current_tournament_wins current_tournament_wins + 1
      set benefit_history lput benefit_A benefit_history
      if length benefit_history > NUM_LEARNING_GAMES [
        set benefit_history but-first benefit_history
      ]
      set games_played games_played + 1
      set k get-k-factor rating
    ]
    ask playerB [
      set rating rating + k * ( 0 - (1 - expected_A_fide) ) ; new rating only depends on game result and rating differences, not learning
      set benefit_history lput benefit_B benefit_history
      if length benefit_history > NUM_LEARNING_GAMES [
        set benefit_history but-first benefit_history
      ]
      set games_played games_played + 1
      set k get-k-factor rating
    ]
  ]
  [ ; player B won
    ask playerA [
      set rating rating + k * ( 0 - expected_A_fide ) ; new rating only depends on game result and rating differences, not learning
      set benefit_history lput benefit_A benefit_history
      if length benefit_history > NUM_LEARNING_GAMES [
        set benefit_history but-first benefit_history
      ]
      set games_played games_played + 1
      set k get-k-factor rating
    ]
    ask playerB [
      set rating rating + k * ( 1 - (1 - expected_A_fide) ) ; new rating only depends on game result and rating differences, not learning
      set current_tournament_wins current_tournament_wins + 1
      set benefit_history lput benefit_B benefit_history
      if length benefit_history > NUM_LEARNING_GAMES [
        set benefit_history but-first benefit_history
      ]
      set games_played games_played + 1
      set k get-k-factor rating
    ]
  ]
end

to-report get-k-factor [rating1]
  ; Returns the k factor of a player with rating rating1
  ifelse rating1 < 2400 [ report 20 ]
  [ report 10]
end

to-report getColor [rtng s]
  ; Returns the color number of a player with rating rtng and sex s
  let center 2000
  let flatness 300
  if s = "M" [
    report 61 + 9 * (exp ((rtng - center) / flatness) / (exp ((rtng - center) / flatness) + 1))
  ]
  if s = "W"[
    report 21 + 9 * (exp ((rtng - center) / flatness) / (exp ((rtng - center) / flatness) + 1))
  ]
end

to-report getStartingRating [playerSex]
  ; Returns the starting rating of a new player
  let playerRating 0
  ifelse playerSex = "W" [
    ; Randomly choose an index based on the length of ratings list
    let index one-of range length RATINGS_F_SOURCE
    ; Set turtle's rating choosing that intex from the ratings list
    set playerRating item 0 item index RATINGS_F_SOURCE
    ; Remove the indexed value from list (sample without replacement)
    set RATINGS_F_SOURCE remove-item index RATINGS_F_SOURCE
    report playerRating
  ]
  [ ;else, new male turtle
    ; Randomly choose an index based on the length of ratings list
    let index one-of range length RATINGS_M_SOURCE
    ; Set turtle's rating choosing that intex from the ratings list
    set playerRating item 0 item index RATINGS_M_SOURCE
    ; Remove the indexed value from list (sample without replacement)
    set RATINGS_M_SOURCE remove-item index RATINGS_M_SOURCE
    report playerRating
  ]
end

to renew-players
  ask turtles with [games_played > GAMES_TO_RETIRE] [
    let ownSex sex
    ;set rating [rating] of one-of turtles with [sex = [sex] of myself]
    ;set rating mean [rating] of turtles with [sex = [sex] of myself and group = [group] of myself]
    let rating_mean mean [rating] of turtles with [sex = [sex] of myself and group = [group] of myself]
    let sd standard-deviation [rating] of turtles
    set rating precision (random-normal rating_mean 220) 0
    set games_played 0
    set color getColor rating sex
    set k get-k-factor rating
    set learning 0 ; Initial total learning
    set benefit_history [0 0]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
81
13
449
382
-1
-1
8.0
1
10
1
1
1
0
1
1
1
0
44
0
44
0
0
1
ticks
30.0

BUTTON
5
36
69
69
Setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
5
88
69
123
Go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

PLOT
472
13
790
201
Ratings distribution
Rating
NIL
500.0
3500.0
0.0
190.0
true
true
"" ""
PENS
"M" 1.0 1 -10899396 true "" "set-histogram-num-bars 20\nhistogram ([rating] of turtles with [sex = \"M\"])"
"W - seg." 1.0 1 -955883 true "" "set-histogram-num-bars 20\nhistogram ([rating] of turtles with [sex = \"W\" and group = \"segregated\"])"
"W - test" 1.0 1 -13345367 true "" "set-histogram-num-bars 20\nhistogram ([rating] of turtles with [sex = \"W\" and group = \"test\"])"

PLOT
471
401
793
549
Learning at the top
NIL
NIL
0.0
40.0
0.0
15.0
true
true
"" ""
PENS
"Top M" 1.0 0 -13840069 true "" "plotxy ticks mean [mean benefit_history] of max-n-of 20 turtles with [sex = \"M\"] [rating]"
"Top W seg." 1.0 0 -955883 true "" "plotxy ticks mean [mean benefit_history] of max-n-of 20 turtles with [sex = \"W\" and group = \"segregated\"] [rating]"
"Top W test" 1.0 0 -13345367 true "" "plotxy ticks mean [mean benefit_history] of max-n-of 20 turtles with [sex = \"W\" and group = \"test\"] [rating]"
"Top W" 1.0 0 -1184463 true "" "plotxy ticks mean [mean benefit_history] of max-n-of 20 turtles with [sex = \"W\"] [rating]"

PLOT
816
11
1134
196
Mean rating
NIL
NIL
0.0
10.0
1500.0
2500.0
true
true
"" ""
PENS
"M" 1.0 0 -13840069 true "" "plot mean [rating] of turtles with [sex = \"M\"]"
"W segregated" 1.0 0 -955883 true "" "plot mean [rating] of turtles with [sex = \"W\" and group = \"segregated\"]"
"W test" 1.0 0 -13345367 true "" "plot mean [rating] of turtles with [sex = \"W\" and group = \"test\"]"
"All" 1.0 0 -16777216 true "" "plot mean [rating] of turtles"

PLOT
815
208
1135
386
Mean rating of Top-20
NIL
NIL
0.0
10.0
2200.0
2500.0
true
true
"" ""
PENS
"Men" 1.0 0 -13840069 true "" "plotxy ticks mean [rating] of max-n-of 20 turtles with [sex = \"M\"] [rating]"
"Women (seg,)" 1.0 0 -955883 true "" "plotxy ticks mean [rating] of max-n-of 20 turtles with [sex = \"W\" and group = \"segregated\"] [rating]"
"Women (test)" 1.0 0 -13345367 true "" "plotxy ticks mean [rating] of max-n-of 20 turtles with [sex = \"W\" and group = \"test\"] [rating]"

SLIDER
52
399
411
432
SEGREGATION_PREFERENCE
SEGREGATION_PREFERENCE
0
1
0.0
0.1
1
NIL
HORIZONTAL

PLOT
471
208
792
390
Difference of means of the top 20
NIL
NIL
0.0
10.0
-180.0
180.0
true
true
"" ""
PENS
"W test - M" 1.0 0 -13345367 true "" "plotxy ticks (mean [rating] of max-n-of 20 turtles with [sex = \"W\" and group = \"test\"] [rating] - mean [rating] of max-n-of 20 turtles with [sex = \"M\"] [rating])"
"W seg. -  M" 1.0 0 -955883 true "" "plotxy ticks (mean [rating] of max-n-of 20 turtles with [sex = \"W\" and group = \"segregated\"] [rating] - mean [rating] of max-n-of 20 turtles with [sex = \"M\"] [rating])"
"Zero" 1.0 0 -7500403 true "" "plotxy ticks 0"

PLOT
817
397
1133
548
Participation in the top 20
NIL
NIL
0.0
10.0
0.0
20.0
true
true
"" ""
PENS
"W (test)" 1.0 0 -13345367 true "" "plotxy ticks count (max-n-of 20 turtles [rating]) with [group = \"test\"]"
"W (seg.)" 1.0 0 -955883 true "" "plotxy ticks count (max-n-of 20 turtles [rating]) with [group = \"segregated\"]"
"M" 1.0 0 -13840069 true "" "plotxy ticks count (max-n-of 20 turtles [rating]) with [sex = \"M\"]"

SLIDER
19
446
151
479
MAX_BENEFIT
MAX_BENEFIT
0
200
200.0
10
1
NIL
HORIZONTAL

SLIDER
315
448
462
481
IDEAL_CHALLENGE
IDEAL_CHALLENGE
0
300
50.0
10
1
NIL
HORIZONTAL

SLIDER
163
447
303
480
BENEFIT_SPREAD
BENEFIT_SPREAD
0
200
200.0
10
1
NIL
HORIZONTAL

SLIDER
50
492
222
525
WOMEN_FRACTION
WOMEN_FRACTION
0
1
0.08
0.01
1
NIL
HORIZONTAL

SLIDER
242
494
414
527
TEST_W_FRACTION
TEST_W_FRACTION
0
1
0.5
0.05
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <steppedValueSet variable="TOURNAMENT_PREFERENCE_TREATMENT_GROUP" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="SEGREGATION_PREFERENCE" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <enumeratedValueSet variable="BENEFIT_SPREAD">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IDEAL_CHALLENGE">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_BENEFIT">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WOMEN_FRACTION">
      <value value="0.67"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TEST_W_FRACTION">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="SEGREGATION_PREFERENCE" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="SEGREGATION_PREFERENCE_small" repetitions="12" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <enumeratedValueSet variable="BENEFIT_SPREAD">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IDEAL_CHALLENGE">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_BENEFIT">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SEGREGATION_PREFERENCE">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SEGREGATION_PREFERENCE" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <enumeratedValueSet variable="BENEFIT_SPREAD">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IDEAL_CHALLENGE">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_BENEFIT">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WOMEN_FRACTION">
      <value value="0.67"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TEST_W_FRACTION">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="SEGREGATION_PREFERENCE" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="SEGREGATION_W_FRACTION_2010_source" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <enumeratedValueSet variable="BENEFIT_SPREAD">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IDEAL_CHALLENGE">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_BENEFIT">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="WOMEN_FRACTION" first="0.1" step="0.1" last="0.7"/>
    <enumeratedValueSet variable="TEST_W_FRACTION">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="SEGREGATION_PREFERENCE" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="PARAMETER_TEST_2010_source" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <enumeratedValueSet variable="BENEFIT_SPREAD">
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IDEAL_CHALLENGE">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_BENEFIT">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="WOMEN_FRACTION" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="TEST_W_FRACTION">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="SEGREGATION_PREFERENCE" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="PARAMETER_TEST_B100_I50_2010_source" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <enumeratedValueSet variable="BENEFIT_SPREAD">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IDEAL_CHALLENGE">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_BENEFIT">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="WOMEN_FRACTION" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="TEST_W_FRACTION">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="SEGREGATION_PREFERENCE" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="PARAMETER_BASE_2010_source" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <enumeratedValueSet variable="BENEFIT_SPREAD">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
      <value value="250"/>
      <value value="300"/>
      <value value="350"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IDEAL_CHALLENGE">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
      <value value="250"/>
      <value value="300"/>
      <value value="350"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_BENEFIT">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
      <value value="250"/>
      <value value="300"/>
      <value value="350"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WOMEN_FRACTION">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TEST_W_FRACTION">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SEGREGATION_PREFERENCE">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="CHALLENGE_BASE_2010_BS200_MB200" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>mean [rating] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [rating] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <metric>mean [last_challenge] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [last_challenge] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [last_challenge] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <metric>mean [positive_of_last_challenge] of max-n-of 20 turtles with [sex = "M"] [rating]</metric>
    <metric>mean [positive_of_last_challenge] of max-n-of 20 turtles with [group = "segregated"] [rating]</metric>
    <metric>mean [positive_of_last_challenge] of max-n-of 20 turtles with [group = "test"] [rating]</metric>
    <metric>mean [rating] of turtles with [sex = "M"]</metric>
    <metric>mean [rating] of turtles with [group = "segregated"]</metric>
    <metric>mean [rating] of turtles with [group = "test"]</metric>
    <enumeratedValueSet variable="BENEFIT_SPREAD">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IDEAL_CHALLENGE">
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
      <value value="250"/>
      <value value="300"/>
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_BENEFIT">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WOMEN_FRACTION">
      <value value="0.08"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TEST_W_FRACTION">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="SEGREGATION_PREFERENCE" first="0" step="0.1" last="1"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
