;This ratings simulation is intended in 3 stages:
;1. A population of players reaches a steady state
;2. Another poulation is then introduced, with it's own rating disitribution
;3. We see what happens

globals [
  color-range
  newTournament
  tournamentRound
  tLimit ; number of player in each tournament
  maxAge
  initRating ;Initial rating for new players (or to distribute new players around this rating)
  W
  meanAll ;mean rating of all players
  sdAll ;standard deviation for the rating of all players
  pctFemale
  ;learning? ; whether players get learning bonus after a game and winning probabilities are affected by past learning
  bonusWidth ; Standard deviation of the laerning bonus distribution
  idealDiff ; Ideally, how much better than you should your oponent be to maximize learning
  maxBonus ; maximum rating bonus from learning
]

turtles-own [ ; Each turtle is a player
  rating ; Elo rating of the player
  sex
  k ; K-factor of the Elo system.
  group ; Current tournament group in which the player is
  learning ; Represents learning
  playing? ; Whether the player has alredy played in the current round or not
  age
  maxAgeOwn
]

to setup
  clear-all
  reset-ticks
  set tLimit 100
  set pctFemale 0
  set bonusWidth 100
  set idealDiff 100
  set maxBonus 50
  set maxAge 1000
  set initRating 1900
  set newTournament true
  set tournamentRound 1
  set W 0.5
  set color-range 5
  ask patches [
    if random 100 < 100 [
      sprout 1 [
        ifelse random 100 >= pctFemale [ ; Create male player
          set sex "M"
          set rating initRating;precision (random-normal initRating 250) 0
        ]
        [ ; Else, create female player
          set sex "F"
          set rating initRating;precision (random-normal (initRating - 150) 250) 0
        ]
        ; These next are for all players
        set color getColor rating sex
        set k get-k-factor rating
        set learning 0
        set group 1
        set playing? false
        set maxAgeOwn random maxAge
        set age 1;
        set size 1
        set shape "square"
      ]
    ]
  ]
  update-plots
  ;arrange-groups

end

to go
  ask turtles [ set playing? false]
  if tournamentRound >= tLimit - 1 [
    set tournamentRound 1
    arrange-groups
  ]
  play-games
  set tournamentRound tournamentRound + 1
  tick
end

to play-games
  ask turtles [
    if not playing? [
      set playing? true ;player 1 is now busy during this tick
      ifelse tournaments? [; Playing is done in tournaments. Players only play against an oponent in the same tournament group
      ask turtle-set one-of turtles with ([group = [group] of myself and who != [who] of myself and not playing?] ) [ ; ask another player in the same tournament. This will be player 2
        ;The 'turtle-set' part is in case there are none, because of the 'expected agentset but got nobody' error in Netlogo
        set playing? true ;player 2 is now busy during this tick

        let rating1 [rating] of myself
        let rating2 rating
        let k1 [k] of myself
        let k2 k
        let age1 [age] of myself
        let age2 age

        ;calculate the learning bonuses that players 1 and 2 will get for this game. This bonus is independent of the result
        ;let bonus1 maxBonus * (1 / (sqrt(2 * pi) * sdBonus)) * exp(-((rating - (([rating] of myself) + idealDiff)) ^ 2) / (2 * (sdBonus ^ 2))) ; bonus for the asker (myself)
        ;let bonus2 maxBonus * (1 / (sqrt(2 * pi) * sdBonus)) * exp(-((([rating] of myself) - (rating + idealDiff)) ^ 2) / (2 * (sdBonus ^ 2))) ; bonus for the player being asked
        let bonus1 getBonus rating1 k1 age1 rating2
        let bonus2 getBonus rating2 k2 age2 rating1

        ;Simulate a game between the two players
        let result1 gameResult rating1 [learning] of myself rating2 learning ; game result from the perspective of player 1 (asker): 1 for a win and 0 for a loss
        let result2 1 - result1 ;game result from the perspective of player 2

        ;update player 2 variables
        set rating newElo rating2 rating1 k result2
        if k > 10 [ set k get-k-factor rating] ; Only check to update development coefficient of player for ratings under 2400
        if learning? [set learning learning + bonus2 ]
        set color getColor rating sex
        ask myself [
          ;update player 1 variables
          set rating newElo rating1 rating2 k result1
          if k > 10 [ set k get-k-factor rating] ; Only check to update development coefficient of player for ratings under 2400
          if learning? [set learning learning + bonus1]
          set color getColor rating sex
          ]
        ]
      ]
      [ ; NO TOURNAMENTS, players are not playing in tournaments...
        ask turtle-set one-of turtles with [ who != [who] of myself and not playing?] [
          ;The 'turtle-set' part is in case there are none, because of the 'expected agentset but got nobody' error in Netlogo
        set playing? true ;player 2 is now busy during this tick


        let rating1 [rating] of myself
        let rating2 rating
        let k1 [k] of myself
        let k2 k
        let age1 [age] of myself
        let age2 age

        ;calculate the learning bonuses that players 1 and 2 will get for this game. This bonus is independent of the result
        ;let bonus1 maxBonus * (1 / (sqrt(2 * pi) * sdBonus)) * exp(-((rating - (([rating] of myself) + idealDiff)) ^ 2) / (2 * (sdBonus ^ 2))) ; bonus for the asker (myself)
        ;let bonus2 maxBonus * (1 / (sqrt(2 * pi) * sdBonus)) * exp(-((([rating] of myself) - (rating + idealDiff)) ^ 2) / (2 * (sdBonus ^ 2))) ; bonus for the player being asked
        let bonus1 getBonus rating1 k1 age1 rating2
        let bonus2 getBonus rating2 k2 age2 rating1

        ;Simulate a game between the two players
        let result1 gameResult rating1 [learning] of myself rating2 learning ; game result from the perspective of player 1 (asker): 1 for a win and 0 for a loss
        let result2 1 - result1 ;game result from the perspective of player 2

        ;update player 2 variables
        set rating newElo rating2 rating1 k result2
        if k > 10 [ set k get-k-factor rating] ; Only check to update development coefficient of player for ratings under 2400
        if learning? [set learning learning + bonus2 ]
        set color getColor rating sex

        ask myself [
          ;update player 1 variables
          set rating newElo rating1 rating2 k result1
          if k > 10 [ set k get-k-factor rating] ; Only check to update development coefficient of player for ratings under 2400
          if learning? [set learning learning + bonus1]
          set color getColor rating sex
          ]
        ]
      ]
      ]
    if renewal? [ ; If player retirement is on, players are renewed after maxAge
      set age age + 1
      if age > maxAgeOwn [ ; Player reaches it's retirment age a new player enters the scene
        set age  1
        if sex = "M" [
          set rating initRating;random-normal initRating 300
        ]
        if sex = "F"[
          set rating initRating;random-normal initRating 300
        ]
        set color getColor rating sex
        set k get-k-factor rating
        set learning 0
        set group 1
        set playing? false
        set age random maxAge;
      ]
    ]
    ]
end


to arrange-groups
  let groupcount 1
  let j 0
  foreach sort-on [rating] turtles [
     player -> ask player [
      set group groupcount
      ;set color 15 + 20 * groupcount
      set j  (j + 1)
      if j >= tLimit [
        set groupcount (groupcount + 1)
        set j  0
      ]
    ]
  ]
end

to-report gameResult [rating1 learning1 rating2 learning2]
  ; Reports the result of the game from the perspective of player 1
  let r1 rating1
  let r2 rating2
  if learning? [
    set r1 r1 + learning1
    set r2 r2 + learning2
  ]
  let wProb1 1 / (1 + 10 ^ ((r2 - r1) / 400))
  ifelse (random 100) / 100 <= wProb1 [
    report 1
  ]
  [
    report 0
  ]
end

to-report newElo [rating1 rating2 k1 result]
  ;Reports the new ELO rating for player 1. result should be the result from the perspective of player 1
  let expected1 1 / (1 + 10 ^ ((rating2 - rating1) / 400))
  report rating1 + k1 * (result - expected1)
end

to-report getBonus [ratingA kA ageA ratingB]
  ;calculates the learning bonus that player 1 gets for playing against player 2
  ;uses parameters maxBOnus, idealDiff, bonusWidth to calculate
  ;multiplies by K of the player geting the bonus so that higher rated players learn less fast
  report maxBonus * (kA / (ageA + 1)) * exp( - ((ratingB - (ratingA + idealDiff)) ^ 2 / (2 * (bonusWidth ^ 2))))
end

to-report winprob [rating1 learning1 rating2 learning2]
  ifelse rating2 - rating1 > 735
  [ report 0]
  [
    ifelse rating2 - rating1 < -735
    [ report 1]
    [
      ; Elo win probability formula for chess. Probability that player with rating1 wins game against player with rating2, both adjusted with their learning value if learning? is on
      ifelse learning? [ report  precision (1 / (1 + (10 ^ (((rating2 + learning2) - (rating1 + learning1)) / 400)))) 2] ; learning affects winning probability
      [report  precision (1 / (1 + (10 ^ (((rating2) - (rating1)) / 400)))) 2] ; learning does not affect winning probability
    ]
  ]
end

to-report get-k-factor [rating1]
  ;Returns the k factor of a player with rating rating1
  ifelse rating1 < 2400 [ report 20 ]
  [ report 10]
end

to-report getColor [rtng s]
  ;Returns the color number of a player with rating rtng and sex s
  let center 2000
  let flatness 300
  if s = "M" [
    report 69 - 7.5 * (exp ((rtng - center) / flatness) / (exp ((rtng - center) / flatness) + 1))
  ]
  if s = "F"[
    report 29 - 7.5 * (exp ((rtng - center) / flatness) / (exp ((rtng - center) / flatness) + 1))
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
235
11
613
390
-1
-1
10.0
1
10
1
1
1
0
1
1
1
-18
18
-18
18
0
0
1
ticks
30.0

BUTTON
21
36
84
69
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
111
36
175
69
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
43
108
175
141
tournaments?
tournaments?
0
1
-1000

SWITCH
45
166
150
199
learning?
learning?
0
1
-1000

SWITCH
45
219
150
252
renewal?
renewal?
0
1
-1000

PLOT
950
20
1150
170
Mean all players
Ticks
Rating
0.0
10.0
1000.0
2500.0
true
false
"" ""
PENS
"mean rating" 1.0 0 -16777216 true "" "plot mean ([rating] of turtles)"

MONITOR
952
182
1031
227
Mean rating
mean ([rating] of turtles)
0
1
11

PLOT
640
10
915
546
Ratings
Rating
NIL
500.0
4000.0
0.0
500.0
false
false
"set-histogram-num-bars 20" ""
PENS
"default" 1.0 1 -16777216 true "" "set-histogram-num-bars 20\nhistogram ([rating] of turtles)"

PLOT
225
406
425
556
Learning
Learning
NIL
0.0
600.0
0.0
10.0
true
false
"set-histogram-num-bars 20" ""
PENS
"default" 1.0 1 -16777216 true "" "set-histogram-num-bars 20\nhistogram ([learning] of turtles)"

MONITOR
928
412
1018
457
Mean learning
mean ([learning] of turtles)
0
1
11

PLOT
927
250
1127
402
Mean learning
Ticks
Learning
0.0
10.0
0.0
600.0
true
false
"" ""
PENS
"mean learning" 1.0 0 -16777216 true "" "plot mean ([learning] of turtles)"

PLOT
22
282
222
432
Learning turtle 1
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot [learning] of (turtle 1)"

MONITOR
32
456
133
501
learning turtle 1
[learning] of (turtle 1)
0
1
11

PLOT
433
402
633
552
Age
NIL
NIL
0.0
1000.0
0.0
10.0
true
false
"set-histogram-num-bars 20" ""
PENS
"default" 1.0 1 -16777216 true "" "set-histogram-num-bars 20\nhistogram [age] of turtles"

MONITOR
1042
182
1109
227
Top 10 All
mean [rating] of max-n-of 10 turtles with [sex = \"M\"] [rating]
0
1
11

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
