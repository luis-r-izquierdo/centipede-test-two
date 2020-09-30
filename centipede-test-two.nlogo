;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; centipede-test-two
;; centipede-test-two is a model designed to analyse
;; the “Test two, choose the better” rule in the centipede game.
;; Copyright (C) 2020 Segismundo S. Izquierdo & Luis R. Izquierdo
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Contact information:
;; Luis R. Izquierdo
;;   University of Burgos, Spain.
;;   e-mail: lrizquierdo@ubu.es


;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals [
  pop-1-agents
  pop-2-agents

  pop-1-payoff-matrix
  pop-2-payoff-matrix

  pop-1-n-of-strategies
  pop-2-n-of-strategies

  pop-1-strategy-numbers
  pop-2-strategy-numbers

  list-of-outcomes          ;; only for displaying purposes
  n-of-players-in-each-pop  ;; this number never changes during a simulation
]

breed [players player]

breed [outcomes outcome] ;; only for displaying purposes

players-own [
  strategy      ;; an integer >= 1
  next-strategy ;; to model synchronous revision

  my-pop-payoff-matrix
  my-pop-strategy-numbers

  the-other-pop ;; this agentset is useful to select the counterparts
  the-other-pop-strategy-numbers
]


;;;;;;;;;;;;;
;;; SETUP ;;;
;;;;;;;;;;;;;

to startup

  clear-all

  setup-globals
  setup-world
  setup-payoffs
  setup-agents

  reset-ticks
  setup-graphs
  update-graphs

end


to setup-globals

  set pop-1-n-of-strategies int ((n-of-nodes + 3) / 2)
  set pop-2-n-of-strategies int ((n-of-nodes + 2) / 2)

  set pop-1-strategy-numbers (range 1 (pop-1-n-of-strategies + 1))
  set pop-2-strategy-numbers (range 1 (pop-2-n-of-strategies + 1))

  set n-of-trials min (list n-of-trials n-of-agents-in-each-pop)
  set n-of-players-in-each-pop n-of-agents-in-each-pop

end


to setup-world

  resize-world 0 (2 * n-of-nodes + 2) 0 2

  set list-of-outcomes []

  let i 0
  create-outcomes (n-of-nodes + 1) [
    setxy (2 * i + 1) 1

    ifelse i != n-of-nodes
      [ set shape "circle"    ask patch-at 0 -1 [set plabel (i + 1)] ]
      [ set shape "default"   set heading 90 ]

    set color 13 + 7 * (i / n-of-nodes) - 0.01

    set list-of-outcomes lput self list-of-outcomes
    set i i + 1
  ]

end

to setup-payoffs

  let centipede-plus  3
  let centipede-minus 1
  let gain (centipede-plus - centipede-minus)

  set pop-1-payoff-matrix
    map [ i ->
      map [ j ->
        ifelse-value (j >= i) [(i - 1) * gain][(j - 1) * gain - centipede-minus]
      ] pop-2-strategy-numbers
    ] pop-1-strategy-numbers

  set pop-2-payoff-matrix
    map [ i ->
      map [ j ->
        ifelse-value (j >= i + 1) [(i - 1) * gain + centipede-plus][(j - 1) * gain]
      ] pop-1-strategy-numbers
    ] pop-2-strategy-numbers

end


to setup-agents

  create-players n-of-players-in-each-pop [
    set strategy 1 + random pop-1-n-of-strategies
    set next-strategy strategy
      ;; to make sure that if you do not change next-strategy, you keep the same strategy
    set my-pop-payoff-matrix    pop-1-payoff-matrix
    set my-pop-strategy-numbers pop-1-strategy-numbers
    set the-other-pop-strategy-numbers pop-2-strategy-numbers
    set hidden? true
  ]
  set pop-1-agents turtle-set players
    ;; turtle-set is needed to include only the players that exist now

  create-players n-of-players-in-each-pop [
    set strategy 1 + random pop-2-n-of-strategies
    set next-strategy strategy
    set my-pop-payoff-matrix    pop-2-payoff-matrix
    set my-pop-strategy-numbers pop-2-strategy-numbers
    set the-other-pop-strategy-numbers pop-1-strategy-numbers
    set hidden? true
  ]
  set pop-2-agents players with [not member? self pop-1-agents]

  ask pop-1-agents [ set the-other-pop pop-2-agents ]
  ask pop-2-agents [ set the-other-pop pop-1-agents ]

end


;;;;;;;;;;;;
;;;  GO  ;;;
;;;;;;;;;;;;

to go
  ask players with [random-float 1 < prob-revision] [update-next-strategy]
  ask players [set strategy next-strategy]
  tick
  update-graphs
end


;;;;;;;;;;;;;;;;;;;;;;;
;;; UPDATE-STRATEGY ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to update-next-strategy
  ifelse random-float 1 < prob-exp

  [ ;; choose a random strategy
    set next-strategy one-of my-pop-strategy-numbers
  ]

  [ ;; use “Test two, choose the better” rule

    ;; test your current strategy
    let payoff-my-current-strategy payoff-for-strategy strategy

    ;; test another strategy
    let another-strategy one-of (remove strategy my-pop-strategy-numbers)
    let payoff-another-strategy payoff-for-strategy another-strategy

    set next-strategy ifelse-value payoff-another-strategy > payoff-my-current-strategy
      [ another-strategy ]
      [ strategy ]
  ]
end

;;;;;;;;;;;;;;;
;;; PAYOFFS ;;;
;;;;;;;;;;;;;;;


to-report payoff-for-strategy [s]
  ;; select n-of-trials counterparts without replacement
  let counterparts n-of n-of-trials the-other-pop

  ;; see your counterparts' strategies
  let counterparts-strategy-counts map [ i -> count counterparts with [strategy = i] ] the-other-pop-strategy-numbers

  ;; retrieve your payoffs, given your population and your strategy
  let my-payoffs item (s - 1) my-pop-payoff-matrix

  ;; report the total payoff
  report sum (map * my-payoffs counterparts-strategy-counts)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  EXPECTED OUTCOME FREQUENCIES  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to-report expected-outcome-frequencies
  let strategy-fract-1 map [i -> count (pop-1-agents with [strategy = i]) / n-of-players-in-each-pop] pop-1-strategy-numbers
  let strategy-fract-2 map [i -> count (pop-2-agents with [strategy = i]) / n-of-players-in-each-pop] pop-2-strategy-numbers

  let outcome-frequencies n-values (pop-1-n-of-strategies + pop-2-n-of-strategies - 1) [i -> ifelse-value (i mod 2 = 0)
    [(item (i / 2) strategy-fract-1) * sum (sublist strategy-fract-2 (i / 2) pop-2-n-of-strategies)]
    [(item ((i - 1) / 2) strategy-fract-2) * sum (sublist strategy-fract-1 ((i + 1) / 2) pop-1-n-of-strategies)]
  ]
  report outcome-frequencies
end



;;;;;;;;;;;;;;
;;; GRAPHS ;;;
;;;;;;;;;;;;;;


to setup-graphs

  foreach [1 2] [[pop] ->
    setup-graph (word "Pop. " pop ": Strategy distribution") 1 pop
  ]

  set-current-plot "Expected fraction of matches for each game duration (terminal node)"

  foreach (range (n-of-nodes + 1)) [i ->
    create-temporary-plot-pen (word (i + 1))
    set-plot-pen-mode 1
    set-plot-pen-color 13 + 7 * (i / n-of-nodes) - 0.01
  ]

end


to setup-graph [name mode pop]
  set-current-plot name
  let strategy-numbers (ifelse-value (pop = 1) [pop-1-strategy-numbers][pop-2-strategy-numbers])
  foreach strategy-numbers [ [i] ->
    create-temporary-plot-pen (word i)
    set-plot-pen-mode mode
    set-plot-pen-color 10 * i / (length strategy-numbers) - 0.01
  ]
end


to update-graphs

  set-current-plot "Pop. 1: Strategy distribution"
  plot-frequencies map [ [s] -> count pop-1-agents with [strategy = s] / n-of-players-in-each-pop] pop-1-strategy-numbers

  set-current-plot "Pop. 2: Strategy distribution"
  plot-frequencies map [ [s] -> count pop-2-agents with [strategy = s] / n-of-players-in-each-pop] pop-2-strategy-numbers

  let outcome-frequencies expected-outcome-frequencies

  set-current-plot "Expected fraction of matches for each game duration (terminal node)"
  plot-frequencies outcome-frequencies

  ;; update the view
  (foreach list-of-outcomes outcome-frequencies [ [o f] -> ask o [set size 3 * sqrt f]])

end


to plot-frequencies [freq]
  let bar 1
  foreach n-values length freq [i -> i] [j ->
    set-current-plot-pen (word (1 + j))
    plotxy ticks bar
    set bar (bar - (item j freq))
  ]
  set-plot-y-range 0 1
end
@#$#@#$#@
GRAPHICS-WINDOW
20
47
995
123
-1
-1
22.5
1
20
1
1
1
0
0
0
1
0
42
0
2
0
0
1
ticks
30.0

SLIDER
142
10
356
43
n-of-agents-in-each-pop
n-of-agents-in-each-pop
10
1000
100.0
10
1
NIL
HORIZONTAL

SLIDER
580
10
726
43
prob-revision
prob-revision
0.001
0.1
0.1
0.001
1
NIL
HORIZONTAL

SLIDER
730
10
873
43
prob-exp
prob-exp
0
0.1
0.0
0.001
1
NIL
HORIZONTAL

BUTTON
360
10
426
43
setup
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
429
10
510
43
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
514
10
577
43
NIL
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

SLIDER
876
10
983
43
n-of-trials
n-of-trials
1
n-of-agents-in-each-pop
1.0
1
1
NIL
HORIZONTAL

PLOT
21
130
985
506
Expected fraction of matches for each game duration (terminal node)
Time steps
Fraction (stacked)
0.0
10.0
0.0
1.0
true
true
"" ""
PENS

SLIDER
21
10
140
43
n-of-nodes
n-of-nodes
2
20
20.0
1
1
NIL
HORIZONTAL

PLOT
22
511
486
769
Pop. 1: Strategy distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

PLOT
490
511
985
769
Pop. 2: Strategy distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

@#$#@#$#@
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
NetLogo 6.1.1
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
