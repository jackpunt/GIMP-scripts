;;; make a full deck
;;; but maybe we want to make 3 copies of just a few sheets
;;; helps to randomize to colors/offsets
;;; 180 = 60 * 3 basic cards: 40*3 + 10*6 PLUS: 6 * 6 of the SITE and 6*1 JACK [37*6 = 222]
;;; 6*(5-Brn,5-Yel,6-Site,1-Jack,1-Order) 6x17 = 102 playing + 6 rules [108 slots]
;;; 3 * 40 = 120: 3 * (18 + 18) + 1* 12 [4 * 3] with six slots left over for leader, bonus token, etc.

;;; 18 per sheet. + 

;;; 5 Yel, 5 Brn, 6 Sites, 1 Jack, 1 Order on a sheet; 6x [108 cards]
;;; 120 left: (3 * 36) + 12 [4 * 3]
;;; 1 * 6 (@ 2.73); 2 * 3 (@2.97); + 1 special (@ 2.97) SHEETS
;;; include 6 of Order of Play, 1-Bonus, Leader!

;; 1. Leader: Lead or Think (end of turn)
;; 2. Others: Follow or Think
;; 3. Each player in turn: 1 action for lead or follow
;; and 1 action for each matching client
;; (resolve effects depth first)
;; When completing building:
;; add site to Influence, then any "On Completion" effects


;;; (+ (* 6 2.73) (* 6 2.97) 2.97) = 37.17
;;; Game mat [camp] 6 @ 2.73 + 9 setup = ~ $27

(define deck #(
#(6 Yellow-014.png Odd-013-Back.png)
#(6 Yellow-015.png Odd-013-Back.png)
#(6 Yellow-016.png Odd-013-Back.png)
#(6 Yellow-017.png Odd-013-Back.png)
#(6 Yellow-018.png Odd-013-Back.png)
#(6 Brown-019.png Odd-013-Back.png)
#(6 Brown-020.png Odd-013-Back.png)
#(6 Brown-021.png Odd-013-Back.png)
#(6 Brown-022.png Odd-013-Back.png)
#(6 Brown-023.png Odd-013-Back.png)
#(3 Blue-021.png  Odd-013-Back.png)
#(3 Blue-022.png  Odd-013-Back.png)
#(3 Blue-023.png Odd-013-Back.png)
#(3 Blue-024.png Odd-013-Back.png)
#(3 Blue-025.png Odd-013-Back.png)
#(3 Blue-026.png Odd-013-Back.png)
#(3 Blue-027.png Odd-013-Back.png)
#(3 Blue-028.png Odd-013-Back.png)
#(3 Blue-029.png Odd-013-Back.png)
#(3 Blue-030.png Odd-013-Back.png)
#(3 Grey-000.png Odd-013-Back.png)
#(3 Grey-001.png Odd-013-Back.png)
#(3 Grey-002.png Odd-013-Back.png)
#(3 Grey-003.png Odd-013-Back.png)
#(3 Grey-004.png Odd-013-Back.png)
#(3 Grey-005.png Odd-013-Back.png)
#(3 Grey-006.png Odd-013-Back.png)
#(3 Grey-007.png Odd-013-Back.png)
#(3 Grey-008.png Odd-013-Back.png)
#(3 Grey-009.png Odd-013-Back.png)
#(3 Purple-031.png Odd-013-Back.png)
#(3 Purple-032.png Odd-013-Back.png)
#(3 Purple-033.png Odd-013-Back.png)
#(3 Purple-034.png Odd-013-Back.png)
#(3 Purple-035.png Odd-013-Back.png)
#(3 Purple-036.png Odd-013-Back.png)
#(3 Purple-037.png Odd-013-Back.png)
#(3 Purple-038.png Odd-013-Back.png)
#(3 Purple-039.png Odd-013-Back.png)
#(3 Purple-040.png Odd-013-Back.png)
#(3 Red-010.png Odd-013-Back.png)
#(3 Red-011.png Odd-013-Back.png)
#(3 Red-012.png Odd-013-Back.png)
#(3 Red-013.png Odd-013-Back.png)
#(3 Red-014.png Odd-013-Back.png)
#(3 Red-015.png Odd-013-Back.png)
#(3 Red-016.png Odd-013-Back.png)
#(3 Red-017.png Odd-013-Back.png)
#(3 Red-018.png Odd-013-Back.png)
#(3 Red-019.png Odd-013-Back.png)

#(6 Odd-000-Jack.png Odd-013-Back.png)
#(6 Odd-001-Stone.png Odd-012-Stone.png)
#(6 Odd-002-Marble.png Odd-011-Marble.png)
#(6 Odd-003-Concrete.png Odd-010-Concrete.png)
#(6 Odd-004-Brick.png Odd-009-Brick.png)
#(6 Odd-005-Wood.png Odd-008-Wood.png)
#(6 Odd-006-Rubble.png Odd-007-Rubble.png)

))
(gimp-message "loaded gtr-deck.scm")
