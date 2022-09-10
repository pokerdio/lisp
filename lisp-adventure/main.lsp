(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (compile-file "util.lsp"))
  (load (compile-file "globals.lsp"))
  (load (compile-file "thing.lsp"))
  (load (compile-file "engine.lsp")))

(make-thing 'pc '(pc) "Just plain old you.")

(block rooms
  (make-thing 'house-ne '(room grass) "You're in the yard of a small house.")
  (make-thing 'house-s '(room) "You're in the yard of a small house. ~
The front door of the house lies to the north. ")
  (make-thing 'house-e '(room) "You're in the yard of a small house. A large oak tree towers above, throwing a pleasant ~
shade. ")
  (make-thing 'house-n '(room grass) "You're behind a small house in the yard. 
There is a small wooden shed here, built against the house, its door missing.")
  (make-thing 'house-w '(room) "You're in the yard of a small house.")
  (make-thing 'house-se '(room) "You're in the yard of a small house.")
  (make-thing 'house-sw '(room) "You're in the yard of a small house.")
  (make-thing 'house-nw '(room grass) "You're in the yard of a small house.")
  (make-thing 'house '(room trapdoor-hidden) "You're inside the only room of the house. It looks much ~
larger than you had expected. The floor is covered by a thick rug.")
  (make-thing 'cellar '(room) "You're inside a small, dark cellar with brick walls ~
lined by empty shelves. Stairs go upward. ")
  (make-thing 'up-tree '(room)  "You're up in the tree at the place the tree trunk splits in multiple branches, forming a wide flat ~
bed in the middle, large enough to have a nap in. Large branches continue to the east and west ~
and are wide enough to walk on. Another turns steeply upward, with enough sub-branches to gain a foothold. ")
  (make-thing 'tree-top '(room) "You're at the highest point you can climb. You're seeing the ~
treetops of an old growth forest in every direction. ")
  (make-thing 'tree-east-branch '(room) "You're up in the tree, amongs lots of branches, standing on a really large one.")
  (make-thing 'tree-west-branch '(room) "You're up in the tree, walking along a really large branch. The branch rests on the ~
roof of the house. The roof is covered by red clay tiles and opens up in a small window. ")
  (make-thing 'house-shed '(room) "You're in a small windowless timber planks shed. The ~
construction is shoddy and some light enters through the boards. There is a work bench ~
and a couple of shelves."))

(block trails
  (trail house-e up up-tree up tree-top)
  (trail tree-east-branch w up-tree w tree-west-branch)  
  (trail-1-way (house-n enter house-shed exit house-n))
  (trail house-s e house-se n house-e n house-ne w house-n w house-nw
         s house-w s house-sw e house-s n house)
  (trail-1-way cellar up house))

(block furniture
  (make-thing 'shed-shelves '(furniture) "A couple of plain planks clumsily fixed to the wall with."
              :owner 'house-shed)
  (make-thing 'yard-oak '(furniture) "A thick old tree, richly foliaged, with a huge bough that splits into thick branches surprisingly low. It looks climbable.")
  (make-thing 'shed-bench '(furniture bench) "A plain four legged work table out of unpolished wood. "
              :owner 'house-shed))




(block pickable
  (make-thing 'scythe '(pickable listable heavy) "A rather large, sharp looking scythe."
              :owner 'house-shed)
  (make-thing 'key '(pickable listable) "A small key."
              :owner (let ((lst '(house-ne house-se house-nw house-sw house-n house-e house-w)))
                       (nth (random (length lst)) lst)))
  (make-thing 'hammer '(pickable listable heavy) "A standard claw hammer."
              :owner 'shed-bench)
  (make-thing 'rug '(listable) "The floor is covered by a black woolen rug painted with bright red and yellow geometric patterns."
              :owner 'house))

(block movement
  (match-coms ((enter shed) (enter) :in house-n)
              (p "You step through the door opening into the shed.")
              (setf *r* 'house-shed))

  (match-coms ((leave shed) (leave) (walk out) (exit) :in house-shed)
              (p "You step through the door opening into the yard.")
              (setf *r* 'house-n))

  (match-coms ((go north) (north) :in house-s :having key
               (go north) (north) :in house-s :room-trait door-unlocked)  
              (if (not (has-trait *r* 'door-unlocked))
                  (progn (p "You unlock the door and enter the house.")
                         (add-trait *r* 'door-unlocked)
						 (setf *r* 'house))
                  (progn 
                    (p "You enter the house.")
                    (setf *r* 'house))))

  (match-coms ((go north) (north) :in house-s)
              (assert (not (has-trait *r* 'door-unlocked)))
              (p "The door is locked."))
  (match-coms ((go down) (down) :in house)
              (if (has-trait *r* 'trapdoor-revealed)
                  (progn
                    (go-room 'cellar "You climb down the steep stairs. "))
                  (p "You can't go there.")))

  (match-coms ((go (x north west east south down up))
               ((x north west east south down up)))
              (let ((new-room (find-connection x *r*)))
                (if new-room
                    (progn
                      (go-room new-room (cat "You go " x ".~%")))
                    (p "You can't go there."))))

  (match-com (go)
             (p "Go where?")))  

(block locks
  (match-com (unlock door :in house-s :having key)
    (if (not (has-trait *r* 'door-unlocked))
        (progn (p "You unlock the door.")
               (add-trait *r* 'door-unlocked))
        (p "It is already unlocked.")))

  (match-com (lock door :in house-s :having key)
    (if (has-trait *r* 'door-unlocked)
        (p "It wouldn't do any good.")
        (p "It is already locked.")))

  (match-coms ((lock x) (unlock x) :having key)
    (p "You can't do that."))

  (match-coms ((lock x) (unlock x))
    (p "You don't have a key.")))

(block inventory
  (match-com (take x :dasein x pickable)
    (del-thing x *r*)
    (add-to-thing x 'pc)
    (p "You take the " (sym-to-low-str x) "."))

  (match-com (take x)
    (p "You can't take that."))

  (match-com (drop x :having x)
    (del-thing x 'pc)
    (add-to-thing x *r*)
    (p "You drop the " (sym-to-low-str x) "."))

  (match-com (drop x)
    (p "You don't have that."))

 
  (match-coms ((inventory) (look inventory) (check inventory) :having x)
    (format t "Your stuff is ~{~A, ~}~A."
            (mapcar #'sym-to-low-str
                    (butlast (thing-contents 'pc)))
            (sym-to-low-str (car (last (thing-contents 'pc))))))
  (match-coms ((inventory) (look inventory) (check inventory)) ; having nothing
    (p "You don't have anything. "))) 

(block examine
  (match-com (look)
    (p (thing-desc *r*)))


  (match-coms ((look x :all-things x)
               (look y :all-things x foo y))
    (p (thing-desc x)))

  (match-com (look x)
	(p "Can't see that here.")))

(block room-house
 (match-com ((push move turn flip) rug :in house :room-trait trapdoor-hidden)
   (p "You push the rug, revealing a small trapdoor. ")
   (trail-1-way house down cellar)
   (add-trait *r* 'trapdoor-revealed)
   (del-trait *r* 'trapdoor-hidden)

   (match-com ((open close) trapdoor :room-trait trapdoor-revealed)
			  (p "To take advantage of the trapdoor, just \"go down\"."))))



(match-coms (() (x) (x y) (x y z) (x y z u) (x y z u v) )
  (p "Huh?"))
