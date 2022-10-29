(declaim #+sbcl(sb-ext:muffle-conditions cl:style-warning))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (compile-file "util.lsp"))
  (load (compile-file "globals.lsp"))
  (load (compile-file "thing.lsp"))
  (load (compile-file "engine.lsp")))

(make-thing 'pc '(pc) "Just plain old you.")

;; ----------- ROOMS ----------

(block rooms
  (make-thing 'house-ne '(room grass beach) "You're in the yard of a small house. To the north a yellow sanded beach opens up to the sea. In the distance there's a sandy island. ")
  (make-thing 'house-s '(room) "You're in the yard of a small house. ~
The front door of the house lies to the north. ")
  (make-thing 'house-e '(room grass) "You're in the yard of a small house. A large oak ~
tree towers above, throwing a pleasant shade. ")
  (make-thing 'house-n '(room grass beach) "You're behind a small house in the yard. ~
There is a small wooden shed here, built against the house, its door missing. To the north a yellow sanded beach opens up to the sea. In the distance there's a sandy island. ")
  (make-thing 'house-w '(room grass) "You're in the yard of a small house.")
  (make-thing 'house-se '(room grass) "You're in the yard of a small house.")
  (make-thing 'house-sw '(room grass) "You're in the yard of a small house.")
  (make-thing 'house-nw '(room grass beach) "You're in the yard of a small house. To the north a yellow sanded beach opens up to the sea. In the distance there's a sandy island. ")
  (make-thing 'house-attic '(room ladder-up) "You're in a small dusty room with wooden beams and planks, angled ceiling, a window. You see a collapsible ladder. ")
  (make-thing 'house '(room trapdoor-hidden) "You're inside the only room of the house. It looks much ~
larger than you had expected. The floor is covered by a thick rug.")
  (make-thing 'cellar '(room) "You're inside a small, dark cellar with brick walls ~
lined by empty shelves. Stairs go upward and downward. ")
  (make-thing 'up-tree '(room tree)  "You're up in the tree at the place the~
 tree trunk splits in multiple branches, forming a wide flat ~
bed in the middle, large enough to have a nap in. Large branches continue ~
to the east and west and are wide enough to walk on. Another turns steeply ~
upward, with enough sub-branches to gain a foothold. ")
  (make-thing 'tree-top '(room tree) "You're at the highest point you can climb. You're seeing the ~
treetops of an old growth forest in every direction. ")
  (make-thing 'tree-east-branch '(room tree) "You're up in the tree, amongs lots of branches, ~
standing on a really large one. There's a large bird's nest here. ")
  (make-thing 'tree-west-branch '(room tree window-closed window-intact) "You're up in the tree, walking along a thick ~
branch. The branch rests on the roof of the house. The roof is covered by red clay ~
tiles and opens up in a small window. ")
  (make-thing 'house-shed '(room) "You're in a small windowless timber planks shed. The ~
construction is shoddy and some light enters through the boards. There is a work bench ~
and a couple of shelves.")
  (make-thing 'island '(room) "You're on the small sandy island. Golden sand dunes is everything you see. Besides the sea. ")
  (make-thing 'labyrinth '(room (position 0)) "You're in a square room tiled with large slabs of stone. The door you used to enter has closed behind you and there are two exits."))

(block trails
  (trail house-e up up-tree up tree-top)
  (trail house-n south house-shed)
  (trail tree-east-branch w up-tree w tree-west-branch)  
  (trail-1-way (house-n enter house-shed exit house-n))
  (trail house-s e house-se n house-e n house-ne w house-n w house-nw
         s house-w s house-sw e house-s n house)
  (trail-1-way cellar up house)
  (trail-1-way house-s south labyrinth))

;; ----------- THINGS ----------

(block furniture
  (make-thing 'boat '(heavy furniture listable)
              "A small wooden boat.")
  (make-thing 'bird-nest '(furniture contents-visible contents-accessible)
              "The nest's a half meter wide round open topped bed of twigs. "
              :owner 'tree-east-branch)
  (make-thing 'shed-shelves '(furniture) "A couple of plain planks clumsily fixed to the wall with."
              :owner 'house-shed)
  (make-thing 'yard-oak '(furniture) "A large old tree, richly foliaged, with a huge bough that splits into thick branches surprisingly close to the ground. It looks climbable.")
  (make-thing 'shed-bench '(furniture bench contents-visible contents-accessible)
              "A plain four legged work table out of unpolished wood. "
              :owner 'house-shed))

(block key-dog
  (let* ((dog-places '(house-w house-nw house-ne))
         (dog-place (nth (random (length dog-places)) dog-places))
         (key-places '(house-ne house-se house-nw house-sw house-n house-e house-w))
         (safe-key-places (set-difference key-places (list dog-place)))
         (key-place (nth (random (length safe-key-places)) safe-key-places)))
    (case dog-place
      (house-w (add-trait 'house-sw 'growl) (add-trait 'house-nw 'growl))
      (house-nw (add-trait 'house-w 'growl) (add-trait 'house-n 'growl))
      (house-ne (add-trait 'house-n 'growl) (add-trait 'house-e 'growl))
      (t (assert nil () "Bad dog in bad dog place." dog-place)))
    (add-to-thing 'boat
                  (car (set-difference '(house-nw house-ne) (list dog-place))))
    (add-to-thing 'boat 'island) ; the boat is owned by two items, sue me
    (make-thing 'bad-dog '(furniture listable grass-visible) "The surprisingly large dog is growling with rage, and getting closer step by step. "
                :owner dog-place)
    (make-thing 'key '(pickable listable) "A small key." :owner key-place)))

(block pickable
  (make-thing 'metal-egg '(pickable listable) "A polished bronze ovoid, smooth and shiny except for a tiny hole."
              :owner 'bird-nest)
  (make-thing 'scythe '(pickable listable heavy) "A rather large, sharp looking scythe."
              :owner 'house-shed)

  (make-thing 'hammer '(pickable listable heavy) "A standard claw hammer."
              :owner 'shed-bench)
  (make-thing 'rug '(listable) "The floor is covered by a black woolen rug painted with ~
bright red and yellow geometric patterns."
              :owner 'house))


;; ----------- COMMANDS ----------

(block dbg
  (match-com (dbg save)
    (game-loop))

  (match-com (dbg :room-trait (position 0) :in-room labyrinth)
    (p "checks out"))
  (match-com (dbg room) (p "room " *r* " " (type-of *r*)  "~%")))

(block-match labyrinth (:in-room labyrinth) ;on top because it's a place with its custom rules
  (match-com (look :room-trait (position x))
    (p "Hello there! pos is " x))

  (match-coms ((go right) (right) :room-trait (position x))
    (setf x (1+ x))
    (if (> (abs x) 3)
        (progn (p "You leave the labyrinth.")
               (setf *r* 'house-s))
        (p "you move to room number " x)))

  (match-coms ((go left) (left) :room-trait (position x))
    (setf x (1- x))
    (if (> (abs x) 3)
        (progn (p "You leave the labyrinth.")
               (setf *r* 'house-s))
        (p "you move to room number " x)))

  (match-coms ((go (north south west east)) ((north south west east)))
    (p "You seem to have lost a sense of geographical orientation. Standing with the back towards where you came from, you can choose to go left or right. "))
  
  (match-com (q *)
    (p "special quit")
    (continue-command))
  (match-com (*)
    (p "I wouldn't bother doing anything except going, looking without parameters and quitting"))
  (defmethod thing-desc ((lab (eql (get-thing 'labyrinth))))
    (p "Hello there!"))
  )


(block examine
  (match-com (look)
    (p (thing-desc *r*)))

  (match-coms ((look x :having x)
               (look x :dasein x :room-trait ! grass)
               (look x :dasein x grass-visible :room-trait grass)
               (look x :dasein x listable heavy :room-trait grass)
               (look x in y :dasein y contents-visible :inside x y))
    (p (thing-desc x))))

(block-match on-the-beach (:room-trait beach)
  (match-com (look (beach north))
    (p "A clean, picture perfect beach, with clear greenish waves lazily crashing against it. In the distance an island beckons."))
  (match-com ((enter launch) boat :dasein (boat leaking))
    (die "You push the boat in the water and jump in, then start rowing for the island. You notice the boat is taking in water halfway through, about as time as it occurs to you you can't swim."))
  (match-com ((enter launch) boat :dasein (boat ! leaking))
    (p "You push the boat in the water and jump in. Rowing to the island under the summer sun is quite enjoyable.")))

(block room-active-effect
  (match-com (* :dasein bad-dog)
    (die "Before you have a chance to do anything, the large dog attacks you. The fight, if it ~
can be called that, is one sided and does not last long. ~%")))

(block inventory
  (match-com (take x :dasein x pickable)
    (move-thing x *r* 'pc)
    (p "You take the " (tostr x) "."))

  (match-com (take x  (from in) y :thing x pickable  :dasein y contents-accessible :inside x y)
    (move-thing x y 'pc)
    (p "You take the " (tostr x) " from the " (tostr y) "."))

  (match-coms ((take x)
               (take x (from in) y))
    (p "You can't take that."))

  (match-com (drop x :having x)
    (move-thing x 'pc *r*)
    (p "You drop the " (tostr x) "."))

  (match-com (drop x)
    (p "You don't have that."))
 
  (match-coms ((inventory) (look inventory) (check inventory) :having x)
    (format t "Your stuff is ~{~A, ~}~A."
            (mapcar #'tostr
                    (butlast (thing-contents 'pc)))
            (tostr (car (last (thing-contents 'pc))))))
  (match-coms ((inventory) (look inventory) (check inventory)) ; having nothing
    (p "You don't have anything. ")))

(block movement
  (match-coms ((enter shed) (enter) :in-room house-n)
    (p "You step through the door opening into the shed.")
    (setf *r* 'house-shed))

  (match-coms ((leave shed) (leave) (walk out) (exit) :in-room house-shed)
    (p "You step through the door opening into the yard.")
    (setf *r* 'house-n))

  (match-coms ((go north) (north) :in-room house-s :room-trait door-unlocked)
    (p "You enter the house.")
    (setf *r* 'house))
  (match-coms ((go north) (north) :in-room house-s :having key)
    (if (not (has-trait *r* 'door-unlocked))
        (progn (p "You unlock the door and enter the house.")
               (add-trait *r* 'door-unlocked)
               (setf *r* 'house))
        (progn 
          (p "You enter the house.")
          (setf *r* 'house))))

  (match-coms ((go north) (north) :in-room house-s)
    (assert (not (has-trait *r* 'door-unlocked)))
    (p "The door is locked."))
  (match-coms ((go down) (down) :in-room house)
    (if (has-trait *r* 'trapdoor-revealed)
        (progn
          (go-room 'cellar "You climb down the steep stairs. "))
        (p "You can't go there.")))


  (match-coms ((go (x north east west south)) ((x north east west south)) :room-trait tree)
    (if (find-connection x *r*)
        (continue-command)
        (die "You lose your grip and fall to your death. ")))

  (match-coms ((go (x north west east south down up))
               ((x north west east south down up)))
    (let ((new-room (find-connection x *r*)))
      (if new-room
          (progn
            (go-room new-room (cat "You go " x ".~%")))
          (p "You can't go there."))))

  (match-com ((enter launch use) boat :dasein boat)
    (let ((other-place (car (set-difference (get-owner-list 'boat)
                                            (list *r*)))))
      (p "You row, row, row your boat across the sea.")
      (setf *r* other-place)))

  (match-com (go)
    (p "Go where?")))

(block locks
  (match-com (unlock door :in-room house-s :having key)
    (if (not (has-trait *r* 'door-unlocked))
        (progn (p "You unlock the door.")
               (add-trait *r* 'door-unlocked))
        (p "It is already unlocked.")))

  (match-com (lock door :in-room house-s :having key)
    (if (has-trait *r* 'door-unlocked)
        (p "It wouldn't do any good.")
        (p "It is already locked.")))

  (match-coms ((lock x) (unlock x) :having key)
    (p "You can't do that."))

  (match-coms ((lock x) (unlock x))
    (p "You don't have a key.")))

(block room-house
  (match-com ((push move turn flip) rug :in-room house :room-trait trapdoor-hidden)
    (p "You push the rug, revealing a small trapdoor. ")
    (trail-1-way house down cellar)
    (swap-trait *r* 'trapdoor-revealed 'trapdoor-hidden)
    (match-com ((open close) trapdoor :room-trait trapdoor-revealed)
      (p "To take advantage of the trapdoor, just \"go down\"."))))

(block-match tree-west-branch (:in-room tree-west-branch)
  (match-com (break window  :room-trait window-intact)
    (p "You're afraid breaking the glass empty handed could get you hurt."))

  (match-coms (((break hit) window with hammer)
               ((break hit) window hammer)
               :room-trait window-intact
               :having hammer)
    (p "You smash the window up.")
    (swap-trait *r* 'window-intact 'window-broken))
  (match-coms (((break hit) window with hammer)
               ((break hit) window hammer)
               :room-trait window-intact)
    (p "You don't have a hammer."))

             
  (match-coms ((go through window)
               (enter window)
               :room-trait window-intact)
    (p "You bump into the window glass pane, but that's as far as you get."))
  (match-coms ((go through window)
               (enter window)
               :room-trait window-broken window-closed)
    (die "You try to enter the window, but give yourself a nasty cut by pushing against the glass shards left in the window pane. There's a lot of blood. You feel a little faint, then black out."))
  (match-coms ((go through window)
               (enter window)
               :room-trait window-broken window-open)
    (p "You squeeze through the window, into the attic.")
    (setf *r* 'house-attic))
  (match-com (open window :room-trait window-broken window-closed)
    (p "You open the window, carefully avoiding glass shards.")
    (swap-trait *r* 'window-closed 'window-open))
  (match-com (open window :room-trait window-intact window-closed)
    (p "The window does not open from outside the house."))
  (match-com (open window :room-trait  window-open)
    (p "The window is already open.")))

(block-match house-attic (:in-room house-attic)
  (match-com (look ladder :room-trait ladder-up)
    (p "The collapsible ladder is raised. "))
  (match-com (look ladder :room-trait ladder-down)
    (p "The collapsible ladder is lowered, and you can climb down. "))
  (match-com ((drop lower kick move) ladder
              ::room-trait ladder-down)
    (p "The ladder is already lowered."))
  (match-com ((lower kic kmove) ladder :room-trait ladder-up)
    (p "You lower the ladder.")
    (swap-trait *r* 'ladder-up 'ladder-down)
    (trail house-attic down house)))

(block misc
  (match-coms ((mow grass scythe)
               (mow grass with scythe)
               (use scythe)
               (use scythe (on with) grass)
               (mow grass) :room-trait grass :having scythe)
    (p "You mow the grass.")
    (del-trait *r* 'grass)))

(match-com (foo (x aa bb cc))
  (p x))

(block catch-failures
  (match-coms ((mow) (mow x :room-trait ! grass))
    (p "Mow what?"))
  (match-com (mow grass :room-trait grass)
    (p "Mow with what?"))
  (match-com (look x)
    (p "Can't see that."))
  (match-com (*)
    (p "Huh?")))


(match-com (mow grass :room-trait foo (bar 1 x y) (baz x foo y))
  (p x y))



(defun foo (x)
  (P "attempting match command "
     '(DBG (:ROOM-TRAIT (POSITION 0)) (:IN-ROOM LABYRINTH))
     " <" x "> ~%")
  (WHEN (AND x (EQ (CAR x) 'DBG))
    (LET ((x (CDR x)))
      (WHEN (HAS-TRAITS *R* '(POSITION))
        (WHEN
            (AND (EQ (LENGTH (TRAIT-VALUE *R* 'POSITION)) 1)
                 (EQUAL '0
                        (ELT (TRAIT-VALUE *R* 'POSITION) 0)))
          (WHEN (MEMBER *R* '(LABYRINTH))
            (WHEN (NOT x)
              (SETF *COMMAND-HANDLED* T)
              (P "checks out"))))))))
