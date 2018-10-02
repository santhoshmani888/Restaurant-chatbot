;;####################################################
;;Templates





(deftemplate user
"User info"
  (slot name (default 0))
  (slot user-id (default 0)))
  


(deftemplate question
"Questions asked"
  (slot text)
  (slot type)
  (slot id))

(deftemplate answer
  (slot id)
  (slot text))
  

  
(deftemplate items
"menu item details"
  (slot itemname)
  (slot quantity)
  (slot price)
  (slot promo))
  
  
  (deftemplate order
"Order details"
  (slot user-id)
  (slot ordernumber)
  (slot date)
  (slot item)
  (slot amount)
  )
  
  
  
  
;;####################################################
;;Questions







  
(deffacts question-data
  "Questions asked."
  (question (id current-user) (type yes-no)
            (text "Are you a registered user?"))
  (question (id sign-up) (type yes-no)
            (text "Do you want to register for a new account?"))
  (question (id user-id) (type text)
            (text "Please provide your user-id ."))
  (question (id user-details) (type text)
            (text "Please Choose a UserID you would like to register with."))
  (question (id order-status) (type yes-no)
            (text "Do you want to track your Order?"))
  (question (id order-no) (type yes-no)
            (text "Do you have the order number/name?"))
  (question (id order-number) (type string)
            (text "Please provide your  order number/name ?"))
  (question (id cancel-feedback) (type string)
            (text "please provide reason for the order cancellation?"))
 
  (question (id cancel-confirm) (type yes-no)
            (text "okay! Can I proceed with the cancellation?"))
  (question (id menu-item) (type text)
            (text "From the menu what would you like to have?"))
  (question (id confirm-order) (type yes-no)
            (text "Do you confirm the order?"))
  (question (id order-more) (type yes-no)
            (text "Would you like to order anything here?"))
  (question (id concern) (type yes-no)
            (text "Do you have any other queries?"))
  (question (id message)(type text)
            (text"Please leave your message here: "))
  (question (id promoq)(type yes-no)
            (text "Do you have any promocode?"))
  (question(id check-promo)(type text)
            (text"enter the promocode here:"))
 
  (question (id cancel-check) (type yes-no)
            (text "Do you wish to cancel your order?"))
  (question (id buy-item) (type text)
            (text "enter the menu item you would like to have?"))
  (question (id feedback) (type yes-no)
            (text "Do you like to give feedback on our assistance?"))
  (question (id feedback-ask) (type string)
            (text "Please help us grow with your suggestions : "))
  (question (id cancelled) (type yes-no)
            (text "Your order is cancelled")
            )
    
    


  

  
 )
            
  (defglobal ?*crlf* = "
")





;;####################################################
;;askuser module







(defmodule ask)

(deffunction ask-user (?question ?type)
  
  (bind ?answer "")
  (while (not (is-of-type ?answer ?type)) do
         (printout t ?question " ")
         (if (eq ?type yes-no) then
           (printout t "(yes or no) "))
         (bind ?answer (read)))
  (return ?answer))

(deffunction is-of-type (?answer ?type)
  
  (if (eq ?type yes-no) then
    (return (or (eq ?answer yes) (eq ?answer no)))
    (elif (eq ?type number) then
           (return (numberp ?answer)))
    else (return (> (str-length ?answer) 0))))
   
(defrule ask::ask-question-by-id
  "Given the identifier of a question, ask the question and assert the answer"
  (declare (auto-focus TRUE))
  (MAIN::question (id ?id) (text ?text) (type ?type))
  (not (MAIN::answer (id ?id)))
  ?ask <- (MAIN::ask ?id)
  =>
  (bind ?answer (ask-user ?text ?type))
  (assert (answer (id ?id) (text ?answer)))
  (retract ?ask)
  (return))  
  
  
  
  
;;####################################################
;;appication startup





(defmodule start)
(defglobal ?*g-ordernum* = 0)
(defglobal ?*item* = 0)
(defglobal ?*uname* = 0)
(defrule print-intro
  =>
  (printout t " " crlf)
  (printout t " " crlf)
  (printout t "WELCOME TO FOOD ON CHAT(FOC)!. I am here to help you!" crlf)
  (printout t "Please type your name and press Enter> ")
  (bind ?*uname* (read))
  (printout t crlf "##################################################" crlf)
  
  (printout t " Hello, " ?*uname* " Greetings!!!" crlf)  
  (printout t " Please answer few questions to assist you." crlf)
  
  (printout t crlf "##################################################" crlf)
  
  
  
  
  (assert (user (name sant) (user-id sant)))
  (assert (user (name veer) (user-id veer)))
  (assert (user (name reg) (user-id reg)))
  (assert (user (name del) (user-id del)))
  (assert (order (user-id veer) (ordernumber 1234) (date 09/15/2018) (item veggie_burger) (amount 3.99)))
  (assert (order (user-id sant) (ordernumber 2345) (date 09/16/2018) (item bacon_cheeseburger) (amount 8.99)))
  
  (assert (order (user-id reg) (ordernumber 8234) (date 09/18/2018) (item veg_pizza) (amount 13.99)))
  (assert (order (user-id del) (ordernumber 9234) (date 09/19/2018) (item veg_taco) (amount 4.99)))
  (assert (items (itemname veg_burger) (quantity 1) (price 3.99)(promo big10)))
  (assert (items (itemname chicken_sandwich) (quantity 1) (price 5.99) (promo sw05)))
  (assert (items (itemname bacon_cheeseburger) (quantity 1) (price 8.99) (promo bc10)))
  (assert (items (itemname veg_friedrice) (quantity 1) (price 7.99) (promo vf25)))
  (assert (items (itemname chicken_friedrice) (quantity 1) (price 8.99) (promo cf10)))
  (assert (items (itemname veg_pizza) (quantity 1) (price 13.99)) (promo vp15))
  (assert (items (itemname veg_taco) (quantity 1) (price 4.99)(promo vt10)))
  (assert (items (itemname smoothies) (quantity 1) (price 3.99)(promo sm10)))
  
  
)




  
;;####################################################
;;Rules






(defmodule ruleset)

(defrule inquire-user
  =>
  (assert (ask current-user)))
  

  
(defrule not-an-existing-user
  (answer (id current-user) (text ?resp&:(eq ?resp no)))
  =>
  (assert (MAIN::ask sign-up)))
  
(defrule existing-user
  (answer (id current-user) (text ?resp&:(eq ?resp yes)))
  =>
  (assert (MAIN::ask user-id)))
  
(defrule user-id-check
  (answer (id user-id) (text ?user))
  ?u <- (user {user-id == ?user})
  =>
  
  (printout t crlf"Your details! Username: " ?u.name crlf"   User-id :" ?u.user-id crlf)
  
  (assert (MAIN::ask order-status)))
  
(defrule not-a-valid-userid
  (answer (id user-id) (text ?user))
  (not (user {user-id == ?user}))
  =>
  (printout t "User-id not found!,please try again."  crlf)
  (assert (MAIN::ask current-user))
  )
  
(defrule no-sign-up
  (answer (id sign-up) (text ?resp&:(eq ?resp no)))
  =>
  (assert (MAIN::ask order-status)))
  
(defrule ask-sign-up
  (answer (id sign-up) (text ?resp&:(eq ?resp yes)))
  =>
  (assert (MAIN::ask user-details)))
  
(defrule new-user
  (answer (id user-details) (text ?user-id))
  =>
  (printout t "Username: " ?*uname* "   User-id :" ?user-id crlf)
  (assert (user (name ?*uname*) (user-id ?user-id)))
  (printout t "Account created successfully!" crlf)
  (printout t crlf"Your referral code is:"?user-id"2018"crlf)
  (printout t crlf "Share the referral code to five person to earn 10$ credit" crlf)
  (assert (MAIN::ask order-status))
 )
(defrule check-order-status
  ;; check if user is requesting order status
  (answer (id order-status) (text ?resp&:(eq ?resp yes)))
  =>
  (assert (MAIN::ask order-no))
  )
  (defrule check-order-no
  
  (answer (id order-status) (text ?resp&:(eq ?resp no)))
  =>
  (assert (MAIN::ask order-more))
  )
(defrule request-order-number
  ;; Ask for the order number
  (answer (id order-no) (text ?resp&:(eq ?resp yes)))
  =>
  (assert (MAIN::ask order-number)))
  
  (defrule request-order-number-no
  
  (answer (id order-no) (text ?resp&:(eq ?resp no)))
  =>
  (printout t"Please call customer service  at 1800-000-000" crlf)
  (printout t"to check on your order status" crlf)
  (assert (MAIN::ask concern))
  )
  
   (defrule concern-no
  
  (answer (id concern) (text ?resp&:(eq ?resp no)))
  =>
  
  (assert (MAIN::ask feedback))
  )
  
(defrule concern-yes
    (answer (id concern) (text ?resp&:(eq ?resp yes)))
  =>
  (assert (MAIN::ask order-more))
  )
  
  
(defrule not-requesting-order-status
  ;; check if user is requesting order status
  (answer (id order-status) (text ?st&:(eq ?st no)))
  =>
  (assert (MAIN::ask order-modify)))

(defrule order-modify
  ;; check if user is requesting reservation
  (answer (id order-modify) (text ?resp&:(eq ?resp yes)))
  =>
  (assert (MAIN::ask reserve)))
  
  
(defrule get-order-number
  (answer (id order-number) (text ?or_no))
  ?o <- (order {ordernumber == ?or_no})
  =>
  (bind ?*g-ordernum* ?o.ordernumber)
  (printout t "Your order: " ?o.ordernumber "   " ?o.date "   " ?o.item "   $" ?o.amount " " crlf)
  (printout t "Your order is being prepared.It will be out for delivery in 10 minutes!" crlf)
  (assert (MAIN::ask cancel-check)))
  
  (defrule invalid-order-no
  (and (answer (id order-number) (text ?or_no)) (not ?o <- (order {ordernumber == ?or_no})))
  =>
 (printout t "Order not found!!! " crlf) 
 (assert (MAIN::ask concern)))

(defrule ask-cancel-reason
  ;; Ask for the order number
  (answer (id cancel-feedback) (text ?cr))
  =>
  (assert (MAIN::ask cancel-confirm)))
  
(defrule ask-cancel-check
  (answer (id cancel-check) (text ?cc&:(eq ?cc yes)))
  =>
  (assert (MAIN::ask cancel-feedback))
 )
 
(defrule no-cancel
  (answer (id cancel-check) (text ?cc&:(eq ?cc no)))
  =>
  (assert (MAIN::ask order-more))
 )
  


   
    (defrule cancel-confirm-yes
   (answer (id cancel-confirm) (text ?resp&:(eq ?resp yes)))
   =>
   (printout t crlf " Order cancelled!!" crlf)
   (assert (MAIN::ask concern))
   )
   
       (defrule cancel-confirm-no
   (answer (id cancel-confirm) (text ?resp&:(eq ?resp no)))
   =>
   (assert (MAIN::ask order-more))
   )
   
   

   

   
  (defrule menu-display
    (answer (id order-more) (text ?resp&:(eq ?resp yes)))
    =>
    (printout t " " crlf)
    (printout t "#####################################"crlf)
  (printout t "Our Recommended Menu: "  crlf)
  (printout t " " crlf)
  (printout t "veg_burger         3.99"  crlf)
  (printout t "chicken_sandwich   5.99"  crlf)
  (printout t "bacon_cheeseburger 8.99"  crlf)
  (printout t "veg_friedrice      7.99"  crlf)
  (printout t "chicken_friedrice  8.99"  crlf)
  (printout t "veg_pizza          13.99"  crlf)
  (printout t "veg_taco           4.99"  crlf)
  (printout t "smoothies          3.99"  crlf)
  

  (printout t " " crlf)
  
    (assert (MAIN::ask buy-item))
    )
    
    
    (defrule cancelled
   (answer (id cancelled) (text ?resp&:(eq ?resp yes)))
   =>
   (assert (MAIN::ask order-more))
   )
   
    (defrule cancelled
   (answer (id cancelled) (text ?resp&:(eq ?resp no)))
   =>
   (assert (MAIN::ask concern))
   )
 
  (defrule order-more-no
    (answer (id order-more) (text ?resp&:(eq ?resp no)))
    =>
    (assert(MAIN::ask concern))
    )
    
  (defrule purchase-item
  (answer (id buy-item) (text ?cc)) 
  ?n <- (items {itemname == ?cc})
  =>
  
  (printout t " " ?cc "  $" ?n.price crlf)
  (assert (MAIN::ask promoq))
  
  )


(defrule promo-yes
  (answer (id promoq) (text ?resp&:(eq ?resp yes)))
  =>
  (assert (MAIN::ask check-promo)))
  
(defrule promo-no
  (answer (id promoq) (text ?resp&:(eq ?resp no)))
  =>
  (assert (MAIN::ask confirm-order)))
  
  (defrule check-promo
  (answer (id check-promo) (text ?cc)) 
  ?n <- (items {promo == ?cc})
  =>
  (if(eq ?cc big10) then
  (bind ?calculated-price (float (* 0.9 ?n.price) ))
  (printout t "After 10% discount,Current price :"?calculated-price crlf)
  elif(eq ?cc sw05) then
  (bind ?calculated-price (float (* 0.95 ?n.price) ))
  (printout t "After 5% discount,Current price :"?calculated-price crlf)
  
  elif(eq ?cc sm10) then
  (bind ?calculated-price (float (* 0.90 ?n.price) ))
  (printout t "After 10% discount,Current price :"?calculated-price crlf)
  
  elif(eq ?cc vt10) then
  (bind ?calculated-price (float (* 0.90 ?n.price) ))
  (printout t "After 10% discount,Current price :"?calculated-price crlf)
  
  elif(eq ?cc cf10) then
  (bind ?calculated-price (float (* 0.90 ?n.price) ))
  (printout t "After 10% discount,Current price :"?calculated-price crlf)
  
  elif(eq ?cc vf25) then
  (bind ?calculated-price (float (* 0.75 ?n.price) ))
  (printout t "After 25% discount,Current price :"?calculated-price crlf)
  
  elif(eq ?cc vp15) then
  (bind ?calculated-price (float (* 0.85 ?n.price) ))
  (printout t "After 15% discount,Current price :"?calculated-price crlf)
  
  
  
  
  elif(eq ?cc bc10) then
  (bind ?calculated-price (float (* 0.90 ?n.price) ))
  (printout t "After 10% discount,Current price :"?calculated-price crlf))
  (assert (MAIN::ask confirm-order)))
  
    (defrule promo-invalid
  (answer (id check-promo) (text ?cc)) 
  (not (items {promo == ?cc}))
  =>
  (printout t "Promocode invalid/expired, discount not available!" crlf)
  (assert (MAIN::ask confirm-order))
  
  )
  

  
  
  
  (defrule confirm-order-yes
  (answer (id confirm-order) (text ?cc&:(eq ?cc yes)))
  =>
  (printout t "Order placed! Your order will be delivered in 30 minutes!" crlf)
  (printout t"################################################" crlf)
  (printout t crlf" ORDER NAME: " ?*uname* crlf)
  (printout t crlf "Call Customer care at 1800-000-000 for any special delivery instructions" crlf)
  (printout t "################################################" crlf)
  (assert (MAIN::ask concern)))
  
  (defrule invalid-item-name
  (answer (id buy-item) (text ?cc)) 
  (not (items {itemname == ?cc}))
  =>
  (printout t "Sorry! Item name incorrect!" crlf)
  (assert (MAIN::ask order-more))
  
  )
  
  (defrule confirm-order-no
  (answer (id confirm-order)(text ?resp&:(eq ?resp no)))
  =>
  (assert (MAIN::ask concern)))
 
 
   (defrule concern-no
  (answer (id concern) (text ?cc&:(eq ?cc no)))
  =>
  
  (assert (MAIN::ask feedback)))
  
    (defrule concern-yes
  (answer (id concern) (text ?cc&:(eq ?cc yes)))
  =>
  (assert(MAIN::ask message))
  
  )
  (defrule message-resp
  (answer (id message)(text ?cr))
  =>
  (printout t crlf"Thank you , We will contact you soon on your query."crlf)
  (assert(MAIN::ask feedback)))
  
    (defrule feedback-yes
  (answer (id feedback) (text ?cc&:(eq ?cc yes)))
  =>

  (assert (MAIN::ask feedback-ask)))
  
    (defrule feedback-no
  (answer (id feedback) (text ?cc&:(eq ?cc no)))
  =>
    (printout t crlf)
  (printout t "################################################"crlf)
   (printout t "Happy assisting you" crlf)
  (printout t "Have a great day!!! see you soon" crlf)
  )
  
  (defrule feedback-ask
  
  (answer (id feedback-ask) (text ?cr))
  =>
  (printout t crlf)
  (printout t "################################################"crlf)
  (printout t "Happy assisting you" crlf)
  (printout t "Have a great day!!! see you soon" crlf)
  )
  
  
  
  
  
;;####################################################
;;run modules






(deffunction start-application ()
  (reset)
  (focus start ruleset)
  (run))

(while TRUE
  (start-application))