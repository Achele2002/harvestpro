;; Marketplace Contract
;; This contract facilitates transactions between farmers and buyers for agricultural products
;; with transparent pricing, automated settlements, and escrow protection.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LISTING-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
(define-constant ERR-QUANTITY-TOO-LOW (err u103))
(define-constant ERR-ALREADY-FULFILLED (err u104))
(define-constant ERR-ESCROW-NOT-FOUND (err u105))
(define-constant ERR-ESCROW-NOT-READY (err u106))
(define-constant ERR-INVALID-PARAMETERS (err u107))
(define-constant ERR-LISTING-INACTIVE (err u108))
(define-constant ERR-QUANTITY-EXCEEDED (err u109))

;; Data structures

;; Listing map: stores all product listings created by farmers
(define-map listings
  { listing-id: uint }
  {
    farmer: principal,
    product-name: (string-ascii 64),
    quantity-available: uint,
    price-per-unit: uint,
    delivery-terms: (string-ascii 256),
    quality-metrics: (string-ascii 256),
    active: bool,
    created-at: uint
  }
)

;; Escrow map: holds funds during transaction process until delivery is confirmed
(define-map escrows
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    listing-id: uint,
    quantity: uint,
    total-amount: uint,
    fulfilled: bool,
    created-at: uint
  }
)

;; Transaction history: records all completed transactions
(define-map transactions
  { tx-id: uint }
  {
    buyer: principal,
    seller: principal,
    listing-id: uint,
    quantity: uint,
    total-amount: uint,
    completed-at: uint
  }
)

;; Counters for generating unique IDs
(define-data-var listing-id-counter uint u0)
(define-data-var escrow-id-counter uint u0)
(define-data-var transaction-id-counter uint u0)

;; Private functions

;; Generate a new unique listing ID
(define-private (generate-listing-id)
  (let ((current-id (var-get listing-id-counter)))
    (var-set listing-id-counter (+ current-id u1))
    current-id
  )
)

;; Generate a new unique escrow ID
(define-private (generate-escrow-id)
  (let ((current-id (var-get escrow-id-counter)))
    (var-set escrow-id-counter (+ current-id u1))
    current-id
  )
)

;; Generate a new unique transaction ID
(define-private (generate-tx-id)
  (let ((current-id (var-get transaction-id-counter)))
    (var-set transaction-id-counter (+ current-id u1))
    current-id
  )
)

;; Check if a listing exists and is active
(define-private (is-listing-active (listing-id uint))
  (match (map-get? listings { listing-id: listing-id })
    listing (get active listing)
    false
  )
)

;; Update listing quantity after purchase
(define-private (update-listing-quantity (listing-id uint) (quantity-purchased uint))
  (match (map-get? listings { listing-id: listing-id })
    listing
      (let ((new-quantity (- (get quantity-available listing) quantity-purchased)))
        (map-set listings 
          { listing-id: listing-id }
          (merge listing { 
            quantity-available: new-quantity,
            active: (> new-quantity u0)
          })
        )
        (ok true)
      )
    ERR-LISTING-NOT-FOUND
  )
)

;; Record transaction in history after successful completion
(define-private (record-transaction (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow
      (let (
        (tx-id (generate-tx-id))
        (listing-id (get listing-id escrow))
        (buyer (get buyer escrow))
        (seller (get seller escrow))
        (quantity (get quantity escrow))
        (total-amount (get total-amount escrow))
      )
        (map-set transactions
          { tx-id: tx-id }
          {
            buyer: buyer,
            seller: seller,
            listing-id: listing-id,
            quantity: quantity,
            total-amount: total-amount,
            completed-at: block-height
          }
        )
        (ok tx-id)
      )
    ERR-ESCROW-NOT-FOUND
  )
)

;; Read-only functions

;; Get listing details by ID
(define-read-only (get-listing (listing-id uint))
  (map-get? listings { listing-id: listing-id })
)

;; Get escrow details by ID
(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows { escrow-id: escrow-id })
)

;; Get transaction details by ID
(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions { tx-id: tx-id })
)

;; Calculate total price for a given quantity of a listed product
(define-read-only (calculate-price (listing-id uint) (quantity uint))
  (match (map-get? listings { listing-id: listing-id })
    listing
      (ok (* (get price-per-unit listing) quantity))
    ERR-LISTING-NOT-FOUND
  )
)

;; Check if a user is the owner of a listing
(define-read-only (is-listing-owner (listing-id uint) (user principal))
  (match (map-get? listings { listing-id: listing-id })
    listing (is-eq (get farmer listing) user)
    false
  )
)

;; Public functions

;; Create a new product listing
(define-public (create-listing 
  (product-name (string-ascii 64))
  (quantity-available uint)
  (price-per-unit uint)
  (delivery-terms (string-ascii 256))
  (quality-metrics (string-ascii 256))
)
  (let ((listing-id (generate-listing-id)))
    ;; Validate inputs
    (asserts! (> quantity-available u0) ERR-INVALID-PARAMETERS)
    (asserts! (> price-per-unit u0) ERR-INVALID-PARAMETERS)
    
    ;; Create the listing
    (map-set listings
      { listing-id: listing-id }
      {
        farmer: tx-sender,
        product-name: product-name,
        quantity-available: quantity-available,
        price-per-unit: price-per-unit,
        delivery-terms: delivery-terms,
        quality-metrics: quality-metrics,
        active: true,
        created-at: block-height
      }
    )
    
    (ok listing-id)
  )
)

;; Update an existing product listing
(define-public (update-listing
  (listing-id uint)
  (quantity-available uint)
  (price-per-unit uint)
  (delivery-terms (string-ascii 256))
  (quality-metrics (string-ascii 256))
  (active bool)
)
  (match (map-get? listings { listing-id: listing-id })
    listing
      ;; Check that sender is the owner
      (asserts! (is-eq (get farmer listing) tx-sender) ERR-NOT-AUTHORIZED)
      
      ;; Update the listing
      (map-set listings
        { listing-id: listing-id }
        (merge listing {
          quantity-available: quantity-available,
          price-per-unit: price-per-unit,
          delivery-terms: delivery-terms,
          quality-metrics: quality-metrics,
          active: active
        })
      )
      
      (ok true)
    ERR-LISTING-NOT-FOUND
  )
)

;; Purchase products and create escrow
(define-public (purchase-product (listing-id uint) (quantity uint))
  (match (map-get? listings { listing-id: listing-id })
    listing
      (let ((total-price (* (get price-per-unit listing) quantity))
            (seller (get farmer listing)))
        
        ;; Validate purchase conditions
        (asserts! (get active listing) ERR-LISTING-INACTIVE)
        (asserts! (>= (get quantity-available listing) quantity) ERR-QUANTITY-EXCEEDED)
        (asserts! (> quantity u0) ERR-QUANTITY-TOO-LOW)
        
        ;; Transfer payment to escrow (contract)
        (try! (stx-transfer? total-price tx-sender (as-contract tx-sender)))
        
        ;; Create escrow record
        (let ((escrow-id (generate-escrow-id)))
          ;; Update listing quantity
          (try! (update-listing-quantity listing-id quantity))
          
          ;; Create escrow entry
          (map-set escrows
            { escrow-id: escrow-id }
            {
              buyer: tx-sender,
              seller: seller,
              listing-id: listing-id,
              quantity: quantity,
              total-amount: total-price,
              fulfilled: false,
              created-at: block-height
            }
          )
          
          (ok escrow-id)
        )
      )
    ERR-LISTING-NOT-FOUND
  )
)

;; Confirm product delivery and release payment from escrow
(define-public (confirm-delivery (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow
      (begin
        ;; Check that sender is the buyer
        (asserts! (is-eq (get buyer escrow) tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check that escrow hasn't been fulfilled yet
        (asserts! (not (get fulfilled escrow)) ERR-ALREADY-FULFILLED)
        
        ;; Release payment to seller
        (try! (as-contract (stx-transfer? (get total-amount escrow) tx-sender (get seller escrow))))
        
        ;; Mark escrow as fulfilled
        (map-set escrows
          { escrow-id: escrow-id }
          (merge escrow { fulfilled: true })
        )
        
        ;; Record the transaction in history
        (try! (record-transaction escrow-id))
        
        (ok true)
      )
    ERR-ESCROW-NOT-FOUND
  )
)

;; Cancel purchase and refund buyer (only callable by either buyer or seller)
(define-public (cancel-purchase (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow
      (begin
        ;; Check that sender is either the buyer or seller
        (asserts! (or (is-eq (get buyer escrow) tx-sender) 
                       (is-eq (get seller escrow) tx-sender)) 
                   ERR-NOT-AUTHORIZED)
        ;; Check that escrow hasn't been fulfilled yet
        (asserts! (not (get fulfilled escrow)) ERR-ALREADY-FULFILLED)
        
        ;; Return funds to buyer
        (try! (as-contract (stx-transfer? (get total-amount escrow) tx-sender (get buyer escrow))))
        
        ;; Return quantity to listing
        (match (map-get? listings { listing-id: (get listing-id escrow) })
          listing
            (map-set listings
              { listing-id: (get listing-id escrow) }
              (merge listing {
                quantity-available: (+ (get quantity-available listing) (get quantity escrow)),
                active: true
              })
            )
          ;; If listing doesn't exist anymore, just continue with cancellation
        )
        
        ;; Mark escrow as fulfilled to prevent double refunds
        (map-set escrows
          { escrow-id: escrow-id }
          (merge escrow { fulfilled: true })
        )
        
        (ok true)
      )
    ERR-ESCROW-NOT-FOUND
  )
)