;; farmer-registry
;; 
;; This contract manages farmer identities, land ownership, and credentials within the HarvestPro ecosystem.
;; It provides a foundation for establishing verified farmer identities, tracking land ownership, and
;; storing agricultural certifications. Other contracts in the ecosystem will reference this registry
;; as the authoritative source of farmer information.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-FARMER-ALREADY-REGISTERED (err u101))
(define-constant ERR-FARMER-NOT-FOUND (err u102))
(define-constant ERR-INVALID-PARAM (err u103))
(define-constant ERR-LAND-ALREADY-REGISTERED (err u104))
(define-constant ERR-LAND-NOT-FOUND (err u105))
(define-constant ERR-UNAUTHORIZED-VERIFIER (err u106))
(define-constant ERR-CREDENTIAL-NOT-FOUND (err u107))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Farmer status constants
(define-constant STATUS-PENDING u1)
(define-constant STATUS-VERIFIED u2)
(define-constant STATUS-SUSPENDED u3)

;; Data structures

;; Authorized verifiers who can validate farmer identities
(define-map authorized-verifiers principal bool)

;; Farmer information
(define-map farmers
  principal
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    contact: (string-ascii 100),
    status: uint,
    registration-date: uint,
    last-updated: uint
  }
)

;; Land parcel ownership and details
(define-map land-parcels
  uint  ;; land-id
  {
    owner: principal,
    size: uint,  ;; in hectares
    location: (string-ascii 200),
    registration-date: uint,
    description: (string-ascii 500)
  }
)

;; Credentials and certifications
(define-map farmer-credentials
  { farmer: principal, credential-id: uint }
  {
    credential-type: (string-ascii 100),
    issuer: (string-ascii 100),
    issue-date: uint,
    expiry-date: uint,
    details: (string-ascii 500),
    verified-by: (optional principal)
  }
)

;; List of land parcels owned by each farmer
(define-map farmer-lands
  principal
  (list 20 uint)  ;; list of land-ids
)

;; Current land parcel ID counter
(define-data-var next-land-id uint u1)

;; Current credential ID counter
(define-data-var next-credential-id uint u1)

;; Private functions

;; Check if a principal is an authorized verifier
(define-private (is-authorized-verifier (verifier principal))
  (default-to false (map-get? authorized-verifiers verifier))
)

;; Check if the caller is the contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Get current block height as a proxy for timestamp
(define-private (get-current-time)
  block-height
)

;; Add a land-id to a farmer's list of lands
(define-private (add-land-to-farmer (farmer principal) (land-id uint))
  (let (
    (current-lands (default-to (list) (map-get? farmer-lands farmer)))
  )
    (map-set farmer-lands farmer (unwrap-panic (as-max-len? (append current-lands land-id) u20)))
  )
)

;; Public functions

;; Register a new farmer
(define-public (register-farmer (name (string-ascii 100)) (location (string-ascii 100)) (contact (string-ascii 100)))
  (let (
    (current-time (get-current-time))
  )
    (asserts! (not (map-get? farmers tx-sender)) ERR-FARMER-ALREADY-REGISTERED)
    (asserts! (> (len name) u0) ERR-INVALID-PARAM)
    (asserts! (> (len location) u0) ERR-INVALID-PARAM)
    
    (map-set farmers tx-sender {
      name: name,
      location: location,
      contact: contact,
      status: STATUS-PENDING,
      registration-date: current-time,
      last-updated: current-time
    })
    
    (ok true)
  )
)

;; Update farmer profile information
(define-public (update-farmer-profile (name (string-ascii 100)) (location (string-ascii 100)) (contact (string-ascii 100)))
  (let (
    (farmer-data (map-get? farmers tx-sender))
    (current-time (get-current-time))
  )
    (asserts! (is-some farmer-data) ERR-FARMER-NOT-FOUND)
    (asserts! (> (len name) u0) ERR-INVALID-PARAM)
    (asserts! (> (len location) u0) ERR-INVALID-PARAM)
    
    (map-set farmers tx-sender (merge (unwrap-panic farmer-data) 
      {
        name: name,
        location: location,
        contact: contact,
        last-updated: current-time
      }
    ))
    
    (ok true)
  )
)

;; Verify a farmer's identity by an authorized verifier
(define-public (verify-farmer (farmer principal))
  (let (
    (farmer-data (map-get? farmers farmer))
    (current-time (get-current-time))
  )
    (asserts! (is-authorized-verifier tx-sender) ERR-UNAUTHORIZED-VERIFIER)
    (asserts! (is-some farmer-data) ERR-FARMER-NOT-FOUND)
    
    (map-set farmers farmer (merge (unwrap-panic farmer-data) 
      {
        status: STATUS-VERIFIED,
        last-updated: current-time
      }
    ))
    
    (ok true)
  )
)

;; Register a new land parcel
(define-public (register-land-parcel 
  (size uint) 
  (location (string-ascii 200)) 
  (description (string-ascii 500))
)
  (let (
    (farmer-data (map-get? farmers tx-sender))
    (land-id (var-get next-land-id))
    (current-time (get-current-time))
  )
    (asserts! (is-some farmer-data) ERR-FARMER-NOT-FOUND)
    (asserts! (> size u0) ERR-INVALID-PARAM)
    (asserts! (> (len location) u0) ERR-INVALID-PARAM)
    
    ;; Register the land parcel
    (map-set land-parcels land-id {
      owner: tx-sender,
      size: size,
      location: location,
      registration-date: current-time,
      description: description
    })
    
    ;; Add land to farmer's list of lands
    (add-land-to-farmer tx-sender land-id)
    
    ;; Increment the land ID counter
    (var-set next-land-id (+ land-id u1))
    
    (ok land-id)
  )
)

;; Transfer land ownership
(define-public (transfer-land (land-id uint) (new-owner principal))
  (let (
    (land-data (map-get? land-parcels land-id))
    (farmer-data (map-get? farmers new-owner))
    (current-time (get-current-time))
  )
    (asserts! (is-some land-data) ERR-LAND-NOT-FOUND)
    (asserts! (is-some farmer-data) ERR-FARMER-NOT-FOUND)
    (asserts! (is-eq (get owner (unwrap-panic land-data)) tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Update land ownership
    (map-set land-parcels land-id (merge (unwrap-panic land-data) 
      {
        owner: new-owner
      }
    ))
    
    ;; Add land to new owner's list
    (add-land-to-farmer new-owner land-id)
    
    (ok true)
  )
)

;; Add a credential or certification for a farmer
(define-public (add-credential 
  (credential-type (string-ascii 100))
  (issuer (string-ascii 100))
  (expiry-date uint)
  (details (string-ascii 500))
)
  (let (
    (farmer-data (map-get? farmers tx-sender))
    (credential-id (var-get next-credential-id))
    (current-time (get-current-time))
  )
    (asserts! (is-some farmer-data) ERR-FARMER-NOT-FOUND)
    (asserts! (> (len credential-type) u0) ERR-INVALID-PARAM)
    (asserts! (> (len issuer) u0) ERR-INVALID-PARAM)
    (asserts! (>= expiry-date current-time) ERR-INVALID-PARAM)
    
    ;; Register the credential
    (map-set farmer-credentials 
      { farmer: tx-sender, credential-id: credential-id }
      {
        credential-type: credential-type,
        issuer: issuer,
        issue-date: current-time,
        expiry-date: expiry-date,
        details: details,
        verified-by: none
      }
    )
    
    ;; Increment the credential ID counter
    (var-set next-credential-id (+ credential-id u1))
    
    (ok credential-id)
  )
)

;; Verify a farmer's credential
(define-public (verify-credential (farmer principal) (credential-id uint))
  (let (
    (credential-key { farmer: farmer, credential-id: credential-id })
    (credential-data (map-get? farmer-credentials credential-key))
  )
    (asserts! (is-authorized-verifier tx-sender) ERR-UNAUTHORIZED-VERIFIER)
    (asserts! (is-some credential-data) ERR-CREDENTIAL-NOT-FOUND)
    
    ;; Update the credential with verification
    (map-set farmer-credentials credential-key
      (merge (unwrap-panic credential-data)
        {
          verified-by: (some tx-sender)
        }
      )
    )
    
    (ok true)
  )
)

;; Admin functions

;; Add an authorized verifier
(define-public (add-authorized-verifier (verifier principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-set authorized-verifiers verifier true)
    (ok true)
  )
)

;; Remove an authorized verifier
(define-public (remove-authorized-verifier (verifier principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-delete authorized-verifiers verifier)
    (ok true)
  )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Read-only functions

;; Get farmer information
(define-read-only (get-farmer (farmer principal))
  (map-get? farmers farmer)
)

;; Get land parcel information
(define-read-only (get-land-parcel (land-id uint))
  (map-get? land-parcels land-id)
)

;; Get lands owned by a farmer
(define-read-only (get-farmer-land-parcels (farmer principal))
  (map-get? farmer-lands farmer)
)

;; Get farmer credential
(define-read-only (get-farmer-credential (farmer principal) (credential-id uint))
  (map-get? farmer-credentials { farmer: farmer, credential-id: credential-id })
)

;; Check if an address is an authorized verifier
(define-read-only (is-verifier (address principal))
  (is-authorized-verifier address)
)