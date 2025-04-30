;; certification-system
;; 
;; This contract implements a certification system for the HarvestPro ecosystem,
;; allowing trusted third parties to verify and certify crops with various quality
;; credentials such as organic, fair trade, or pesticide-free status.
;; The system maintains immutable records of certifications, including issuance,
;; expiration, and possible revocation, thereby enhancing transparency and trust
;; throughout the agricultural supply chain.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-CERTIFIER (err u101))
(define-constant ERR-INVALID-HARVEST-ID (err u102))
(define-constant ERR-CERTIFICATION-EXISTS (err u103))
(define-constant ERR-CERTIFICATION-NOT-FOUND (err u104))
(define-constant ERR-CERTIFICATION-EXPIRED (err u105))
(define-constant ERR-CERTIFICATION-REVOKED (err u106))
(define-constant ERR-INVALID-EXPIRY (err u107))
(define-constant ERR-INVALID-CERTIFICATION-TYPE (err u108))

;; Constants for certification types
(define-constant CERTIFICATION-TYPE-ORGANIC u1)
(define-constant CERTIFICATION-TYPE-FAIR-TRADE u2)
(define-constant CERTIFICATION-TYPE-PESTICIDE-FREE u3)
(define-constant CERTIFICATION-TYPE-QUALITY-ASSURED u4)
(define-constant CERTIFICATION-TYPE-SUSTAINABLE u5)

;; Data structures

;; Map of authorized certifiers - address to name
(define-map certifiers principal (string-ascii 100))

;; Certification data structure
(define-map certifications 
  { harvest-id: uint, certification-type: uint }
  {
    certifier: principal,
    issue-date: uint,
    expiry-date: uint,
    details: (string-utf8 500),
    is-revoked: bool,
    revocation-reason: (optional (string-utf8 500))
  }
)

;; List of certifications per harvest
(define-map harvest-certifications
  { harvest-id: uint }
  { certification-types: (list 10 uint) }
)

;; Contract administration
(define-data-var contract-owner principal tx-sender)

;; Private functions

;; Check if a principal is an authorized certifier
(define-private (is-certifier (address principal))
  (is-some (map-get? certifiers address))
)

;; Check if certification type is valid
(define-private (is-valid-certification-type (type uint))
  (or
    (is-eq type CERTIFICATION-TYPE-ORGANIC)
    (is-eq type CERTIFICATION-TYPE-FAIR-TRADE)
    (is-eq type CERTIFICATION-TYPE-PESTICIDE-FREE)
    (is-eq type CERTIFICATION-TYPE-QUALITY-ASSURED)
    (is-eq type CERTIFICATION-TYPE-SUSTAINABLE)
  )
)

;; Update the list of certifications for a harvest
(define-private (add-certification-to-harvest (harvest-id uint) (certification-type uint))
  (let ((current-certs (default-to { certification-types: (list) } 
                         (map-get? harvest-certifications { harvest-id: harvest-id }))))
    (map-set harvest-certifications
      { harvest-id: harvest-id }
      { certification-types: (unwrap! (as-max-len? 
                                        (append (get certification-types current-certs) certification-type)
                                        u10)
                                      ERR-CERTIFICATION-EXISTS) }
    )
    (ok true)
  )
)

;; Read-only functions

;; Get certifier information
(define-read-only (get-certifier (address principal))
  (map-get? certifiers address)
)

;; Get certification details
(define-read-only (get-certification (harvest-id uint) (certification-type uint))
  (map-get? certifications { harvest-id: harvest-id, certification-type: certification-type })
)

;; Check if a certification is valid (exists, not expired, not revoked)
(define-read-only (is-certification-valid (harvest-id uint) (certification-type uint))
  (match (map-get? certifications { harvest-id: harvest-id, certification-type: certification-type })
    certification (and 
                    (not (get is-revoked certification))
                    (> (get expiry-date certification) block-height))
    false
  )
)

;; Get all certification types for a harvest
(define-read-only (get-harvest-certifications (harvest-id uint))
  (match (map-get? harvest-certifications { harvest-id: harvest-id })
    harvest-cert (ok (get certification-types harvest-cert))
    (ok (list))
  )
)

;; Public functions

;; Add a new certifier
(define-public (register-certifier (certifier principal) (name (string-ascii 100)))
  (begin
    ;; Only contract owner can register certifiers
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set certifiers certifier name)
    (ok true)
  )
)

;; Remove a certifier
(define-public (remove-certifier (certifier principal))
  (begin
    ;; Only contract owner can remove certifiers
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-delete certifiers certifier)
    (ok true)
  )
)

;; Issue a new certification for a harvest
(define-public (issue-certification 
  (harvest-id uint) 
  (certification-type uint) 
  (expiry-date uint) 
  (details (string-utf8 500)))
  
  (begin
    ;; Check that sender is an authorized certifier
    (asserts! (is-certifier tx-sender) ERR-INVALID-CERTIFIER)
    
    ;; Check that certification type is valid
    (asserts! (is-valid-certification-type certification-type) ERR-INVALID-CERTIFICATION-TYPE)
    
    ;; Check that expiry date is in the future
    (asserts! (> expiry-date block-height) ERR-INVALID-EXPIRY)
    
    ;; Check that certification doesn't already exist
    (asserts! (is-none (map-get? certifications 
                         { harvest-id: harvest-id, certification-type: certification-type }))
              ERR-CERTIFICATION-EXISTS)
    
    ;; Set the certification
    (map-set certifications
      { harvest-id: harvest-id, certification-type: certification-type }
      {
        certifier: tx-sender,
        issue-date: block-height,
        expiry-date: expiry-date,
        details: details,
        is-revoked: false,
        revocation-reason: none
      }
    )
    
    ;; Update the harvest's certification list
    (add-certification-to-harvest harvest-id certification-type)
  )
)

;; Renew an existing certification
(define-public (renew-certification 
  (harvest-id uint) 
  (certification-type uint) 
  (new-expiry-date uint) 
  (new-details (string-utf8 500)))
  
  (let ((existing-certification (map-get? certifications 
                                  { harvest-id: harvest-id, certification-type: certification-type })))
    (begin
      ;; Check that certification exists
      (asserts! (is-some existing-certification) ERR-CERTIFICATION-NOT-FOUND)
      
      ;; Check that sender is either the original certifier or contract owner
      (asserts! (or 
                 (is-eq tx-sender (get certifier (unwrap-panic existing-certification)))
                 (is-eq tx-sender (var-get contract-owner)))
                ERR-NOT-AUTHORIZED)
      
      ;; Check that expiry date is in the future
      (asserts! (> new-expiry-date block-height) ERR-INVALID-EXPIRY)
      
      ;; Update the certification
      (map-set certifications
        { harvest-id: harvest-id, certification-type: certification-type }
        {
          certifier: (get certifier (unwrap-panic existing-certification)),
          issue-date: (get issue-date (unwrap-panic existing-certification)),
          expiry-date: new-expiry-date,
          details: new-details,
          is-revoked: false,
          revocation-reason: none
        }
      )
      
      (ok true)
    )
  )
)

;; Revoke a certification
(define-public (revoke-certification 
  (harvest-id uint) 
  (certification-type uint) 
  (reason (string-utf8 500)))
  
  (let ((existing-certification (map-get? certifications 
                                  { harvest-id: harvest-id, certification-type: certification-type })))
    (begin
      ;; Check that certification exists
      (asserts! (is-some existing-certification) ERR-CERTIFICATION-NOT-FOUND)
      
      ;; Check that sender is either the original certifier or contract owner
      (asserts! (or 
                 (is-eq tx-sender (get certifier (unwrap-panic existing-certification)))
                 (is-eq tx-sender (var-get contract-owner)))
                ERR-NOT-AUTHORIZED)
      
      ;; Update the certification to revoked status
      (map-set certifications
        { harvest-id: harvest-id, certification-type: certification-type }
        {
          certifier: (get certifier (unwrap-panic existing-certification)),
          issue-date: (get issue-date (unwrap-panic existing-certification)),
          expiry-date: (get expiry-date (unwrap-panic existing-certification)),
          details: (get details (unwrap-panic existing-certification)),
          is-revoked: true,
          revocation-reason: (some reason)
        }
      )
      
      (ok true)
    )
  )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)