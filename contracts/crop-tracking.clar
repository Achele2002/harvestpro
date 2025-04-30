;; crop-tracking.clar
;; A smart contract for recording and tracking crop planting, growth cycles, harvest data, and yield metrics.
;; This contract serves as a central data repository for agricultural production information, enabling
;; transparent and verifiable tracking of crops from planting to harvest.

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u1001))
(define-constant ERR_INVALID_CROP_ID (err u1002))
(define-constant ERR_INVALID_LAND_PARCEL (err u1003))
(define-constant ERR_INVALID_PLANTING_DATE (err u1004))
(define-constant ERR_CROP_ALREADY_EXISTS (err u1005))
(define-constant ERR_CROP_NOT_FOUND (err u1006))
(define-constant ERR_GROWTH_MILESTONE_INVALID (err u1007))
(define-constant ERR_HARVEST_COMPLETED (err u1008))
(define-constant ERR_INVALID_HARVEST_DATA (err u1009))
(define-constant ERR_NOT_READY_FOR_HARVEST (err u1010))

;; Data structures

;; Crop data map: stores basic information about each planted crop
(define-map crops
  { crop-id: uint }
  {
    farmer: principal,
    land-parcel-id: uint,
    crop-variety: (string-ascii 64),
    planting-date: uint,
    expected-harvest-date: uint,
    status: (string-ascii 20),  ;; "planted", "growing", "harvested"
    harvest-completed: bool
  }
)

;; Crop inputs map: records all inputs used on a specific crop
(define-map crop-inputs
  { crop-id: uint, input-id: uint }
  {
    input-type: (string-ascii 20),  ;; "seed", "fertilizer", "pesticide", etc.
    input-name: (string-ascii 64),
    quantity: uint,
    application-date: uint,
    notes: (string-utf8 500)
  }
)

;; Crop environmental conditions: records environmental data during growth
(define-map crop-environment
  { crop-id: uint, record-id: uint }
  {
    record-date: uint,
    temperature: int,  ;; in celsius * 100 (for decimal precision)
    humidity: uint,    ;; percentage
    rainfall: uint,    ;; in millimeters * 100 (for decimal precision)
    notes: (string-utf8 500)
  }
)

;; Growth milestones: records key stages in crop development
(define-map growth-milestones
  { crop-id: uint, milestone-id: uint }
  {
    milestone-date: uint,
    milestone-name: (string-ascii 64),
    description: (string-utf8 500),
    recorded-by: principal
  }
)

;; Harvest data: records final yield and harvest information
(define-map harvest-data
  { crop-id: uint }
  {
    harvest-date: uint,
    yield-quantity: uint,  ;; in kilograms * 100 (for decimal precision)
    yield-quality: (string-ascii 20), ;; e.g. "premium", "standard", "low"
    harvester: principal,
    notes: (string-utf8 500)
  }
)

;; Counter variables for generating unique IDs
(define-data-var crop-id-counter uint u1)
(define-data-var input-id-counter uint u1)
(define-data-var environment-record-id-counter uint u1)
(define-data-var milestone-id-counter uint u1)

;; Private functions

;; Checks if the caller is the registered farmer for this crop
(define-private (is-crop-owner (crop-id uint))
  (let ((crop-info (map-get? crops { crop-id: crop-id })))
    (if (is-some crop-info)
      (is-eq tx-sender (get farmer (unwrap-panic crop-info)))
      false
    )
  )
)

;; Check if a crop exists
(define-private (crop-exists (crop-id uint))
  (is-some (map-get? crops { crop-id: crop-id }))
)

;; Get next crop ID and increment counter
(define-private (get-next-crop-id)
  (let ((next-id (var-get crop-id-counter)))
    (var-set crop-id-counter (+ next-id u1))
    next-id
  )
)

;; Get next input ID and increment counter
(define-private (get-next-input-id)
  (let ((next-id (var-get input-id-counter)))
    (var-set input-id-counter (+ next-id u1))
    next-id
  )
)

;; Get next environment record ID and increment counter
(define-private (get-next-environment-record-id)
  (let ((next-id (var-get environment-record-id-counter)))
    (var-set environment-record-id-counter (+ next-id u1))
    next-id
  )
)

;; Get next milestone ID and increment counter
(define-private (get-next-milestone-id)
  (let ((next-id (var-get milestone-id-counter)))
    (var-set milestone-id-counter (+ next-id u1))
    next-id
  )
)

;; Read-only functions

;; Get crop information
(define-read-only (get-crop-info (crop-id uint))
  (match (map-get? crops { crop-id: crop-id })
    crop-data (ok crop-data)
    ERR_CROP_NOT_FOUND
  )
)

;; Get all inputs for a specific crop
(define-read-only (get-crop-input (crop-id uint) (input-id uint))
  (match (map-get? crop-inputs { crop-id: crop-id, input-id: input-id })
    input-data (ok input-data)
    ERR_CROP_NOT_FOUND
  )
)

;; Get environmental data for a specific crop
(define-read-only (get-crop-environment-record (crop-id uint) (record-id uint))
  (match (map-get? crop-environment { crop-id: crop-id, record-id: record-id })
    env-data (ok env-data)
    ERR_CROP_NOT_FOUND
  )
)

;; Get growth milestone for a specific crop
(define-read-only (get-growth-milestone (crop-id uint) (milestone-id uint))
  (match (map-get? growth-milestones { crop-id: crop-id, milestone-id: milestone-id })
    milestone (ok milestone)
    ERR_CROP_NOT_FOUND
  )
)

;; Get harvest data for a specific crop
(define-read-only (get-harvest-data (crop-id uint))
  (match (map-get? harvest-data { crop-id: crop-id })
    harvest (ok harvest)
    ERR_CROP_NOT_FOUND
  )
)

;; Public functions

;; Register a new crop planting
(define-public (register-crop-planting 
    (land-parcel-id uint) 
    (crop-variety (string-ascii 64)) 
    (planting-date uint) 
    (expected-harvest-date uint))
  (let 
    (
      (new-crop-id (get-next-crop-id))
    )
    ;; Basic validation
    (asserts! (> planting-date u0) ERR_INVALID_PLANTING_DATE)
    (asserts! (> expected-harvest-date planting-date) ERR_INVALID_PLANTING_DATE)
    (asserts! (> land-parcel-id u0) ERR_INVALID_LAND_PARCEL)
    
    ;; Store the crop data
    (map-set crops
      { crop-id: new-crop-id }
      {
        farmer: tx-sender,
        land-parcel-id: land-parcel-id,
        crop-variety: crop-variety,
        planting-date: planting-date,
        expected-harvest-date: expected-harvest-date,
        status: "planted",
        harvest-completed: false
      }
    )
    (ok new-crop-id)
  )
)

;; Record an input application (seeds, fertilizers, pesticides, etc.)
(define-public (record-crop-input 
    (crop-id uint) 
    (input-type (string-ascii 20)) 
    (input-name (string-ascii 64))
    (quantity uint)
    (application-date uint)
    (notes (string-utf8 500)))
  (let 
    (
      (new-input-id (get-next-input-id))
    )
    ;; Validate ownership and crop exists
    (asserts! (crop-exists crop-id) ERR_INVALID_CROP_ID)
    (asserts! (is-crop-owner crop-id) ERR_UNAUTHORIZED)
    
    ;; Store the input data
    (map-set crop-inputs
      { crop-id: crop-id, input-id: new-input-id }
      {
        input-type: input-type,
        input-name: input-name,
        quantity: quantity,
        application-date: application-date,
        notes: notes
      }
    )
    (ok new-input-id)
  )
)

;; Record environmental conditions
(define-public (record-environment-data
    (crop-id uint)
    (record-date uint)
    (temperature int)
    (humidity uint)
    (rainfall uint)
    (notes (string-utf8 500)))
  (let
    (
      (new-record-id (get-next-environment-record-id))
    )
    ;; Validate ownership and crop exists
    (asserts! (crop-exists crop-id) ERR_INVALID_CROP_ID)
    (asserts! (is-crop-owner crop-id) ERR_UNAUTHORIZED)
    
    ;; Store the environmental data
    (map-set crop-environment
      { crop-id: crop-id, record-id: new-record-id }
      {
        record-date: record-date,
        temperature: temperature,
        humidity: humidity,
        rainfall: rainfall,
        notes: notes
      }
    )
    (ok new-record-id)
  )
)

;; Record a growth milestone
(define-public (record-growth-milestone
    (crop-id uint)
    (milestone-date uint)
    (milestone-name (string-ascii 64))
    (description (string-utf8 500)))
  (let
    (
      (new-milestone-id (get-next-milestone-id))
      (crop-info (map-get? crops { crop-id: crop-id }))
    )
    ;; Validate ownership and crop exists
    (asserts! (is-some crop-info) ERR_INVALID_CROP_ID)
    (asserts! (is-crop-owner crop-id) ERR_UNAUTHORIZED)
    
    ;; Validate milestone date is after planting
    (asserts! (>= milestone-date (get planting-date (unwrap-panic crop-info))) ERR_GROWTH_MILESTONE_INVALID)
    
    ;; Store the milestone
    (map-set growth-milestones
      { crop-id: crop-id, milestone-id: new-milestone-id }
      {
        milestone-date: milestone-date,
        milestone-name: milestone-name,
        description: description,
        recorded-by: tx-sender
      }
    )
    
    ;; Update the crop status to "growing" if it was "planted"
    (if (is-eq (get status (unwrap-panic crop-info)) "planted")
      (map-set crops
        { crop-id: crop-id }
        (merge (unwrap-panic crop-info) { status: "growing" })
      )
      true
    )
    
    (ok new-milestone-id)
  )
)

;; Record harvest data
(define-public (record-harvest
    (crop-id uint)
    (harvest-date uint)
    (yield-quantity uint)
    (yield-quality (string-ascii 20))
    (notes (string-utf8 500)))
  (let
    (
      (crop-info (map-get? crops { crop-id: crop-id }))
    )
    ;; Validate ownership and crop exists
    (asserts! (is-some crop-info) ERR_INVALID_CROP_ID)
    (asserts! (is-crop-owner crop-id) ERR_UNAUTHORIZED)
    
    ;; Check the crop hasn't been harvested already
    (asserts! (not (get harvest-completed (unwrap-panic crop-info))) ERR_HARVEST_COMPLETED)
    
    ;; Validate harvest date
    (asserts! (>= harvest-date (get planting-date (unwrap-panic crop-info))) ERR_INVALID_HARVEST_DATA)
    
    ;; Store the harvest data
    (map-set harvest-data
      { crop-id: crop-id }
      {
        harvest-date: harvest-date,
        yield-quantity: yield-quantity,
        yield-quality: yield-quality,
        harvester: tx-sender,
        notes: notes
      }
    )
    
    ;; Update the crop status to "harvested" and mark harvest as completed
    (map-set crops
      { crop-id: crop-id }
      (merge (unwrap-panic crop-info) 
        { 
          status: "harvested",
          harvest-completed: true
        }
      )
    )
    
    (ok true)
  )
)

;; Update crop expected harvest date
(define-public (update-expected-harvest-date
    (crop-id uint)
    (new-expected-harvest-date uint))
  (let
    (
      (crop-info (map-get? crops { crop-id: crop-id }))
    )
    ;; Validate ownership and crop exists
    (asserts! (is-some crop-info) ERR_INVALID_CROP_ID)
    (asserts! (is-crop-owner crop-id) ERR_UNAUTHORIZED)
    
    ;; Check the crop hasn't been harvested already
    (asserts! (not (get harvest-completed (unwrap-panic crop-info))) ERR_HARVEST_COMPLETED)
    
    ;; Validate new harvest date is after planting date
    (asserts! (> new-expected-harvest-date (get planting-date (unwrap-panic crop-info))) ERR_INVALID_PLANTING_DATE)
    
    ;; Update the crop information
    (map-set crops
      { crop-id: crop-id }
      (merge (unwrap-panic crop-info) 
        { 
          expected-harvest-date: new-expected-harvest-date
        }
      )
    )
    
    (ok true)
  )
)