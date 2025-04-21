(define-data-var admin principal tx-sender)
(define-data-var total-credits uint u0)
(define-data-var paused bool false)

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VERIFIED (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-HOURS (err u103))
(define-constant ERR-CONTRACT-PAUSED (err u104))
(define-constant ERR-ALREADY-REGISTERED (err u105))
(define-constant ERR-INSUFFICIENT-BALANCE (err u106))
(define-constant ERR-ORGANIZATION-NOT-FOUND (err u107))
(define-constant ERR-VOLUNTEER-NOT-FOUND (err u108))

(define-map volunteers
  { volunteer: principal }
  {
    name: (string-ascii 50),
    total-hours: uint,
    verified-hours: uint,
    credits-balance: uint,
    registered: bool
  }
)

(define-map organizations
  { org-id: principal }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    verified: bool,
    total-hours-verified: uint
  }
)

(define-map volunteer-activities
  { activity-id: uint }
  {
    volunteer: principal,
    organization: principal,
    hours: uint,
    description: (string-ascii 200),
    date: uint,
    verified: bool
  }
)

(define-data-var activity-counter uint u0)

(define-read-only (get-admin)
  (var-get admin)
)

(define-read-only (get-volunteer-info (volunteer principal))
  (match (map-get? volunteers { volunteer: volunteer })
    volunteer-data (ok volunteer-data)
    (err ERR-VOLUNTEER-NOT-FOUND)
  )
)

(define-read-only (get-organization-info (org-id principal))
  (match (map-get? organizations { org-id: org-id })
    org-data (ok org-data)
    (err ERR-ORGANIZATION-NOT-FOUND)
  )
)

(define-read-only (get-activity (activity-id uint))
  (match (map-get? volunteer-activities { activity-id: activity-id })
    activity (ok activity)
    (err ERR-NOT-FOUND)
  )
)

(define-read-only (get-total-credits)
  (var-get total-credits)
)

(define-public (register-volunteer (name (string-ascii 50)))
  (begin
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (match (map-get? volunteers { volunteer: tx-sender })
      volunteer-data (err ERR-ALREADY-REGISTERED)
      (begin
        (map-set volunteers
          { volunteer: tx-sender }
          {
            name: name,
            total-hours: u0,
            verified-hours: u0,
            credits-balance: u0,
            registered: true
          }
        )
        (ok true)
      )
    )
  )
)

(define-public (register-organization (name (string-ascii 50)) (description (string-ascii 200)))
  (begin
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (match (map-get? organizations { org-id: tx-sender })
      org-data (err ERR-ALREADY-REGISTERED)
      (begin
        (map-set organizations
          { org-id: tx-sender }
          {
            name: name,
            description: description,
            verified: false,
            total-hours-verified: u0
          }
        )
        (ok true)
      )
    )
  )
)

(define-public (verify-organization (org-id principal))
  (begin
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (match (map-get? organizations { org-id: org-id })
      org-data
        (begin
          (map-set organizations
            { org-id: org-id }
            (merge org-data { verified: true })
          )
          (ok true)
        )
      (err ERR-ORGANIZATION-NOT-FOUND)
    )
  )
)

(define-public (log-volunteer-activity (organization principal) (hours uint) (description (string-ascii 200)) (date uint))
  (let ((activity-id (var-get activity-counter)))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (> hours u0) (err ERR-INVALID-HOURS))
    (asserts! (match (map-get? volunteers { volunteer: tx-sender })
      volunteer-data true
      false) (err ERR-VOLUNTEER-NOT-FOUND))
    (asserts! (match (map-get? organizations { org-id: organization })
      org-data true
      false) (err ERR-ORGANIZATION-NOT-FOUND))
    
    (map-set volunteer-activities
      { activity-id: activity-id }
      {
        volunteer: tx-sender,
        organization: organization,
        hours: hours,
        description: description,
        date: date,
        verified: false
      }
    )
    
    (var-set activity-counter (+ activity-id u1))
    (ok activity-id)
  )
)

(define-public (verify-activity (activity-id uint))
  (let ((activity (unwrap! (map-get? volunteer-activities { activity-id: activity-id }) (err ERR-NOT-FOUND))))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (is-eq tx-sender (get organization activity)) (err ERR-NOT-AUTHORIZED))
    (asserts! (not (get verified activity)) (err ERR-ALREADY-VERIFIED))
    
    (let (
      (volunteer (get volunteer activity))
      (hours (get hours activity))
      (volunteer-data (unwrap! (map-get? volunteers { volunteer: volunteer }) (err ERR-VOLUNTEER-NOT-FOUND)))
      (org-data (unwrap! (map-get? organizations { org-id: tx-sender }) (err ERR-ORGANIZATION-NOT-FOUND)))
    )
      
      (map-set volunteer-activities
        { activity-id: activity-id }
        (merge activity { verified: true })
      )
      
      (map-set volunteers
        { volunteer: volunteer }
        (merge volunteer-data {
          total-hours: (+ (get total-hours volunteer-data) hours),
          verified-hours: (+ (get verified-hours volunteer-data) hours),
          credits-balance: (+ (get credits-balance volunteer-data) hours)
        })
      )
      
      (map-set organizations
        { org-id: tx-sender }
        (merge org-data {
          total-hours-verified: (+ (get total-hours-verified org-data) hours)
        })
      )
      
      (var-set total-credits (+ (var-get total-credits) hours))
      (ok true)
    )
  )
)

(define-public (transfer-credits (recipient principal) (amount uint))
  (let ((sender-data (unwrap! (map-get? volunteers { volunteer: tx-sender }) (err ERR-VOLUNTEER-NOT-FOUND)))
        (recipient-data (unwrap! (map-get? volunteers { volunteer: recipient }) (err ERR-VOLUNTEER-NOT-FOUND))))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (>= (get credits-balance sender-data) amount) (err ERR-INSUFFICIENT-BALANCE))
    
    (map-set volunteers
      { volunteer: tx-sender }
      (merge sender-data {
        credits-balance: (- (get credits-balance sender-data) amount)
      })
    )
    
    (map-set volunteers
      { volunteer: recipient }
      (merge recipient-data {
        credits-balance: (+ (get credits-balance recipient-data) amount)
      })
    )
    
    (ok true)
  )
)

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (var-set admin new-admin)
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (var-set paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (var-set paused false)
    (ok true)
  )
)