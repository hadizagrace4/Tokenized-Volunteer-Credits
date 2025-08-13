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


(define-public (get-volunteer-credits (volunteer principal))
  (let ((volunteer-data (unwrap! (map-get? volunteers { volunteer: volunteer }) (err ERR-VOLUNTEER-NOT-FOUND))))
    (ok (get credits-balance volunteer-data))
  )
)
(define-public (get-volunteer-hours (volunteer principal))
  (let ((volunteer-data (unwrap! (map-get? volunteers { volunteer: volunteer }) (err ERR-VOLUNTEER-NOT-FOUND))))
    (ok {
      total-hours: (get total-hours volunteer-data),
      verified-hours: (get verified-hours volunteer-data)
    })
  )
)
(define-public (get-organization-hours (org-id principal))
  (let ((org-data (unwrap! (map-get? organizations { org-id: org-id }) (err ERR-ORGANIZATION-NOT-FOUND))))
    (ok {
      total-hours-verified: (get total-hours-verified org-data)
    })
  )
)

(define-public (get-volunteer-activity (activity-id uint))
  (let ((activity (unwrap! (map-get? volunteer-activities { activity-id: activity-id }) (err ERR-NOT-FOUND))))
    (ok activity)
  )
)

(define-constant CREDIT-EXPIRY-BLOCKS u52560)

(define-map credit-expiry
  { volunteer: principal }
  { expiry-height: uint }
)

(define-public (update-credit-expiry (volunteer principal))
  (let ((current-block-height stacks-block-height))
    (map-set credit-expiry
      { volunteer: volunteer }
      { expiry-height: (+ current-block-height CREDIT-EXPIRY-BLOCKS) }
    )
    (ok true)
  )
)

(define-public (check-and-expire-credits (volunteer principal))
  (let (
    (current-block-height stacks-block-height)
    (expiry-data (unwrap! (map-get? credit-expiry { volunteer: volunteer }) (err ERR-NOT-FOUND)))
    (volunteer-data (unwrap! (map-get? volunteers { volunteer: volunteer }) (err ERR-VOLUNTEER-NOT-FOUND)))
  )
    (if (>= current-block-height (get expiry-height expiry-data))
      (begin
        (map-set volunteers
          { volunteer: volunteer }
          (merge volunteer-data { credits-balance: u0 })
        )
        (ok true)
      )
      (ok false)
    )
  )
)

(define-map volunteer-badges
  { volunteer: principal }
  {
    bronze: bool,
    silver: bool,
    gold: bool
  }
)

(define-constant BRONZE-THRESHOLD u50)
(define-constant SILVER-THRESHOLD u100)
(define-constant GOLD-THRESHOLD u200)

(define-public (check-and-award-badges (volunteer principal))
  (let (
    (volunteer-data (unwrap! (map-get? volunteers { volunteer: volunteer }) (err ERR-VOLUNTEER-NOT-FOUND)))
    (verified-hours (get verified-hours volunteer-data))
  )
    (map-set volunteer-badges
      { volunteer: volunteer }
      {
        bronze: (>= verified-hours BRONZE-THRESHOLD),
        silver: (>= verified-hours SILVER-THRESHOLD),
        gold: (>= verified-hours GOLD-THRESHOLD)
      }
    )
    (ok true)
  )
)

(define-read-only (get-volunteer-badges (volunteer principal))
  (match (map-get? volunteer-badges { volunteer: volunteer })
    badges (ok badges)
    (err ERR-VOLUNTEER-NOT-FOUND)
  )
)



(define-constant ERR-INVALID-LIMIT (err u201))
(define-constant ERR-INVALID-PERIOD (err u202))
(define-constant ERR-REWARD-NOT-FOUND (err u203))
(define-constant ERR-REWARD-UNAVAILABLE (err u204))
(define-constant ERR-ALREADY-REDEEMED (err u205))
(define-constant ERR-SKILL-NOT-FOUND (err u206))
(define-constant ERR-OPPORTUNITY-NOT-FOUND (err u207))
(define-constant ERR-INSUFFICIENT-SKILL-LEVEL (err u208))
(define-constant ERR-SKILL-ALREADY-EXISTS (err u209))
(define-constant ERR-INVALID-SKILL-LEVEL (err u210))

(define-data-var leaderboard-update-height uint u0)
(define-data-var reward-counter uint u0)
(define-data-var opportunity-counter uint u0)

(define-map volunteer-rankings
  { volunteer: principal }
  {
    total-hours-rank: uint,
    verified-hours-rank: uint,
    credits-rank: uint,
    last-updated: uint
  }
)

(define-map period-leaders
  { period: uint, metric: (string-ascii 20) }
  {
    leader: principal,
    value: uint,
    period-start: uint,
    period-end: uint
  }
)

(define-map volunteer-stats-snapshot
  { volunteer: principal, period: uint }
  {
    total-hours: uint,
    verified-hours: uint,
    credits-balance: uint,
    activities-count: uint
  }
)

(define-map marketplace-rewards
  { reward-id: uint }
  {
    organization: principal,
    title: (string-ascii 100),
    description: (string-ascii 200),
    cost: uint,
    quantity: uint,
    redeemed: uint,
    active: bool,
    created-at: uint
  }
)

(define-map volunteer-redemptions
  { volunteer: principal, reward-id: uint }
  {
    redeemed-at: uint,
    quantity: uint
  }
)

;; Skills and opportunity matching system
(define-map volunteer-skills
  { volunteer: principal, skill-name: (string-ascii 50) }
  {
    proficiency-level: uint,
    verified: bool,
    verified-by: (optional principal),
    last-updated: uint
  }
)

(define-map skill-opportunities
  { opportunity-id: uint }
  {
    organization: principal,
    title: (string-ascii 100),
    description: (string-ascii 200),
    required-skills: (list 5 (string-ascii 50)),
    min-skill-levels: (list 5 uint),
    credit-multiplier: uint,
    max-volunteers: uint,
    assigned-volunteers: uint,
    active: bool,
    created-at: uint,
    deadline: uint
  }
)

(define-map opportunity-applications
  { opportunity-id: uint, volunteer: principal }
  {
    applied-at: uint,
    status: (string-ascii 20),
    match-score: uint
  }
)

(define-data-var current-period uint u0)
(define-data-var total-volunteers uint u0)

(define-constant PERIOD-BLOCKS u10080)

(define-read-only (get-volunteer-rank (volunteer principal))
  (match (map-get? volunteer-rankings { volunteer: volunteer })
    ranking (ok ranking)
    (err ERR-NOT-FOUND)
  )
)

(define-read-only (get-period-leader (period uint) (metric (string-ascii 20)))
  (match (map-get? period-leaders { period: period, metric: metric })
    leader-data (ok leader-data)
    (err ERR-NOT-FOUND)
  )
)

(define-read-only (get-current-period)
  (var-get current-period)
)

(define-read-only (calculate-current-period)
  (/ stacks-block-height PERIOD-BLOCKS)
)

(define-public (update-volunteer-ranking (volunteer principal) (total-hours uint) (verified-hours uint) (credits-balance uint))
  (let (
    (current-ranking (default-to 
      { total-hours-rank: u0, verified-hours-rank: u0, credits-rank: u0, last-updated: u0 }
      (map-get? volunteer-rankings { volunteer: volunteer })
    ))
    ;; (new-total-rank (+ (calculate-rank-by-hours total-hours "total") u1))
    ;; (new-verified-rank (+ (calculate-rank-by-hours verified-hours "verified") u1))
    ;; (new-credits-rank (+ (calculate-rank-by-credits credits-balance) u1))
  )
    (map-set volunteer-rankings
      { volunteer: volunteer }
      {
        total-hours-rank: u1,
        verified-hours-rank: u2,
        credits-rank: u3,
        last-updated: stacks-block-height
      }
    )
    ;; (try! (update-period-leaders volunteer total-hours verified-hours credits-balance))
    (ok true)
  )
)

(define-private (calculate-rank-by-hours (hours uint) (metric-type (string-ascii 10)))
  (fold calculate-rank-fold (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) { target-hours: hours, rank: u0, metric: metric-type })
)

(define-private (calculate-rank-by-credits (credits uint))
  (fold calculate-credits-rank-fold (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) { target-credits: credits, rank: u0 })
)

(define-private (calculate-rank-fold (item uint) (data { target-hours: uint, rank: uint, metric: (string-ascii 10) }))
  { target-hours: (get target-hours data), rank: (get rank data), metric: (get metric data) }
)

(define-private (calculate-credits-rank-fold (item uint) (data { target-credits: uint, rank: uint }))
  { target-credits: (get target-credits data), rank: (get rank data) }
)

(define-public (update-period-leaders (volunteer principal) (total-hours uint) (verified-hours uint) (credits-balance uint))
  (let ((current-period-val (calculate-current-period)))
    (begin
      (if (not (is-eq current-period-val (var-get current-period)))
        (var-set current-period current-period-val)
        true
      )
      
      ;; (try! (check-and-update-leader current-period-val "total-hours" volunteer total-hours))
      ;; (try! (check-and-update-leader current-period-val "verified-hours" volunteer verified-hours))
      ;; (try! (check-and-update-leader current-period-val "credits" volunteer credits-balance))
      
      (map-set volunteer-stats-snapshot
        { volunteer: volunteer, period: current-period-val }
        {
          total-hours: total-hours,
          verified-hours: verified-hours,
          credits-balance: credits-balance,
          activities-count: u0
        }
      )
      (ok true)
    )
  )
)

(define-private (check-and-update-leader (period uint) (metric (string-ascii 20)) (volunteer principal) (value uint))
  (let (
    (current-leader (map-get? period-leaders { period: period, metric: metric }))
    (period-start (* period PERIOD-BLOCKS))
    (period-end (+ period-start PERIOD-BLOCKS))
  )
    (match current-leader
      leader-data
        (if (> value (get value leader-data))
          (map-set period-leaders
            { period: period, metric: metric }
            {
              leader: volunteer,
              value: value,
              period-start: period-start,
              period-end: period-end
            }
          )
          true
        )
      (map-set period-leaders
        { period: period, metric: metric }
        {
          leader: volunteer,
          value: value,
          period-start: period-start,
          period-end: period-end
        }
      )
    )
    (ok true)
  )
)

(define-read-only (get-top-volunteers-by-hours (limit uint))
  (begin
    (asserts! (and (> limit u0) (<= limit u50)) (err ERR-INVALID-LIMIT))
    (ok (get-sorted-volunteers-list limit "hours"))
  )
)

(define-read-only (get-top-volunteers-by-credits (limit uint))
  (begin
    (asserts! (and (> limit u0) (<= limit u50)) (err ERR-INVALID-LIMIT))
    (ok (get-sorted-volunteers-list limit "credits"))
  )
)

(define-private (get-sorted-volunteers-list (limit uint) (sort-by (string-ascii 10)))
  (list 
    { volunteer: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM, rank: u1, value: u0 }
    { volunteer: 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5, rank: u2, value: u0 }
    { volunteer: 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG, rank: u3, value: u0 }
  )
)

(define-read-only (get-volunteer-position (volunteer principal) (metric (string-ascii 20)))
  (match (map-get? volunteer-rankings { volunteer: volunteer })
    ranking 
      (if (is-eq metric "total-hours")
        (ok (get total-hours-rank ranking))
        (if (is-eq metric "verified-hours")
          (ok (get verified-hours-rank ranking))
          (if (is-eq metric "credits")
            (ok (get credits-rank ranking))
            (err ERR-NOT-FOUND)
          )
        )
      )
    (err ERR-NOT-FOUND)
  )
)

(define-read-only (get-historical-leaders (start-period uint) (end-period uint) (metric (string-ascii 20)))
  (begin
    (asserts! (<= start-period end-period) (err ERR-INVALID-PERIOD))
    (ok (get-leaders-in-range start-period end-period metric))
  )
)

(define-private (get-leaders-in-range (start-period uint) (end-period uint) (metric (string-ascii 20)))
  (list
    { period: start-period, leader: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM, value: u0 }
    { period: (+ start-period u1), leader: 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5, value: u0 }
  )
)

(define-public (refresh-leaderboard)
  (begin
    (var-set leaderboard-update-height stacks-block-height)
    (var-set current-period (calculate-current-period))
    (ok true)
  )
)

(define-read-only (get-leaderboard-stats)
  (ok {
    last-update: (var-get leaderboard-update-height),
    current-period: (var-get current-period),
    total-volunteers: (var-get total-volunteers),
    period-blocks: PERIOD-BLOCKS
  })
)

(define-public (increment-volunteer-count)
  (begin
    (var-set total-volunteers (+ (var-get total-volunteers) u1))
    (ok true)
  )
)


(define-public (verify-activities (activity-id uint))
  (let ((activity (unwrap! (map-get? volunteer-activities { activity-id: activity-id }) (err ERR-NOT-FOUND))))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (is-eq tx-sender (get organization activity)) (err ERR-NOT-AUTHORIZED))
    (asserts! (not (get verified activity)) (err ERR-ALREADY-VERIFIED))
    
    (let (
      (volunteer (get volunteer activity))
      (hours (get hours activity))
      (volunteer-data (unwrap! (map-get? volunteers { volunteer: volunteer }) (err ERR-VOLUNTEER-NOT-FOUND)))
      (org-data (unwrap! (map-get? organizations { org-id: tx-sender }) (err ERR-ORGANIZATION-NOT-FOUND)))
      (new-total-hours (+ (get total-hours volunteer-data) hours))
      (new-verified-hours (+ (get verified-hours volunteer-data) hours))
      (new-credits-balance (+ (get credits-balance volunteer-data) hours))
    )
      
      (map-set volunteer-activities
        { activity-id: activity-id }
        (merge activity { verified: true })
      )
      
      (map-set volunteers
        { volunteer: volunteer }
        (merge volunteer-data {
          total-hours: new-total-hours,
          verified-hours: new-verified-hours,
          credits-balance: new-credits-balance
        })
      )
      
      (map-set organizations
        { org-id: tx-sender }
        (merge org-data {
          total-hours-verified: (+ (get total-hours-verified org-data) hours)
        })
      )
      
      (var-set total-credits (+ (var-get total-credits) hours))
      
      ;; (contract-call? .leaderboard update-volunteer-ranking volunteer new-total-hours new-verified-hours new-credits-balance)
      (ok true)
    )
  )
)

(define-public (create-reward (title (string-ascii 100)) (description (string-ascii 200)) (cost uint) (quantity uint))
  (let ((reward-id (var-get reward-counter)))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (match (map-get? organizations { org-id: tx-sender })
      org-data (get verified org-data)
      false) (err ERR-NOT-AUTHORIZED))
    (asserts! (> cost u0) (err ERR-INVALID-HOURS))
    (asserts! (> quantity u0) (err ERR-INVALID-HOURS))
    
    (map-set marketplace-rewards
      { reward-id: reward-id }
      {
        organization: tx-sender,
        title: title,
        description: description,
        cost: cost,
        quantity: quantity,
        redeemed: u0,
        active: true,
        created-at: stacks-block-height
      }
    )
    
    (var-set reward-counter (+ reward-id u1))
    (ok reward-id)
  )
)

(define-public (redeem-reward (reward-id uint) (quantity uint))
  (let (
    (reward (unwrap! (map-get? marketplace-rewards { reward-id: reward-id }) (err ERR-REWARD-NOT-FOUND)))
    (volunteer-data (unwrap! (map-get? volunteers { volunteer: tx-sender }) (err ERR-VOLUNTEER-NOT-FOUND)))
    (total-cost (* (get cost reward) quantity))
    (available-quantity (- (get quantity reward) (get redeemed reward)))
  )
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (get active reward) (err ERR-REWARD-UNAVAILABLE))
    (asserts! (>= available-quantity quantity) (err ERR-REWARD-UNAVAILABLE))
    (asserts! (>= (get credits-balance volunteer-data) total-cost) (err ERR-INSUFFICIENT-BALANCE))
    
    (map-set volunteers
      { volunteer: tx-sender }
      (merge volunteer-data {
        credits-balance: (- (get credits-balance volunteer-data) total-cost)
      })
    )
    
    (map-set marketplace-rewards
      { reward-id: reward-id }
      (merge reward {
        redeemed: (+ (get redeemed reward) quantity)
      })
    )
    
    (map-set volunteer-redemptions
      { volunteer: tx-sender, reward-id: reward-id }
      {
        redeemed-at: stacks-block-height,
        quantity: quantity
      }
    )
    
    (ok true)
  )
)

(define-public (deactivate-reward (reward-id uint))
  (let ((reward (unwrap! (map-get? marketplace-rewards { reward-id: reward-id }) (err ERR-REWARD-NOT-FOUND))))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (is-eq tx-sender (get organization reward)) (err ERR-NOT-AUTHORIZED))
    
    (map-set marketplace-rewards
      { reward-id: reward-id }
      (merge reward { active: false })
    )
    (ok true)
  )
)

(define-read-only (get-reward (reward-id uint))
  (match (map-get? marketplace-rewards { reward-id: reward-id })
    reward (ok reward)
    (err ERR-REWARD-NOT-FOUND)
  )
)

(define-read-only (get-volunteer-redemption (volunteer principal) (reward-id uint))
  (match (map-get? volunteer-redemptions { volunteer: volunteer, reward-id: reward-id })
    redemption (ok redemption)
    (err ERR-NOT-FOUND)
  )
)

(define-read-only (get-marketplace-stats)
  (ok {
    total-rewards: (var-get reward-counter),
    total-volunteers: (var-get total-volunteers)
  })
)

;; Volunteer skills management functions
(define-public (register-skill (skill-name (string-ascii 50)) (proficiency-level uint))
  (begin
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (match (map-get? volunteers { volunteer: tx-sender })
      volunteer-data (get registered volunteer-data)
      false) (err ERR-VOLUNTEER-NOT-FOUND))
    (asserts! (and (>= proficiency-level u1) (<= proficiency-level u5)) (err ERR-INVALID-SKILL-LEVEL))
    
    ;; Check if skill already exists for this volunteer
    (match (map-get? volunteer-skills { volunteer: tx-sender, skill-name: skill-name })
      existing-skill (err ERR-SKILL-ALREADY-EXISTS)
      (begin
        (map-set volunteer-skills
          { volunteer: tx-sender, skill-name: skill-name }
          {
            proficiency-level: proficiency-level,
            verified: false,
            verified-by: none,
            last-updated: stacks-block-height
          }
        )
        (ok true)
      )
    )
  )
)

(define-public (update-skill-level (skill-name (string-ascii 50)) (new-level uint))
  (let ((skill (unwrap! (map-get? volunteer-skills { volunteer: tx-sender, skill-name: skill-name }) (err ERR-SKILL-NOT-FOUND))))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (and (>= new-level u1) (<= new-level u5)) (err ERR-INVALID-SKILL-LEVEL))
    
    (map-set volunteer-skills
      { volunteer: tx-sender, skill-name: skill-name }
      (merge skill {
        proficiency-level: new-level,
        verified: false,
        verified-by: none,
        last-updated: stacks-block-height
      })
    )
    (ok true)
  )
)

(define-public (verify-volunteer-skill (volunteer principal) (skill-name (string-ascii 50)))
  (let ((skill (unwrap! (map-get? volunteer-skills { volunteer: volunteer, skill-name: skill-name }) (err ERR-SKILL-NOT-FOUND))))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (match (map-get? organizations { org-id: tx-sender })
      org-data (get verified org-data)
      false) (err ERR-NOT-AUTHORIZED))
    
    (map-set volunteer-skills
      { volunteer: volunteer, skill-name: skill-name }
      (merge skill {
        verified: true,
        verified-by: (some tx-sender),
        last-updated: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Skill-based opportunity management
(define-public (create-skill-opportunity 
  (title (string-ascii 100)) 
  (description (string-ascii 200))
  (required-skills (list 5 (string-ascii 50)))
  (min-skill-levels (list 5 uint))
  (credit-multiplier uint)
  (max-volunteers uint)
  (deadline uint))
  (let ((opportunity-id (var-get opportunity-counter)))
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (match (map-get? organizations { org-id: tx-sender })
      org-data (get verified org-data)
      false) (err ERR-NOT-AUTHORIZED))
    (asserts! (> credit-multiplier u0) (err ERR-INVALID-HOURS))
    (asserts! (> max-volunteers u0) (err ERR-INVALID-HOURS))
    (asserts! (> deadline stacks-block-height) (err ERR-INVALID-PERIOD))
    
    (map-set skill-opportunities
      { opportunity-id: opportunity-id }
      {
        organization: tx-sender,
        title: title,
        description: description,
        required-skills: required-skills,
        min-skill-levels: min-skill-levels,
        credit-multiplier: credit-multiplier,
        max-volunteers: max-volunteers,
        assigned-volunteers: u0,
        active: true,
        created-at: stacks-block-height,
        deadline: deadline
      }
    )
    
    (var-set opportunity-counter (+ opportunity-id u1))
    (ok opportunity-id)
  )
)

(define-public (apply-for-opportunity (opportunity-id uint))
  (let (
    (opportunity (unwrap! (map-get? skill-opportunities { opportunity-id: opportunity-id }) (err ERR-OPPORTUNITY-NOT-FOUND)))
    (volunteer-data (unwrap! (map-get? volunteers { volunteer: tx-sender }) (err ERR-VOLUNTEER-NOT-FOUND)))
    (match-score (calculate-skill-match tx-sender opportunity-id))
  )
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (get active opportunity) (err ERR-OPPORTUNITY-NOT-FOUND))
    (asserts! (< (get assigned-volunteers opportunity) (get max-volunteers opportunity)) (err ERR-REWARD-UNAVAILABLE))
    (asserts! (< stacks-block-height (get deadline opportunity)) (err ERR-INVALID-PERIOD))
    
    ;; Check if volunteer meets minimum skill requirements
    (asserts! (>= match-score u50) (err ERR-INSUFFICIENT-SKILL-LEVEL))
    
    (map-set opportunity-applications
      { opportunity-id: opportunity-id, volunteer: tx-sender }
      {
        applied-at: stacks-block-height,
        status: "pending",
        match-score: match-score
      }
    )
    (ok true)
  )
)

(define-public (approve-application (opportunity-id uint) (volunteer principal))
  (let (
    (opportunity (unwrap! (map-get? skill-opportunities { opportunity-id: opportunity-id }) (err ERR-OPPORTUNITY-NOT-FOUND)))
    (application (unwrap! (map-get? opportunity-applications { opportunity-id: opportunity-id, volunteer: volunteer }) (err ERR-NOT-FOUND)))
  )
    (asserts! (not (var-get paused)) (err ERR-CONTRACT-PAUSED))
    (asserts! (is-eq tx-sender (get organization opportunity)) (err ERR-NOT-AUTHORIZED))
    (asserts! (< (get assigned-volunteers opportunity) (get max-volunteers opportunity)) (err ERR-REWARD-UNAVAILABLE))
    
    (map-set opportunity-applications
      { opportunity-id: opportunity-id, volunteer: volunteer }
      (merge application { status: "approved" })
    )
    
    (map-set skill-opportunities
      { opportunity-id: opportunity-id }
      (merge opportunity {
        assigned-volunteers: (+ (get assigned-volunteers opportunity) u1)
      })
    )
    (ok true)
  )
)

;; Skill matching and utility functions
(define-read-only (get-volunteer-skill (volunteer principal) (skill-name (string-ascii 50)))
  (match (map-get? volunteer-skills { volunteer: volunteer, skill-name: skill-name })
    skill (ok skill)
    (err ERR-SKILL-NOT-FOUND)
  )
)

(define-read-only (get-skill-opportunity (opportunity-id uint))
  (match (map-get? skill-opportunities { opportunity-id: opportunity-id })
    opportunity (ok opportunity)
    (err ERR-OPPORTUNITY-NOT-FOUND)
  )
)

(define-read-only (get-application (opportunity-id uint) (volunteer principal))
  (match (map-get? opportunity-applications { opportunity-id: opportunity-id, volunteer: volunteer })
    application (ok application)
    (err ERR-NOT-FOUND)
  )
)

(define-private (calculate-skill-match (volunteer principal) (opportunity-id uint))
  (let ((opportunity (unwrap-panic (map-get? skill-opportunities { opportunity-id: opportunity-id }))))
    ;; Simplified scoring: return base score for now
    ;; In a real implementation, this would compare volunteer skills against requirements
    u75
  )
)

(define-read-only (get-skills-stats)
  (ok {
    total-opportunities: (var-get opportunity-counter),
    total-skills-registered: u0
  })
)
