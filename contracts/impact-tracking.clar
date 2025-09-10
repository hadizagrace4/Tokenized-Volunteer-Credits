;; title: impact-tracking
;; version: 1.0
;; summary: Volunteer Impact Metrics & Recognition System

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-ACTIVITY-NOT-FOUND (err u1001))
(define-constant ERR-INVALID-METRICS (err u1002))
(define-constant ERR-IMPACT-ALREADY-LOGGED (err u1003))
(define-constant ERR-VOLUNTEER-NOT-FOUND (err u1004))
(define-constant ERR-ORGANIZATION-NOT-FOUND (err u1005))
(define-constant ERR-CONTRACT-PAUSED (err u1006))

;; Impact category constants
(define-constant IMPACT-PEOPLE-HELPED u1)
(define-constant IMPACT-PROJECTS-COMPLETED u2)
(define-constant IMPACT-RESOURCES-DISTRIBUTED u3)
(define-constant IMPACT-COMMUNITY-EVENTS u4)
(define-constant IMPACT-ENVIRONMENTAL-ACTIONS u5)

;; Recognition tier constants
(define-constant TIER-COMMUNITY-HELPER u1)
(define-constant TIER-IMPACT-MAKER u2)
(define-constant TIER-CHANGE-AGENT u3)
(define-constant TIER-COMMUNITY-CHAMPION u4)

;; Data variables
(define-data-var impact-record-counter uint u0)
(define-data-var total-impact-score uint u0)
(define-data-var paused bool false)

;; Core impact tracking maps
(define-map activity-impact-metrics
    { activity-id: uint }
    {
        people-helped: uint,
        projects-completed: uint,
        resources-distributed: uint,
        community-events-organized: uint,
        environmental-actions: uint,
        impact-score: uint,
        verified: bool,
        logged-by: principal,
        logged-at: uint
    }
)

(define-map volunteer-impact-summary
    { volunteer: principal }
    {
        total-impact-score: uint,
        people-helped-total: uint,
        projects-completed-total: uint,
        resources-distributed-total: uint,
        community-events-total: uint,
        environmental-actions-total: uint,
        recognition-tier: uint,
        impact-multiplier: uint,
        last-updated: uint
    }
)

(define-map organization-impact-tracking
    { organization: principal }
    {
        total-volunteers-impacted: uint,
        cumulative-impact-score: uint,
        avg-volunteer-impact: uint,
        top-impact-categories: (list 3 uint),
        recognition-level: uint,
        last-assessment: uint
    }
)

(define-map impact-achievements
    { volunteer: principal, achievement-id: uint }
    {
        achievement-type: uint,
        threshold-reached: uint,
        impact-category: uint,
        earned-at: uint,
        bonus-credits: uint
    }
)

(define-map monthly-impact-leaders
    { month: uint, category: uint }
    {
        top-volunteer: principal,
        impact-value: uint,
        recognition-bonus: uint
    }
)

;; Public functions

(define-public (log-activity-impact 
    (activity-id uint)
    (people-helped uint)
    (projects-completed uint)
    (resources-distributed uint)
    (community-events uint)
    (environmental-actions uint))
    (let 
        (
            (impact-score (calculate-impact-score people-helped projects-completed resources-distributed community-events environmental-actions))
        )
        (asserts! (not (var-get paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-none (map-get? activity-impact-metrics { activity-id: activity-id })) ERR-IMPACT-ALREADY-LOGGED)
        
        (map-set activity-impact-metrics
            { activity-id: activity-id }
            {
                people-helped: people-helped,
                projects-completed: projects-completed,
                resources-distributed: resources-distributed,
                community-events-organized: community-events,
                environmental-actions: environmental-actions,
                impact-score: impact-score,
                verified: false,
                logged-by: tx-sender,
                logged-at: stacks-block-height
            }
        )
        (var-set impact-record-counter (+ (var-get impact-record-counter) u1))
        (ok impact-score)
    )
)

(define-public (verify-impact-metrics (activity-id uint))
    (let 
        (
            (impact-data (unwrap! (map-get? activity-impact-metrics { activity-id: activity-id }) ERR-ACTIVITY-NOT-FOUND))
        )
        (asserts! (not (var-get paused)) ERR-CONTRACT-PAUSED)
        (asserts! (not (get verified impact-data)) ERR-IMPACT-ALREADY-LOGGED)
        
        (map-set activity-impact-metrics
            { activity-id: activity-id }
            (merge impact-data { verified: true })
        )
        
        (unwrap-panic (update-volunteer-impact-summary (get logged-by impact-data) impact-data))
        (unwrap-panic (update-organization-impact-tracking tx-sender impact-data))
        (unwrap-panic (check-and-award-impact-achievements (get logged-by impact-data) impact-data))
        
        (var-set total-impact-score (+ (var-get total-impact-score) (get impact-score impact-data)))
        (ok true)
    )
)

(define-public (nominate-monthly-leader (volunteer principal) (category uint))
    (let 
        (
            (current-month (/ stacks-block-height u4320))  ;; ~30 days in blocks
            (volunteer-impact (unwrap! (map-get? volunteer-impact-summary { volunteer: volunteer }) ERR-VOLUNTEER-NOT-FOUND))
            (category-value (get-category-value volunteer-impact category))
        )
        (asserts! (and (>= category u1) (<= category u5)) ERR-INVALID-METRICS)
        
        (let ((current-leader (map-get? monthly-impact-leaders { month: current-month, category: category })))
            (if (or (is-none current-leader) (> category-value (get impact-value (unwrap-panic current-leader))))
                (begin
                    (map-set monthly-impact-leaders
                        { month: current-month, category: category }
                        {
                            top-volunteer: volunteer,
                            impact-value: category-value,
                            recognition-bonus: (calculate-recognition-bonus category-value)
                        }
                    )
                    (ok true)
                )
                (ok false)
            )
        )
    )
)

;; Private helper functions

(define-private (calculate-impact-score 
    (people uint) 
    (projects uint) 
    (resources uint) 
    (events uint) 
    (environmental uint))
    (+ 
        (* people u10)
        (* projects u25)
        (* resources u5)
        (* events u50)
        (* environmental u15)
    )
)

(define-private (update-volunteer-impact-summary (volunteer principal) (impact-data {people-helped: uint, projects-completed: uint, resources-distributed: uint, community-events-organized: uint, environmental-actions: uint, impact-score: uint, verified: bool, logged-by: principal, logged-at: uint}))
    (let 
        (
            (current-summary (default-to
                {
                    total-impact-score: u0,
                    people-helped-total: u0,
                    projects-completed-total: u0,
                    resources-distributed-total: u0,
                    community-events-total: u0,
                    environmental-actions-total: u0,
                    recognition-tier: TIER-COMMUNITY-HELPER,
                    impact-multiplier: u100,
                    last-updated: u0
                }
                (map-get? volunteer-impact-summary { volunteer: volunteer })
            ))
            (new-total-score (+ (get total-impact-score current-summary) (get impact-score impact-data)))
            (new-tier (calculate-recognition-tier new-total-score))
        )
        (map-set volunteer-impact-summary
            { volunteer: volunteer }
            {
                total-impact-score: new-total-score,
                people-helped-total: (+ (get people-helped-total current-summary) (get people-helped impact-data)),
                projects-completed-total: (+ (get projects-completed-total current-summary) (get projects-completed impact-data)),
                resources-distributed-total: (+ (get resources-distributed-total current-summary) (get resources-distributed impact-data)),
                community-events-total: (+ (get community-events-total current-summary) (get community-events-organized impact-data)),
                environmental-actions-total: (+ (get environmental-actions-total current-summary) (get environmental-actions impact-data)),
                recognition-tier: new-tier,
                impact-multiplier: (calculate-impact-multiplier new-tier),
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-private (update-organization-impact-tracking (organization principal) (impact-data {people-helped: uint, projects-completed: uint, resources-distributed: uint, community-events-organized: uint, environmental-actions: uint, impact-score: uint, verified: bool, logged-by: principal, logged-at: uint}))
    (let 
        (
            (current-tracking (default-to
                {
                    total-volunteers-impacted: u0,
                    cumulative-impact-score: u0,
                    avg-volunteer-impact: u0,
                    top-impact-categories: (list u1 u2 u3),
                    recognition-level: u1,
                    last-assessment: u0
                }
                (map-get? organization-impact-tracking { organization: organization })
            ))
        )
        (map-set organization-impact-tracking
            { organization: organization }
            {
                total-volunteers-impacted: (+ (get total-volunteers-impacted current-tracking) u1),
                cumulative-impact-score: (+ (get cumulative-impact-score current-tracking) (get impact-score impact-data)),
                avg-volunteer-impact: (/ (+ (get cumulative-impact-score current-tracking) (get impact-score impact-data)) 
                                        (+ (get total-volunteers-impacted current-tracking) u1)),
                top-impact-categories: (get top-impact-categories current-tracking),
                recognition-level: (calculate-org-recognition-level (+ (get cumulative-impact-score current-tracking) (get impact-score impact-data))),
                last-assessment: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-private (check-and-award-impact-achievements (volunteer principal) (impact-data {people-helped: uint, projects-completed: uint, resources-distributed: uint, community-events-organized: uint, environmental-actions: uint, impact-score: uint, verified: bool, logged-by: principal, logged-at: uint}))
    (let 
        (
            (achievement-id (var-get impact-record-counter))
        )
        (if (>= (get people-helped impact-data) u100)
            (map-set impact-achievements
                { volunteer: volunteer, achievement-id: achievement-id }
                {
                    achievement-type: u1,  ;; Community Impact
                    threshold-reached: (get people-helped impact-data),
                    impact-category: IMPACT-PEOPLE-HELPED,
                    earned-at: stacks-block-height,
                    bonus-credits: u50
                }
            )
            false
        )
        (if (>= (get projects-completed impact-data) u10)
            (map-set impact-achievements
                { volunteer: volunteer, achievement-id: (+ achievement-id u1) }
                {
                    achievement-type: u2,  ;; Project Leadership
                    threshold-reached: (get projects-completed impact-data),
                    impact-category: IMPACT-PROJECTS-COMPLETED,
                    earned-at: stacks-block-height,
                    bonus-credits: u25
                }
            )
            false
        )
        (ok true)
    )
)

(define-private (calculate-recognition-tier (impact-score-value uint))
    (if (>= impact-score-value u5000)
        TIER-COMMUNITY-CHAMPION
        (if (>= impact-score-value u2000)
            TIER-CHANGE-AGENT
            (if (>= impact-score-value u500)
                TIER-IMPACT-MAKER
                TIER-COMMUNITY-HELPER
            )
        )
    )
)

(define-private (calculate-impact-multiplier (tier uint))
    (if (is-eq tier TIER-COMMUNITY-CHAMPION)
        u200
        (if (is-eq tier TIER-CHANGE-AGENT)
            u150
            (if (is-eq tier TIER-IMPACT-MAKER)
                u125
                u100
            )
        )
    )
)

(define-private (calculate-org-recognition-level (cumulative-score uint))
    (if (>= cumulative-score u10000)
        u4  ;; Elite Organization
        (if (>= cumulative-score u5000)
            u3  ;; Distinguished Organization
            (if (>= cumulative-score u1000)
                u2  ;; Recognized Organization
                u1  ;; Standard Organization
            )
        )
    )
)

(define-private (get-category-value (summary {total-impact-score: uint, people-helped-total: uint, projects-completed-total: uint, resources-distributed-total: uint, community-events-total: uint, environmental-actions-total: uint, recognition-tier: uint, impact-multiplier: uint, last-updated: uint}) (category uint))
    (if (is-eq category IMPACT-PEOPLE-HELPED)
        (get people-helped-total summary)
        (if (is-eq category IMPACT-PROJECTS-COMPLETED)
            (get projects-completed-total summary)
            (if (is-eq category IMPACT-RESOURCES-DISTRIBUTED)
                (get resources-distributed-total summary)
                (if (is-eq category IMPACT-COMMUNITY-EVENTS)
                    (get community-events-total summary)
                    (get environmental-actions-total summary)
                )
            )
        )
    )
)

(define-private (calculate-recognition-bonus (impact-value uint))
    (/ (* impact-value u10) u100)  ;; 10% of impact value as bonus
)

;; Read-only functions

(define-read-only (get-activity-impact (activity-id uint))
    (map-get? activity-impact-metrics { activity-id: activity-id })
)

(define-read-only (get-volunteer-impact-summary (volunteer principal))
    (map-get? volunteer-impact-summary { volunteer: volunteer })
)

(define-read-only (get-organization-impact (organization principal))
    (map-get? organization-impact-tracking { organization: organization })
)

(define-read-only (get-impact-achievement (volunteer principal) (achievement-id uint))
    (map-get? impact-achievements { volunteer: volunteer, achievement-id: achievement-id })
)

(define-read-only (get-monthly-leader (month uint) (category uint))
    (map-get? monthly-impact-leaders { month: month, category: category })
)

(define-read-only (get-system-impact-stats)
    {
        total-impact-records: (var-get impact-record-counter),
        total-impact-score: (var-get total-impact-score),
        current-month: (/ stacks-block-height u4320)
    }
)

(define-read-only (calculate-volunteer-impact-rank (volunteer principal))
    (let 
        (
            (volunteer-summary (map-get? volunteer-impact-summary { volunteer: volunteer }))
        )
        (if (is-some volunteer-summary)
            (let ((summary-data (unwrap-panic volunteer-summary)))
                {
                    impact-score: (get total-impact-score summary-data),
                    recognition-tier: (get recognition-tier summary-data),
                    tier-name: (get-tier-name (get recognition-tier summary-data)),
                    impact-multiplier: (get impact-multiplier summary-data),
                    next-tier-threshold: (get-next-threshold (get recognition-tier summary-data))
                }
            )
            {
                impact-score: u0,
                recognition-tier: TIER-COMMUNITY-HELPER,
                tier-name: "Community Helper",
                impact-multiplier: u100,
                next-tier-threshold: u500
            }
        )
    )
)

(define-read-only (get-impact-leaderboard (category uint))
    (let ((current-month (/ stacks-block-height u4320)))
        {
            current-leader: (map-get? monthly-impact-leaders { month: current-month, category: category }),
            category-name: (get-category-name category),
            month: current-month
        }
    )
)

;; Helper functions for display

(define-private (get-tier-name (tier uint))
    (if (is-eq tier TIER-COMMUNITY-CHAMPION)
        "Community Champion"
        (if (is-eq tier TIER-CHANGE-AGENT)
            "Change Agent"
            (if (is-eq tier TIER-IMPACT-MAKER)
                "Impact Maker"
                "Community Helper"
            )
        )
    )
)

(define-private (get-category-name (category uint))
    (if (is-eq category IMPACT-PEOPLE-HELPED)
        "People Helped"
        (if (is-eq category IMPACT-PROJECTS-COMPLETED)
            "Projects Completed"
            (if (is-eq category IMPACT-RESOURCES-DISTRIBUTED)
                "Resources Distributed"
                (if (is-eq category IMPACT-COMMUNITY-EVENTS)
                    "Community Events"
                    "Environmental Actions"
                )
            )
        )
    )
)

(define-private (get-next-threshold (tier uint))
    (if (is-eq tier TIER-COMMUNITY-HELPER)
        u500
        (if (is-eq tier TIER-IMPACT-MAKER)
            u2000
            (if (is-eq tier TIER-CHANGE-AGENT)
                u5000
                u5000  ;; Already at max tier
            )
        )
    )
)