;; Campaign Scheduler & Automation Contract
;; Allows advertisers to schedule campaigns for future activation and automate management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-invalid-time (err u302))
(define-constant err-unauthorized (err u303))
(define-constant err-already-scheduled (err u304))
(define-constant err-campaign-active (err u305))
(define-constant err-insufficient-balance (err u306))

;; Campaign status constants
(define-constant STATUS_SCHEDULED u1)
(define-constant STATUS_ACTIVE u2)
(define-constant STATUS_PAUSED u3)
(define-constant STATUS_COMPLETED u4)
(define-constant STATUS_CANCELLED u5)

;; Automation trigger types
(define-constant TRIGGER_TIME u1)
(define-constant TRIGGER_BUDGET u2)
(define-constant TRIGGER_PERFORMANCE u3)

;; Data variables
(define-data-var total-campaigns uint u0)
(define-data-var total-automated-actions uint u0)
(define-data-var scheduler-enabled bool true)

;; Scheduled campaigns storage
(define-map ScheduledCampaigns
  uint ;; campaign-id
  {
    advertiser: principal,
    title: (string-ascii 50),
    content: (string-ascii 200),
    stake-amount: uint,
    start-block: uint,
    end-block: uint,
    status: uint,
    auto-renew: bool,
    renewal-count: uint,
    max-renewals: uint,
    created-at: uint
  }
)

;; Campaign automation rules
(define-map AutomationRules
  uint ;; campaign-id
  {
    pause-on-low-performance: bool,
    performance-threshold: uint,
    auto-bid-adjustment: bool,
    budget-cap: uint,
    daily-spend-limit: uint,
    time-zone-targeting: bool,
    preferred-hours: (list 8 uint),
    weekend-pause: bool
  }
)

;; Campaign execution tracking
(define-map CampaignExecution
  uint ;; campaign-id
  {
    actual-start-block: uint,
    total-spent: uint,
    performance-score: uint,
    automated-actions: uint,
    last-action-block: uint,
    pause-count: uint,
    adjustment-count: uint
  }
)

;; Advertiser campaign balances
(define-map CampaignBalances
  {advertiser: principal, campaign-id: uint}
  {
    allocated-funds: uint,
    spent-funds: uint,
    reserved-funds: uint,
    last-updated: uint
  }
)

;; Queue system for pending actions
(define-map ActionQueue
  uint ;; action-id
  {
    campaign-id: uint,
    action-type: uint,
    trigger-block: uint,
    executed: bool,
    created-at: uint
  }
)

(define-data-var action-queue-counter uint u0)

;; Create a scheduled campaign
(define-public (schedule-campaign 
  (title (string-ascii 50)) 
  (content (string-ascii 200))
  (stake-amount uint) 
  (start-block uint) 
  (duration uint)
  (auto-renew bool)
  (max-renewals uint))
  (let
    (
      (campaign-id (+ (var-get total-campaigns) u1))
      (end-block (+ start-block duration))
    )
    (asserts! (> start-block stacks-block-height) err-invalid-time)
    (asserts! (> duration u144) err-invalid-time) ;; Minimum 1 day
    (asserts! (> stake-amount u0) err-insufficient-balance)
    
    ;; Transfer and reserve funds
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Create scheduled campaign
    (map-set ScheduledCampaigns campaign-id
      {
        advertiser: tx-sender,
        title: title,
        content: content,
        stake-amount: stake-amount,
        start-block: start-block,
        end-block: end-block,
        status: STATUS_SCHEDULED,
        auto-renew: auto-renew,
        renewal-count: u0,
        max-renewals: max-renewals,
        created-at: stacks-block-height
      }
    )
    
    ;; Set up campaign balance tracking
    (map-set CampaignBalances {advertiser: tx-sender, campaign-id: campaign-id}
      {
        allocated-funds: stake-amount,
        spent-funds: u0,
        reserved-funds: stake-amount,
        last-updated: stacks-block-height
      }
    )
    
    ;; Initialize execution tracking
    (map-set CampaignExecution campaign-id
      {
        actual-start-block: u0,
        total-spent: u0,
        performance-score: u0,
        automated-actions: u0,
        last-action-block: u0,
        pause-count: u0,
        adjustment-count: u0
      }
    )
    
    ;; Queue activation action
    (let ((action-id (+ (var-get action-queue-counter) u1)))
      (map-set ActionQueue action-id
        {
          campaign-id: campaign-id,
          action-type: TRIGGER_TIME,
          trigger-block: start-block,
          executed: false,
          created-at: stacks-block-height
        }
      )
      (var-set action-queue-counter action-id)
    )
    
    (var-set total-campaigns campaign-id)
    (ok campaign-id)
  )
)

;; Set automation rules for a campaign
(define-public (set-automation-rules 
  (campaign-id uint)
  (pause-on-low-performance bool)
  (performance-threshold uint)
  (auto-bid-adjustment bool)
  (budget-cap uint)
  (daily-spend-limit uint)
  (preferred-hours (list 8 uint))
  (weekend-pause bool))
  (let
    ((campaign (unwrap! (map-get? ScheduledCampaigns campaign-id) err-not-found)))
    
    (asserts! (is-eq tx-sender (get advertiser campaign)) err-unauthorized)
    
    (map-set AutomationRules campaign-id
      {
        pause-on-low-performance: pause-on-low-performance,
        performance-threshold: performance-threshold,
        auto-bid-adjustment: auto-bid-adjustment,
        budget-cap: budget-cap,
        daily-spend-limit: daily-spend-limit,
        time-zone-targeting: true,
        preferred-hours: preferred-hours,
        weekend-pause: weekend-pause
      }
    )
    (ok true)
  )
)

;; Execute pending automation actions
(define-public (execute-automation-actions)
  (let
    ((current-block stacks-block-height))
    
    (asserts! (var-get scheduler-enabled) err-unauthorized)
    
    ;; Process up to 5 pending actions per call to avoid timeout
    (try! (process-pending-action u1))
    (try! (process-pending-action u2))
    (try! (process-pending-action u3))
    (try! (process-pending-action u4))
    (try! (process-pending-action u5))
    
    (ok true)
  )
)

;; Helper function to process individual pending actions
(define-private (process-pending-action (action-id uint))
  (match (map-get? ActionQueue action-id)
    action-data (if (and 
                     (not (get executed action-data))
                     (<= (get trigger-block action-data) stacks-block-height))
      (begin
        (try! (execute-campaign-action (get campaign-id action-data) (get action-type action-data)))
        (map-set ActionQueue action-id (merge action-data {executed: true}))
        (ok true)
      )
      (ok false)
    )
    (ok false)
  )
)

;; Execute specific campaign action based on type
(define-private (execute-campaign-action (campaign-id uint) (action-type uint))
  (let
    ((campaign (unwrap! (map-get? ScheduledCampaigns campaign-id) err-not-found)))
    
    (if (is-eq action-type TRIGGER_TIME)
      ;; Activate scheduled campaign
      (begin
        (map-set ScheduledCampaigns campaign-id (merge campaign {status: STATUS_ACTIVE}))
        (map-set CampaignExecution campaign-id
          (merge (default-to 
            {actual-start-block: u0, total-spent: u0, performance-score: u0,
             automated-actions: u0, last-action-block: u0, pause-count: u0, adjustment-count: u0}
            (map-get? CampaignExecution campaign-id))
            {actual-start-block: stacks-block-height, last-action-block: stacks-block-height}))
        (ok true)
      )
      (ok false)
    )
  )
)

;; Manually pause/resume campaign
(define-public (toggle-campaign-status (campaign-id uint))
  (let
    ((campaign (unwrap! (map-get? ScheduledCampaigns campaign-id) err-not-found)))
    
    (asserts! (is-eq tx-sender (get advertiser campaign)) err-unauthorized)
    
    (let
      ((new-status (if (is-eq (get status campaign) STATUS_ACTIVE) STATUS_PAUSED STATUS_ACTIVE)))
      
      (map-set ScheduledCampaigns campaign-id (merge campaign {status: new-status}))
      
      ;; Update execution tracking
      (let
        ((execution (default-to 
          {actual-start-block: u0, total-spent: u0, performance-score: u0,
           automated-actions: u0, last-action-block: u0, pause-count: u0, adjustment-count: u0}
          (map-get? CampaignExecution campaign-id))))
        
        (map-set CampaignExecution campaign-id
          (merge execution {
            pause-count: (if (is-eq new-status STATUS_PAUSED) 
                           (+ (get pause-count execution) u1)
                           (get pause-count execution)),
            last-action-block: stacks-block-height
          })
        )
      )
      (ok new-status)
    )
  )
)

;; Extend campaign duration
(define-public (extend-campaign (campaign-id uint) (additional-blocks uint) (additional-funds uint))
  (let
    ((campaign (unwrap! (map-get? ScheduledCampaigns campaign-id) err-not-found)))
    
    (asserts! (is-eq tx-sender (get advertiser campaign)) err-unauthorized)
    (asserts! (> additional-blocks u0) err-invalid-time)
    
    (if (> additional-funds u0)
      (try! (stx-transfer? additional-funds tx-sender (as-contract tx-sender)))
      true
    )
    
    (map-set ScheduledCampaigns campaign-id
      (merge campaign {
        end-block: (+ (get end-block campaign) additional-blocks),
        stake-amount: (+ (get stake-amount campaign) additional-funds)
      })
    )
    
    ;; Update balance tracking
    (let
      ((balance (unwrap! (map-get? CampaignBalances {advertiser: tx-sender, campaign-id: campaign-id}) err-not-found)))
      
      (map-set CampaignBalances {advertiser: tx-sender, campaign-id: campaign-id}
        (merge balance {
          allocated-funds: (+ (get allocated-funds balance) additional-funds),
          reserved-funds: (+ (get reserved-funds balance) additional-funds),
          last-updated: stacks-block-height
        })
      )
    )
    
    (ok true)
  )
)

;; Cancel scheduled campaign and refund
(define-public (cancel-campaign (campaign-id uint))
  (let
    ((campaign (unwrap! (map-get? ScheduledCampaigns campaign-id) err-not-found)))
    
    (asserts! (is-eq tx-sender (get advertiser campaign)) err-unauthorized)
    (asserts! (not (is-eq (get status campaign) STATUS_COMPLETED)) err-campaign-active)
    
    ;; Refund unused funds
    (let
      ((balance (unwrap! (map-get? CampaignBalances {advertiser: tx-sender, campaign-id: campaign-id}) err-not-found))
       (refund-amount (- (get allocated-funds balance) (get spent-funds balance))))
      
      (if (> refund-amount u0)
        (try! (as-contract (stx-transfer? refund-amount tx-sender (get advertiser campaign))))
        true
      )
      
      (map-set ScheduledCampaigns campaign-id (merge campaign {status: STATUS_CANCELLED}))
      (ok refund-amount)
    )
  )
)

;; Read-only functions

(define-read-only (get-scheduled-campaign (campaign-id uint))
  (map-get? ScheduledCampaigns campaign-id)
)

(define-read-only (get-campaign-automation-rules (campaign-id uint))
  (map-get? AutomationRules campaign-id)
)

(define-read-only (get-campaign-execution-stats (campaign-id uint))
  (map-get? CampaignExecution campaign-id)
)

(define-read-only (get-campaign-balance (advertiser principal) (campaign-id uint))
  (map-get? CampaignBalances {advertiser: advertiser, campaign-id: campaign-id})
)

(define-read-only (get-scheduler-status)
  (ok {
    total-campaigns: (var-get total-campaigns),
    total-automated-actions: (var-get total-automated-actions),
    scheduler-enabled: (var-get scheduler-enabled),
    current-block: stacks-block-height
  })
)

;; Admin functions
(define-public (toggle-scheduler (enabled bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set scheduler-enabled enabled)
    (ok enabled)
  )
)
