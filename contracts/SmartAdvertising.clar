;; SmartAdvertising Contract
;; A decentralized marketplace for advertising with staking and engagement tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-already-exists (err u103))
(define-constant minimum-stake-amount u1000)

;; Data Variables
(define-data-var total-ads uint u0)
(define-data-var platform-fee uint u50) ;; 5% in basis points
(define-data-var total-categories uint u0)

;; Data Maps
(define-map Advertisements
    uint ;; ad-id
    {
        advertiser: principal,
        title: (string-ascii 50),
        content: (string-ascii 200),
        stake-amount: uint,
        total-views: uint,
        total-clicks: uint,
        active: bool,
        created-at: uint
    }
)

(define-map AdvertiserStats
    principal ;; advertiser address
    {
        total-ads: uint,
        total-stake: uint,
        reputation-score: uint
    }
)

(define-map ViewerEngagement
    {ad-id: uint, viewer: principal}
    {
        viewed: bool,
        clicked: bool,
        timestamp: uint
    }
)

;; Public Functions

;; Create a new advertisement
(define-public (create-advertisement (title (string-ascii 50)) (content (string-ascii 200)) (stake-amount uint))
    (let
        (
            (new-ad-id (+ (var-get total-ads) u1))
            (advertiser-stats (default-to 
                {total-ads: u0, total-stake: u0, reputation-score: u100}
                (map-get? AdvertiserStats tx-sender)))
        )
        (asserts! (>= stake-amount minimum-stake-amount) err-invalid-amount)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        (map-set Advertisements new-ad-id
            {
                advertiser: tx-sender,
                title: title,
                content: content,
                stake-amount: stake-amount,
                total-views: u0,
                total-clicks: u0,
                active: true,
                created-at: stacks-block-height
            }
        )

        (map-set AdvertiserStats tx-sender
            {
                total-ads: (+ (get total-ads advertiser-stats) u1),
                total-stake: (+ (get total-stake advertiser-stats) stake-amount),
                reputation-score: (get reputation-score advertiser-stats)
            }
        )

        (var-set total-ads new-ad-id)
        (ok new-ad-id)
    )
)

;; Record viewer engagement
(define-public (record-engagement (ad-id uint) (clicked bool))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found))
         (engagement-key {ad-id: ad-id, viewer: tx-sender}))
        
        (asserts! (get active ad) err-not-found)
        
        (map-set ViewerEngagement engagement-key
            {
                viewed: true,
                clicked: clicked,
                timestamp: stacks-block-height
            }
        )

        (map-set Advertisements ad-id
            (merge ad {
                total-views: (+ (get total-views ad) u1),
                total-clicks: (if clicked (+ (get total-clicks ad) u1) (get total-clicks ad))
            })
        )
        
        (ok true)
    )
)

;; Withdraw staked tokens
(define-public (withdraw-stake (ad-id uint))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found)))
        
        (asserts! (is-eq tx-sender (get advertiser ad)) err-owner-only)
        (asserts! (get active ad) err-not-found)
        
        (try! (as-contract (stx-transfer? (get stake-amount ad) tx-sender (get advertiser ad))))
        
        (map-set Advertisements ad-id
            (merge ad {active: false})
        )
        
        (ok true)
    )
)

;; Read-only Functions

;; Get advertisement details
(define-read-only (get-advertisement (ad-id uint))
    (ok (unwrap! (map-get? Advertisements ad-id) err-not-found))
)

;; Get advertiser statistics
(define-read-only (get-advertiser-stats (advertiser principal))
    (ok (unwrap! (map-get? AdvertiserStats advertiser) err-not-found))
)

;; Get total number of advertisements
(define-read-only (get-total-ads)
    (ok (var-get total-ads))
)

;; Get engagement metrics for an ad
(define-read-only (get-engagement-metrics (ad-id uint))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found)))
        (ok {
            total-views: (get total-views ad),
            total-clicks: (get total-clicks ad),
            engagement-rate: (if (is-eq (get total-views ad) u0)
                u0
                (/ (* (get total-clicks ad) u100) (get total-views ad)))
        })
    )
)


(define-map Categories uint (string-ascii 20))

(define-map AdCategories 
    uint 
    {category-id: uint}
)

(define-public (create-category (category-name (string-ascii 20)))
    (let
        ((category-id (+ (var-get total-categories) u1)))
        (map-set Categories category-id category-name)
        (var-set total-categories category-id)
        (ok category-id)
    )
)

(define-public (set-ad-category (ad-id uint) (category-id uint))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found)))
        (asserts! (is-eq tx-sender (get advertiser ad)) err-owner-only)
        (map-set AdCategories ad-id {category-id: category-id})
        (ok true)
    )
)

(define-map AdExpirations uint uint)

(define-public (set-ad-expiration (ad-id uint) (blocks uint))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found))
         (expiration (+ blocks stacks-block-height)))
        (asserts! (is-eq tx-sender (get advertiser ad)) err-owner-only)
        (map-set AdExpirations ad-id expiration)
        (ok true)
    )
)

(define-read-only (is-ad-expired (ad-id uint))
    (let
        ((expiration (unwrap! (map-get? AdExpirations ad-id) err-not-found)))
        (ok (> stacks-block-height expiration))
    )
)

(define-map AdBudgets
    uint
    {
        daily-budget: uint,
        spent-today: uint,
        last-reset: uint
    }
)

(define-public (set-daily-budget (ad-id uint) (budget uint))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found)))
        (asserts! (is-eq tx-sender (get advertiser ad)) err-owner-only)
        (map-set AdBudgets ad-id 
            {
                daily-budget: budget,
                spent-today: u0,
                last-reset: stacks-block-height
            }
        )
        (ok true)
    )
)


(define-map AudienceTargeting
    uint
    {
        age-min: uint,
        age-max: uint,
        location: (string-ascii 50),
        interests: (list 5 (string-ascii 20))
    }
)

(define-public (set-targeting (ad-id uint) (age-min uint) (age-max uint) (location (string-ascii 50)) (interests (list 5 (string-ascii 20))))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found)))
        (asserts! (is-eq tx-sender (get advertiser ad)) err-owner-only)
        (map-set AudienceTargeting ad-id
            {
                age-min: age-min,
                age-max: age-max,
                location: location,
                interests: interests
            }
        )
        (ok true)
    )
)


(define-constant reward-amount u10)

(define-map UserRewards
    principal
    {
        total-rewards: uint,
        last-claim: uint
    }
)

(define-public (claim-engagement-reward (ad-id uint))
    (let
        ((user-rewards (default-to {total-rewards: u0, last-claim: u0} (map-get? UserRewards tx-sender)))
         (engagement (unwrap! (map-get? ViewerEngagement {ad-id: ad-id, viewer: tx-sender}) err-not-found)))
        (asserts! (get clicked engagement) err-invalid-amount)
        (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
        (map-set UserRewards tx-sender
            {
                total-rewards: (+ (get total-rewards user-rewards) reward-amount),
                last-claim: stacks-block-height
            }
        )
        (ok true)
    )
)


(define-map AdAnalytics
    uint
    {
        unique-viewers: uint,
        conversion-rate: uint,
        peak-hours: (list 24 uint),
        viewer-retention: uint
    }
)

(define-public (update-analytics (ad-id uint))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found))
         (current-analytics (default-to {unique-viewers: u0, conversion-rate: u0, peak-hours: (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0), viewer-retention: u0} (map-get? AdAnalytics ad-id))))
        (map-set AdAnalytics ad-id
            (merge current-analytics {
                unique-viewers: (+ (get unique-viewers current-analytics) u1)
            })
        )
        (ok true)
    )
)


(define-map Referrals
    {ad-id: uint, referrer: principal}
    {
        referral-count: uint,
        earned-rewards: uint
    }
)

(define-public (track-referral (ad-id uint) (referrer principal))
    (let
        ((current-refs (default-to {referral-count: u0, earned-rewards: u0} 
                      (map-get? Referrals {ad-id: ad-id, referrer: referrer}))))
        (map-set Referrals {ad-id: ad-id, referrer: referrer}
            {
                referral-count: (+ (get referral-count current-refs) u1),
                earned-rewards: (+ (get earned-rewards current-refs) u5)
            }
        )
        (ok true)
    )
)


(define-map AdPerformanceScores
    uint
    {
        performance-score: uint,
        last-updated: uint
    }
)

(define-public (calculate-ad-performance (ad-id uint))
    (let
        ((ad (unwrap! (map-get? Advertisements ad-id) err-not-found))
         (views (get total-views ad))
         (clicks (get total-clicks ad))
         (engagement-rate (if (is-eq views u0) 
            u0 
            (/ (* clicks u100) views)))
         (score (* engagement-rate u10)))
        
        (map-set AdPerformanceScores ad-id
            {
                performance-score: score,
                last-updated: stacks-block-height
            }
        )
        (ok score)
    )
)

(define-read-only (get-ad-performance-score (ad-id uint))
    (ok (unwrap! (map-get? AdPerformanceScores ad-id) err-not-found))
)


(define-map PremiumSlots
    uint
    {
        current-holder: principal,
        bid-amount: uint,
        expires-at: uint
    }
)

(define-constant premium-slot-duration u144) ;; 1 day in blocks
(define-constant minimum-premium-bid u1000)

(define-public (bid-premium-slot (slot-id uint) (bid-amount uint))
    (let
        ((current-slot (default-to 
            {current-holder: tx-sender, bid-amount: u0, expires-at: u0}
            (map-get? PremiumSlots slot-id))))
        
        (asserts! (> bid-amount (get bid-amount current-slot)) err-invalid-amount)
        (asserts! (>= bid-amount minimum-premium-bid) err-invalid-amount)
        
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        (if (> (get bid-amount current-slot) u0)
            (try! (as-contract (stx-transfer? (get bid-amount current-slot) tx-sender (get current-holder current-slot))))
            true
        )
        
        (map-set PremiumSlots slot-id
            {
                current-holder: tx-sender,
                bid-amount: bid-amount,
                expires-at: (+ stacks-block-height premium-slot-duration)
            }
        )
        (ok true)
    )
)

(define-read-only (get-premium-slot-info (slot-id uint))
    (ok (unwrap! (map-get? PremiumSlots slot-id) err-not-found))
)


(define-public (withdraw-premium-slot (slot-id uint))
    (let
        ((current-slot (unwrap! (map-get? PremiumSlots slot-id) err-not-found)))
        
        (asserts! (is-eq tx-sender (get current-holder current-slot)) err-owner-only)
        (asserts! (> stacks-block-height (get expires-at current-slot)) err-not-found)
        
        (try! (as-contract (stx-transfer? (get bid-amount current-slot) tx-sender tx-sender)))
        
        (map-set PremiumSlots slot-id
            {
                current-holder: tx-sender,
                bid-amount: u0,
                expires-at: u0
            }
        )
        (ok true)
    )
)
(define-public (get-premium-slot-bid (slot-id uint))
    (ok (unwrap! (map-get? PremiumSlots slot-id) err-not-found))
)

;; === DYNAMIC PRICING MARKET SYSTEM ===

;; Constants for dynamic pricing
(define-constant base-price-multiplier u100) ;; 100% baseline
(define-constant peak-hour-multiplier u150) ;; 150% during peak hours  
(define-constant off-peak-multiplier u75) ;; 75% during off-peak hours
(define-constant high-demand-threshold u10) ;; When 10+ ads are active
(define-constant low-demand-threshold u3) ;; When 3 or fewer ads are active
(define-constant demand-surge-multiplier u200) ;; 200% during high demand
(define-constant low-demand-discount u50) ;; 50% during low demand
(define-constant max-price-history u48) ;; Store 48 periods of pricing history

;; Time periods (in blocks, roughly 1 hour = 6 blocks)
(define-constant peak-start-hour u32) ;; Block 32 of daily cycle
(define-constant peak-end-hour u42) ;; Block 42 of daily cycle
(define-constant daily-block-cycle u144) ;; 24 hours in blocks

;; Data structures for dynamic pricing
(define-map MarketConditions
    uint ;; timestamp-period
    {
        active-ads-count: uint,
        current-multiplier: uint,
        base-price: uint,
        demand-level: (string-ascii 10) ;; "low", "normal", "high"
    }
)

(define-map PriceHistory
    uint ;; period-index
    {
        timestamp: uint,
        price-multiplier: uint,
        total-volume: uint,
        period-revenue: uint
    }
)

(define-map AutoBidders
    principal ;; advertiser
    {
        max-bid-limit: uint,
        target-position: uint, ;; Preferred ranking position
        auto-bidding-active: bool,
        current-bid: uint,
        last-bid-time: uint
    }
)

(define-map MarketMetrics
    uint ;; metric-id (always 1 for singleton)
    {
        total-market-volume: uint,
        average-price-multiplier: uint,
        peak-demand-reached: uint,
        current-period: uint,
        next-price-update: uint
    }
)

;; Data variables for market state
(define-data-var current-market-period uint u0)
(define-data-var total-autobidders uint u0)
(define-data-var market-revenue-pool uint u0)

;; Calculate current time-based pricing multiplier
(define-read-only (get-time-based-multiplier)
    (let
        ((current-block stacks-block-height)
         (daily-position (mod current-block daily-block-cycle)))
        
        (if (and (>= daily-position peak-start-hour) (<= daily-position peak-end-hour))
            peak-hour-multiplier
            off-peak-multiplier
        )
    )
)

;; Calculate demand-based pricing multiplier
(define-read-only (get-demand-multiplier)
    (let
        ((active-count (var-get total-ads)))
        
        (if (>= active-count high-demand-threshold)
            demand-surge-multiplier
            (if (<= active-count low-demand-threshold)
                low-demand-discount
                base-price-multiplier
            )
        )
    )
)

;; Get current dynamic market price
(define-read-only (get-current-market-price)
    (let
        ((time-multiplier (get-time-based-multiplier))
         (demand-multiplier (get-demand-multiplier))
         (combined-multiplier (/ (* time-multiplier demand-multiplier) u100))
         (dynamic-price (/ (* minimum-stake-amount combined-multiplier) u100)))
        
        (ok dynamic-price)
    )
)

;; Update market conditions for current period
(define-public (update-market-conditions)
    (let
        ((current-period (var-get current-market-period))
         (active-ads (var-get total-ads))
         (current-price (unwrap! (get-current-market-price) err-invalid-amount))
         (time-mult (get-time-based-multiplier))
         (demand-mult (get-demand-multiplier))
         (combined-mult (/ (* time-mult demand-mult) u100))
         (demand-level (if (>= active-ads high-demand-threshold)
            "high"
            (if (<= active-ads low-demand-threshold)
                "low"
                "normal"))))
        
        ;; Update current market conditions
        (map-set MarketConditions current-period
            {
                active-ads-count: active-ads,
                current-multiplier: combined-mult,
                base-price: current-price,
                demand-level: demand-level
            }
        )
        
        ;; Store price history
        (map-set PriceHistory current-period
            {
                timestamp: stacks-block-height,
                price-multiplier: combined-mult,
                total-volume: active-ads,
                period-revenue: (var-get market-revenue-pool)
            }
        )
        
        ;; Update metrics
        (map-set MarketMetrics u1
            {
                total-market-volume: (+ (default-to u0 (get total-market-volume (map-get? MarketMetrics u1))) active-ads),
                average-price-multiplier: combined-mult,
                peak-demand-reached: (if (is-eq demand-level "high") 
                    (+ (default-to u0 (get peak-demand-reached (map-get? MarketMetrics u1))) u1)
                    (default-to u0 (get peak-demand-reached (map-get? MarketMetrics u1)))),
                current-period: current-period,
                next-price-update: (+ stacks-block-height u6) ;; Update every 6 blocks
            }
        )
        
        (var-set current-market-period (+ current-period u1))
        (ok true)
    )
)

;; Enable automatic bidding for advertisers
(define-public (enable-auto-bidding (max-bid-limit uint) (target-position uint))
    (begin
        (asserts! (> max-bid-limit minimum-stake-amount) err-invalid-amount)
        (asserts! (> target-position u0) err-invalid-amount)
        
        (map-set AutoBidders tx-sender
            {
                max-bid-limit: max-bid-limit,
                target-position: target-position,
                auto-bidding-active: true,
                current-bid: u0,
                last-bid-time: stacks-block-height
            }
        )
        
        (var-set total-autobidders (+ (var-get total-autobidders) u1))
        (ok true)
    )
)

;; Create advertisement with dynamic pricing
(define-public (create-ad-dynamic-pricing (title (string-ascii 50)) (content (string-ascii 200)))
    (let
        ((dynamic-price (unwrap! (get-current-market-price) err-invalid-amount))
         (auto-bidder (map-get? AutoBidders tx-sender))
         (final-bid (match auto-bidder
            bidder-info (if (and 
                            (get auto-bidding-active bidder-info)
                            (<= dynamic-price (get max-bid-limit bidder-info)))
                        dynamic-price
                        (get max-bid-limit bidder-info))
            dynamic-price))
         (new-ad-id (+ (var-get total-ads) u1)))
        
        (asserts! (>= final-bid dynamic-price) err-invalid-amount)
        (try! (stx-transfer? final-bid tx-sender (as-contract tx-sender)))
        
        ;; Update market revenue pool
        (var-set market-revenue-pool (+ (var-get market-revenue-pool) final-bid))
        
        ;; Create the advertisement
        (map-set Advertisements new-ad-id
            {
                advertiser: tx-sender,
                title: title,
                content: content,
                stake-amount: final-bid,
                total-views: u0,
                total-clicks: u0,
                active: true,
                created-at: stacks-block-height
            }
        )
        
        ;; Update advertiser stats
        (let
            ((advertiser-stats (default-to 
                {total-ads: u0, total-stake: u0, reputation-score: u100}
                (map-get? AdvertiserStats tx-sender))))
            
            (map-set AdvertiserStats tx-sender
                {
                    total-ads: (+ (get total-ads advertiser-stats) u1),
                    total-stake: (+ (get total-stake advertiser-stats) final-bid),
                    reputation-score: (get reputation-score advertiser-stats)
                }
            )
        )
        
        ;; Update auto-bidder current bid if applicable
        (match auto-bidder
            bidder-info (map-set AutoBidders tx-sender
                (merge bidder-info {current-bid: final-bid, last-bid-time: stacks-block-height}))
            true
        )
        
        (var-set total-ads new-ad-id)
        (try! (update-market-conditions))
        (ok new-ad-id)
    )
)

;; Get market insights and analytics
(define-read-only (get-market-analytics)
    (let
        ((current-conditions (map-get? MarketConditions (var-get current-market-period)))
         (market-metrics (map-get? MarketMetrics u1))
         (current-price (unwrap! (get-current-market-price) err-invalid-amount)))
        
        (ok {
            current-price: current-price,
            time-multiplier: (get-time-based-multiplier),
            demand-multiplier: (get-demand-multiplier),
            market-conditions: current-conditions,
            market-metrics: market-metrics,
            total-autobidders: (var-get total-autobidders),
            revenue-pool: (var-get market-revenue-pool)
        })
    )
)

;; Disable automatic bidding
(define-public (disable-auto-bidding)
    (let
        ((auto-bidder (unwrap! (map-get? AutoBidders tx-sender) err-not-found)))
        
        (map-set AutoBidders tx-sender
            (merge auto-bidder {auto-bidding-active: false}))
        
        (var-set total-autobidders (- (var-get total-autobidders) u1))
        (ok true)
    )
)

;; Get price history for analysis
(define-read-only (get-price-history (start-period uint) (end-period uint))
    (let
        ((period-range (- end-period start-period)))
        
        (asserts! (<= period-range u20) err-invalid-amount) ;; Limit to 20 periods max
        (ok {
            start-period: start-period,
            end-period: end-period,
            sample-data: (map-get? PriceHistory start-period)
        })
    )
)

;; Get auto-bidder information
(define-read-only (get-auto-bidder-info (advertiser principal))
    (ok (map-get? AutoBidders advertiser))
)

(define-constant reputation-bronze-threshold u50)
(define-constant reputation-silver-threshold u100)
(define-constant reputation-gold-threshold u200)
(define-constant reputation-platinum-threshold u500)

(define-constant bronze-discount u100)
(define-constant silver-discount u200)
(define-constant gold-discount u350)
(define-constant platinum-discount u500)

(define-map ReputationTiers
    principal
    {
        tier: (string-ascii 10),
        tier-level: uint,
        discount-amount: uint,
        last-updated: uint
    }
)

(define-public (calculate-reputation-tier (advertiser principal))
    (let
        ((stats (default-to 
            {total-ads: u0, total-stake: u0, reputation-score: u100}
            (map-get? AdvertiserStats advertiser)))
         (tier-info
            (if (>= (get reputation-score stats) reputation-platinum-threshold)
                {tier: "platinum", tier-level: u4, discount-amount: platinum-discount}
                (if (>= (get reputation-score stats) reputation-gold-threshold)
                    {tier: "gold", tier-level: u3, discount-amount: gold-discount}
                    (if (>= (get reputation-score stats) reputation-silver-threshold)
                        {tier: "silver", tier-level: u2, discount-amount: silver-discount}
                        (if (>= (get reputation-score stats) reputation-bronze-threshold)
                            {tier: "bronze", tier-level: u1, discount-amount: bronze-discount}
                            {tier: "none", tier-level: u0, discount-amount: u0}
                        )
                    )
                )
            )
         )
        )
        
        (map-set ReputationTiers advertiser
            (merge tier-info {last-updated: stacks-block-height}))
        
        (ok (get tier-level tier-info))
    )
)

(define-read-only (get-minimum-stake-for-advertiser (advertiser principal))
    (let
        ((tier-info (default-to 
            {tier: "none", tier-level: u0, discount-amount: u0, last-updated: u0}
            (map-get? ReputationTiers advertiser))))
        
        (ok (- minimum-stake-amount (get discount-amount tier-info)))
    )
)

(define-read-only (get-advertiser-tier (advertiser principal))
    (ok (map-get? ReputationTiers advertiser))
)

(define-public (update-advertiser-reputation (advertiser principal) (performance-bonus uint))
    (let
        ((current-stats (default-to 
            {total-ads: u0, total-stake: u0, reputation-score: u100}
            (map-get? AdvertiserStats advertiser)))
         (new-reputation (+ (get reputation-score current-stats) performance-bonus)))
        
        (map-set AdvertiserStats advertiser
            (merge current-stats {reputation-score: new-reputation}))
        
        (unwrap! (calculate-reputation-tier advertiser) err-not-found)
        (ok true)
    )
)

(define-public (create-advertisement-with-tier (title (string-ascii 50)) (content (string-ascii 200)) (stake-amount uint))
    (let
        ((new-ad-id (+ (var-get total-ads) u1))
         (advertiser-stats (default-to 
            {total-ads: u0, total-stake: u0, reputation-score: u100}
            (map-get? AdvertiserStats tx-sender)))
         (minimum-required (unwrap! (get-minimum-stake-for-advertiser tx-sender) err-invalid-amount)))
        
        (asserts! (>= stake-amount minimum-required) err-invalid-amount)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        (map-set Advertisements new-ad-id
            {
                advertiser: tx-sender,
                title: title,
                content: content,
                stake-amount: stake-amount,
                total-views: u0,
                total-clicks: u0,
                active: true,
                created-at: stacks-block-height
            }
        )

        (map-set AdvertiserStats tx-sender
            {
                total-ads: (+ (get total-ads advertiser-stats) u1),
                total-stake: (+ (get total-stake advertiser-stats) stake-amount),
                reputation-score: (get reputation-score advertiser-stats)
            }
        )

        (var-set total-ads new-ad-id)
        (ok new-ad-id)
    )
)



