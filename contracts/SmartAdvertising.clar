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


