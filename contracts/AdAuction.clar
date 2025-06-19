(define-constant err-auction-not-found (err u200))
(define-constant err-auction-ended (err u201))
(define-constant err-auction-active (err u202))
(define-constant err-bid-too-low (err u203))
(define-constant err-not-winner (err u204))
(define-constant minimum-auction-duration u144)
(define-constant minimum-bid-increment u100)

(define-data-var total-auctions uint u0)

(define-map Auctions
    uint
    {
        slot-id: uint,
        creator: principal,
        start-block: uint,
        end-block: uint,
        minimum-bid: uint,
        current-winner: (optional principal),
        winning-bid: uint,
        total-bids: uint,
        finalized: bool
    }
)

(define-map AuctionBids
    {auction-id: uint, bidder: principal}
    {
        bid-amount: uint,
        bid-time: uint,
        refunded: bool
    }
)

(define-map UserAuctionHistory
    principal
    {
        auctions-won: uint,
        total-bid-amount: uint,
        active-bids: uint
    }
)

(define-public (create-auction (slot-id uint) (duration uint) (minimum-bid uint))
    (let
        (
            (auction-id (+ (var-get total-auctions) u1))
            (end-block (+ stacks-block-height duration))
        )
        (asserts! (>= duration minimum-auction-duration) err-auction-active)
        (asserts! (> minimum-bid u0) err-bid-too-low)
        
        (map-set Auctions auction-id
            {
                slot-id: slot-id,
                creator: tx-sender,
                start-block: stacks-block-height,
                end-block: end-block,
                minimum-bid: minimum-bid,
                current-winner: none,
                winning-bid: u0,
                total-bids: u0,
                finalized: false
            }
        )
        
        (var-set total-auctions auction-id)
        (ok auction-id)
    )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let
        (
            (auction (unwrap! (map-get? Auctions auction-id) err-auction-not-found))
            (user-history (default-to 
                {auctions-won: u0, total-bid-amount: u0, active-bids: u0}
                (map-get? UserAuctionHistory tx-sender)))
            (required-bid (+ (get winning-bid auction) minimum-bid-increment))
        )
        (asserts! (< stacks-block-height (get end-block auction)) err-auction-ended)
        (asserts! (not (get finalized auction)) err-auction-ended)
        (asserts! (>= bid-amount (get minimum-bid auction)) err-bid-too-low)
        (asserts! (>= bid-amount required-bid) err-bid-too-low)
        
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        (match (get current-winner auction)
            previous-winner
            (begin
                (try! (as-contract (stx-transfer? (get winning-bid auction) tx-sender previous-winner)))
                (map-set AuctionBids {auction-id: auction-id, bidder: previous-winner}
                    (merge 
                        (unwrap! (map-get? AuctionBids {auction-id: auction-id, bidder: previous-winner}) err-auction-not-found)
                        {refunded: true}
                    )
                )
            )
            true
        )
        
        (map-set AuctionBids {auction-id: auction-id, bidder: tx-sender}
            {
                bid-amount: bid-amount,
                bid-time: stacks-block-height,
                refunded: false
            }
        )
        
        (map-set Auctions auction-id
            (merge auction {
                current-winner: (some tx-sender),
                winning-bid: bid-amount,
                total-bids: (+ (get total-bids auction) u1)
            })
        )
        
        (map-set UserAuctionHistory tx-sender
            (merge user-history {
                total-bid-amount: (+ (get total-bid-amount user-history) bid-amount),
                active-bids: (+ (get active-bids user-history) u1)
            })
        )
        
        (ok true)
    )
)

(define-public (finalize-auction (auction-id uint))
    (let
        (
            (auction (unwrap! (map-get? Auctions auction-id) err-auction-not-found))
        )
        (asserts! (>= stacks-block-height (get end-block auction)) err-auction-active)
        (asserts! (not (get finalized auction)) err-auction-ended)
        
        (match (get current-winner auction)
            winner
            (let
                (
                    (winner-history (default-to 
                        {auctions-won: u0, total-bid-amount: u0, active-bids: u0}
                        (map-get? UserAuctionHistory winner)))
                )
                (map-set UserAuctionHistory winner
                    (merge winner-history {
                        auctions-won: (+ (get auctions-won winner-history) u1),
                        active-bids: (- (get active-bids winner-history) u1)
                    })
                )
            )
            true
        )
        
        (map-set Auctions auction-id
            (merge auction {finalized: true})
        )
        
        (ok true)
    )
)

(define-public (claim-auction-slot (auction-id uint))
    (let
        (
            (auction (unwrap! (map-get? Auctions auction-id) err-auction-not-found))
        )
        (asserts! (get finalized auction) err-auction-active)
        (asserts! (is-eq (some tx-sender) (get current-winner auction)) err-not-winner)
        
        (ok {
            slot-id: (get slot-id auction),
            winning-bid: (get winning-bid auction),
            duration: (- (get end-block auction) (get start-block auction))
        })
    )
)

(define-read-only (get-auction-details (auction-id uint))
    (ok (unwrap! (map-get? Auctions auction-id) err-auction-not-found))
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
    (ok (unwrap! (map-get? AuctionBids {auction-id: auction-id, bidder: bidder}) err-auction-not-found))
)

(define-read-only (get-user-auction-history (user principal))
    (ok (unwrap! (map-get? UserAuctionHistory user) err-auction-not-found))
)

(define-read-only (get-active-auctions-count)
    (ok (var-get total-auctions))
)

(define-read-only (is-auction-active (auction-id uint))
    (let
        (
            (auction (unwrap! (map-get? Auctions auction-id) err-auction-not-found))
        )
        (ok (and 
            (< stacks-block-height (get end-block auction))
            (not (get finalized auction))
        ))
    )
)

(define-read-only (get-auction-time-remaining (auction-id uint))
    (let
        (
            (auction (unwrap! (map-get? Auctions auction-id) err-auction-not-found))
        )
        (ok (if (> (get end-block auction) stacks-block-height)
            (- (get end-block auction) stacks-block-height)
            u0
        ))
    )
)